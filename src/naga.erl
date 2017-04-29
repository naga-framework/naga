-module(naga).
-description('NAGA OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1, watch/1, unwatch/1]).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

mode()     -> wf:config(naga,mode,prod).
tables()   -> [ ?MODULE ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> supervisor:start_link({local,naga},naga,[]).
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              { ok, { { one_for_one, 5, 10 }, 
	                [	                  
                    ?CHILD(naga_load, worker, [apps()])
	                ] 
              }}.

trans(_,X,undefined) -> X;
trans(A,X,L)         -> case naga_lang:lookup(A,{wf:to_list(L),X}) of
                          undefined -> {M,F} = wf:config(A,i18n_undefined,{naga_mvc,i18n_undefined}),M:F(X);
                          E -> E end.

stop(Apps) when is_list(Apps) -> [stop(App)||App<-Apps];
stop(App)  -> case lists:member(App, wf:config(naga,watch,[])) of 
                true  -> Listeners = wf:config(App,listeners,[]),
                         [begin
                            IpType = ip_type(ip(O)),
                            Port = port(O),
                            Ref = listener_name(X,App,IpType,Port),
                            error_logger:info_msg("stoping ~p:~s", 
                              [App, begin [P,_] = string:tokens(wf:to_list(Ref),"_"), P end]),
                            cowboy:stop_listener(Ref)
                          end || {X,O} <-Listeners],
                          application:stop(App), ok;
                false -> ok 
              end.

start(App) when is_atom(App) -> start([App]);
start(Apps) -> [case protoOpts(mode(),App) of
                 {error, Err} -> error_logger:error_msg("Cannot Start Listener (~p)"
                                          " for reason: ~p~n",[App,Err]),{App, Err};
                  ProtoOpts -> start_listeners(App, ProtoOpts) end || App <- Apps].

%middlewares(prod) -> wf:config(naga,middlewares,[cowboy_router,cowboy_handler]);
middlewares()  -> wf:config(naga,middlewares,[naga_router,cowboy_handler]).

protoOpts(Mode,App) ->
  case naga_router:dispatch_routes(App) of
    {error,_} = Err -> Err;
    {ok, App, Modules, Components, DispatchModule, N, Rules} ->
      naga_load:print(App, DispatchModule, Rules),
      _AppsInfo = boot_apps(Components),
      [{env,[{application, {App,Modules}} 
            %,{appsInfo, AppsInfo}                       
             ,{dispatch, DispatchModule}]}
             ,{middlewares, middlewares()}]
  end.

watch(App)        -> naga_load:watch(App).
unwatch(App)      -> naga_load:unwatch(App).
apps()            -> wf:config(naga,watch,[]).

ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;{error, _} -> false end.

get_dispatch(App) ->
   D = lists:foldr(fun(Rule,Acc)-> 
                [{_,Rules}] = ets:lookup(?MODULE,{App,Rule}),
                Rules ++ Acc
               end,[], [routes, view, doc, mvc]),
   lists:foldr(fun(Domain,Bcc) -> [{Domain, lists:flatten(D)}] ++ Bcc end, [], naga_router:domains(App)).

print_dispatch(App) ->
  case naga_router:dispatch_routes(App) of
    {ok, _, _, _, DispatchModule, _, Rules} -> 
      naga_load:print(App, DispatchModule, Rules);
    _ -> skip end.

match(App,Path) when is_list(Path)-> 
  Url = lists:map(fun wf:to_binary/1,filename:split(Path)),
  match1(App, Url).

match1(App,[<<"/">>|Path]) -> match1(App,Path);
match1(App,Path) ->
 DispatchModule = naga_router:module_dispatch_name(App),
 DispatchModule:match(Path,undefined).



start_listeners(App, ProtoOpts) ->
    case wf:config(App, listeners, []) of
        [] -> wf:info(?MODULE, "listeners notfound for app ~p []",[App]),            
              skip;
        Listeners ->
              listener(App, Listeners, ProtoOpts, []) end.

ip(O) -> case proplists:get_value(ip, O, {0, 0, 0, 0}) of
          {_,_,_,_} =Ipv4 -> Ipv4;
          {_,_,_,_,_,_,_,_}=Ipv6 -> Ipv6;
          Val when is_list(Val) -> 
            case inet:parse_address(Val) of 
              {ok,Ip} -> Ip; 
              Err -> Err end end.

port(O) -> case proplists:get_value(port, O, ?DEFAULT_HTTP_PORT) of
            P when is_integer(P),
                   P >= 0, P =< 65535 -> P; 
            _ -> {error, invalid_port} end.

ip_type(ipv4) -> ipv4;
ip_type(ipv6) -> ipv6;
ip_type({ok,{_,_,_,_}}) -> ipv4;
ip_type({ok,{_,_,_,_,_,_,_,_}}) -> ipv6;
ip_type({_,_,_,_}) -> ipv4;
ip_type({_,_,_,_,_,_,_,_}) -> ipv6;
ip_type(Ip) when is_list(Ip)-> 
  ip_type(inet:parse_address(Ip));
ip_type(_) -> {error,unknow_ip_type}.

ipv4_to_ipv6({A,B,C,D}) -> 
  Ip = wf:to_list("~B.~B.~B.~B",[A,B,C,D]),
  inet:parse_address("FFFF::" ++ Ip).

dft_port(Ip,Dft) -> 
  dft_port(ip_type(Ip),Dft).

abort({error,_}=Err,Fmt) -> io:format(Fmt,[Err]),halt(abort,[]);
abort(Val,_) -> Val.

listener_name(Type,App) -> wf:atom([Type,App]).
listener_name(Type,App,Ip,Port) -> wf:atom([Type,App,ip_type(Ip),Port]).

listener(_,[], _,Acc)   -> Acc;
listener(App, [{Proto, Opts}|T], ProtoOpts, Acc) ->
    Ip          = abort(ip(Opts),   "Invalid ip format ~p\r\n"), 
    IpType      = abort(ip_type(Ip),"Invalid ip type ~p\r\n"),
    Port        = abort(port(Opts), "Invalid port ~p\r\n"),
    Listener    = listener_name(Proto,App,IpType,Port),
    NbAcceptors = proplists:get_value(acceptors, Opts, ?DEFAULT_ACCEPTOR_PROCESSES),
    SslOpts     = case (Proto == https) or (Proto == spdy) of false -> [] ;
                       true -> proplists:get_value(ssl_opts, Opts, ?DEFAULT_SSL_OPTS) end,
    TransOpts   = [{ip, Ip},{port, Port}|SslOpts],
    Start       = wf:atom([start,Proto]),
    case cowboy:Start(Listener, NbAcceptors, TransOpts, ProtoOpts) of
        {ok, Pid} -> io:format(green("~p: Starting ~s server ~p:~p (~p)~n"),[App, Proto, Ip, Port, Listener]),
                     listener(App, T, ProtoOpts, [{Listener, Pid, TransOpts, ProtoOpts}|Acc]); 
        {error,_} = Err -> abort(Err,"Can't start Web Server: ~p\r\n")
    end.

red(S)         -> lists:concat(["\e[31m",S,"\e[0m"]).
green(S)       -> lists:concat(["\e[32m",S,"\e[0m"]).
yellow(S)      -> lists:concat(["\e[33m",S,"\e[0m"]).
blue(S)        -> lists:concat(["\e[34m",S,"\e[0m"]).
magenta(S)     -> lists:concat(["\e[35m",S,"\e[0m"]).
light_green(S) -> lists:concat(["\e[92m",S,"\e[0m"]).
light_yellow(S)-> lists:concat(["\e[93m",S,"\e[0m"]).
light_blue(S)  -> lists:concat(["\e[94m",S,"\e[0m"]).
light_gray(S)  -> lists:concat(["\e[97m",S,"\e[0m"]).
default(S)     -> lists:concat(["\e[39m",S,"\e[0m"]).
blink(S)       -> lists:concat(["\e[5m",S,"\e[25m"]).
inverted(S)    -> lists:concat(["\e[7m",S,"\e[27m"]).


boot_apps(Apps)         -> boot_app(Apps, []).
boot_app([], AppsInfo)  -> AppsInfo;
boot_app([App|T], Acc)  -> {ok, Modules} = application:get_key(App,modules), 
                           [code:ensure_loaded(M)||M<-Modules],
                           AppInfo = #{                                       
                                       locale         => naga_router:locale(App),
                                       base_url       => naga_router:base_url(App),
                                       static_prefix  => naga_router:static_prefix(App),
                                       doc_prefix     => naga_router:doc_prefix(App),
                                       rest_prefix    => naga_router:rest_prefix(App),
                                       n2o_prefix     => naga_router:n2o_prefix(App),
                                       %fcgi_prefix    => fcgi_prefix(App),
                                       %fcgi_opts      => boot_fcgi(App),
                                       domains        => naga_router:domains(App)
                                      },
                           boot_app(T, [{App, AppInfo}|Acc]).

to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

dateformat()            -> erlydtl_dateformat:format("r").
dateformat(Format)      -> erlydtl_dateformat:format(Format).
dateformat(Date,Format) -> erlydtl_dateformat:format(Date,Format).

urlencode(Path,Params)  -> Qs= cow_qs:qs([{wf:to_binary(K),wf:to_binary(V)}||{K,V}<-Params]),
                           P = wf:to_binary(Path),<<P/binary,$?,Qs/binary>>.
%urldecode(U)            -> cow_qs:urldecode(U).
urldecode(U)            -> {P,Qs} = cow_http:parse_fullpath(U),
                           {P,[{wf:to_atom(K),V}||{K,V}<-cow_qs:parse_qs(Qs)]}.

%%FIXME: reverse routing
location(L,Ctx) when is_binary(L) -> L;
location(L,Ctx) when is_list(L) -> wf:to_binary(L); 
location(#{app:=App,controller:=C,action:=A}=L,_) -> P=maps:get(path_info,L),Path = path(App,C,A,P), naga:urlencode(Path,maps:get(params,L,[]));
location(#{         controller:=C,action:=A}=L,#{'_application':=App}) -> P=maps:get(path_info,L),Path = path(App,C,A,P), naga:urlencode(Path,maps:get(params,L,[]));
location(#{                       action:=A}=L,#{'_application':=App, '_controller':=C}) -> P=maps:get(path_info,L),Path = path(App,C,A,P), naga:urlencode(Path,maps:get(params,L,[])).

path(App,M,A,P) -> 
 Path = naga_router:base_url(wf:to_atom(App),string:join(["/",naga_router:sub(wf:to_list(App),wf:to_list(M)),"/",wf:to_list(A)],"")),
 string:join([Path, string:join([wf:to_list(X)||X<-P],"/")],"/").

% post param

post_params()   -> ?CTX#cx.form.  %% multipart | list().
post_param(K)   -> proplists:get_value(wf:to_binary(K),?CTX#cx.form).
post_param(K,D) -> proplists:get_value(wf:to_binary(K),?CTX#cx.form,D).

%%FIXME: ?
%% Get the value of a given "deep" POST parameter. 
%% This function parses parameters that have numerical 
%% or labeled indices, such as "widget[4][name]", 
%% and returns either a value or a set of nested lists 
%% (for numerical indices) and proplists (for string indices).
deep_post_param() ->
    Params = post_params(),
    parse_deep_post_params(Params, []).

deep_post_param(Path) ->
    find_deep_post_param(Path, deep_post_param()).

deep_post_param(Path,Params) ->
    Parsed = parse_deep_post_params(Params, []),
    find_deep_post_param(Path, Parsed).

find_deep_post_param([], Params) ->
    Params;
find_deep_post_param([Index|Rest], Params) when is_integer(Index) ->
    find_deep_post_param(Rest, lists:nth(Index, Params));
find_deep_post_param([Index|Rest], Params) when is_list(Index) ->
    find_deep_post_param(Rest, proplists:get_value(Index, Params)).

parse_deep_post_params([], Acc) ->
    Acc;
parse_deep_post_params([{Key, Value}|Rest], Acc) ->
    case re:run(Key, "^(\\w+)(?:\\[([\\w-\\[\\]]+)\\])?$", [{capture, all_but_first, list}]) of
        {match, [_]} ->
            parse_deep_post_params(Rest, [{Key, Value}|Acc]);
        {match, [KeyName, Path]} ->
            PathList = re:split(Path, "\\]\\[", [{return, list}]),
            parse_deep_post_params(Rest, insert_into(Acc, [KeyName|PathList], Value))
    end.

insert_into(_List, [], Value) ->
    Value;
insert_into(undefined, PathList, Value) ->
    insert_into([], PathList, Value);
insert_into(N, PathList, Value) when is_integer(N) ->
    insert_into([], PathList, Value);
insert_into(List, [ThisKey|Rest], Value) ->
    case catch list_to_integer(ThisKey) of
        {'EXIT', _} ->
            ExistingVal = proplists:get_value(ThisKey, List),
            [{ThisKey, insert_into(ExistingVal, Rest, Value)}|
                proplists:delete(ThisKey, List)];
        N when N < erlang:length(List) ->
            ExistingVal = lists:nth(N+1, List),
            lists:sublist(List, N) ++ [insert_into(ExistingVal, Rest, Value)|
                lists:nthtail(N+1, List)];
        N when N >= erlang:length(List) ->
            List ++ lists:reverse([insert_into(undefined, Rest, Value)|
                    lists:seq(0, N - erlang:length(List) - 1)])
    end.

to_seconds()  -> to_seconds(calendar:local_time()).
to_seconds(D) -> calendar:datetime_to_gregorian_seconds(D).
to_time(S)    -> calendar:gregorian_seconds_to_datetime(S).

expired(Issued) -> Issued < calendar:local_time().

expire_in(Issued) -> 
  calendar:time_difference(calendar:local_time(),Issued).

expire(SecondsToLive) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
    cow_date:rfc2109(DateTime).

till(TTL) -> till(calendar:local_time(),TTL).
till(Now,TTL) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + TTL).
