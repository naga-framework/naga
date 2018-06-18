-module(naga).
-description('NAGA OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1, watch/1, unwatch/1]).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

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

cap(A,P)   -> string:to_upper(wf:to_list(A)) ++ "_" ++ string:to_upper(wf:to_list(P)).  
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

stop(Apps) when is_list(Apps) -> [stop(App)||App<-Apps];
stop(App)  -> case lists:member(App, wf:config(naga,watch,[])) of 
                true  -> Listeners = wf:config(App,listeners,[]),
                         [begin
                            IpType = ip_type(ip(cap(App,X),O)),
                            Port   = port(cap(App,X),O),
                            Ref    = listener_name(X,App,IpType,Port),
                            error_logger:info_msg("stoping ~p:~s", 
                              [App, begin [P,_] = string:tokens(wf:to_list(Ref),"_"), P end]),
                            cowboy:stop_listener(Ref)
                          end || {X,O} <-Listeners],
                          application:stop(App), ok;
                false -> ok 
              end.

start(App) when is_atom(App) -> start([App]);
start(Apps) -> [case protoOpts(mode(),App) of
                 {error, Err} -> 
                  error_logger:error_msg("Cannot Start Listener (~p)"
                                         " for reason: ~p~n",[App,Err]),
                  {App, Err};
                 ProtoOpts -> 
                  start_listeners(App, ProtoOpts) %?
                end || App <- Apps].

protoOpts(Mode,App) ->
  case naga_router:dispatch_routes(App) of
    {error,_} = Err -> Err;
    {ok, App, Modules, Components, DispatchModule, N, Rules} ->
      naga_load:print(App, DispatchModule, Rules),
      _AppsInfo = boot_apps(Components),
      [{env,[{application, {App,Modules}} 
             %,{appsInfo, AppsInfo} 
             ,{websocket_port, wsport(App)}                      
             ,{dispatch, DispatchModule}]}
             ,{middlewares, middlewares()}]
  end.

start_listeners(App, ProtoOpts) -> 
  case wf:config(App, listeners, []) of
    [] -> wf:info(?MODULE, "listeners notfound for app ~p []",[App]),            
          skip;
    Listeners ->
          listener(App, Listeners, ProtoOpts, []) 
  end.

wsport(App) ->
    WsPortEnv = cap(App,websocket_port),
    case os:getenv(WsPortEnv) of 
     false  -> wf:config(App,websocket_port,?DEFAULT_HTTP_PORT); 
     WsPort -> io:format(blue("~s ~p~n"),[WsPortEnv,WsPort]),
               %application:set_env(n2o,websocket_port,),
               application:set_env(App,websocket_port,list_to_integer(WsPort)),
               io:format(magenta("~s ~p~n"),[WsPortEnv,wf:config(App,websocket_port)]),
               WsPort
    end.
listener(_,[], _,Acc)   -> Acc;
listener(App, [{Proto0, Opts}|T], ProtoOpts, Acc) ->
    Proto       = case os:getenv(cap(App,"PROTO")) of false -> Proto0; E -> E end,
    Ip          = abort(ip(cap(App,Proto),Opts),  "Invalid ip format ~p\r\n"), 
    IpType      = abort(ip_type(Ip),              "Invalid ip type ~p\r\n"),
    Port        = abort(port(cap(App,Proto),Opts),"Invalid port ~p\r\n"),
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


parse_ip(S)    -> case inet:parse_address(S) of 
                    {ok,Ip} -> Ip; 
                    Err -> Err 
                  end.

trans(_,X,undefined)            -> X;
trans(A,X,L)                    -> case naga_lang:lookup(A,{wf:to_list(L),X}) of
                                     undefined -> 
                                      {M,F} = wf:config(A,i18n_undefined,{naga_mvc,i18n_undefined}),
                                      M:F(X);
                                     E -> E 
                                   end.
env(E)                          -> Env = wf:f("~s",[E]),
                                   Val = os:getenv(Env),
                                   io:format(red("~s ~p~n"),[Env,Val]),
                                   Val.                                   
ip(E,O)                         -> Ip = case proplists:get_value(ip,O,{127,0,0,1}) of
                                          {_,_,_,_}=Ipv4         -> Ipv4;
                                          {_,_,_,_,_,_,_,_}=Ipv6 -> Ipv6;
                                          Val when is_list(Val)  -> parse_ip(Val)
                                        end,
                                   case env(cap(E,"IP")) of false -> Ip;V -> parse_ip(V) end.

port(E,O)                       -> Port = case proplists:get_value(port,O,?DEFAULT_HTTP_PORT) of
                                            P when is_integer(P),
                                                   P >= 0, P =< 65535 -> P; 
                                            _ -> {error, invalid_port} 
                                          end,
                                   case env(cap(E,"PORT")) of 
                                      false -> Port;V -> list_to_integer(V) 
                                   end.

%middlewares(prod) -> wf:config(naga,middlewares,[cowboy_router,cowboy_handler]);
middlewares()                   -> wf:config(naga,middlewares,[naga_router,cowboy_handler]).
watch(App)                      -> naga_load:watch(App).
unwatch(App)                    -> naga_load:unwatch(App).
apps()                          -> wf:config(naga,watch,[]).
post_params()                   -> ?CTX#cx.form.  %% multipart | list().
post_param(K)                   -> proplists:get_value(wf:to_binary(K),?CTX#cx.form).
post_param(K,D)                 -> proplists:get_value(wf:to_binary(K),?CTX#cx.form,D).
dft_port(Ip,Dft)                -> dft_port(ip_type(Ip),Dft).
abort({error,_}=Err,Fmt)        -> io:format(Fmt,[Err]),halt(abort,[]);
abort(Val,_)                    -> Val.
listener_name(Type,App)         -> wf:atom([Type,App]).
listener_name(Type,App,Ip,Port) -> wf:atom([Type,App,ip_type(Ip),Port]).
ip_type(ipv4)                   -> ipv4;
ip_type(ipv6)                   -> ipv6;
ip_type({ok,{_,_,_,_}})         -> ipv4;
ip_type({ok,{_,_,_,_,_,_,_,_}}) -> ipv6;
ip_type({_,_,_,_})              -> ipv4;
ip_type({_,_,_,_,_,_,_,_})      -> ipv6;
ip_type(Ip) when is_list(Ip)    -> ip_type(inet:parse_address(Ip));
ip_type(_)                      -> {error,unknow_ip_type}.
ipv4_to_ipv6({A,B,C,D})         -> Ip = wf:to_list("~B.~B.~B.~B",[A,B,C,D]),
                                   inet:parse_address("FFFF::" ++ Ip).
ipv6_supported()                -> case (catch inet:getaddr("localhost", inet6)) of
                                   {ok, _Addr} -> true;{error, _} -> false end.
get_dispatch(App)               -> D = lists:foldr(fun(Rule,Acc)-> 
                                      [{_,Rules}] = ets:lookup(?MODULE,{App,Rule}),
                                      Rules ++ Acc
                                    end,[], [routes, view, doc, mvc]),
                                   lists:foldr(fun(Domain,Bcc) -> 
                                                 [{Domain, lists:flatten(D)}] ++ Bcc 
                                               end, [], naga_router:domains(App)).
print_dispatch(App)             -> case naga_router:dispatch_routes(App) of
                                    {ok, _, _, _, DispatchModule, _, Rules} -> 
                                      naga_load:print(App, DispatchModule, Rules);
                                    _ -> skip end.
match(App,Path)
            when is_list(Path)  -> Url = lists:map(fun wf:to_binary/1,filename:split(Path)),
                                   match1(App, Url).
match1(App,[<<"/">>|Path])      -> match1(App,Path);
match1(App,Path)                -> DispatchModule = naga_router:module_dispatch_name(App),
                                   DispatchModule:match(Path,undefined).

to_seconds()  -> to_seconds(calendar:local_time()).
to_seconds(D) -> calendar:datetime_to_gregorian_seconds(D).
to_time(S)    -> calendar:gregorian_seconds_to_datetime(S).

expired(Issued)   -> Issued < calendar:local_time().
expire_in(Issued) -> calendar:time_difference(calendar:local_time(),Issued).

expire(SecondsToLive) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
    cow_date:rfc2109(DateTime).

till(TTL) -> till(calendar:local_time(),TTL).
till(Now,TTL) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + TTL).
