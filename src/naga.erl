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

stop(Apps) when is_list(Apps) -> [stop(App)||App<-Apps];
stop(App)  -> case lists:member(App, wf:config(naga,watch,[])) of 
                true  -> Listeners = wf:config(App,listeners,[]),
                         [begin
                            Ref = listener_name(X, App),
                            wf:info(?MODULE, "stoping ~p:~s", 
                              [App, begin [P,_] = string:tokens(wf:to_list(Ref),"_"), P end]),
                            cowboy:stop_listener(Ref)
                          end || {X,_} <-Listeners],
                          application:stop(App), ok;
                false -> ok 
              end.

trans(_,X,undefined) -> X;
trans(A,X,L)         -> case naga_lang:lookup(A,{wf:to_list(L),X}) of
                          undefined -> {M,F} = wf:config(A,i18n_undefined,{naga_mvc,i18n_undefined}),M:F(X);
                          E -> E end.

start(App) when is_atom(App) -> start([App]);
start(Apps) -> [start_listeners(App, protoOpts(mode(),App)) || App <- Apps].

protoOpts(prod,App) -> Modules = wf:config(App,modules,[]),
                       Components = [App] ++ Modules,
                       DispatchApps = read_dump_file(Components),
                       io:format("MODE PROD ~p~n",[DispatchApps]),
                       _AppsInfo = boot_apps(Components),
                       [{env,[{application, {App,Modules}} %,{appsInfo, AppsInfo}                         
                             ,{dispatch, cowboy_router:compile(DispatchApps)}]}
                             ,{middlewares, wf:config(naga,middlewares,[cowboy_router,cowboy_handler])}];

protoOpts(dev ,App) -> Modules = wf:config(App,modules,[]),
                       Components = [App] ++ Modules,
                       DispatchApps = dispatch({dev,Components}),
                       ets:insert(?MODULE,{{App,rules},DispatchApps}),
                       ets:insert(?MODULE,{{App,dispatch},cowboy_router:compile(DispatchApps)}),
                       io:format("MODE DEV ~p~n",[DispatchApps]),
                       AppsInfo = boot_apps(Components),
                       ets:insert(?MODULE,{apps_info,AppsInfo}),
                       [{env,[{application, {App,Modules}}                         
                             ,{dispatch, []}]}
                             ,{middlewares, wf:config(naga,middlewares,[naga_router,cowboy_handler])}].
get_dispatch(App) ->
   D = lists:foldr(fun(Rule,Acc)-> 
                [{_,Rules}] = ets:lookup(?MODULE,{App,Rule}),
                Rules ++ Acc
               end,[], [routes, view, doc, mvc]),
   lists:foldr(fun(Domain,Bcc) -> [{Domain, lists:flatten(D)}] ++ Bcc end, [], domains(App)).

put_dispatch(App,Modules,Rule) ->
  Rules = dispatch(Rule,order([App]++Modules)),  
  ets:insert(?MODULE,{{App,Rule},Rules}),
  Rules.

%%lists of rules for each modules.


dispatch({dev,Components}) -> 
      App = hd(Components),
      Rules = [routes, view, doc, mvc],
      D = lists:foldr(fun(Rule,Acc)-> 
                        R = dispatch(Rule,order(Components)),
                        %wf:info(?MODULE,"~p:~p:~p",[App,Rule,R]),
                        ets:insert(?MODULE,{{App,Rule},R}),
                        R ++ Acc 
                      end,[],Rules),
      lists:foldr(fun(Domain,Bcc) -> [{Domain, lists:flatten(D)}] ++ Bcc end, [], domains(App));

dispatch(Components) -> 
      App = hd(Components),
      Rules = [routes, view, doc, mvc],
      D = lists:foldr(fun(Rule,Acc)-> 
                        dispatch(Rule,order(Components)) ++ Acc 
                      end,[],wf:config(App,rules,Rules)),
      lists:foldr(fun(Domain,Bcc) -> [{Domain, lists:flatten(D)}] ++ Bcc end, [], domains(App)).

order(A) -> [Y||{_,Y} <-lists:reverse(lists:ukeysort(1,[{wf:config(X,base_url,[]),X}||X<-A]))].
            %%order by prefix

start_listeners(App, ProtoOpts) ->
    case wf:config(App, listeners, []) of
        [] -> wf:info(?MODULE, "listeners notfound for app ~p []",[App]),            
              skip;
        Listeners ->
              listener(App, Listeners, ProtoOpts, []) end.

listener_name(Type,App) -> wf:atom([Type,App]).

listener(_,[], _,Acc)   -> Acc;
listener(App, [{Proto, Opts}|T], ProtoOpts, Acc) ->
    Listener    = listener_name(Proto,App),
    Ip          = proplists:get_value(ip, Opts, {0, 0, 0, 0}),
    Port        = proplists:get_value(port, Opts, ?DEFAULT_HTTP_PORT),
    NbAcceptors = proplists:get_value(acceptors, Opts, ?DEFAULT_ACCEPTOR_PROCESSES),
    SslOpts     = case (Proto == https) or (Proto == spdy) of false -> [] ;
                       true -> proplists:get_value(ssl_opts, Opts, ?DEFAULT_SSL_OPTS) end,
    TransOpts   = [{ip, Ip},{port, Port}|SslOpts],
    Start       = wf:atom([start,Proto]),
    case cowboy:Start(Listener, NbAcceptors, TransOpts, ProtoOpts) of
        {ok, Pid} -> wf:info(?MODULE, "starting ~s server ~p at ~p:~p",[Proto, App, Ip,Port]),
                     listener(App, T, ProtoOpts, [{Listener, Pid, TransOpts, ProtoOpts}|Acc]); 
        {error,_} = Err -> 
            io:format("Can't start Web Server: ~p ~p\r\n",[Err, {App, Ip, Port}]);
        X -> 
            io:format("Unknown Error: ~p\r\n",[X]), halt(abort,[])
    end.

module_info(M,T)  -> case catch M:module_info(T) of 
                      {'EXIT', Err} -> io:format("Error ~p~n",[Err]),[]; E -> E end.

sep()             -> "/". %%FIXME: linux/unix/macosx ok, windows?
apps()            -> wf:config(naga,watch,[]).
watch(App)        -> naga_load:watch(App).
unwatch(App)      -> naga_load:unwatch(App).
is_view(M)        -> naga_load:is_view(M).
source(M)         -> naga_load:source(M).
app(M)            -> naga_load:app(M).
route_file(App)   -> filename:join([priv_dir(App), lists:concat([wf:atom([App]), ".routes"])]).
dump_file(App)    -> filename:join([priv_dir(App), lists:concat([wf:atom([App]), ".routes.dat"])]).
dump_route(App)   -> File = dump_file(App),
                     Components = [App] ++ wf:config(App,modules,[]), 
                     B = term_to_binary(cowboy_router:compile(dispatch(Components))),
                     file:write_file(File,B).
static_prefix(App)-> wf:config(App,static_prefix,"/static").
static_url(App,Uri)-> string:join([static_prefix(App),Uri],"").

doc_prefix(App)   -> wf:config(App,doc_prefix,   "/doc").
doc_url(App,Uri)  -> string:join([doc_prefix(App),Uri],"").

rest_prefix(App)  -> wf:config(App,rest_prefix,  "/rest").
% fcgi_prefix(App)  -> wf:config(App,fcgi_prefix,  "/fcgi").
n2o_prefix(App)   -> wf:config(App,n2o_prefix,   "/ws").
n2o_url(App,Uri)  -> string:join([n2o_prefix(App),Uri],"").
locale(App)       -> wf:config(App,static_prefix,none).

base_url(App)     -> wf:config(App,base_url,"/").
base_url(App,Url) -> case base_url(App) of "/" -> Url; Base -> string:join([Base,Url],"") end.
base_dir(App)     -> filename:join(lists:reverse(tl(lists:reverse(filename:split(priv_dir(App)))))).
source_dir(App)   -> filename:join([base_dir(App), "src", "controller"]).                      

%files(controller,App) -> [{F, module(F)}|| F <- mad_compile:files(source_dir(App),".erl")];
files(controller,App) -> naga_load:controller_files(App);
files(view,App)   -> naga_load:view_files(App).
module(F)         -> wf:atom([filename:basename(F, ".erl")]).

domains(App)      -> case wf:config(App, domains, ['_']) of all -> ['_']; E -> E end.
mime()            -> [{mimetypes,cow_mimetypes,all}].
is_dir(D)         -> case filelib:is_dir(D) of true -> D; false -> false end.
priv_dir(App)     -> case code:priv_dir(App) of
                       {error,_} -> case is_dir(filename:join(["apps", wf:to_list(App), "priv"])) of                                  
                                      false   -> case is_dir(filename:join(["deps", wf:to_list(App), "priv"])) of
                                                  false -> {error, notfound};
                                                  DepsDir -> DepsDir end;
                                      AppsDir -> AppsDir end;
                       Dir       -> Dir
                     end.

want_session(M)  -> E = module_info(M,attributes), [R]=proplists:get_value(session,E,[true]), R. %% by default true
default_action(M)-> E = module_info(M,attributes),
                   case proplists:get_value(default_action,E) of 
                    undefined -> case erlang:function_exported(M,index,3) of 
                                  true  -> {dft,index};
                                  false -> case erlang:function_exported(M,main,0) of 
                                                true  -> {dft,main}; 
                                                false -> [] 
                                           end 
                                 end;
                    [Default] -> {dft,Default} end.
actions(M)       -> Attr = module_info(M,attributes), 
                    Actions = lists:usort(proplists:get_value(actions,Attr,[]) ++ [default_action(M)]),
                    E = module_info(M,exports),
                    [{X,proplists:get_value(X,E)}|| X <- Actions].
                    %[ X ||{N,A} = X <- M:module_info(exports), A == 3 ]. 
is_steroid(M)    -> erlang:function_exported(M,event,1).

split(F)         -> filename:split(F).
sub(A,M) when is_atom(A), is_atom(M) -> sub(wf:to_list(A), wf:to_list(M));
sub(A,M)  -> case lists:prefix(A,M) of true -> M -- A; _ -> 
                  re:replace(M, "_", "/", [global, {return, list}]) end.

url(App,M,{dft,A})-> base_url(App,string:join(["/",sub(App,M),"/[...]"],""));
url(App,M,A)      -> base_url(App,string:join(["/",sub(App,M),"/",wf:to_list(A),"/[...]"],"")).
url(App,M)        -> case string:tokens(wf:to_list(M), "_") of
                     [_,"mail","view",Name,Ext|_] -> base_url(App,"/"++Name++"."++Ext);
                     _ -> {ok,Cwd} = file:get_cwd(), 
                         F=((((split(naga:source(M))--split(Cwd))--split(base_dir(App)))--[sep()])--["src","view"]),
                         base_url(App,"/"++string:join(F,"/"))
                     end. 

get_kv(K, O, D)  -> V = proplists:get_value(K,O,D), KV = {K,V}, {KV, O -- [KV]}.
%module(A,C)      -> wf:atom([A,C]).
%controller(A,M)  -> wf:atom([wf:to_list(M) -- wf:to_list([A,"_"])]).
code_url(Code)   -> wf:to_list(["/$_",Code,"_$"]).

handler(_App,H) when is_list(H) -> wf:config(naga,bridge,naga_cowboy);
handler(_App,H) when is_atom(H) -> H.

opts(App, H, Opts) 
        when is_list(H) -> {{application,App1},O } = get_kv(application,H,App),
                           {{controller,C},    O1} = get_kv(controller,O,index),
                           {{action,Act},      P } = get_kv(action,O1,index), 
                           #route{type=mvc,
                                  application=App1,
                                  controller=C,
                                  action=Act,
                                  arity=3,
                                  want_session=want_session(C),
                                  is_steroid=is_steroid(C),
                                  params=P,
                                  opts=Opts};

opts(_App, H, Opts) 
        when is_atom(H) -> Opts.

read_dump_file(Components)
      when is_list(Components)-> L = case read_dump_file(hd(Components)) of {ok, R} -> R;
                                         {error, enoent} -> dispatch(Components) end,
                                 lists:flatten(L);
read_dump_file(App)
     when is_atom(App)  -> Path = wf:f("apps/~s/priv/~s.routes.dat",[wf:to_list(App),wf:to_list(App)]), %%FIXME, window, deps?
                           case mad_repl:load_file(Path) of
                                {ok, ETSFile} -> {ok, ETSFile}; 
                                _ ->  file:read_file(Path) end.

consult(App)            -> Path = wf:f("apps/~s/priv/~s.routes",[wf:to_list(App),wf:to_list(App)]), %%FIXME, window, deps?
                           case mad_repl:load_file(Path) of
                                {ok, ETSFile} -> 
                                     %wf:info(?MODULE,"LOAD ROUTE FILE from bundle. ~s", [Path]),
                                     {ok, mad_tpl:consult(ETSFile)};
                                _ -> file:consult(route_file(App)) end.


dispatch_route(App,{Code, Handler, Opts}) 
                   when is_integer(Code) -> [{base_url(App,code_url(Code)), handler(App,Handler), opts(App,Handler,Opts)}];
dispatch_route(App,{Url, Handler, Opts}) -> O = opts(App,Handler,Opts), 
                                            [{base_url(App,Url), handler(App,Handler), O}] ++ 
                                            case O of #route{is_steroid=true} -> 
                                              [{ base_url(App,n2o_url(App,Url)), wf:config(naga,stream,n2o_stream), O}];
                                              _ -> [] 
                                            end;
dispatch_route(App,Route)                -> wf:error(?MODULE, "Invalid route ~p~n",[App,Route]), [].

dispatch_view(App,ViewModule) 
            when is_atom(ViewModule)     -> [{url(App,ViewModule), 
                                              wf:config(naga,bridge,naga_cowboy), 
                                              #route{type        = view,
                                                     application = App,
                                                     view        = ViewModule,
                                                     action      = render,
                                                     arity       = 0,
                                                     want_session= false,
                                                     is_steroid  = false}}].

dispatch_doc(App)                        -> [{ base_url(App,doc_url(App,"/[:docname]")), 
                                              wf:config(naga,bridge,naga_cowboy), 
                                            [#route{type=doc,application=App}]}].

dispatch_mvc(App,CtrlModule)             -> [begin 
                                               Url = url(App,CtrlModule,A),
                                               wf:info(?MODULE,"MVC URL ~p  <- ~p~n",[Url,{App,CtrlModule,A}]),
                                               {Url, wf:config(naga,bridge,naga_cowboy),
                                                #route{type        = mvc,
                                                       application = App,                                                                     
                                                       controller  = CtrlModule,
                                                       action      = case A of {dft,B} -> B; A -> A end,
                                                       arity       = N,
                                                       want_session= want_session(CtrlModule),
                                                       is_steroid  = is_steroid(CtrlModule)}} 
                                             end || {A,N} <- actions(CtrlModule)].

dispatch(routes,Components)-> lists:foldr( fun(App,Acc) -> 
                                    [case consult(App) of
                                     {ok, Routes} ->    
                                        lists:foldr(fun({Code, Handler, Opts}, Acc) ->
                                                      dispatch_route(App,{Code, Handler, Opts}) ++ Acc                                
                                                    end, [], lists:flatten(Routes));
                                     {error,_} = Err -> 
                                        wf:error(?MODULE, "Missing or invalid NAGA routes file: ~p~n~p~n", 
                                        [route_file(App), Err]), [] 
                                   end| Acc] 
                                 end, [], Components);

dispatch(view, Components) -> lists:foldr(fun(App,Bcc)->
                                            Views = files(view, App),                             
                                            [lists:foldr(fun({_,M},Acc) -> 
                                                          dispatch_view(App,M) ++ Acc
                                                         end, [], Views)|Bcc]
                                          end,[],Components);

dispatch(doc, Components)  -> lists:foldr(fun(App,Acc)->
                                           dispatch_doc(App) ++ Acc
                                          end,[],Components);

dispatch(mvc, Components)  -> lists:foldr(fun(App,Acc)->
                                            Controllers = files(controller,App),
                                            wf:info(?MODULE, "MVC CTRL ~p~n",[Controllers]),
                                            [lists:foldr(fun({_,M},Bcc) ->
                                                            dispatch_mvc(App,M) ++ Bcc
                                                         end, [], Controllers)] ++ Acc
                                          end, [], Components);

dispatch(      _, _App)    -> [].

boot_apps(Apps)         -> boot_app(Apps, []).
boot_app([], AppsInfo)  -> AppsInfo;
boot_app([App|T], Acc)  -> {ok, Modules} = application:get_key(App,modules), 
                           [code:ensure_loaded(M)||M<-Modules],
                           AppInfo = #{                                       
                                       locale         => locale(App),
                                       base_url       => base_url(App),
                                       static_prefix  => static_prefix(App),
                                       doc_prefix     => doc_prefix(App),
                                       rest_prefix    => rest_prefix(App),
                                       n2o_prefix     => n2o_prefix(App),
                                       %fcgi_prefix    => fcgi_prefix(App),
                                       %fcgi_opts      => boot_fcgi(App),
                                       domains        => domains(App)
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
 Path = base_url(wf:to_atom(App),string:join(["/",sub(wf:to_list(App),wf:to_list(M)),"/",wf:to_list(A)],"")),
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


