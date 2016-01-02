-module(naga).
-description('NAGA OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-behaviour(cowboy_middleware).
-export([start/2, stop/1, init/1, watch/1, unwatch/1]).
-compile(export_all).
-include("naga.hrl").

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

tables()   -> [ ?MODULE ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> supervisor:start_link({local,naga},naga,[]).
stop(_)    -> ok.
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              { ok, { { one_for_one, 5, 10 }, 
	                [	                  
                      ?CHILD(naga_load, worker, [apps()])
	                ] 
              }}.

start(Apps) -> DispatchApps = dispatchApps(Apps),
               AppsInfo = boot_apps(Apps),
               ProtoOpts = [{env,[{applications, [Apps]}
                                 ,{appsInfo, AppsInfo}                      	  
                                 ,{dispatch, cowboy_router:compile(DispatchApps)}]},
                            {middlewares, [naga_router, naga]}],
               [start_listeners(App, ProtoOpts) || App <- Apps].

dispatchApps(Apps)-> lists:foldr(fun(App,Acc) -> 
                                      [{domains(App),                                         
                                         dispatch(service,   App) ++  %% TODO: websocket service, async server
                                         dispatch(revproxy,  App) ++  %% TODO: reverse proxy
                                         dispatch(fcgi,      App) ++  %% TODO: memcache protocol (ranch?)
                                         dispatch(rest,      App) ++  %% TODO: 
                                         dispatch(static,    App) ++  %% TOTO: dispatch special files.
                                         [{base_url(App, "/[...]"), {app,App}, []}]
                                      }] ++ Acc
                                 end, [], Apps).
dispatch(App)     -> [{'_',
						            dispatch(route,     App) ++  %% TODO: from dispatch file				
	                      dispatch(controller,App) ++  
	                      dispatch(view,      App) ++                                 
	                      dispatch(doc,       App) ++  %% TODO                    
	                      dispatch(default,   App)
                     }].

start_listeners(App, ProtoOpts) ->
    case wf:config(App, listeners, []) of
        [] -> wf:info(?MODULE, "listeners notfound for app ~p []",[App]),            
              listener(App, ?DEFAULT_LISTENER, ProtoOpts, []);
        Listeners -> listener(App, Listeners, ProtoOpts, []) end.

%%% -------------- cowboy middleware 

execute(Req, Env) ->
    {_, Handler}     = lists:keyfind(handler, 1, Env),
    {_, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
    wf:info(?MODULE, "Handler ~p",[Handler]),
    wf:info(?MODULE, "HandlerOpts ~p",[HandlerOpts]),
    try Handler:init(Req, HandlerOpts) of
        {ok, Req2, State} ->
            Result = terminate(normal, Req2, State, Handler),
            {ok, Req2, [{result, Result}|Env]};
        {fcgi_static, Req2, {FilePath,_,_Extra}=State} ->
            Env2 = lists:keyreplace(handler, 1, Env, {handler, cowboy_static}),
            _Env3 = lists:keyreplace(handler_opts, 1, Env, {handler_opts, {file, FilePath}}),
            cowboy_rest:upgrade(Req2, Env2, cowboy_static, State, infinity, run);
        {Mod, Req2, State} ->
            Mod:upgrade(Req2, Env, Handler, State, infinity, run);
        {Mod, Req2, State, hibernate} ->
            Mod:upgrade(Req2, Env, Handler, State, infinity, hibernate);
        {Mod, Req2, State, Timeout} ->
            Mod:upgrade(Req2, Env, Handler, State, Timeout, run);
        {Mod, Req2, State, Timeout, hibernate} ->
            Mod:upgrade(Req2, Env, Handler, State, Timeout, hibernate)
    catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            cowboy_req:maybe_reply(Stacktrace, Req),
            terminate({crash, Class, Reason}, Req, HandlerOpts, Handler),
            erlang:Class([
                          {reason, Reason},
                          {mfa, {Handler, init, 2}},
                          {stacktrace, Stacktrace},
                          {req, cowboy_req:to_list(Req)},
                          {opts, HandlerOpts}
                         ])
    end.

terminate(Reason, Req, State, Handler) ->
	cowboy_handler:terminate(Reason, Req, State, Handler).

%%% ----- internal function 
listener_name(Type,App) -> wf:atom([Type,App]).

listener(_,[], _,Acc) -> Acc;
listener(App, [{https, Opts}|T], ProtoOpts, Acc) ->
    Listener    = listener_name(https,App),
    Ip          = proplists:get_value(ip, Opts, {0, 0, 0, 0}),
    Port        = proplists:get_value(port, Opts, ?DEFAULT_HTTPS_PORT),
    NbAcceptors = proplists:get_value(acceptors, Opts, ?DEFAULT_ACCEPTOR_PROCESSES),
    SslOpts     = proplists:get_value(ssl_opts, Opts, ?DEFAULT_SSL_OPTS),
     TransOpts  = [{ip, Ip},{port, Port}|SslOpts],
    case cowboy:start_https(Listener, NbAcceptors, TransOpts, ProtoOpts) of
        {ok, Pid} ->
            wf:info(?MODULE, "starting HTTPS server ~p at ~p:~p",[App, Ip,Port]),
            listener(App, T, ProtoOpts, [{Listener, Pid, TransOpts, ProtoOpts}|Acc]); 
        {error,{{_,{_,_,{_,_}}},_}} = Err -> 
            io:format("Can't start Web Server: ~p ~p\r\n",[Err, {App, Ip, Port}]);
        X -> 
            io:format("Unknown Error: ~p\r\n",[X]), halt(abort,[])
    end;

listener(App, [{http, Opts}|T], ProtoOpts, Acc) ->
    Listener    = listener_name(http,App),
    Ip          = proplists:get_value(ip, Opts, {0, 0, 0, 0}),
    Port        = proplists:get_value(port, Opts, ?DEFAULT_HTTP_PORT),
    NbAcceptors = proplists:get_value(acceptors, Opts, ?DEFAULT_ACCEPTOR_PROCESSES),
    TransOpts   = [{ip, Ip},{port, Port}],
    case cowboy:start_http(Listener, NbAcceptors, TransOpts, ProtoOpts) of
        {ok, Pid} -> wf:info(?MODULE, "starting HTTP server ~p at ~p:~p",[App, Ip,Port]),
                     listener(App, T, ProtoOpts, [{Listener, Pid, TransOpts, ProtoOpts}|Acc]); 
        {error,{{_,{_,_,{_,_}}},_}} = Err -> 
            io:format("Can't start Web Server: ~p ~p\r\n",[Err, {App, Ip, Port}]);
        X -> 
            io:format("Unknown Error: ~p\r\n",[X]), halt(abort,[])
    end.

sep()             -> "/". %%FIXME: linux/unix/macosx ok, windows?
apps()            -> wf:config(naga,watch,[]).
watch(App)        -> naga_load:watch(App).
unwatch(App)      -> naga_load:unwatch(App).
is_view(M)        -> naga_load:is_view(M).
source(M)         -> naga_load:source(M).
app(M)            -> naga_load:app(M).

static_prefix(App)-> wf:config(App,static_prefix,"/static").
doc_prefix(App)   -> wf:config(App,doc_prefix,   "/doc").
rest_prefix(App)  -> wf:config(App,rest_prefix,  "/rest").
fcgi_prefix(App)  -> wf:config(App,fcgi_prefix,  "/fcgi").
n2o_prefix(App)   -> wf:config(App,n2o_prefix,   "/ws").
n2o_url(App,Uri)  -> string:join([wf:config(App,n2o_prefix,"/ws"),Uri],"").
locale(App)       -> wf:config(App,static_prefix,none).

base_url(App)     -> wf:config(App,base_url,"/").
base_url(App,Url) -> case base_url(App) of "/" -> Url; Base -> string:join([Base,Url],"") end.
base_dir(App)     -> filename:join(lists:reverse(tl(lists:reverse(filename:split(priv_dir(App)))))).
source_dir(App)   -> filename:join([base_dir(App), "src", "controller"]).                      

files(controller,App)  -> [{F, module(F)}|| F <- mad_compile:files(source_dir(App),".erl")];
files(view,App)        -> naga_load:view_files(App).
module(F)              -> wf:atom([filename:basename(F, ".erl")]).

domains(App)  -> case wf:config(App, domains, '_') of all -> '_'; E -> E end.
mime()        -> [{mimetypes,cow_mimetypes,all}].
is_dir(D)     -> case filelib:is_dir(D) of true -> D; false -> false end.
priv_dir(App) ->  {ok,Cwd} = file:get_cwd(), 
                  case code:priv_dir(App) of
                   {error,_} -> case is_dir(filename:join(["apps", wf:to_list(App), "priv"])) of                                  
                                  false   -> case is_dir(filename:join(["deps", wf:to_list(App), "priv"])) of
                                              false -> {error, notfound};
                                              DepsDir -> DepsDir end;
                                  AppsDir -> AppsDir end;
                   Dir       -> D = filename:join(filename:split(Dir) -- filename:split(Cwd)),
                                error_logger:info_msg("Dir ~p~n",[D]),D
                                 
                  end.

want_session(M)  -> E = M:module_info(attributes), proplists:get_value(session,E,true). %% by default true
default_action(M)-> E = M:module_info(attributes),
                   case proplists:get_value(defaut,E) of 
                    undefined -> case erlang:function_exported(M,index,3) of 
                                  true  -> index;
                                  false -> case erlang:function_exported(M,main,0) of 
                                                true -> main; false -> {error, '404'} end end;
                    Default -> Default end.
actions(M)       -> A = M:module_info(attributes), 
                    Actions = lists:usort(proplists:get_value(actions,A,[]) ++ [default_action(M)]),
                    E = M:module_info(exports),
                    [{A,proplists:get_value(A,E)}|| A <- Actions].
                    %[ X ||{N,A} = X <- M:module_info(exports), A == 3 ]. 
                    %% maybe can use dializer here to find out 
is_steroid(M)    -> erlang:function_exported(M,event,1).

url(App,M,main) -> base_url(App,string:join(["/",wf:to_list(M)--wf:to_list([App,"_"]),"/[...]"],""));
url(App,M,index)-> base_url(App,string:join(["/",wf:to_list(M)--wf:to_list([App,"_"]),"/[...]"],""));
url(App,M,A)    -> base_url(App,string:join(["/",wf:to_list(M)--wf:to_list([App,"_"]),"/",wf:to_list(A),"/[...]"],"")).
url(App,M)      -> case string:tokens(wf:to_list(M), "_") of
                    [_,"mail","view",Name,Ext|_] -> base_url(App,"/"++Name++"."++Ext);
                    _ -> {ok,Cwd} = file:get_cwd(), 
                         F = naga:source(M),                         
                         (F -- Cwd) -- ("/" ++ base_dir(App)++"/src/view")
                   end. 

dispatch(static,App)    -> lists:foldr(fun({Url,dir,App}, Acc)  -> [{Url, n2o_static, {dir, priv_dir(App), mime()}}|Acc]
                              %({Url,file,App}, Acc) -> [{Url, cowboy_static, {file, }}|Acc]
                               end, [], wf:config(App, static, []));

dispatch(controller,App)-> Controllers = files(controller,App),
                            lists:foldr(fun({F,M},Acc) ->
                                  Actions = actions(M),
                                  [{url(App,M,A), {controller,App,M,A,N,want_session(M),is_steroid(M)}, []} || {A,N} <- Actions]++Acc
                               end, [], Controllers);

dispatch(view,App)      -> Views = files(view, App),                             
                             lists:foldr(fun({F,M},Acc) ->                                  
                                  [{url(App,M), {view,App,M,render,0,false,false}, []}]++Acc
                               end, [], Views);

dispatch(default,App)   -> [{ base_url(App,n2o_url(App,"/:controller/:action/[...]")), n2o_stream,  [] },
                            { base_url(App,n2o_url(App,"/ws/:controller/[...]")),      n2o_stream,  [] },
                            { base_url(App,n2o_url(App,"/ws/[...]")),                  n2o_stream,  [] },
                            { base_url(App,"/:controller/:action/[...]"),              naga_cowboy, [] },
                            { base_url(App,"/:controller/[...]"),                      naga_cowboy, [] },
                            { base_url(App,"/[...]"),                                  naga_cowboy, [] }];

dispatch(_,App)         -> [].


boot_apps(Apps)        -> boot_app(Apps, []).
boot_app([], AppsInfo) -> AppsInfo;
boot_app([App|T], Acc) -> {ok, Modules} = application:get_key(App,modules), 
                          [code:ensure_loaded(M)||M<-Modules],
                          AppInfo = #{ dispatch     => dispatch(App),   
                                       locale       => locale(App),
                                       base_url     => base_url(App),
                                       static_prefix=> static_prefix(App),
                                       doc_prefix   => doc_prefix(App),
                                       rest_prefix  => rest_prefix(App),
                                       n2o_prefix   => n2o_prefix(App),
                                       fcgi_prefix  => fcgi_prefix(App),
                                       fcgi_opts    => boot_fcgi(App),
                                       domains      => domains(App)
                                      },
                          boot_app(T, [{App, AppInfo}|Acc]).

boot_fcgi(App)       -> boot_fcgi(App, wf:config(App, fcgi_enabled, false)).
boot_fcgi(App, false)-> skip;
boot_fcgi(App, true) -> Fcgi     = wf:config(App, fcgi_exe, 'php-fpm'),    
                        FcgiHost = wf:config(App, fcgi_host, localhost),    
                        FcgiPort = wf:config(App, fcgi_port, 33000),  
                        ex_fcgi:start(Fcgi, FcgiHost, FcgiPort),
                        #{ fcgi_exe => Fcgi,
                           fcgi_host=> FcgiHost,
                           fcgi_port=> FcgiPort 
                        }.
