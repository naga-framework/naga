-module(naga).
-description('NAGA OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1, watch/1, unwatch/1]).
-compile(export_all).
-include("naga.hrl").

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

tables()   -> [ ?MODULE ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> supervisor:start_link({local,naga},naga,[]).
init([])   -> %[ ets:new(T,opt()) || T <- tables() ],
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

start(App) when is_atom(App) -> start([App]);
start(Apps)-> DispatchApps = case read_dump_file(Apps) of 
                                  {ok, Routes} -> wf:info(?MODULE,"Boot from binary route files.",[]), 
                                             Routes; 
                                  _ -> dispatchApps(Apps) end,
              _AppsInfo    = boot_apps(Apps),
              ProtoOpts    = [{env,[{applications, Apps} %,{appsInfo, AppsInfo}                      	  
                                   ,{dispatch, cowboy_router:compile(DispatchApps)}]},
                              {middlewares, wf:config(naga,middlewares,[cowboy_router,cowboy_handler])}],
              [start_listeners(App, ProtoOpts) || App <- Apps].

dispatchApps(Apps)-> lists:foldr(fun(App,Acc) ->                                       
                        lists:foldr(fun(Domain,Bcc) ->
                                      [{Domain, dispatch(App)}] ++ Bcc 
                                     end, [], domains(App)) ++ Acc
                                 end, [], Apps).

dispatch(App)     -> Rules = [service, revproxy, fcgi, rest, route, view, doc, mvc],
                     lists:foldr(fun(Rule,Acc)-> dispatch(Rule,App) ++ Acc end,[],
                                 wf:config(App,rules,Rules)).

start_listeners(App, ProtoOpts) ->
    case wf:config(App, listeners, []) of
        [] -> wf:info(?MODULE, "listeners notfound for app ~p []",[App]),            
              listener(App, ?DEFAULT_LISTENER, ProtoOpts, []);
        Listeners -> listener(App, Listeners, ProtoOpts, []) end.

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
                     B = term_to_binary(cowboy_router:compile(dispatchApps(App))),
                     file:write_file(File,B).
static_prefix(App)-> wf:config(App,static_prefix,"/static").
static_url(App,Uri)-> string:join([static_prefix(App),Uri],"").

doc_prefix(App)   -> wf:config(App,doc_prefix,   "/doc").
doc_url(App,Uri)  -> string:join([doc_prefix(App),Uri],"").

rest_prefix(App)  -> wf:config(App,rest_prefix,  "/rest").
fcgi_prefix(App)  -> wf:config(App,fcgi_prefix,  "/fcgi").
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

want_session(M)  -> E = M:module_info(attributes), [R]=proplists:get_value(session,E,[true]), R. %% by default true
default_action(M)-> E = M:module_info(attributes),
                   case proplists:get_value(defaut_action,E) of 
                    undefined -> case erlang:function_exported(M,index,3) of 
                                  true  -> index;
                                  false -> case erlang:function_exported(M,main,0) of 
                                                true -> main; false -> {error, '404'} end end;
                    Default -> Default end.
actions(M)       -> Attr = M:module_info(attributes), 
                    Actions = lists:usort(proplists:get_value(actions,Attr,[]) ++ [default_action(M)]),
                    E = M:module_info(exports),
                    [{X,proplists:get_value(X,E)}|| X <- Actions].
                    %[ X ||{N,A} = X <- M:module_info(exports), A == 3 ]. 
is_steroid(M)    -> erlang:function_exported(M,event,1).

split(F)         -> filename:split(F).
url(App,M,main)  -> base_url(App,string:join(["/",wf:to_list(M)--wf:to_list([App,"_"]),"/[...]"],""));
url(App,M,index) -> base_url(App,string:join(["/",wf:to_list(M)--wf:to_list([App,"_"]),"/[...]"],""));
url(App,M,A)     -> base_url(App,string:join(["/",wf:to_list(M)--wf:to_list([App,"_"]),"/",wf:to_list(A),"/[...]"],"")).
url(App,M)       -> case string:tokens(wf:to_list(M), "_") of
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

read_dump_file(Apps)
      when is_list(Apps)-> {ok, lists:flatten([case read_dump_file(A) of {ok, B} -> binary_to_term(B); _ -> [] end||A<-Apps])};
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

dispatch(route,     App)-> case consult(App) of
                             {ok, Routes} ->    
                                lists:foldr(fun
                                              ({Code, Handler, Opts}, Acc) when is_integer(Code) ->
                                                [{base_url(App,code_url(Code)), handler(App,Handler), opts(App,Handler,Opts)}] ++ Acc;                                   
                                              ({Url, Handler, Opts},Acc) -> 
                                                [{base_url(App,Url), handler(App,Handler), opts(App,Handler,Opts)}] ++ Acc                                   
                                            end, 
                                  [], lists:flatten(Routes));
                             {error,_} = Err -> 
                                wf:error(?MODULE, "Missing or invalid NAGA routes file: ~p~n~p~n", 
                                [route_file(App), Err]), [] 
                           end;

dispatch(view,      App)-> Views = files(view, App),                             
                             lists:foldr(fun({_,M},Acc) ->                                  
                                  [{url(App,M), wf:config(naga,bridge,naga_cowboy), 
                                                #route{type=view,
                                                       application=App,
                                                       view=M,
                                                       action=render,
                                                       arity=0,
                                                       want_session=false,
                                                       is_steroid=false}}]++Acc
                               end, [], Views);
%FIXME
dispatch(doc,      App)-> [{ base_url(App,doc_url(App,"/[:docname]")),                 wf:config(naga,bridge,naga_cowboy), [#route{type=doc,application=App}]}];

dispatch(mvc,      App)-> Controllers = files(controller,App),
                          lists:foldr(fun({_,M},Acc) ->
                                          [{url(App,M,A), wf:config(naga,bridge,naga_cowboy),
                                                          #route{type=mvc,
                                                                 application=App,                                                                     
                                                                 controller=M,
                                                                 action=A,
                                                                 arity=N,
                                                                 want_session=want_session(M),
                                                                 is_steroid=is_steroid(M)}} || {A,N} <- actions(M)]++Acc
                                      end, [], Controllers) ++
                           [{ base_url(App,n2o_url(App,"/:controller/:action/[...]")), wf:config(naga,stream,n2o_stream),  [] },
                            { base_url(App,n2o_url(App,"/:controller/[...]")),         wf:config(naga,stream,n2o_stream),  [] },
                            { base_url(App,n2o_url(App,"/[...]")),                     wf:config(naga,stream,n2o_stream),  [] },
                            { base_url(App,"/:controller/:action/[...]"),              wf:config(naga,bridge,naga_cowboy), [] },
                            { base_url(App,"/:controller/[...]"),                      wf:config(naga,bridge,naga_cowboy), [] },
                            { base_url(App,"/[...]"),                                  wf:config(naga,bridge,naga_cowboy), [] }];
dispatch(      _, _App) -> [].

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
                                       fcgi_prefix    => fcgi_prefix(App),
                                       fcgi_opts      => boot_fcgi(App),
                                       domains        => domains(App)
                                      },
                           boot_app(T, [{App, AppInfo}|Acc]).

boot_fcgi(App)          -> boot_fcgi(App, wf:config(App, fcgi_enabled, false)).
boot_fcgi(_App, false)  -> undefined;
boot_fcgi(App, true)    -> Fcgi     = wf:config(App, fcgi_exe, 'php-fpm'),    
                           FcgiHost = wf:config(App, fcgi_host, localhost),    
                           FcgiPort = wf:config(App, fcgi_port, 33000),  
                           ex_fcgi:start(Fcgi, FcgiHost, FcgiPort),
                           #{ fcgi_exe => Fcgi,
                              fcgi_host=> FcgiHost,
                              fcgi_port=> FcgiPort 
                            }.

dateformat()            -> erlydtl_dateformat:format("r").
dateformat(Format)      -> erlydtl_dateformat:format(Format).
dateformat(Date,Format) -> erlydtl_dateformat:format(Date,Format).