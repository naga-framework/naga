-module(naga).
-description('NAGA OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1, watch/1, unwatch/1]).
-compile(export_all).
-include("naga.hrl").

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

tables()   -> [ translation ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> supervisor:start_link({local,naga},naga,[]).
stop(_)    -> ok.
init([])   -> 
              { ok, { { one_for_one, 5, 10 }, 
	                [	                  
                      ?CHILD(naga_load, worker, [apps()])
	                ] 
              }}.


dispatch(App)     -> [{domains(App),
                      dispatch(service,   App) ++  %% TODO: websocket service, async server
                      dispatch(revproxy,  App) ++  %% TODO: reverse proxy
                      dispatch(fcgi,      App) ++  %% TODO: memcache protocol (ranch?)
                      dispatch(rest,      App) ++  %% TODO: 
                      dispatch(static,    App) ++ 
                      dispatch(controller,App) ++  
                      dispatch(view,      App) ++                                  
                      dispatch(doc,       App) ++  %% TODO                    
                      dispatch(default,   App)
                     }].

sep()             -> "/". %%FIXME: linux/unix/macosx ok, windows?
apps()            -> wf:config(naga,watch,[]).
watch(App)        -> naga_load:watch(App).
unwatch(App)      -> naga_load:unwatch(App).
is_view(M)        -> naga_load:is_view(M).
source(M)         -> naga_load:source(M).
app(M)            -> naga_load:app(M).

base_url(App)     -> wf:config(App,base_url,"/").
base_url(App,Url) -> case base_url(App) of "/" -> Url; Base -> string:join([Base,Url],"") end.
base_dir(App)     -> filename:join(lists:reverse(tl(lists:reverse(filename:split(priv_dir(App)))))).
source_dir(App)   -> filename:join([base_dir(App), "src", "controller"]).                      


files(controller,App)  -> [{F, module(F)}|| F <- mad_compile:files(source_dir(App),".erl")];
files(view,App)        -> naga_load:view_files(App).
module(F)              -> wf:atom([filename:basename(F, ".erl")]).

domains(App)  -> case wf:config(App, domains, ['_']) of all -> ['_']; E -> E end.
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

want_session(M)  -> E = M:module_info(attributes), proplists:get_value(session,E,true).
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
url(App,M)      -> string:tokens(wf:to_list(M), "_").

dispatch(static,App)    -> lists:foldr(fun({Url,dir,App}, Acc)  -> [{Url, n2o_static, {dir, priv_dir(App), mime()}}|Acc]
                              %({Url,file,App}, Acc) -> [{Url, cowboy_static, {file, }}|Acc]
                               end, [], wf:config(App, static, []));

dispatch(controller,App)-> Controllers = files(controller,App),
                            lists:foldr(fun({F,M},Acc) ->
                                  Actions = actions(M),
                                  [{url(App,M,A), {App,M,A,N,want_session(M),is_steroid(M)}, []} || {A,N} <- Actions]++Acc
                               end, [], Controllers);

dispatch(view,App)      -> Views = files(view, App),                             
                             lists:foldr(fun({F,M},Acc) ->                                  
                                  [{url(App,M), {App,M,render,0,false,false}, []}]++Acc
                               end, [], Views);

dispatch(default,App)   -> [{ base_url(App,"/ws/:controller/:action/[...]"), n2o_stream,  [] },
                            { base_url(App,"/ws/:controller/[...]"),         n2o_stream,  [] },
                            { base_url(App,"/ws/[...]"),                     n2o_stream,  [] },
                            { base_url(App,"/:controller/:action/[...]"),    naga_cowboy, [] },
                            { base_url(App,"/:controller/[...]"),            naga_cowboy, [] },
                            { base_url(App,"/[...]"),                        naga_cowboy, [] }];

dispatch(_,App)         -> [].

