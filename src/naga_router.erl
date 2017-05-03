-module(naga_router).
-behaviour(cowboy_middleware).
-compile(export_all).
-include("naga.hrl").

%%FIXME: reverse routing
location(L,Ctx) when is_binary(L) -> L;
location(L,Ctx) when is_list(L) -> wf:to_binary(L); 
location(#{app:=App,controller:=C,action:=A}=L,_) -> P=maps:get(path_info,L),Path = path(App,C,A,P), naga:urlencode(Path,maps:get(params,L,[]));
location(#{         controller:=C,action:=A}=L,#{'_application':=App}) -> P=maps:get(path_info,L),Path = path(App,C,A,P), naga:urlencode(Path,maps:get(params,L,[]));
location(#{                       action:=A}=L,#{'_application':=App, '_controller':=C}) -> P=maps:get(path_info,L),Path = path(App,C,A,P), naga:urlencode(Path,maps:get(params,L,[])).

path(App,M,A,P) -> 
 Path = base_url(wf:to_atom(App),string:join(["/",naga_router:sub(wf:to_list(App),wf:to_list(M)),"/",wf:to_list(A)],"")),
 string:join([Path, string:join([wf:to_list(X)||X<-P],"/")],"/").

urlencode(Path,Params)  -> Qs= cow_qs:qs([{wf:to_binary(K),wf:to_binary(V)}||{K,V}<-Params]),
                           P = wf:to_binary(Path),<<P/binary,$?,Qs/binary>>.
%urldecode(U)            -> cow_qs:urldecode(U).
urldecode(U)            -> {P,Qs} = cow_http:parse_fullpath(U),
                           {P,[{wf:to_atom(K),V}||{K,V}<-cow_qs:parse_qs(Qs)]}.

execute(Req, Env) ->
	 {_, DispatchModule} = lists:keyfind(dispatch, 1, Env),
	[Host,HostInfo,Path] = cowboy_req:get([host,host_info,path],Req),
	Path0 = filename:split(Path) -- [<<"/">>],
	case DispatchModule:match(Path0,undefined) of
		{ok,{{_,Pattern,naga_static,HandlerOpts}=R,Bindings}=Match} ->	
		  PathInfo = path_info(Pattern,Path0),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, naga_static},{handler_opts, HandlerOpts}|Env]};

		{ok,{{_,Pattern,cowboy_static,HandlerOpts}=R,Bindings}=Match} ->	
		  PathInfo = path_info(Pattern,Path0),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, cowboy_static},{handler_opts, HandlerOpts}|Env]};

		{ok,{{_,Pattern,n2o_static,HandlerOpts}=R,Bindings}} ->
		  PathInfo = path_info(Pattern,Path0),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, n2o_static},{handler_opts, HandlerOpts}|Env]};

		{ok,{{N,Pattern,n2o_stream,HandlerOpts}=R,Bindings}=Match} ->
			PathInfo = path_info(Pattern,Path0),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, n2o_stream},{handler_opts, HandlerOpts#route{match={N,Pattern,n2o_stream,Bindings}}}|Env]};

		{ok,{{N,Pattern,naga_cowboy,HandlerOpts}=R,Bindings}=Match} ->
			PathInfo = path_info(Pattern,Path0),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, naga_cowboy},{handler_opts, HandlerOpts#route{match={N,Pattern,naga_cowboy,Bindings}}}|Env]};

		{ok,{{N,Pattern,Handler,HandlerOpts}=R,Bindings}=Match} ->
			PathInfo = path_info(Pattern,Path0),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, Handler},{handler_opts, HandlerOpts}|Env]};

		_ -> case not_found(DispatchModule,Path0) of fail -> {error, 404, Req};
					NotFound ->
						case DispatchModule:match(NotFound,undefined) of
							{ok,{{N,Pattern,Handler,HandlerOpts}=R,Bindings}=Match} ->
								PathInfo = path_info(Pattern,Path0),
							 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
								{ok, Req2, [{handler, Handler},{handler_opts, HandlerOpts}|Env]};
							_ -> {error, 404, Req} end end
	end.

not_found(DispatchModule, Path) ->
  P = [<<"$$_{Components}_$$">>]++Path,
  case DispatchModule:match(P, undefined) of
  	{ok,{{_,_,{B,_},_},_}} -> B ++ [<<"$_404_$">>]; fail -> fail end.

path_info([],    P)          -> P;
path_info(['*'], [])         -> [];
path_info([_,'*'|_], [_|T2]) -> T2;
path_info([_|T1],    [_|T2]) -> path_info(T1, T2).


%% dispatch

dispatch_routes(App) -> 
  Modules = wf:config(App,modules,[]),
  Components = [App] ++ Modules,
  case read_dump_file(Components) of
   {error, Raison} = Err -> Err; 
   [{_,R0}] ->
    RC = dispatch_components(App),
    R = RC ++ R0,  
    {Rules,N} = lists:foldl(fun({A,B,C},{Acc,Count}) ->
                                  {Acc++[{Count,convert(A),B,C}], Count+1};
                                ({N,A,B,C},{Acc,Count}) ->
                                  {Acc++[{N,convert(A),B,C}], Count}
                            end,{[],1-length(RC)}, R),
    DispatchModule = module_dispatch_name(App),
    ok = dispatch_compiler:compile_load(DispatchModule,Rules),
    {ok, App, Modules, Components, DispatchModule, N, Rules}
  end.

read_dump_file(Components)
   when is_list(Components)-> App = hd(Components),
                               case read_dump_file(App) of 
                                {ok, R} -> R;
                                {error, enoent} -> 
                                  Path = naga_router:route_file(App),
                                  case filelib:is_file(Path) of
                                    true -> lists:flatten(dispatch(Components));
                                    false-> {error, wf:f("Missing route file (~s)",[Path])}
                                  end 
                               end;
read_dump_file(App)
   when is_atom(App)       -> Path = dump_file(App),
                              case mad_repl:load_file(Path) of
                               {ok, ETSFile} -> {ok, ETSFile}; 
                               _ ->  file:read_file(Path) end.

dispatch_components(App) ->
  Components = wf:config(App,modules,[]),
  Order = order([App]++Components),
  [begin BU = wf:config(C,base_url,"/"),
    case BU of "/" ->
      {"$$_{Components}_$$"++base_url(C,"/[...]"),{[],C},[]};
      _ -> {"$$_{Components}_$$"++base_url(C,"/[...]"),{[wf:to_binary(BU--"/")],C},[]}
    end end || C <- Order].

convert(P) -> P1 = split(P) -- ["/"],
              convert(P1, []).
convert([],Acc)          -> lists:reverse(Acc);
convert(["[...]"|T],Acc) -> convert(T,['*']++Acc);
convert([[$:|T1]|T],Acc) -> convert(T,[wf:atom([T1])]++Acc);
convert([H|T],Acc)       -> convert(T,[H]++Acc).

module_dispatch_name(App) 
                  -> wf:atom([App,dispatch_routes]).

order(A)          -> [Y||{_,Y} <-lists:reverse(lists:ukeysort(1,[{wf:config(X,base_url,[]),X}||X<-A]))].
            %%order by prefix

dispatch(Components) 
                  -> App = hd(Components),
								     Rules = [routes, view, doc, mvc],
								     Order = order(Components),
								     D = lists:foldr(fun(Rule,Acc)-> 
								                      dispatch(Rule,Order) ++ Acc 
								                     end,[],wf:config(App,rules,Rules)),
								     lists:foldr(fun(Domain,Bcc) -> [{Domain, lists:flatten(D)}] ++ Bcc end, [], domains(App)).

module_info(M,T)  -> case catch M:module_info(T) of {'EXIT', Err} -> err(Err),[]; E -> E end.
sep()             -> "/". %%FIXME: linux/unix/macosx ok, windows?

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
ctrl_dir(App)     -> filename:join([base_dir(App), "src", "controller"]).                      
view_dir(App)     -> filename:join([base_dir(App), "src", "view"]).                      
model_dir(App)    -> filename:join([base_dir(App), "src", "model"]).                      
static_dir(App)   -> {ok,[Rules]} = consult(App), 
                     AppName = wf:to_list(App),
                     [{Url,Dir}] = [{U1,D} || {U1,[_,Ap|_] = D} 
                       <- [{U,filename:split(B)}||{U,H,{A,B,C}}=R<-Rules, 
                           H == naga_static orelse H == n2o_static, A == dir ], 
                      Ap == AppName],{Url,filename:join(Dir)}.
tpl_name(App,Ctr,Act,E) 
                  -> naga_mvc:tpl_name(App,Ctr,Act,E).
%files(controller,App) -> [{F, module(F)}|| F <- mad_compile:files(ctrl_dir(App),".erl")];
files(controller,App) 
                  -> naga_load:controller_files(App);
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
actions(M)        -> Attr = module_info(M,attributes), 
                     Actions = lists:usort(proplists:get_value(actions,Attr,[]) ++ [default_action(M)]),
                     E = module_info(M,exports),
                     [{X,proplists:get_value(X,E)}|| X <- Actions].
                    %[ X ||{N,A} = X <- M:module_info(exports), A == 3 ]. 
is_steroid(M)     -> erlang:function_exported(M,event,1).
split(F)          -> filename:split(F).
sub(A,M) 
 when is_atom(A), 
      is_atom(M)  -> sub(wf:to_list(A), wf:to_list(M));
sub(A,M)          -> case lists:prefix(A,M) of true -> M -- A; _ -> re:replace(M, "_", "/", [global, {return, list}]) end.
url(App,M,{dft,A})-> base_url(App,string:join(["/",sub(App,M),"/[...]"],""));
url(App,M,A)      -> base_url(App,string:join(["/",sub(App,M),"/",wf:to_list(A),"/[...]"],"")).
url({theme,A},M)  -> sep()++wf:to_list(A)++url(A,M); 
url(App,M)        -> case string:tokens(wf:to_list(M), "_") of
                     [_,"mail","view",Name,Ext|_] -> base_url(App,"/"++Name++"."++Ext);
                     _ -> {ok,Cwd} = file:get_cwd(), 
                         F=((((split(source(M))--split(Cwd))--split(base_dir(App)))--[sep()])--["src","view"]),
                         base_url(App,"/"++string:join(F,"/"))
                     end. 
get_kv({K1,K2}, O, D)
                  -> case proplists:get_value(K1,O) of
                            undefined -> get_kv(K2, O, D);
                            V         -> KV = {K1,V}, {KV, O -- [KV]} end;
get_kv(K, O, D)   -> V = proplists:get_value(K,O,D), KV = {K,V}, {KV, O -- [KV]}.
%module(A,C)      -> wf:atom([A,C]).
%controller(A,M)  -> wf:atom([wf:to_list(M) -- wf:to_list([A,"_"])]).
code_url(Code)    -> wf:to_list(["/$_",Code,"_$"]).

handler(_App,H) 
	when is_list(H) -> wf:config(naga,bridge,naga_cowboy);
handler(_App,H) 
  when is_atom(H) -> H.

err({undef,[{Ctrl,_,_,_}|_]}) -> 
  mad:info("~s~n",[naga:red("controller missing => ")++naga:magenta(wf:to_list(Ctrl))]).

opts(App, H, Opts) 
  when is_list(H) -> {{_,App1},O } = get_kv({application,wf:config(naga,def_application,app)},H,App),
                     {{_,C},   O1} = get_kv({controller,wf:config(naga,def_controller,ctrl)},O,index),
                     {{_,Act}, P } = get_kv({action,wf:config(naga,def_action,act)},O1,index), 
                     #route{type        =mvc,
                            application =App1,
                            controller  =C,
                            action      =Act,
                            arity       =3,
                            want_session=want_session(C),
                            is_steroid  =is_steroid(C),
                            params      =P,
                            opts        =Opts};

opts(_App,H,Opts) 
  when is_atom(H) -> Opts.
consult(App)      -> case mad_repl:load_file(route_file(App)) of
                      {ok, ETSFile} -> 
                           %wf:info(?MODULE,"LOAD ROUTE FILE from bundle. ~s", [Path]),
                           {ok, mad_tpl:consult(ETSFile)};
                      _ -> file:consult(route_file(App)) end.

routeIndexof(A,O) ->  #route{type=mvc,application=A,controller=naga_indexof,
                        action=index,want_session=true,is_steroid=true,opts=O}.


dispatch_route(App,{Code, Handler, Opts}) 
                   when is_integer(Code) -> [{base_url(App,code_url(Code)), handler(App,Handler), opts(App,Handler,Opts)}];
dispatch_route(App,{Url, Handler, Opts}) -> O = opts(App,Handler,Opts), 
                                            %io:format("URL ~p : ~p~n",[Url,O]),
                                            case O of #route{is_steroid=true} -> 
                                              BaseUrl = n2o_url(App,base_url(App,Url)),
                                              %io:format("URL WS ~p : ~p~n",[BaseUrl,O]),
                                              [{ BaseUrl, wf:config(naga,stream,n2o_stream), O}];
                                              _ -> case Handler of
                                                    naga_indexof ->  
                                                      BaseUrl = n2o_url(App,base_url(App,Url)),
                                                      R = routeIndexof(App,Opts),
                                                      [{ BaseUrl, wf:config(naga,stream,n2o_stream), R}];
                                                    _ -> []
                                                   end 
                                            end ++ [{base_url(App,Url), handler(App,Handler), O}];
dispatch_route(App,Route)                -> wf:error(?MODULE, "Invalid route ~p~n",[App,Route]), [].

dispatch_view(App,ViewModule) 
            when is_atom(ViewModule)     -> [{url(App,ViewModule), 
                                              wf:config(naga,bridge,naga_cowboy), 
                                              #route{type        = view,
                                                     application = case App of {theme,A} -> A;_->App end,
                                                     view        = ViewModule,
                                                     action      = render,
                                                     arity       = 0,
                                                     want_session= false,
                                                     is_steroid  = false}}].
%%FIXME
dispatch_doc(App)                        -> [{ base_url(App,doc_url(App,"/[:docname]")), 
                                              wf:config(naga,bridge,naga_cowboy), 
                                            [#route{type=doc,application=App}]}].

dispatch_mvc(App,CtrlModule)             -> [begin 
                                               Url = url(App,CtrlModule,A),
                                               wf:info(?MODULE,"MVC URL ~p  <- ~p",[Url,{App,CtrlModule,A}]),
                                               {Url, wf:config(naga,bridge,naga_cowboy),
                                                #route{type        = mvc,
                                                       application = App,                                                                     
                                                       controller  = CtrlModule,
                                                       action      = case A of {dft,B} -> B; A -> A end,
                                                       arity       = N,
                                                       want_session= want_session(CtrlModule),
                                                       is_steroid  = is_steroid(CtrlModule)}} 
                                             end || {A,N} <- actions(CtrlModule)].

dispatch(routes,Components)-> Themes = [T||T<-[wf:config(App,theme,[])||App<-Components],T/=[]],
                              lists:foldr( fun(App,Acc) -> 
                                    [case consult(App) of
                                     {ok,[]} = Err -> 
                                        io:format("Invalid NAGA routes file: ~p", 
                                        [route_file(App)]), halt(abort,[]);                                      
                                     {ok, Routes} ->    
                                        lists:foldr(fun({Code, Handler, Opts}, Bcc) ->
                                                      dispatch_route(App,{Code, Handler, Opts}) ++ Bcc                                
                                                    end, [], lists:flatten(Routes));
                                     {error,enoent} = Err -> 
                                        io:format("Missing NAGA routes file: ~p", 
                                        [route_file(App)]), halt(abort,[])
                                   end| Acc] 
                                 end, [], Themes ++ Components );

dispatch(view, Components) -> Themes = [{theme,T}||T<-[wf:config(App,theme,[])||App<-Components],T/=[]],
                              lists:foldr(fun(App,Bcc)->
                                              Views = case App of 
                                                       {theme,A} -> files(view, A);
                                                       App -> files(view, App)
                                                      end,                            
                                              [lists:foldr(fun({_,M},Acc) -> 
                                                            dispatch_view(App,M) ++ Acc
                                                           end, [], Views)|Bcc]
                                          end,[],Components++Themes);

dispatch(doc, Components)  -> lists:foldr(fun(App,Acc)->
                                           dispatch_doc(App) ++ Acc
                                          end,[],Components);

dispatch(mvc, Components)  -> lists:foldr(fun(App,Acc)->
                                            Controllers = files(controller,App),
                                            wf:info(?MODULE, "MVC CTRL ~p",[Controllers]),
                                            [lists:foldr(fun({_,M},Bcc) ->
                                                            dispatch_mvc(App,M) ++ Bcc
                                                         end, [], Controllers)] ++ Acc
                                          end, [], Components);

dispatch(      _, _App)    -> [].
