-module(naga_load).
-author('Chanrotha Sisowath').
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1,start_link/0]).
-export([onload/1,onnew/2,topsort/1,watch/1,unwatch/1,static/1,routes/1,lang/1]).
-export([view_graph/1,app/1,parents/1,deps/1,source/1, source/2, is_view/1,view_files/1, controller_files/1]).
-record(state,{graphs=#{},apps=[]}).
-include("naga.hrl").

start_link() -> start_link([]).
start_link(A)-> gen_server:start_link({local, ?SERVER}, ?MODULE, A, []).
init(Apps)   -> wf:info(?MODULE,"Starting naga load. watch ~p",[Apps]), 
                active_events:subscribe_onload({?MODULE,onload}), 
                active_events:subscribe_onnew({?MODULE,onnew}),
                active_events:subscribe(lang,{?MODULE,lang}),
                active_events:subscribe(static,{?MODULE,static}),
                active_events:subscribe(routes,{?MODULE,routes}),
                %active_events:subscribe_compile({?MODULE,oncompile}),
                %fixme: watch apps after they started, doesn't mean here. 
                %in dev mode               
                {ok, #state{}}.

handle_call({parents, Module}, _From, State) -> {reply, parents(Module,State), State};
handle_call({deps, Module}, _From, State)    -> {reply, deps(Module,State), State};
handle_call({topsort, App}, _From, State)    -> {reply, topsort(App,State), State};
handle_call({digraph, App}, _From, State)    -> {reply, maps:get(App,State#state.graphs, undefined), State};
handle_call(_Request, _From, State)          -> {reply, ok, State}.
handle_cast({watch, App}, State)             -> {noreply, watch(App,State)};
handle_cast({unwatch, App}, State)           -> {noreply, unwatch(App,State)};
handle_cast({onload, Module}, State)         -> {ok, N} = onload(Module, State),{noreply, N};
handle_cast({onnew, Module}, State)          -> {ok, N} = onnew(Module, State),{noreply, N};
handle_cast({static, Module}, State)         -> {ok, N} = static(Module, State),{noreply, N};
handle_cast({lang, Module}, State)           -> {ok, N} = lang(Module, State),{noreply, N};
handle_cast({routes, Module}, State)         -> {ok, N} = routes(Module, State),{noreply, N};

handle_cast(_Request, State)                 -> {noreply, State}.
handle_info(Info, State)                     -> {noreply, State}.
terminate(_Reason, _State)                   -> ok.
code_change(_OldVsn, State, _Extra)          -> {ok, State}.

oncompile(Module) -> gen_server:cast(?MODULE,{oncompile, Module}).
onload(Module) -> gen_server:cast(?MODULE,{onload, Module}).
onnew(Module)  -> gen_server:cast(?MODULE,{onnew, Module}).
static(Module) -> gen_server:cast(?MODULE,{static, Module}).
lang(Module)   -> gen_server:cast(?MODULE,{lang, Module}).
routes(Module) -> gen_server:cast(?MODULE,{routes, Module}).
watch(App)     -> gen_server:cast(?MODULE,{watch, App}).
unwatch(App)   -> gen_server:cast(?MODULE,{unwatch, App}).
parents(Module)-> gen_server:call(?MODULE,{parents, Module}).
deps(Module)   -> gen_server:call(?MODULE,{deps, Module}).
topsort(App)   -> gen_server:call(?MODULE,{topsort, App}).
digraph(App)   -> gen_server:call(?MODULE,{digraph, App}).


url([]) -> "/";
url(L) -> url(L,[]).

url([],Acc) -> "/"++filename:join(lists:reverse(Acc)); 
url(['*'|T],Acc) -> url(T,[["[...]"]|Acc]);
url([H|T],Acc) when is_atom(H)-> url(T,[[wf:to_list([':',H])]|Acc]);
url([H|T],Acc) -> url(T,[[H]|Acc]).

print(App,Module,L) ->
 Max = lists:foldr(fun({N,_,_,_},A) when N =< 0 -> A;
                      ({N,P,_,O},A) -> max(length(url(P)),A)end,0, L),
 Pad = fun(X) -> S0 = length(X),X ++"\""++ string:chars(32, Max - S0) end,
 io:format("-----~s-~s~n",[string:chars($-, Max+4), string:chars($-, Max+4)]),
 io:format(" DISPATCH for ~p, ~p~n",[App,Module]),
 io:format("-----~s-~s~n",[string:chars($-, Max+4), string:chars($-, Max+4)]),
 io:format(" id | url~s| handler,app/ctrl:action~n",[string:chars(32, Max)]),
 io:format("----|~s|~s~n",[string:chars($-, Max+4), string:chars($-, Max+4)]),
 lists:foreach(
  fun({N,_,_,_}) when N =< 0-> skip;
     ({N,P,naga_static,O})-> 
       io:format("~3.B | \"~s | naga_static~n",[N,Pad(url(P))]);
     ({N,P,cowboy_static,O})->
       io:format("~3.B | \"~s | cowboy_static~n",[N,Pad(url(P))]);
     ({N,P,H,#route{application=A,controller=C,action=Act}=O})->
       io:format("~3.B | \"~s | ~p/~p:~p~n",[N,Pad(url(P)),A,C,Act]);
     ({N,P,H,O})->
       io:format("~3.B | \"~s | ~s~n",[N,Pad(url(P)),wf:to_list(H)])                   
  end,L),
 io:format("----|~s|~s~n",[string:chars($-, Max+4), string:chars($-, Max+4)]).

routes([{A,_}], #state{apps=Apps}=State) -> 
  App = wf:atom([A]),
  {ok, _, _, _, DispatchModule, _N, Rules} = naga:dispatch_routes(App),
  %%FIXME: show diff
  print(App,DispatchModule, Rules),
  Parents = lists:foldl(fun({P,C},Acc) ->
                          case lists:member(App,C) of
                            true -> [P|Acc]; false-> Acc end 
                        end,[],Apps),
  [begin 
    R = naga:route_file(P),
    io:format("=> recompile route file, ~p:~p~n",[P,R]),
    touch(R) 
   end || P<- (Parents--[App])],
  {ok,State}.
static([E], State) -> wf:info(?MODULE, "Receive STATIC event: ~p", [E]),{ok,State}.
lang([E], State)   -> wf:info(?MODULE, "Receive LANG event: ~p", [E]),{ok,State}.
onnew(E, State)  -> wf:info(?MODULE, "Receive ONNEW event: ~p", [E]),{ok,State}.
onload([Module]=E, State)-> 
  wf:info(?MODULE, "Receive ONLOAD event: ~p", [E]),
  case is_view(Module) of 
    false -> {ok,State}; 
    true -> case deps(Module,State) of
              {error,graph_notfound} -> 
                io:format("Graph not found for app ~p~n",[app(Module)]),
                io:format("Use naga:watch(~p).~n",[app(Module)]),
                {ok,State};
              not_a_view -> {ok,State};
              Deps ->
                [touch(P)||P<-Deps],
              %%FIXME: for now, delete,rebuild graph each time, small graph, fast enought
              % [begin {E, V1, V2, Label} = digraph:edge(G,E),{V1,V2} end|| E <- digraph:edges(G,V1)] 
              App = app(Module),
              NewState = watch(App,unwatch(App, State)),
              {ok,NewState}              
            end 
  end.

%FIXME: work 4 linux, macosx ?, window?
touch(File) -> case os:type() of
                {unix,darwin} -> case file:read_file(File) of
                                  {ok,Bin} -> file:write_file(File,Bin);
                                  _ -> sh:run(["touch",File])
                                 end;
                _ -> sh:run(["touch",File]), ok end.

watch([A|T], State) -> watch(T, watch(A,State));
watch([], State) -> State;
 watch(App, #state{graphs=Graphs}=State) -> 
  case maps:get(App, Graphs, undefined) of 
    undefined ->
      C = wf:config(App,modules,[]),
      State#state{apps = State#state.apps ++ [{App,C}],
                  graphs = Graphs#{App => view_graph(App)}}; 
    G -> State end.

unwatch([A|T], State) -> unwatch(T, unwatch(A,State));
unwatch([], State) -> State;
unwatch(App, #state{graphs=Graphs}=State) -> 
  case maps:get(App, Graphs, undefined) of undefined -> skip; 
    G -> New=maps:remove(App,Graphs),
         digraph:delete(G), %%check memory
         State#state{graphs=New} end.

parents(M, #state{graphs=Graphs}=State) ->  
  case is_view(M) of 
    false -> not_a_view; 
    true -> case maps:get(app(M),Graphs, undefined) of undefined -> {error, graph_notfound};
                 G -> digraph:in_neighbours(G,source(M)) end end.

deps(M, #state{graphs=Graphs}=State) -> 
  case is_view(M) of 
    false -> not_a_view; 
    true -> case maps:get(app(M),Graphs, undefined) of undefined -> {error, graph_notfound};
                 G ->  digraph:out_neighbours(G,source(M)) end end.

topsort(App, #state{graphs=Graphs}=State) -> 
  case maps:get(App,Graphs, undefined) of undefined -> {error, graph_notfound};
                 G ->  digraph_utils:topsort(G) end.

view_graph(App) ->
    Nodes = view_files(App),
    {ok, Cwd} = file:get_cwd(),    
    G = digraph:new(),
    [ digraph:add_vertex(G, N) || {N,_} <- Nodes ],
    case all_edges(App, Nodes) of 
        [] -> [];
        Edges -> 
          [ begin 
             E = (B--Cwd) -- "/",
             digraph:add_edge(G, E, A) end || {A, B} <- Edges] end, 
    G.

all_edges(App, Nodes)      -> all_edges(App, Nodes, []).
all_edges(_, [], Acc)      -> lists:flatten(Acc);
all_edges(App, [H|T], Acc) -> all_edges(App, T, [edge(App, H)|Acc]). 

edge(App, {File,Module}) ->
   case  Module:dependencies() of 
    [] -> []; Deps ->[{File, X}||{X,_} <- Deps] end.
  
view_files(App) ->
  Bin = filename:join([code:lib_dir(App),"ebin"]),
  BeamFiles = filelib:wildcard("*.beam", Bin),
  Modules = [list_to_atom(filename:basename(X, ".beam")) || X <- BeamFiles],
  [code:ensure_loaded(M)||M<-Modules],
  [{source(M),M}||M <- Modules, is_view(M)].

controller_files(App) ->
  {ok, Modules} = application:get_key(App,modules),
  [code:ensure_loaded(M)||M<-Modules],
  [{source(App,M),M} || M <- Modules, not is_view(M), is_controller(App,M)].

is_view(M) ->
  erlang:function_exported(M,dependencies,0) andalso 
  erlang:function_exported(M,source,0) andalso
  erlang:function_exported(M,render,0) andalso
  erlang:function_exported(M,render,1) andalso
  erlang:function_exported(M,render,2).

source(M) -> {S, _} = M:source(), S.
source(App, M) 
    when is_atom(App), is_atom(M) -> C = M:module_info(compile),
                                     proplists:get_value(source,C).
  
is_controller(App, M) 
    when is_atom(App), is_atom(M) -> C = M:module_info(compile),
                                     F = proplists:get_value(source,C),
                                     is_controller(atom_to_list(App),filename:split(F));
is_controller(App, []) -> false;
is_controller(App, [H,"apps",App,"src","controller"|_]) -> true;
is_controller(App, [H|T])  ->  is_controller(App,T).

app(M) -> Path = proplists:get_value(source,M:module_info(compile)),
          {ok, Cwd} = file:get_cwd(),          
          [_,App|_] = filename:split(Path) -- filename:split(Cwd),
          wf:to_atom(App).
        

