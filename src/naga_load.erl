-module(naga_load).
-author('Chanrotha Sisowath').
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1,start_link/0]).
-export([onload/1,onnew/2,topsort/1,watch/1,unwatch/1]).
-export([view_graph/1,app/1,parents/1,deps/1,source/1, source/2, is_view/1,view_files/1, controller_files/1]).
-record(state,{graphs=#{}}).

start_link() -> start_link([]).
start_link(A)-> gen_server:start_link({local, ?SERVER}, ?MODULE, A, []).
init(Apps)   -> wf:info(?MODULE,"Starting naga load. watch ~p",[Apps]), 
                active_events:subscribe_onload({?MODULE,onload}), 
                active_events:subscribe_onnew({?MODULE,onnew}),
                %active_events:subscribe_compile({?MODULE,oncompile}),
                %fixme: watch apps after they started, doesn't mean here. 
                %in dev mode               
                {ok, #state{}}.

handle_call({parents, Module}, _From, State) -> {reply, parents(Module,State), State};
handle_call({deps, Module}, _From, State)    -> {reply, deps(Module,State), State};
handle_call({topsort, App}, _From, State)    -> {reply, topsort(App,State), State};
handle_call(_Request, _From, State)          -> {reply, ok, State}.
handle_cast({watch, App}, State)             -> {noreply, watch(App,State)};
handle_cast({unwatch, App}, State)           -> {noreply, unwatch(App,State)};
handle_cast({onload, Module}, State)         -> {ok, N} = onload(Module, State),{noreply, N};
handle_cast({onnew, Module}, State)          -> {ok, N} = onnew(Module, State),{noreply, N};
handle_cast(_Request, State)                 -> {noreply, State}.
handle_info(Info, State)                     -> {noreply, State}.
terminate(_Reason, _State)                   -> ok.
code_change(_OldVsn, State, _Extra)          -> {ok, State}.

oncompile(Module) -> gen_server:cast(?MODULE,{oncompile, Module}).
onload(Module) -> gen_server:cast(?MODULE,{onload, Module}).
onnew(Module)  -> gen_server:cast(?MODULE,{onnew, Module}).
watch(App)     -> gen_server:cast(?MODULE,{watch, App}).
unwatch(App)   -> gen_server:cast(?MODULE,{unwatch, App}).
parents(Module)-> gen_server:call(?MODULE,{parents, Module}).
deps(Module)   -> gen_server:call(?MODULE,{deps, Module}).
topsort(App)   -> gen_server:call(?MODULE,{topsort, App}).

onnew(E, State) -> wf:info(?MODULE, "Receive ONNEW event: ~p", [E]),{ok,State}.
onload([Module]=E, State)-> 
  wf:info(?MODULE, "Receive ONLOAD event: ~p", [E]),
  case is_view(Module) of false -> skip; true ->
      case parents(Module,State) of [] -> skip;
        Parents -> [compile(P)||P<-Parents] end end, 
  %%FIXME: for now, delete,rebuild graph each time, small graph, fast enought
  % [begin {E, V1, V2, Label} = digraph:edge(G,E),{V1,V2} end|| E <- digraph:edges(G,V1)] 
  App = app(Module),
  NewState = watch(App,unwatch(App, State)),
  {ok,NewState}.

%FIXME: work 4 linux, macosx ?, window?
compile(File) -> sh:run(["touch",File]), ok.

watch([A|T], State) -> watch(T, watch(A,State));
watch([], State) -> State;
 watch(App, #state{graphs=Graphs}=State) -> 
  case maps:get(App, Graphs, undefined) of 
    undefined -> State#state{graphs = Graphs#{App => view_graph(App)}}; 
    G -> State end.

unwatch([A|T], State) -> unwatch(T, unwatch(A,State));
unwatch([], State) -> State;
unwatch(App, #state{graphs=Graphs}=State) -> 
  case maps:get(App, Graphs, undefined) of undefined -> State; 
       G -> digraph:delete(G), State#state{graphs=maps:remove(App,Graphs)} end.

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
    case all_edges(App, Nodes, Cwd) of 
        [] -> [];Edges -> [digraph:add_edge(G, A, B) || {A, B} <- Edges], {App, G} end,
    G.

all_edges(App, Nodes)      -> all_edges(App, Nodes, []).
all_edges(_, [], Acc)      -> lists:flatten(Acc);
all_edges(App, [H|T], Acc) -> all_edges(App, T, [edge(App, H)|Acc]). 

edge(App, {File,Module}) ->
   case  Module:dependencies() of [] -> []; Deps ->[{File, X}||{X,_} <- Deps] end.
  
view_files(App) ->
  case application:get_key(App,modules) of
    undefined -> wf:error(?MODULE, "App (~p) is not started.",[App]),[];
    {ok , Modules} -> [code:ensure_loaded(M)||M<-Modules],
                      [{source(M),M}||M <- Modules, is_view(M)] end.

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

app(M) -> [_, "ebin", App |_] = lists:reverse(filename:split(code:which(M))), 
          list_to_atom(App).

