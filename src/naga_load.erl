-module(naga_load).
-author('Chanrotha Sisowath').
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {graphs}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init(Apps) ->wf:info(?MODULE,"Starting naga load.",[]), 
             active_events:subscribe_onload({?MODULE,onload}), 
             active_events:subscribe_onnew({?MODULE,onnew}),
             {ok, #state{}}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast({add,App}, State) -> {noreply, State#state{graphs=view_graph(App)}};
handle_cast({onload, Module}, State) -> {ok, N} = onload(Module, State),{noreply, N};
handle_cast({onnew, Module}, State) -> {ok, N} = onnew(Module, State),{noreply, N};
handle_cast(_Request, State) -> {noreply, State}.
handle_info(Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

onload(Module) -> gen_server:cast(?MODULE,{onload, Module}).
onnew(Module) ->  gen_server:cast(?MODULE,{onnew, Module}).

%FIXME: update graph, when depencies change -> add/remove include template, force recompile tpl
onnew(Module, State) -> wf:info(?MODULE, "Receive ONNEW event: ~p~n", [Module]),{ok,State}.
onload(Module, State)-> wf:info(?MODULE, "Receive ONLOAD event: ~p~n", [Module]),{ok,State}.

%FIXME
view_parents(G, View) -> digraph:in_neighbours(G,View).
view_deps(G, View)    -> digraph:out_neighbours(G,View).

view_graph(App) ->
    Views = mad_naga:files(App, view),
    {ok, Cwd} = file:get_cwd(),
    G = digraph:new(),
    [ digraph:add_vertex(G, N) || N <- Views ],
    case all_edges(App, Views, Cwd) of [] -> []; Edges -> 
        [digraph:add_edge(G, A, B) || {A, B} <- Edges], {App, G} end.

all_edges(App, Files, Cwd)      -> all_edges(App, Files, Cwd, []).
all_edges(_, [],_, Acc)         -> lists:flatten(Acc);
all_edges(App, [H|T], Cwd, Acc) -> all_edges(App, T, Cwd, [edge(App, H, Cwd)|Acc]). 

edge(App, File) -> {ok, Cwd} = file:get_cwd(), edge(App, File, Cwd).
edge(App, File, Cwd) ->
   Handler = mad_naga:modules(App, view),
   SEP="/",
   case  Handler:dependencies() of
      [] -> [];
      Deps ->[{File, lists:subtract(X, Cwd ++ SEP)}||{X,_} <- Deps] end.
  
all_nodes(App, Files) ->all_nodes(App, Files, []).
all_nodes(_, [], Acc) -> Acc;
all_nodes(App, [H|T], Acc) -> all_nodes(App, T, [node(App, H)|Acc]).
node(App, File) -> {F, _} = (mad_naga:modules(App, File)):source(), F.
