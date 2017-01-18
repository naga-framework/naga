-module(naga_router).
-behaviour(cowboy_middleware).
-export([execute/2]).
-include("naga.hrl").

execute(Req, Env) ->
	 {_, DispatchModule} = lists:keyfind(dispatch, 1, Env),
	[Host,HostInfo,Path] = cowboy_req:get([host,host_info,path],Req),
	Path0 = filename:split(Path) -- [<<"/">>],
	wf:info(?MODULE,"naga_router ~p -> ~p~n",[Path, Path0]),
	case DispatchModule:match(Path0,undefined) of
		{ok,{{_,Pattern,naga_static,HandlerOpts}=R,Bindings}=Match} ->	
		  PathInfo = path_info(Pattern,Path0),
		  wf:info(?MODULE,"naga_static ~p x (~p) -> ~p~n",[Path0,Pattern,PathInfo]),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, naga_static},{handler_opts, HandlerOpts}|Env]};

		{ok,{{_,Pattern,cowboy_static,HandlerOpts}=R,Bindings}=Match} ->	
		  PathInfo = path_info(Pattern,Path0),
		  wf:info(?MODULE,"cowboy_static ~p x (~p) -> ~p~n",[Path0,Pattern,PathInfo]),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, cowboy_static},{handler_opts, HandlerOpts}|Env]};

		{ok,{{_,Pattern,n2o_static,HandlerOpts}=R,Bindings}} ->
		  PathInfo = path_info(Pattern,Path0),
		  wf:info(?MODULE,"n2o_static ~p x (~p) -> ~p~n",[Path0,Pattern,PathInfo]),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, n2o_static},{handler_opts, HandlerOpts}|Env]};

		{ok,{{N,Pattern,n2o_stream,HandlerOpts}=R,Bindings}=Match} ->
			PathInfo = path_info(Pattern,Path0),
			wf:info(?MODULE,"n2o_stream ~p x (~p) -> ~p~n",[Path0,Pattern,PathInfo]),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, n2o_stream},{handler_opts, HandlerOpts#route{match={N,Pattern,n2o_stream,Bindings}}}|Env]};

		{ok,{{N,Pattern,naga_cowboy,HandlerOpts}=R,Bindings}=Match} ->
			PathInfo = path_info(Pattern,Path0),
			wf:info(?MODULE,"naga_cowboy ~p x (~p) -> ~p~n",[Path0,Pattern,PathInfo]),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, naga_cowboy},{handler_opts, HandlerOpts#route{match={N,Pattern,naga_cowboy,Bindings}}}|Env]};

		{ok,{{N,Pattern,Handler,HandlerOpts}=R,Bindings}=Match} ->
			PathInfo = path_info(Pattern,Path0),
			wf:info(?MODULE,"~p ~p x (~p) -> ~p~n",[Handler,Path0,Pattern,PathInfo]),
		 	Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, naga_cowboy},{handler_opts, HandlerOpts}|Env]};

		_ -> {error, 400, Req}
	end.

path_info([],    P)          -> P;
path_info([_,'*'|_], [_|T2]) -> T2;
path_info([_|T1],    [_|T2]) -> path_info(T1, T2).
