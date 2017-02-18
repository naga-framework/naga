-module(naga_router).
-behaviour(cowboy_middleware).
-export([execute/2]).
-include("naga.hrl").

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
