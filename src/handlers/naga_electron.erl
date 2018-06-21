-module(naga_electron).
-include_lib("n2o/include/wf.hrl").

-compile(export_all).

info({electron,Msg},Req,State) ->
  Module = State#cx.module,
  try Module:event({electron,Msg}) 
  catch E:R -> Error = wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
  Actions = render_actions(wf:actions()),
  {reply,wf_convert:format({io,iolist_to_binary(Actions),<<>>},json),Req,State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

render_actions(Actions) ->
  wf:actions([]),
  First  = wf:render(Actions),
  Second = wf:render(wf:actions()),
  wf:actions([]),
  [First,Second].