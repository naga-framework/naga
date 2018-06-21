-module(naga_electron).
-include_lib("n2o/include/wf.hrl").

-compile(export_all).

info({electron,Msg},Req,State) ->
  Module = State#cx.module,
  Reply = try Module:event({electron,Msg}) 
          catch E:R -> Error = wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
  Actions = n2o_nitrogen:render_actions(wf:actions()),
  wf:info(?MODULE,"Actions ~p~n",[Actions]),
  {reply,wf_convert:format({io,Actions,Reply},json),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.