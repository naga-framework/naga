-module(naga_electron).
-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").
-compile(export_all).

info({electron,init},Req,State) ->
  Module = State#cx.module,
  UserCx = try Module:event(electron) catch 
              C:E -> Error = wf:stack(C,E),
                     wf:error(?MODULE,"Event Init: ~p:~p~n~p",Error),
                     {stack,Error} 
           end,
  Actions = render_actions(wf:actions()),
  {reply,wf_convert:format({io,iolist_to_binary(Actions),<<>>},json),
         Req,wf:context(State,?MODULE,{electron,UserCx})};

info(Message,Req,State) -> {unknown,Message,Req,State}.

render_actions(Actions) ->
  wf:actions([]),
  First  = wf:render(Actions),
  Second = wf:render(wf:actions()),
  wf:actions([]),
  [First,Second].