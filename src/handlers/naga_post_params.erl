-module(naga_post_params).
-include_lib("n2o/include/wf.hrl").
-export([init/2,finish/2]).

init(_State, Ctx) ->
    {Form,NewReq} = wf:form(Ctx#cx.req),
    NewCtx = Ctx#cx{form=Form,req=NewReq},
    {ok, [], NewCtx}.

finish(_State, Ctx) ->  {ok, [], Ctx}.
