-module(naga_routes).
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, #cx{path=[{formatter,_}|Route]}=Ctx) ->
    #route{application= App,
           controller = Ctr,
           action     = Act} = Route,
    {ok, State, Ctx#cx{path=#{application => App,
                              controller  => Ctr,          
                              action      => Act}, module=Ctr}}.