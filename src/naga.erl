-module(naga).
-description('NAGA OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1, watch/1, unwatch/1]).
-include("naga.hrl").
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

tables()   -> [ translation ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> supervisor:start_link({local,naga},naga,[]).
stop(_)    -> ok.
init([])   -> 
              { ok, { { one_for_one, 5, 10 }, 
	                [	                  
                      ?CHILD(naga_load, worker, [apps()])
	                ] 
              }}.

apps()       -> wf:config(naga,watch,[]).
watch(App)   -> naga_load:watch(App).
unwatch(App) -> naga_load:unwatch(App).
is_view(M)   -> naga_load:is_view(M).
source(M)    -> naga_load:source(M).
app(M)       -> naga_load:app(M).

