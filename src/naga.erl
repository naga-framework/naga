-module(naga).
-description('NAGA OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1]).
-include("naga.hrl").
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

tables()   -> [ translation ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> supervisor:start_link({local,naga},naga,[]).
stop(_)    -> ok.
init([])   -> 
              { ok, { { one_for_one, 5, 10 }, 
	                [
                      ?CHILD(naga_load, worker, [])
	                ] 
              }}.
