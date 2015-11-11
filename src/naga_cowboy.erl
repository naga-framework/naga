-module(naga_cowboy).
-description('NAGA Cowboy Server Bridge to HTTP Server Pages').
-author('Roman Shestakov').
-include_lib("n2o/include/wf.hrl").
-export([init/2]).
-compile(export_all).
-record(state, {headers, body}).

% Cowboy HTTP Handler

init(Req, Opts)  -> {ok, NewReq} = naga_mvc:run(Req), {ok, NewReq, State}.


% Cowboy Bridge Abstraction

params(Req)                -> cowboy_req:qs_vals(Req).
form(Req)                  -> {ok,Params,NewReq} = cowboy_req:body_qs(Req), {Params,NewReq}.
path(Req)                  -> cowboy_req:path(Req).
method(Req)                -> cowboy_req:method(Req).
request_body(Req)          -> cowboy_req:body(Req).
headers(Req)               -> cowboy_req:headers(Req).
header(Name, Value, Req)   -> cowboy_req:set_resp_header(Name, Value, Req).
response(Html,Req)         -> cowboy_req:set_resp_body(Html,Req).
reply(StatusCode,Req)      -> cowboy_req:reply(StatusCode, Req).
peer(Req)                  -> cowboy_req:peer(Req).
delete_cookie(Cookie,Req)  -> cookie(Cookie,<<"">>,<<"/">>,0,Req).
cookies(Req)               -> element(1,cowboy_req:cookies(Req)).
cookie(Cookie,Req)         -> element(1,cowboy_req:cookie(wf:to_binary(Cookie),Req)).
cookie(Cookie, Value, Req) -> cookie(Cookie,Value,<<"/">>,0,Req).
cookie(Name, Value, Path, TTL, Req) ->
    Options = [{path, Path}, {max_age, TTL}],
    cowboy_req:set_resp_cookie(Name, Value, Options, Req).