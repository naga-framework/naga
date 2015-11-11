-module(naga_mvc).
-description('NAGA Server Pages HTTP MVC endpoint handler').
-author('Maxim Sokhatsky').
-author('Chanrotha Sisowath').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").
-compile (export_all).
-record(mvc,{route,pid,ctx,ctx1,reqCtx}).

transition(Actions) -> receive {'INIT',A} -> transition(A); {'N2O',Pid} -> Pid ! {actions,Actions} end.
run(Req) ->
    wf:state(status,200),
    Pid = spawn(fun() -> transition([]) end),
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(n2o,websocket_port,wf:config(n2o,port,8000))),"'}"]),
    Ctx = wf:init_context(Req),
    Ctx1 = wf:fold(init,Ctx#cx.handlers,Ctx),
    wf:actions(Ctx1#cx.actions),
    wf:context(Ctx1),
    case Ctx1#cx.path of #route{}=Route ->
            #route{app=App,controller=Ctrl,action=Act,opts=O} = Route,
            case erlang:function_exported(Ctrl, Act, 3) of 
                true  ->
                    Mvc = #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1,route=Route, reqCtx=reqCtx(Route)},
                    handler(Req, Mvc);
                    % case before(Mvc, wf:config(App, filter_config, undefined), wf:config(App,before,[])) of
                    %     {ok, NewMvc} ->  
                    %       handler(Req, NewMvc);
                    %     {error, NewMvc, ErrorElement} -> render_elements(Req, NewMvc, ErrorElement) end;
                false ->
                    Elements = try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end,
                    render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements) end;
         _->
            Elements = try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end,
            render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements) end.

render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements) ->
    Html = wf_render:render(Elements),
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = wf:fold(finish,Ctx#cx.handlers,?CTX),   
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    %wf:info(?MODULE,"Cookies Req: ~p",[Req2]),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2).

set_cookies([],Req)-> Req;
set_cookies([{Name,Value,Path,TTL}|Cookies],Req)->
    set_cookies(Cookies,wf:cookie_req(Name,Value,Path,TTL,Req)).

reqCtx(#route{app=App,controller=Ctrl,action=Act}) -> 
    #{ '_app'       => App, 
       '_controller'=> Ctrl, 
       '_action'    => Act, 
       '_lang'      => ?CTX#cx.lang,
       '_session_id'=> wf:session_id()
     }.

%% FIXME: check time spend here
before(Mvc, undefined, []) -> {ok, Mvc};
before(#mvc{route=#route{controller=Ctrl},reqCtx=ReqCtx}=Mvc, CfgFilters, Filters) ->
    NewFilters = case erlang:function_exported(Ctrl, before_filters, 2) of 
                     true -> Ctrl:before_filters(Filters, ReqCtx); false -> Filters end,
    case lists:foldl(fun(F, {ok, Ctx}) -> 
                        try F:before_filter(CfgFilters,Ctx) catch C:E -> {error, Ctx, wf:error_page(C,E)} end;
                        (_, {error, _, _}=Err) -> Err;
                        (_, Acc) -> Acc 
                      end, {ok, ReqCtx},  NewFilters) of
    {ok, NewCtx} -> {ok, Mvc#mvc{reqCtx=NewCtx}};
    {error, NewCtx, Error} -> {error, Mvc#mvc{reqCtx=NewCtx}, Error} end.


handler(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act,opts=O},reqCtx=ReqCtx}=Mvc) ->
  R = try Ctrl:Act(wf:method(Req),[],ReqCtx) catch C:E -> {error,wf:error_page(C,E)} end, 
  wf:info(?MODULE, "~p~n", [R]), 
  try render(Req, Mvc, R) catch C1:E1 -> render_elements(Req, Mvc, wf:error_page(C1,E1)) end.

render(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, #dtl{}=Elements) ->
    render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements);
render(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, {error,Elements}) ->
    render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements);

render(Req, Mvc, {ok, Vars}) -> render(Req, Mvc, {ok, [], Vars});
render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act},reqCtx=ReqCtx}=Mvc, {ok, Headers, Vars}) ->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers),
  render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"_html"}, app=App, bindings=Vars++maps:to_list(ReqCtx)});

render(Req, Mvc, {redirect, Location}) -> render(Req, Mvc, {redirect, Location, []});
render(Req, Mvc, {redirect, Location, Headers}) ->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++[{<<"Location">>,wf:to_binary(Location)}]), 
  wf:state(status,302),
  {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

render(Req, Mvc, {moved, Location}) -> render(Req, Mvc, {moved, Location, []});
render(Req, Mvc, {moved, Location, Headers}) ->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end, Req, Headers++[{<<"Location">>,wf:to_binary(Location)}]), 
  wf:state(status,301),
  {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

render(Req, Mvc, {action_other, #route{}=New}) -> 
   wf:info(?MODULE, "action other ~p ~n",[{New#route.controller,New#route.action}]),
   handler(Req, Mvc#mvc{route=New});

% render(Req, Mvc#mvc{reqCtx=ReqCtx}, {render_other, #route{app=App,controller=Ctrl,action=Act}}) -> 
%    wf:info(?MODULE, "render other ~p ~n",[{New#route.controller,New#route.action}]),
%    render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"_html"}, app=App, bindings=maps:to_list(ReqCtx)}).

render(Req, Mvc, {json, Vars}) -> render(Req, Mvc, {json, Vars, []});
render(Req, Mvc, {json, Vars, Headers}) -> render(Req, Mvc, {json, Vars, Headers, 200});
render(Req, Mvc, {json, Vars, Headers, Status}) ->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++?CTYPE_JSON), 
  Req3 = wf:response(wf:json(Vars),set_cookies(wf:cookies(),Req2)),
  wf:state(status,Status),
  %% FIXME: call finish
  {ok, _ReqFinal} = wf:reply(wf:state(status), Req3);

render(Req, Mvc, {{json,dtl}, Vars}) -> render(Req, Mvc, {{json,dtl}, Vars, []});
render(Req, Mvc, {{json,dtl}, Vars, Headers}) -> render(Req, Mvc, {{json,dtl}, Vars, Headers, 200});
render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act},reqCtx=ReqCtx}=Mvc, {{json,dtl}, Vars, Headers, Status}) ->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++?CTYPE_JSON), 
  wf:state(status,Status),
  Json = wf_render:render(#dtl{file={App,Ctrl,Act,"_json"}, app=App, bindings=Vars++maps:to_list(ReqCtx)}),
  Req3 = wf:response(Json,Req2),
  {ok, _ReqFinal} = wf:reply(wf:state(status), Req3);

render(Req, Mvc, {Status, Body, Headers}) when is_integer(Status)->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers), 
  Req3 = wf:response(Body,set_cookies(wf:cookies(),Req2)),
  wf:state(status,Status),
  %% FIXME: call finish
  {ok, _ReqFinal} = wf:reply(wf:state(status), Req3).

%{StatusCode::integer(), Body::iolist(), Headers::proplist()}


