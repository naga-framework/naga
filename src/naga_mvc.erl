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
            #route{app=App,controller=Ctrl,action=Act} = Route,
            case erlang:function_exported(Ctrl, Act, 3) of 
                true  -> handler(Req, #mvc{pid=Pid,route=Route, ctx=Ctx, ctx1=Ctx1, reqCtx=reqCtx(Route)});
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

%%req => (1)routing  -> {app ,controller,action} *? hook {before}
%%    -> (2)handler  -> {code,   headers,  vars} *? hook {middle}
%%    -> (3)render   -> {code,   headers,  body} *? hook {after}
%%    -> (4)response => {code,   headers,  body} 

reqCtx(#route{app=App,controller=Ctrl,action=Act}) -> 
    #{ '_app' => App, '_controller'=>Ctrl, 
       '_action'=>Act, '_lang' => ?CTX#cx.lang,
       '_session_id' => wf:session_id()
     }.

handler(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act,opts=O},reqCtx=ReqCtx}=Mvc) ->
  Result = try Ctrl:Act(wf:method(Req),[],ReqCtx) catch C:E -> {error,wf:error_page(C,E)} end,  
  try render(Req, Mvc, Result) catch C1:E1 -> render_elements(Req, Mvc, wf:error_page(C1,E1)) end.

render(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, #dtl{}=Elements) ->
    render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements);
render(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, {error,Elements}) ->
    render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements);

render(Req, Mvc, {ok, Vars}) -> render(Req, Mvc, {ok, [], Vars});
render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act},reqCtx=ReqCtx}=Mvc, {ok, Headers, Vars}) ->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++ctype(html)),
  render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"_html"}, app=App, bindings=Vars++maps:to_list(ReqCtx)}).

% % render json with erlydtl
% render(Req, Mvc, {{json,view}, Vars}) -> render(Req, Mvc, {js, [], Vars});
% render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act}}=Mvc, {js, Headers, Vars}) ->
%   Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++ctype(json)),
%   render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"js"}, app=App, bindings=Vars});

% % render json with jsone
% render(Req, Mvc, {json, Vars}) -> render(Req, Mvc, {js, [], Vars});
% render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act}}=Mvc, {js, Headers, Vars}) ->
%   Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++ctype(json)),
%   render_elements(Req2, Mvc, jsone:encode(Vars));

% % render js with erlydtl
% render(Req, Mvc, {js, Vars}) -> render(Req, Mvc, {js, [], Vars});
% render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act}}=Mvc, {js, Headers, Vars}) ->
%   Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++ctype(js)),
%   render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"js"}, app=App, bindings=Vars});

% % render css with erlydtl
% render(Req, Mvc, {css, Vars}) -> render(Req, Mvc, {css, [], Vars});
% render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act}}=Mvc, {css, Headers, Vars}) ->
%   Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++ctype(css)),
%   render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"css"}, app=App, bindings=Vars});

% % render txt with erlydtl
% render(Req, Mvc, {txt, Vars}) -> render(Req, Mvc, {txt, [], Vars});
% render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act}}=Mvc, {css, Headers, Vars}) ->
%   Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++ctype(txt)),
%   render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"txt"}, app=App, bindings=Vars}).

% render(Req, Mvc, {output, Vars}) -> 
%  render(Req, Mvc#mvc{route=New}, {ok, Vars});

% render(Req, Mvc, {render_other, #route{}=New, Vars}) -> 
%  render(Req, Mvc#mvc{route=New}, {ok, Vars});

%301
%redirect({http,Url}) -> wf:header(<<"Location">>,wf:to_binary(Url)), wf:state(status,301), [];
% render(Req,Mvc,{redirect, Location}) -> 
%     wf:redirect({http,Location}), render_elements(Req,Mvc,#dtl{});
% render(Req,Mvc,{redirect, Location, Headers}) -> 
%     Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers),
%     wf:redirect({http,Location}),render_elements(Req2,Mvc,#dtl{}).

%FIXME: 302 ?


ctype(Type) ->
    case Type of
        js   -> [{<<"Content-Type">>,<<"application/javascript; charset=UTF">>}];
        html -> [{<<"Content-Type">>,<<"text/html; charset=UTF-8">>}];
        xml  -> [{<<"Content-Type">>,<<"text/xml; charset=UTF-8">>}];
        css  -> [{<<"Content-Type">>,<<"text/css; charset=UTF-8">>}];
        json -> [{<<"Content-Type">>,<<"application/json; charset=UTF-8">>}];
        txt  -> [{<<"Content-Type">>,<<"text/plain; charset=UTF-8">>}]
    end.

