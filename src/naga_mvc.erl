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

%%req => (1)routing  -> {app ,controller,action} *? hook {before}
%%    -> (2)handler  -> {code,   headers,  vars} *? hook {middle}
%%    -> (3)render   -> {code,   headers,  body} *? hook {after}
%%    -> (4)response => {code,   headers,  body} 

%% ----------- controller return value from CB API ---------- code ----------------------- DOC ---------------------------------
%% {ok, Variables::proplist()}                                200  Variables will be passed into the associated Django template.
%%
%% {ok, Variables::proplist(), Headers::proplist()}           200  Variables will be passed into the associated Django template, 
%%                                                                  and Headers are HTTP headers you want to set (e.g., Content-Type).
%% js                                                         200  The template will be rendered without any variables 
%%                                                                  and served as Content-Type: application/javascript.
%% {js, Variables::proplist()}                                200  Variables will be passed into the associated Django template and 
%%                                                                  the result will be served as Content-Type: application/javascript.
%% {js, Variables::proplist(), Headers::proplist()}           200  Variables will be passed into the associated Django template and 
%%                                                                  the result will be served as Content-Type: application/javascript.
%%                                                                  and Headers are HTTP headers you want to set.
%% {redirect, Location}                                       302  Perform a 302 HTTP redirect to Location, 
%%                                                                  which may be a URL string or a proplist of parameters that will be
%%                                                                  converted to a URL using the routes system.
%% {redirect, Location, Headers::proplist()}                  302  Perform a 302 HTTP redirect to Location and set additional 
%%                                                                  HTTP Headers.
%% {moved, Location}                                          301  Perform a 301 HTTP redirect to Location, which may be a URL string 
%%                                                                  or a proplist of parameters that will be converted to a URL using 
%%                                                                  the routes system.
%% {{moved, Location, Headers::proplist()}                    301  Perform a 301 HTTP redirect to Location and set additional 
%%                                                                  HTTP Headers.
%% {action_other, OtherLocation}                              200  Execute the controller action specified by OtherLocation, 
%%                                                                  but without performing an HTTP redirect.
%% {render_other, OtherLocation}                              200  Render the view from OtherLocation, but don't actually execute 
%%                                                                  the associated controller action. 
%% {render_other, OtherLocation, Variables}                   200  Render the view from OtherLocation using Variables, 
%%                                                                  but don't actually execute the associated controller action.
%% {output, Output::iolist()}                                 200  Skip views altogether and return Output to the client.
%%
%% {output, Output::iolist(), Headers::proplist()}            200  Skip views altogether and return Output to the client
%%                                                                  while setting additional HTTP Headers.
%% {stream, Generator::function(), Acc0}                      200  Stream a response to the client using HTTP chunked encoding. 
%%                                                                  For each chunk, the Generator function is passed
%%                                                                  an accumulator (initally Acc0) and should return either 
%%                                                                  {output, Data, Acc1} or done.
%% {stream, Generator::function(), Acc0, Headers::proplist()} 200  Same as above, but set additional HTTP Headers.
%% 
%% {json, Data::proplist()}                                   200  Return Data as a JSON object to the client. Performs appropriate 
%%                                                                  serialization if the values in Data contain a BossRecord or a list of BossRecords.
%% {json, Data::proplist(), Headers::proplist()}              200  Return Data as a JSON object to the client. Performs appropriate 
%%                                                                  serialization if the values in Data contain a BossRecord or a list of BossRecords.
%% {jsonp, Data::proplist()}                                  200  Returns Data as a JSONP method call to the client. 
%%                                                                  Performs appropriate serialization if the values in Data contain a BossRecord 
%%                                                                  or a list of BossRecords.                                                            
%% {jsonp, Data::proplist(), Headers::proplist()}             200  Return Data as a JSON object to the client. Performs appropriate 
%%                                                                 serialization if the values in Data contain a BossRecord or a list of BossRecords.
%% {jsonp, Callback::string(), Data::proplist(), Headers::proplist()} 
%%                                                            200  Return Data to the client as a JSONP method call (as above) 
%%                                                                  while setting additional HTTP Headers.
%% not_found                                                  404  Invoke the 404 File Not Found handler.
%% 
%% {Code::integer(), Body::iolist(), Headers::proplist()}     Code Return an arbitary HTTP integer StatusCode along with a Body 
%%                                                                  and additional HTTP Headers.
%%
%% ---------------------------------------------------------------------------------------


%% ===== Filter Config ======
%% Filter module can be installed with the controller_filter_modules config option:
%% {controller_filter_modules, [my_awesome_filter1, my_awesome_filter2]}
%% Filters are applied in order. For a particular controller, you can override the default filter list with the following three functions:


%% ===== SHORT NAME ======
%% Setting a short name and default config value
%% Filter modules can export two functions to set a short name for themselves and to provide a default config value:

%% -module(my_awesome_filter).
%% -export([config_key/0, config_default_value/0]).

%% config_key() -> 'awesome'.
%% config_default_value() -> [{awesomeness, 50}].


before(Mvc, undefined, []) -> {ok, Mvc};
before(#mvc{route=#route{controller=Ctrl},reqCtx=ReqCtx}=Mvc, CfgFilters, Filters) ->
    NewFilters = case erlang:function_exported(Ctrl, before_filters, 2) of 
                     true -> Ctrl:before_filters(Filters, ReqCtx); false -> Filters end,
    case lists:foldl(fun(F, {ok, Ctx}) -> 
                        try F:before_filter(CfgFilters,Ctx) catch C:E -> {error, Ctx, wf:error_page(C,E)} end;
                        (_, {error, _, _}=Err) -> Err;
                        (_, Acc) -> Acc % redirect 
                      end, {ok, ReqCtx},  NewFilters) of
    {ok, NewCtx} -> {ok, Mvc#mvc{reqCtx=NewCtx}};
    {error, NewCtx, Error} -> {error, Mvc#mvc{reqCtx=NewCtx}, Error} end.

%% no related to before filter        
handler(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act,opts=O},reqCtx=ReqCtx}=Mvc) ->
  R = try Ctrl:Act(wf:method(Req),[],ReqCtx) catch C:E -> {error,wf:error_page(C,E)} end,  
  try render(Req, Mvc, R) catch C1:E1 -> render_elements(Req, Mvc, wf:error_page(C1,E1)) end.

render(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, #dtl{}=Elements) ->
    render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements);
render(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, {error,Elements}) ->
    render_elements(Req, #mvc{pid=Pid,ctx=Ctx,ctx1=Ctx1}, Elements);

render(Req, Mvc, {ok, Vars}) -> render(Req, Mvc, {ok, [], Vars});
render(Req, #mvc{route=#route{app=App,controller=Ctrl,action=Act},reqCtx=ReqCtx}=Mvc, {ok, Headers, Vars}) ->
  Req2 = lists:foldl(fun({K,V},Rq) -> wf:header(K,V, Rq) end,Req, Headers++ctype(html)),
  render_elements(Req2, Mvc, #dtl{file={App,Ctrl,Act,"_html"}, app=App, bindings=Vars++maps:to_list(ReqCtx)}).


ctype(Type) ->
    case Type of
        js   -> [{<<"Content-Type">>,<<"application/javascript; charset=UTF">>}];
        html -> [{<<"Content-Type">>,<<"text/html; charset=UTF-8">>}];
        xml  -> [{<<"Content-Type">>,<<"text/xml; charset=UTF-8">>}];
        css  -> [{<<"Content-Type">>,<<"text/css; charset=UTF-8">>}];
        json -> [{<<"Content-Type">>,<<"application/json; charset=UTF-8">>}];
        txt  -> [{<<"Content-Type">>,<<"text/plain; charset=UTF-8">>}]
    end.

