-module(naga_mvc).
-description('NAGA Server Pages HTTP MVC endpoint handler').
-author('Maxim Sokhatsky').
-author('Chanrotha Sisowath').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").
-export([run/2,transition/1]).
-export([controller/2]).

transition(Actions) -> receive {'INIT',A} -> transition(A); {'N2O',Pid} -> Pid ! {actions,Actions} end.
run(Req, []) ->
    wf:state(status,200),
    Pid = spawn(fun() -> transition([]) end),
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(n2o,websocket_port,wf:config(n2o,port,8000))),"'}"]),
    Ctx = wf:init_context(Req),
    Ctx1 = wf:fold(init,Ctx#cx.handlers,Ctx),    
    wf:actions(Ctx1#cx.actions),
    wf:context(Ctx1),
    Elements = case Ctx1#cx.path of              
                #{}=R -> #{application:=App,module:=Module,action:=A,method:=M,params:=P,bindings:=B}=R,
                         try handle(App,Module,A,M,P,B) catch C:E -> wf:error_page(C,E) end;
                    _ -> try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end
               end,
    Html = render(Elements),    
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = wf:fold(finish,Ctx#cx.handlers,?CTX),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    %wf:info(?MODULE,"Cookies Req: ~p",[Req2]),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

run(Req, #route{type=controller,is_steroid=true}=Route) ->
    #route{application=App,module=Module,action=Act,arity=A,
           want_session=WantSession}=Route,
    wf:state(status,200),
    Pid = spawn(fun() -> transition([]) end),
    %FIXME: websocket port 
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(n2o,websocket_port,wf:config(n2o,port,8000))),"'}"]),
    Ctx = wf:init_context(Req),
    Ctx1 = init(Ctx, false, WantSession),
    wf:actions(Ctx1#cx.actions),
    wf:context(Ctx1),
    Elements = case (Act == main) andalso (A == 0) of 
                  true -> try Module:main() catch C:E -> wf:error_page(C,E) end;
                  false  -> {M, _} = cowboy_req:method(Req),
                            {P, _} = cowboy_req:path_info(Req),
                            {B, _} = cowboy_req:bindings(Req),
                            try handle(App,Module,Act,M,P,B) catch C:E -> wf:error_page(C,E) end
               end,
    Html = render(Elements),    
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = finish(Ctx,?CTX, false, WantSession),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

run(Req, #route{type=controller,is_steroid=false}=Route) ->
    #route{application=App,module=Module,action=Act,arity=A,
           want_session=WantSession} = Route,
    wf:state(status,200),
    Ctx = wf:init_context(Req),
    Ctx1 = init(Ctx, false, WantSession),
    wf:context(Ctx1),
    Elements = case (Act == main) andalso (A == 0) of 
                  true  -> try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end;
                  false -> {M, _} = cowboy_req:method(Req),
                           {P, _} = cowboy_req:path_info(Req),
                           {B, _} = cowboy_req:bindings(Req),
                           try handle(App,Module,Act,M,P,B) catch C:E -> wf:error_page(C,E) end
               end,    
    Html = render(Elements),    
    Ctx2 = finish(Ctx,?CTX, false, WantSession),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

run(Req, #route{type=view,module=Module}) ->
    wf:state(status,200),   
    {ok,Html} = Module:render(),    
    Req2 = wf:response(Html,Req),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2).


no_session(L)-> lists:keydelete(session,1,L).
no_route(L)  -> lists:keydelete(route,1,L).

  init(C,true ,true )     -> wf:fold(init  ,                     C#cx.handlers  ,C);
  init(C,false,true )     -> wf:fold(init  ,no_route(            C#cx.handlers) ,C);
  init(C,true ,false)     -> wf:fold(init  ,         no_session( C#cx.handlers) ,C);
  init(C,false,false)     -> wf:fold(init  ,no_route(no_session( C#cx.handlers)),C).
finish(C1,C2,true ,true ) -> wf:fold(finish,                    C1#cx.handlers  ,C2);
finish(C1,C2,false,true ) -> wf:fold(finish,no_route(           C1#cx.handlers) ,C2);
finish(C1,C2,true ,false) -> wf:fold(finish,         no_session(C1#cx.handlers) ,C2);
finish(C1,C2,false,false) -> wf:fold(finish,no_route(no_session(C1#cx.handlers)),C2).
 
set_cookies([],Req)-> Req;
set_cookies([{Name,Value,Path,TTL}|Cookies],Req)->
    set_cookies(Cookies,wf:cookie_req(Name,Value,Path,TTL,Req)).

handle(App,C,undefined,M,P,B) -> case erlang:function_exported(C,index,3) of 
                                    true -> handle(App,C,index,M,P,B); _-> C:main() end;
handle(App,C,A,M,P,B)         -> case before(App,C,req_ctx(App,C,A,M,P,B)) of
                                      {ok, Ctx} -> {C:A(M,P,Ctx),Ctx}; 
                                      {error,E} -> error(E);
                                      {redirect, R} -> wf:redirect(R) end.

req_ctx(App,C,A,M,P,B)  -> #{'_app'        => App,
                             '_method'     => M,
                             '_controller' => C,
                             '_action'     => A,
                             '_bindings'   => B
                            %'_params'     => P
                            %'_lang'       => ?CTX#cx.lang,
                            %'_sid'        => wf:session_id()
                          }.

before(App,C,Ctx)   -> O = [], %%FIXME: filter config
                       G = wf:config(App,filter,[]),
                       Filters = case erlang:function_exported(C,before_filters,2) of 
                                      true -> C:before_filters(G,Ctx); _ -> G end,
                       lists:foldr(fun(M, {ok,A}) -> 
                                        case  erlang:function_exported(M,before_filter,2) of
                                              true -> M:before_filter(O,A); _-> {ok, A} end;
                                      (M, {redirect,_}=R) -> R;
                                      (M, {error,_}=E) -> E
                                   end, {ok,Ctx},Filters).
%%todo: middle, after filter?
%%todo: render_other
%%todo: action_other
controller(App,M) -> wf:to_list(M) -- (wf:to_list(App)++"_"). 
header([])        -> ok;
header([{K,V}|T]) -> wf:header(K,V),header(T).

render({{ok,V},Ctx})             -> render({{ok,[],V},Ctx});
render({{ok,H,V},Ctx})           -> header(H),
                                    #{'_app':=App,'_controller':=C,'_action':=A} = Ctx,
                                    wf:info(?MODULE,"RENDER {OK, Vars} -> ~p ",[{App,controller(App,C),A,"html"}]),
                                    render(#dtl{file={App,controller(App,C),A,"html"}, bindings=V});
render({{redirect,L},Ctx})       -> render({{redirect,L,[]},Ctx});
render({{redirect,L,H},_})       -> header([H|{<<"Location">>,L}]),
                                    wf:state(status,302),
                                    render(#dtl{});
render({{moved,L},Ctx})          -> render({{moved,L,[]},Ctx});
render({{moved,L,H},_})          -> header([H|{<<"Location">>,L}]),
                                    wf:state(status,301),
                                    render(#dtl{});
render({{json,V},Ctx})           -> render({{json,V,[]},Ctx});
render({{json,V},Ctx})           -> render({{json,V,[],200},Ctx});
render({{json,V,H,S},_})         -> header(H++?CTYPE_JSON),
                                    wf:state(status,S),
                                    wf:json(V);
%% todo: render css,xml,js,txt via dtl? 
render({{{json,dtl},V},Ctx})     -> render({{json,V,[]},Ctx});
render({{{json,dtl},V},Ctx})     -> render({{json,V,[],200},Ctx});
render({{{json,dtl},V,H,S},Ctx}) -> header(H++?CTYPE_JSON),
                                    wf:state(status,S),
                                    #{'_app':=App,'_controller':=C,'_action':=A} = Ctx,
                                    wf_render:render(#dtl{app=App,file={App,C,A,"json"},bindings=V++maps:to_list(Ctx)});
render({#dtl{}=E,_})             -> wf_render:render(E);                                    
render(E)                        -> wf_render:render(E).

