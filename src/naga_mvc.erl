-module(naga_mvc).
-description('NAGA Server Pages HTTP MVC endpoint handler').
-author('Maxim Sokhatsky').
-author('Chanrotha Sisowath').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").
-export([run/2,transition/1]).

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
                #{}=R -> #{application:=App,controller:=C,action:=A,method:=M,params:=P,bindings:=B}=R,
                         try handle(App,C,A,M,P,B) catch C:E -> wf:error_page(C,E) end;
                    _ -> try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end
               end,
    Html = render(Elements),    
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = wf:fold(finish,Ctx#cx.handlers,?CTX),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    %wf:info(?MODULE,"Cookies Req: ~p",[Req2]),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

run(Req, #route{type=mvc,is_steroid=true}=Route) ->
    #route{application=App,controller=Module,action=Act,arity=A,want_session=WantSession}=Route,
    wf:state(status,200),
    Pid = spawn(fun() -> transition([]) end),
    %FIXME: websocket port 
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(n2o,websocket_port,wf:config(n2o,port,8000))),"'}"]),
    Ctx0 = wf:init_context(Req),
    Ctx  = Ctx0#cx{module=Module},
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
    Html    = render(Elements),    
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = finish(Ctx,?CTX, false, WantSession),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

run(Req, #route{type=mvc,is_steroid=false}=Route) ->
    #route{application=App,controller=C,action=Act,arity=A,want_session=WantSession} = Route,
    wf:state(status,200),
    Ctx0 = wf:init_context(Req),
    Ctx  = Ctx0#cx{module=C},
    Ctx1 = init(Ctx, false, WantSession),
    wf:context(Ctx1),
    Elements = case (Act == main) andalso (A == 0) of 
                  true  -> try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end;
                  false -> {M, _} = cowboy_req:method(Req),
                           {P, _} = cowboy_req:path_info(Req),
                           {B, _} = cowboy_req:bindings(Req),
                           try handle(App,C,Act,M,P,B) catch C:E -> wf:error_page(C,E) end
               end,    
    Html = render(Elements),
    Ctx2 = finish(Ctx,?CTX, false, WantSession),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2);

run(Req, #route{type=view,view=Module}) ->
    wf:state(status,200),   
    {ok,Html} = Module:render(),    
    Req2 = wf:response(Html,Req),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2).

no_session(L)-> lists:keydelete(session,1,L).
no_route(L)  -> lists:keydelete(route,1,L).

  init(C,    true ,true ) -> wf:fold(  init,                     C#cx.handlers  ,C );
  init(C,    false,true ) -> wf:fold(  init,no_route(            C#cx.handlers) ,C );
  init(C,    true ,false) -> wf:fold(  init,         no_session( C#cx.handlers) ,C );
  init(C,    false,false) -> wf:fold(  init,no_route(no_session( C#cx.handlers)),C ).
finish(C1,C2,true ,true ) -> wf:fold(finish,                    C1#cx.handlers  ,C2);
finish(C1,C2,false,true ) -> wf:fold(finish,no_route(           C1#cx.handlers) ,C2);
finish(C1,C2,true ,false) -> wf:fold(finish,         no_session(C1#cx.handlers) ,C2);
finish(C1,C2,false,false) -> wf:fold(finish,no_route(no_session(C1#cx.handlers)),C2).
 
set_cookies([],Req)-> Req;
set_cookies([{Name,Value,Path,TTL}|Cookies],Req)->
    set_cookies(Cookies,wf:cookie_req(Name,Value,Path,TTL,Req)).

handle(App,Mod,undefined,M,P,B) -> case erlang:function_exported(Mod,index,3) of 
                                    true -> handle(App,Mod,index,M,P,B); _-> Mod:main() end;
handle(App,Mod,A,M,undefined,B) -> handle(App,Mod,A,M,[],B);
handle(App,Mod,A,M,P,B)         -> case before(App,Mod,req_ctx(App,Mod,A,M,P,B)) of
                                      {ok, Ctx} -> {Mod:A(M,P,Ctx),Ctx}; 
                                      {error,E} -> error(E);
                                      {redirect, R} -> wf:redirect(R) end.

req_ctx(App,Mod,A,M,P,B)  -> #{'_application'=> App,
                               '_method'     => M,
                               '_controller' => Mod,
                               '_action'     => A,
                               '_params'     => P,                             
                               '_bindings'   => B,
                               '_base_url'   => wf:config(App,base_url,"/")
                              }.

before(App,Mod,Ctx) -> O = [], %%FIXME: filter config
                       G = wf:config(App,filter,[]),                      
                       Filters = case erlang:function_exported(Mod,before_filters,2) of 
                                      true -> Mod:before_filters(G,Ctx); _ -> G end,
                       lists:foldr(fun(M, {ok,A}) -> 
                                        case  erlang:function_exported(M,before_filter,2) of
                                              true -> M:before_filter(O,A); _-> {ok, A} end;
                                      (M, {redirect,_}=R) -> R;
                                      (M, {error,_}=E) -> E
                                   end, {ok,Ctx},Filters).

%%todo: middle, after filter?
%%todo: not_found
%%todo: {stream, Generator::function(), Acc0}
%%todo: {jsonp, Callback::string(), Data::proplist()} 
%%todo: {jsonp, Callback::string(), Data::proplist(), Headers::proplist()} 
%%todo: dev mode, header([{<<"Cache-Control">>, <<"no-cache">>}])
%%      reload current page ? if modfied (css/js/controller)
%%todo: unit test, mad_eunit ? 
%%todo check bullet?

header([])        -> ok;
header([{K,V}|T]) -> wf:header(K,V),header(T).

render({{output,Io},Ctx})        -> render({{output,Io,?CTYPE_PLAIN},Ctx});
render({{output,Io,H},Ctx})      -> header(H),Io;
render({{S,Io,H},Ctx}) 
              when is_integer(S) -> wf:state(status,S),header(H),Io;
render({ok,Ctx})                 -> render({{ok,[]},Ctx}); 
render({{ok,V},Ctx})             -> render({{ok,[],V},Ctx});
render({{ok,H,V},Ctx})           -> header(H),
                                    #{'_application':=App,'_controller':=C,'_action':=A} = Ctx,
                                    render(#dtl{file={App,C,A,"html"}, bindings=V++maps:to_list(Ctx)});
render({{redirect,L},Ctx})       -> render({{redirect,L,[]},Ctx});
render({{redirect,L,H},_})       -> header([H|{<<"Location">>,L}]),
                                    wf:state(status,302),
                                    render(#dtl{});
render({{moved,L},Ctx})          -> render({{moved,L,[]},Ctx});
render({{moved,L,H},_})          -> header([H|{<<"Location">>,L}]),
                                    wf:state(status,301),
                                    render(#dtl{});                                    
render({{json_,V},Ctx})          -> render({{json_,V,[]},Ctx});
render({{json_,V,H},Ctx})        -> render({{json_,V,H,200},Ctx});
render({{json_,V,H,S},Ctx})      -> header(H++?CTYPE_JSON),
                                    wf:state(status,S),
                                    #{'_application':=App,'_controller':=C,'_action':=A} = Ctx,
                                    wf_render:render(#dtl{app=App,file={App,C,A,"json"},bindings=V++maps:to_list(Ctx)});                                    
render({{json,V},Ctx})           -> render({{json,V,[]},Ctx});
render({{json,V,H},Ctx})         -> render({{json,V,H,200},Ctx});
render({{json,V,H,S},_})         -> header(H++?CTYPE_JSON),
                                    wf:state(status,S),
                                    wf:json(V);
render({{action_other,L},Ctx})   -> #route{application=App,controller=C,action=A}=L,
                                    #{'_method':=M,'_params':=P,'_bindings':=B} = Ctx,
                                    try handle(App,C,A,M,P,B) catch C:E -> wf:error_page(C,E) end;
render({{render_other,L},Ctx})   -> render({{render_other,L,[]},Ctx});
render({{render_other,L,V},Ctx}) -> case L of 
                                      #route{controller=[]} ->
                                        #route{application=App,controller=C,action=A}=L,
                                        wf_render:render(#dtl{app=App,file={App,C,A,"html"},bindings=V++maps:to_list(Ctx)});
                                      #route{controller=Module} ->                                         
                                        {ok,Html} = Module:render(V++maps:to_list(Ctx)), Html
                                    end;                                    
render({{js,V},Ctx})             -> render({{js,V,[]},Ctx});
render({{js,V,H},Ctx})           -> header(H++?CTYPE_JS),
                                    #{'_application':=App,'_controller':=C,'_action':=A} = Ctx,
                                    wf_render:render(#dtl{app=App,file={App,C,A,"js"},bindings=V++maps:to_list(Ctx)});

%% todo: render css,xml,js,txt via dtl? 
render({#dtl{}=E,_})             -> wf_render:render(E);                                    
render(E)                        -> wf_render:render(E).

