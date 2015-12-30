-module(naga_mvc).
-description('NAGA Server Pages HTTP MVC endpoint handler').
-author('Maxim Sokhatsky').
-author('Chanrotha Sisowath').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").
-export([run/2,transition/1]).

transition(Actions) -> receive {'INIT',A} -> transition(A); {'N2O',Pid} -> Pid ! {actions,Actions} end.
run(Req, Opts) ->
    wf:state(status,200),
    Pid = spawn(fun() -> transition([]) end),
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(n2o,websocket_port,wf:config(n2o,port,8000))),"'}"]),
    Ctx = wf:init_context(Req),
    Ctx1 = wf:fold(init,Ctx#cx.handlers,Ctx),
    wf:actions(Ctx1#cx.actions),
    wf:context(Ctx1),
    Elements = case Ctx1#cx.path of              
                #{}=R -> #{app:=App,controller:=C,action:=A,method:=M,params:=P}=R,
                         try handle(App,C,A,M,P) catch C:E -> wf:error_page(C,E) end;
                    _ -> try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end
               end,
    Html = render(Elements),    
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = wf:fold(finish,Ctx#cx.handlers,?CTX),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    %wf:info(?MODULE,"Cookies Req: ~p",[Req2]),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2).

set_cookies([],Req)-> Req;
set_cookies([{Name,Value,Path,TTL}|Cookies],Req)->
    set_cookies(Cookies,wf:cookie_req(Name,Value,Path,TTL,Req)).

handle(App,C,undefined,M,P) -> case erlang:function_exported(C,index,3) of 
                                    true -> handle(App,C,index,M,P); _-> C:main() end;
handle(App,C,A,M,P)         -> case before(App,C,req_ctx(App,C,A,M,P)) of
                                    {ok, Ctx} -> {C:A(M,P,Ctx),Ctx}; 
                                    {error,E} -> error(E);
                                    {redirect, R} -> wf:redirect(R) end.

req_ctx(App,C,A,M,P)  -> #{'_app'        => App,
                           '_method'     => M,
                           '_controller' => C,
                           '_action'     => A
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
header([])        -> ok;
header([{K,V}|T]) -> wf:header(K,V),header(T).

render({{ok,V},Ctx})             -> render({ok,[],V});
render({{ok,H,V},Ctx})           -> header(H),
                                    #{'_app':=App,'_controller':=C,'_action':=A} = Ctx,
                                    render(#dtl{file={App,C,A,"html"}, bindings=V++maps:to_list(Ctx)});
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
render(#dtl{}=E)                 -> wf_render:render(E).

