-module(naga_mvc).
-description('NAGA Server Pages HTTP MVC endpoint handler').
-author('Maxim Sokhatsky').
-author('Chanrotha Sisowath').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").
-compile (export_all).

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
                #{app:=App,controller:=C,action:=A,
                  method:=M,
                  params:=P} -> try handle(App,C,A,M,P) catch C:E -> wf:error_page(C,E) end;
                           _ -> try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end
               end,
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

handle(App,C,undefined,M,P) -> case erlang:function_exported(C, index, 3) of 
                                    true -> handle(App,C,index,M,P); _-> C:main() end;
handle(App,C,A,M,P) -> case before(App,C,req_ctx(App,C,A,M,P)) of
                        {ok, Ctx} -> wf:info(?MODULE,"execute ~p ~p:~p(~p,~p,~p)",[App,C,A,M,P,Ctx]),
                                     C:A(M,P,Ctx); 
                        {error,E} -> error(E);
                        {redirect, R} -> wf:redirect(R) end.

req_ctx(App,C,A,M,P)  -> #{'_app'        => App,
                           '_method'     => M,
                           '_controller' => C,
                           '_action'     => A,
                           '_params'     => P,
                           '_lang'       => ?CTX#cx.lang,
                           '_sid'        => wf:session_id()
                        }.

before(App,C,Ctx)   -> O = [], %%FIXME: filter config
                       G = wf:config(App,filter,[]),
                       wf:info(?MODULE,"BEFORE FILTER ~p ~p",[App,G]),
                       Filters = case erlang:function_exported(C,before_filters,2) of 
                                      true -> C:before_filters(G,Ctx); _ -> G end,
                       wf:info(?MODULE,"BEFORE FILTER ~p ~p",[App,G]),                                      
                       Res = lists:foldr(fun(M, {ok,A}) -> case  erlang:function_exported(M,before_filter,2) 
                                                           of true -> M:before_filter(O,A); _-> {ok, A} end;
                                      (M, {redirect,_}=R) -> R;
                                      (M, {error,_}=E) -> E
                                   end, {ok,Ctx},Filters), 
                       wf:info(?MODULE,"BEFORE CTX ~p",[Res]),                                                             
                       Res.
