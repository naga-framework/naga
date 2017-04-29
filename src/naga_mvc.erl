-module(naga_mvc).
-description('NAGA Server Pages HTTP MVC endpoint handler').
-author('Maxim Sokhatsky').
-author('Chanrotha Sisowath').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").
-export([run/2,transition/1]).
-export([i18n_undefined/1]).
-export([index/3,before_filters/2,tpl_name/4]).

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
                         try handle(App,C,A,M,P,B,[]) catch C:E -> wf:error_page(C,E) end;
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
    #route{application=App,controller=Module,action=Act,arity=A,want_session=WantSession,opts=Opts}=Route,
    wf:state(status,200),
    Pid = spawn(fun() -> transition([]) end),
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(App,websocket_port,wf:config(App,port,8000))),"'}"]),
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
                            post_params(Route,M),
                            try handle(App,Module,Act,M,P,B,Opts) catch C:E -> wf:error_page(C,E) end
               end,
    case render(Elements) of
     {stream,Gen,Acc} -> streamer({stream,Gen,Acc},Req,Route);
     Html -> 
      Actions = wf:actions(),
      Pid ! {'INIT',Actions},
      Ctx2 = finish(Ctx,?CTX, false, WantSession),
      Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
      {ok, _ReqFinal} = wf:reply(wf:state(status), Req2) end;

run(Req, #route{type=mvc,is_steroid=false}=Route) ->
    #route{application=App,controller=C,action=Act,arity=A,want_session=WantSession,opts=Opts} = Route,
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
                           post_params(Route,M),
                           try handle(App,C,Act,M,P,B,Opts) catch C:E -> wf:error_page(C,E) end
               end,    
    case render(Elements) of
     {stream,Gen,Acc} -> streamer({stream,Gen,Acc},Req,Route);
     Html ->
      Ctx2 = finish(Ctx,?CTX, false, WantSession),
      set_sid(WantSession),
      Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
      {ok, _ReqFinal} = wf:reply(wf:state(status), Req2) end;

run(Req, #route{type=view,application=App,view=Module}=R) ->
    wf:state(status,200),
    Vars = case catch App:dummy() of {'EXIT',_} -> []; E -> E end,
    {ok,Html} = Module:render(Vars),
    Req2 = wf:response(Html,Req),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2).

before_filters(_,_) -> [].
index(_,_,_) -> {ok,[]}.

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

post_params(S,<<"PUT">>)  -> C=fold(S,init,[{post_params,naga_post_param}],?CTX),wf:context(C);
post_params(S,<<"POST">>) -> C=fold(S,init,[{post_params,naga_post_param}],?CTX),wf:context(C);
post_params(_,_)          -> ok.

fold(State,Fun,Handlers,Ctx) ->
    lists:foldl(fun({_,Module},Ctx1) ->
        {ok,_,NewCtx} = Module:Fun(State,Ctx1),
        NewCtx end,Ctx,Handlers).

set_sid(true) -> Name = n2o_session:session_cookie_name(site),
                 Value= wf:session_id(),
                 Path = "/",
                 TTL  = 2147483647,
                 % NOTE: Infinity-expire cookie will allow to clean up all session cookies
                 %       by request from browser so we don't need to sweep them on server.
                 %       Actually we should anyway to cleanup outdated cookies
                 %       that will never be requested.
                 wf:cookie(Name,Value,Path,TTL);
set_sid(_) -> ok.

set_cookies([],Req)-> Req;
set_cookies([{Name,Value,Path,TTL}|Cookies],Req)->
    set_cookies(Cookies,wf:cookie_req(Name,Value,Path,TTL,Req)).

handle(App,Mod,undefined,M,P,B,O) -> case erlang:function_exported(Mod,index,3) of 
                                    true -> handle(App,Mod,index,M,P,B,O); _-> Mod:main() end;
handle(App,Mod,A,M,undefined,B,O) -> handle(App,Mod,A,M,[],B,O);
handle(App,Mod,A,M,P,B,O)         -> case apply_before(App,Mod,req_ctx(App,Mod,A,M,P,B,O)) of
                                      {ok, Ctx} -> {Mod:A(M,P,Ctx),Ctx}; 
                                      {error,E} -> error(E);
                                      {redirect, R} -> wf:redirect(R) end.

req_ctx(App,Mod,A,M,P,B,O)-> #{'_application'=> App,
                               '_method'     => M,
                               '_controller' => Mod,
                               '_action'     => A,
                               '_params'     => P,                             
                               '_bindings'   => B,
                               '_opts'       => O,
                               script        => case wf:script() of undefined -> <<>>; S -> S end,
                               '_base_url'   => case wf:config(App,base_url,"") of "/" -> ""; E -> E end
                              }.

apply_before(App,Ctr,Ctx) -> O = wf:config(App,controller_filters_config,[]), %%FIXME: filter config
                             G = wf:config(App,controller_filters,[]),                     
                             Filters0 = case erlang:function_exported(Ctr,before_filters,2) of 
                                            true -> Ctr:before_filters(G,Ctx); _ -> G end,
                             {Filters,_} = lists:partition(fun(X) ->
                                                              erlang:function_exported(X,before_filter,2)
                                                           end, Filters0 ++ [Ctr]),
                             %io:format("BEFORE FILTERS ~p~n",[Filters]),
                             lists:foldl(fun(F, {ok,Acc})       -> FC=proplists:get_value(F,O,[]),
                                                                   F:before_filter(FC,Acc); 
                                            (F, {redirect,_}=R) -> R;
                                            (F, {error,_}=E)    -> E
                                         end, {ok,Ctx},Filters).

apply_middle(A,C,Vars,Ctx) -> O = wf:config(A,controller_filters_config,[]),
                              G = wf:config(A,controller_filters,[]),
                              Filters0 = case erlang:function_exported(C,middle_filters,2) of
                                         true -> C:middle_filters(G,Ctx); _ -> G end,
                              {Filters,_} = lists:partition(fun(X) ->
                                                            erlang:function_exported(X,middle_filter,3)
                                                           end, Filters0 ++ [C]),
                              %io:format("MIDDLE FILTERS ~p~n",[Filters]), 
                              lists:foldl(fun(_F,{S,P,H}) when is_integer(S) -> {S,P,H};
                                             (_F,{ok,P,H})                   -> {ok,P,H};
                                             (F,Result) when is_atom(F)      -> 
                                              FC = proplists:get_value(F,O,[]),
                                              F:middle_filter(Result,FC,Ctx)
                                          end, Vars, Filters).

apply_after(Io,Ctx) ->  #{'_application':=A, '_controller':=C} = Ctx,
                        O = wf:config(A,controller_filters_config,[]),
                        G = wf:config(A,controller_filters,[]),
                        Filters0 = case erlang:function_exported(C,after_filters,3) of
                                   true -> C:middle_filters(G,Ctx); _ -> G end,
                        {Filters,_} = lists:partition(fun(X) ->
                                                      erlang:function_exported(X,after_filter,3)
                                                     end, Filters0 ++ [C]),
                        %io:format("AFTER FILTERS ~p:~p~n",[C,Filters]), 
                        lists:foldl(fun(F,Rendered) when is_atom(F) -> 
                                        FC = proplists:get_value(F,O,[]),
                                        F:after_filter(Rendered,FC,Ctx)
                                    end, Io, Filters).
 
%%todo: not_found
%%todo: dev mode, header([{<<"Cache-Control">>, <<"no-cache">>}])
%%      reload current page ? if modfied (css/js/controller)
%%todo: unit test, mad_eunit ? 
%%todo check bullet?

streamer({stream,Generator,Acc},Req,State) ->
   {ok, Req2} = cowboy_req:chunked_reply(200, Req),
   generator(Generator,Acc,Req2),
   {ok, Req2}.

generator(Generator,Acc0,Req) ->
  case Generator(Acc0) of
    {output, Chunk, Acc1} ->
      ok = cowboy_req:chunk([Chunk,<<"\r\n">>], Req),
      generator(Generator,Acc1,Req);
    done -> ok
  end.

i18n_undefined(X)  -> X. %% you can define a callback when the translation is undefined, here is default 
trans(Vars,Ctx)    -> #{'_application':=A} = Ctx,
                      Dft = maps:get(locale, Ctx, wf:config(A,assume_locale,"en")),
                      Locale = proplists:get_value('_lang',Vars,Dft),
                      case wf:config(A,i18n,false) of
                       false -> [{locale, Locale},{translation_fun, fun(X,_L) -> X end}];
                       true  -> [{locale, Locale},
                                 {translation_fun, fun(X,undefined) -> X;
                                                      (X,L) -> 
                                                       case naga_lang:lookup(A,{wf:to_list(L),X}) of
                                                        undefined -> {M,F} = wf:config(A,i18n_undefined,{?MODULE,i18n_undefined}),M:F(X);
                                                        E -> E end
                                                   end}] end.

%% theming can be done with an naga app with only views and static assets 
%% FIXME: maybe base_url, static asset 
%%        switch default static asset with the one from the theme
tpl({_,C,Ac,E},#{'_theme':=T})   -> tpl_name(T,C,Ac,E);                       
tpl({A,C,Ac,E},_)                -> tpl_name(A,C,Ac,E).
tpl_name(A,C,Ac,E)               -> wf:to_atom(wf:to_list(A)++
                                    "_view_"++wf:to_list(C)++
                                    "_"++wf:to_list(Ac)++
                                    "_"++wf:to_list(E)).

nocache(App)                     -> case wf:config(App,mode,prod) of prod -> skip;
                                     dev -> header([{<<"Cache-Control">>, <<"no-cache">>}]) end.

header([])                       -> ok;
header([{K,V}|T])                -> wf:header(K,V),header(T).

render({{output,Io},Ctx})        -> render({{output,Io,?CTYPE_PLAIN},Ctx});
render({{output,Io,H},_})        -> header(H),Io;
render({{S,Io,H},_}) 
              when is_integer(S) -> wf:state(status,S),header(H),Io;
render({ok,Ctx})                 -> render({{ok,[]},Ctx}); 
render({{ok,V},Ctx})             -> render({{ok,V,[]},Ctx});
render({{ok,V,H},Ctx})           -> header(H),
                                    #{'_application':=App,'_controller':=C,'_action':=A} = Ctx,
                                    Tpl = tpl({App,C,A,"html"},Ctx),
                                    %io:format("CTX ~p~n",[maps:to_list(Ctx)]),
                                    {ok,Io} = Tpl:render(V++maps:to_list(Ctx),trans(V,Ctx)),
                                    apply_after(Io,Ctx);
render({{redirect,L},Ctx})       -> render({{redirect,L,[]},Ctx});
render({{redirect,L,H},Ctx})     -> header(H++[{<<"Location">>,naga:location(L,Ctx)}]),
                                    wf:state(status,302),
                                    apply_after([],Ctx);
render({{moved,L},Ctx})          -> render({{moved,L,[]},Ctx});
render({{moved,L,H},Ctx})        -> header(H++[{<<"Location">>,naga:location(L,Ctx)}]),
                                    wf:state(status,301),
                                    apply_after([],Ctx);
%% jsond, json render with DTL
render({{jsond,V},Ctx})          -> render({{jsond,V,[]},Ctx});
render({{jsond,V,H},Ctx})        -> render({{jsond,V,H,200},Ctx});
render({{jsond,V,H,S},Ctx})      -> header(H++?CTYPE_JSON),
                                    wf:state(status,S),
                                    #{'_application':=App,'_controller':=C,'_action':=A} = Ctx,
                                    Tpl = tpl({App,C,A,"json"},Ctx),
                                    {ok,Io} = Tpl:render(V++maps:to_list(Ctx),trans(V,Ctx)),
                                    apply_after(Io,Ctx);                                    
render({{json,V},Ctx})           -> render({{json,V,[]},Ctx});
render({{json,V,H},Ctx})         -> render({{json,V,H,200},Ctx});
render({{json,V,H,S},Ctx})       -> header(H++?CTYPE_JSON),
                                    wf:state(status,S),
                                    Io = wf:json(V),
                                    apply_after(Io,Ctx);
render({{jsonp,C,V},Ctx})        -> render({{jsonp,C,V,[]},Ctx});
render({{jsonp,C,V,H},Ctx})      -> header(H++?CTYPE_JSON),
                                    Io = [wf:to_binary(C),<<"(">>,wf:json(V),<<");">>],
                                    apply_after(Io,Ctx); 
render({{action_other,L},Ctx})   
                  when is_map(L) -> P = maps:get(params,L,[]),
                                    {App1,C1,A1} = case [L,Ctx] of
                                                    [#{app:=App,controller:=Ctr,action:=A},_] -> {App,Ctr,A};
                                                    [#{         controller:=Ctr,action:=A},#{'_application':=App}] -> {App,Ctr,A};
                                                    [#{                         action:=A},#{'_application':=App, '_controller':=Ctr}] -> {App,Ctr,A}
                                                   end,
                                    #{'_method':=M,'_bindings':=B} = Ctx,
                                    render({C1:A1(M,P,Ctx),Ctx});

render({{render_other,L},Ctx})   -> render({{render_other,L,[]},Ctx});
render({{render_other,L,V},Ctx}) 
                  when is_map(L) -> P = maps:get(params,L,[]),
                                    {App1,C1,A1} = case [L,Ctx] of
                                                      [#{app:=App,controller:=C,action:=A},_] -> {App,C,A};
                                                      [#{         controller:=C,action:=A},#{'_application':=App}] -> {App,C,A};
                                                      [#{                       action:=A},#{'_application':=App, '_controller':=C}] -> {App,C,A}
                                                   end,
                                    Ext = maps:get('_ext',L,"html"),
                                    Tpl = tpl({App1,C1,A1,Ext},Ctx),
                                    {ok,Io} = Tpl:render(V++maps:to_list(Ctx),trans(V,Ctx)),
                                    apply_after(Io,Ctx); 

render({{yaml,V},Ctx})           -> render({{yaml,V,[]},Ctx});
render({{yaml,V,H},Ctx})         -> header(H++?CTYPE_YAML),
                                    #{'_application':=App,'_controller':=C,'_action':=A} = Ctx,
                                    Tpl = tpl({App,C,A,"yaml"},Ctx),
                                    {ok,Io} = Tpl:render(V++maps:to_list(Ctx),trans(V,Ctx)),
                                    apply_after(Io,Ctx);   
render({{js,V},Ctx})             -> render({{js,V,[]},Ctx});
render({{js,V,H},Ctx})           -> header(H++?CTYPE_JS),
                                    #{'_application':=App,'_controller':=C,'_action':=A} = Ctx,
                                    Tpl = tpl({App,C,A,"js"},Ctx),
                                    {ok,Io} = Tpl:render(V++maps:to_list(Ctx),trans(V,Ctx)),
                                    apply_after(Io,Ctx); 
render({{stream,Gen,Acc},_})     -> {stream,Gen,Acc};
render({#dtl{}=E,Ctx})           -> Io = wf_render:render(E),
                                    apply_after(Io,Ctx);
render(E)                        -> wf_render:render(E).
