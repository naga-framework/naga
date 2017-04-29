-module(naga_indexof).
-export([index/3,event/1]).
-export([init/3,terminate/3,handle/2]).
-export([view/3,static/3,is_theme/1]).
-default_action(index).
-actions([index]).

-include_lib("kernel/include/file.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").

 % in your route file.
 % ,{"/<BaseURL>/[...]", naga_indexof, {<APPNAME>,<TYPE>,"/<BaseURL>"}}
 % where TYPE: view|static|controller
 % i.e:
 % ,{"/$v/[...]", naga_indexof, {<APPNAME>,view,"/$c"}}
 % ,{"/$s/[...]", naga_indexof, {<APPNAME>,static,"/$s"}}

init(_Transport, Req, Opts)      -> {ok, Req, Opts}.
terminate(_Reason, _Req, Opts)   -> ok.
handle(Req, {A,_,_}=Opts)        -> Route = #route{type=mvc,
                                                   application=A,
                                                   controller=?MODULE,
                                                   action=index,
                                                   want_session=true,
                                                   is_steroid=true,
                                                   opts=Opts},
                                    {ok, NewReq} = naga_mvc:run(Req, Route),
                                    {ok, NewReq, Opts}.

%% ----------------
%% INDEX CONTROLLER
%% ----------------
index(<<"GET">>, _, #{'_opts' := {App,Type,Base}, 
                       script := [_, PicklePid, _, _, _ ,_]} = Ctx) -> 

  {PathInfo,_} = cowboy_req:path_info(?REQ),
  {ok, Vsn} = application:get_key(naga,vsn), 
  Bindings = [
              {parent, <<(wf:to_binary(App))/binary, " : ", (wf:to_binary(Type))/binary>>},
              {script, new_script(App,PicklePid)},
              {naga,[{vsn, Vsn}]},
              {rows, ?MODULE:Type(App,PathInfo,Base)}
             ],

  #dtl{file = naga_indexof,
       bind_script=false, 
       app=naga, bindings=Bindings}.

%% ----------------
%% EVENT
%% ----------------
event(Ev) -> wf:info(?MODULE,"received event ~p~n",[Ev]).

%% ----------------
%% INTERNAL FUNCTION
%% ----------------
split(F) -> filename:split(F).
join(F)  -> filename:join(F).

new_script(App,PicklePid) ->
  Port = wf:to_list(wf:config(App,websocket_port,wf:config(App,port,8000))),
  NewScript = ["var transition = {pid: '", PicklePid, "', ", "port:'", Port ,"'}"].

is_theme(App) ->
 Themes = [T||T<-[wf:config(A,theme,[])||{A,_,_}<-application:loaded_applications()],T/=[]],
 lists:member(App,Themes).

view(App1,PathInfo,Base) ->
  App = wf:config(App1,theme,App1),
  BaseUrl = case is_theme(App) of
             true -> "/"++wf:to_list(App)++naga_router:base_url(App);
             _ -> naga_router:base_url(App) end,
  ViewDir = naga_router:view_dir(App),
  Files   = naga_router:files(view, App),
  {ok, Cwd} = file:get_cwd(),
  lists:foldr(fun({X,_},Acc) ->
                Name=join([BaseUrl]++(split(X) -- split(Cwd))-- split(ViewDir)),
                {ok, I} = file:read_file_info(X, [{time, universal}]),
                [#tr{cells=[
                   #td{class=[n],body=[
                       #link{href=Name, body=[ Name ]}
                   ]},
                   #td{class=[m],body=[ naga:dateformat(I#file_info.mtime,"M d Y H:i:s") ]},
                   #td{class=[s],body=[ wf:to_list(I#file_info.size),"&nbsp;" ]},
                   #td{class=[s],body=[ "view&nbsp;" ]}
                 ]}] ++ Acc
              end,[],Files).

static(App,PathInfo,Base) ->
  {Prefix, StaticDir} = naga_router:static_dir(App),
  PathInfo1 = [binary_to_list(X)||X<-PathInfo],
  Dir = case PathInfo1 of 
         [] -> StaticDir;
         PathInfo1 -> filename:join(StaticDir,filename:join(PathInfo1)) 
        end,
  case filelib:is_dir(Dir) of
    true -> {ok, Files} = file:list_dir(Dir),
            [begin 
                XX = filename:join(PathInfo1 ++ [X]),
                Name = re:replace(Prefix,"\\\[...\\\]",XX,[{return,list}]),
                Path = filename:join(Dir,X),
                %io:format("PATH ~p~n",[Path]),
                {ok, I} = file:read_file_info(Path, [{time, universal}]),
                {Type,Href} = case filelib:is_dir(Path) of 
                                true -> {"DIR&nbsp;", Base ++"/"++ XX}; 
                                _ -> {"-&nbsp;", Name} end,
                #tr{cells=[
                   #td{class=[n],body=[
                       #link{href=Href, body=[ Name ]}
                   ]},
                   #td{class=[m],body=[ naga:dateformat(I#file_info.mtime,"M d Y H:i:s") ]},
                   #td{class=[s],body=[ wf:to_list(I#file_info.size),"&nbsp;" ]},
                   #td{class=[s],body=[ Type ]}
                ]} end || X<-Files];
    false -> []
  end.


%%FIXME: controller
% controller(App,PathInfo,Base) ->
%  Controllers = [N||{_,N}<-naga:files(controller,App)],
%  lists:foldr(fun(X,Acc)->
%                 Actions = [A||{A,B} <- naga:actions(X), B == 3],
%                 lists:foldr(fun(Y,Bcc) ->

%                             [#tr{cells=[
%                                #td{class=[n],body=[
%                                    #link{href=Base ++"/"++ XX, body=[ Name ]}
%                                ]},
%                                #td{class=[m],body=[ "undefined" ]},
%                                #td{class=[s],body=[ "undefined&nbsp;" ]},
%                                #td{class=[s],body=[ "&nbsp;" ]}
%                             ]}]++Bcc                               
%                             end, [], Actions) ++ Acc
%              end,[],Controllers).
%%
