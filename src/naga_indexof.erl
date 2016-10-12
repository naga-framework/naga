-module(naga_indexof).
-export([index/3,event/1]).
-export([init/3,terminate/3,handle/2]).
-export([view/3,static/3]).
-compile(export_all).
-default_action(index).
-actions([index]).

-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("naga.hrl").

 % in your route file.
 % ,{"/$v/[...]", naga_indexof, {<APPNAME>,view,"/$v"}}
 % ,{"/$c/[...]", naga_indexof, {<APPNAME>,controller,"/$c"}}
 % ,{"/$s/[...]", naga_indexof, {<APPNAME>,static,"/$s"}}

init(_Transport, Req, Opts)      -> {ok, Req, Opts}.
terminate(_Reason, _Req, Opts)   -> ok.
handle(Req, Opts)                -> Route = #route{type=mvc,
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

  #dtl{file = "naga_indexof",
       bind_script=false, 
       app=naga, bindings=Bindings}.

%% ----------------
%% EVENT
%% ----------------
event(Ev) -> wf:info(?MODULE,"received event ~p~n",[Ev]).


%% ----------------
%% INTERNAL FUNCTION
%% ----------------
new_script(App,PicklePid) ->
  Port = wf:to_list(wf:config(App,websocket_port,wf:config(App,port,8000))),
  NewScript = ["var transition = {pid: '", PicklePid, "', ", "port:'", Port ,"'}"].

view(App,PathInfo1,Base) ->
    io:format("PathInfo : ~p~n",[PathInfo1]),
    PathInfo = lists:reverse([binary_to_list(X)||X<-PathInfo1]),
    Files = naga:files(view, App),
    Dir = filename:split(naga:view_dir(App)),
    BaseUrl = naga:base_url(App),
    B = Base ++ BaseUrl,
    [X||X<-rows(B, Dir, PathInfo, Files), X /= []].

static(App,PathInfo,Base) ->
  {Prefix, StaticDir} = naga:static_dir(App),
  PathInfo1 = [binary_to_list(X)||X<-PathInfo],
  Dir = filename:join(StaticDir,PathInfo1),
  case filelib:is_dir(Dir) of
    true -> {ok, Files} = file:list_dir(Dir),
            [begin 
                XX = filename:join(PathInfo1 ++ [X]),
                Name = re:replace(Prefix,"\\\[...\\\]",XX,[{return,list}]),
                #tr{cells=[
                   #td{class=[n],body=[
                       #link{href=Base ++"/"++ XX, body=[ Name ]}
                   ]},
                   #td{class=[m],body=[ "undefined" ]},
                   #td{class=[s],body=[ "undefined&nbsp;" ]},
                   #td{class=[s],body=[ "&nbsp;" ]}
                ]} end || X<-Files];
    false -> [] %% 
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


rows(BaseUrl, Dir, PathInfo, Files) ->
    rows(BaseUrl, Dir, PathInfo, Files, []).
rows(BaseUrl, Dir, _, [], Acc) -> lists:reverse(Acc);
rows(BaseUrl, Dir, PathInfo, [{F,_}|T], Acc) ->
    Path = F -- Dir,
    View = row(BaseUrl, tl(filename:split(Path)), PathInfo),
    rows(BaseUrl, Dir, PathInfo, T, [View] ++ Acc).

row(Base, [Controller|_] = View, _) ->
    Href = Base ++ filename:join(View),
    Ext = extension(View),
    #tr{cells=[
       #td{class=[n],body=[
           #link{href=Href, body=[ filename:join(View) ]}
       ]},

       #td{class=[m],body=[ "undefined" ]},
       #td{class=[s],body=[ "undefined&nbsp;" ]},
       #td{class=[s],body=[ wf:to_list(Ext), "&nbsp;" ]}
    ]};

row(_,_,_) -> [].

extension(View) ->
    [Ex] = string:tokens(filename:extension(lists:last(View)), "."),
    list_to_atom(Ex).