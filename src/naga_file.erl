-module(naga_file).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

-define(ROOT, wf:config(n2o,upload,code:priv_dir(n2o))).
-define(NEXT, 256*1024). % 256K chunks for best 25MB/s speed
-define(STOP, 0).

% Callbacks

filename(#ftp{sid=Sid,filename=FileName}) -> filename:join(wf:to_list(Sid),FileName).


field_name(    <<"item_", F/binary>>) -> F;
field_name( <<"itemAdd_", F/binary>>) -> F;
field_name(<<"itemEdit_", F/binary>>) -> F.

% N2O Protocols

info(#ftp{status={event,_}}=FTP, Req, State) ->
    wf:info(?MODULE,"Event Message: ~p",[FTP#ftp{data= <<>>}]),
    Module=State#cx.module,
    Reply=try Module:event(FTP)
          catch E:R -> Error=wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
    {reply,wf:format({io,n2o_nitrogen:render_actions(wf:actions()),Reply}),Req,State};

info(#ftp{sid=Sid,data=Data,status= <<"queue">>}=FTP,Req,State) ->
    %wf:info(?MODULE, "~s", [p:n(FTP)]),
    {Name, SelectedFiles} = binary_to_term(Data),
    %wf:info(?MODULE, "NAME ~p: SelectedFiles: ~p", [Name,SelectedFiles]),
    %wf:info(?MODULE, "STATE ~s",[p:n(State)]),
    wf:info(?MODULE, "SID ~p",[Sid]),
    %Module = State#cx.module,
    %wf:info(?MODULE, "MODULE ~p",[Module]),
    wf:send(Sid,{client,{ftpQueue, Name, SelectedFiles}}),
    {noreply,Req,State};

%%FIXME mv to proc(init...)
info(#ftp{sid=Sid,meta={FieldName,Idx},filename=FileName,status= <<"init">>,block=Block,offset=Offset,size=TotalSize}=FTP,Req,State) ->
    application:set_env(n2o,formatter,bert),
    Root=?ROOT,
    RelPath=(wf:config(n2o,filename,n2o_file)):filename(FTP),
    FilePath=filename:join(Root,RelPath),
    ok=filelib:ensure_dir(FilePath),
    FileSize=case file:read_file_info(FilePath) of {ok,Fi} -> Fi#file_info.size; {error,_} -> 0 end,

    wf:info(?MODULE,"Info Init: Meta ~p, ~p Offset: ~p Block: ~p~n",[{FieldName,Idx},FilePath,FileSize,Block]),

    Name={Sid,filename:basename(FileName)},
    Block2=case Block of 0 -> ?STOP; _ -> ?NEXT end,
    Offset2=case FileSize >= Offset of true -> FileSize; false -> 0 end,
    FTP2=FTP#ftp{meta=Idx,block=Block2,offset=Offset2,filename=RelPath,data= <<>>},

    n2o_async:stop(file,Name),
    n2o_async:start(#handler{module=?MODULE,class=file,group=n2o,state=FTP2,name=Name}),
    wf:send(Sid,{client,{ftpInit, FilePath, FieldName, Idx, FileName, FileSize, TotalSize}}),
    {reply,wf:format(FTP2),Req,State};

info(#ftp{sid=Sid,meta={_,Idx},filename=FileName,status= <<"send">>}=FTP,Req,State) ->
    wf:info(?MODULE,"Info Send: ~p",[FTP#ftp{data= <<>>}]),
    Reply=try gen_server:call(n2o_async:pid({file,{Sid,filename:basename(FileName)}}),FTP)
        catch E:R -> wf:error(?MODULE,"Info Error call the sync: ~p~n",[FTP#ftp{data= <<>>}]),
            FTP#ftp{meta=Idx,data= <<>>,block=?STOP} end,
    wf:info(?MODULE,"reply ~p",[Reply#ftp{data= <<>>}]),
    {reply,wf:format(Reply),Req,State};

info(#ftp{sid=Sid, meta={FieldName, Idx}, filename=Filename, status= <<"cancel">>}=FTP, Req, State) ->
    %FIXME: ensure filename 
    Dir   = lists:concat([?ROOT,'/',wf:to_list(Sid),'/']),
    File  = filename:join([Dir,Filename]),
    FSize = case file:read_file_info(File) of {ok, Fi} -> file:delete(File), 0; {error, _} -> 0 end,
    wf:info(?MODULE, "File Transfer cancel: ~p: Offset:~p~n",[File, FSize]),
    F2    = FTP#ftp{block = 0, offset = FSize, data = <<>>, meta=Idx },
    Name  = {Sid,Filename},
    
    Module= State#cx.module,
    % Reply = try Module:event(FTP)
    %             catch E:R -> Error = wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
    wf:send(Sid,{client,{ftpCancel, {FieldName, Idx}, Filename}}),            
    n2o_async:stop(file,Name),
    {reply,wf:format(F2),Req,State};  

info(#ftp{status= <<"recv">>}=FTP,Req,State) -> {reply,wf:format(FTP),Req,State};

info(#ftp{status= <<"relay">>}=FTP,Req,State) -> {reply,wf:format(FTP),Req, State};

info(Message,Req,State) -> 
    wf:info(?MODULE, "Info Unknown message: ~s",[p:n(Message)]),
    {unknown,Message,Req,State}.

% N2O Handlers

proc(init,#handler{state=#ftp{sid=Sid}=FTP}=Async) ->
    wf:info(?MODULE,"Proc Init: ~p",[FTP#ftp{data= <<>>}]),
    wf:send(Sid,FTP#ftp{data= <<>>,status={event,init}}),
    {ok,Async};

proc(#ftp{sid=Sid,meta={FieldName,Idx},data=Data,filename=FileName,status= <<"send">>,block=Block}=FTP,
     #handler{state=#ftp{data=State,size=TotalSize,offset=Offset,filename=RelPath}}=Async) when Offset+Block >= TotalSize ->
    wf:info(?MODULE,"Proc Stop ~p, last piece size: ~p", [FTP#ftp{data= <<>>},byte_size(Data)]),
    case file:write_file(filename:join(?ROOT,RelPath),<<Data/binary>>,[append,raw]) of
        {error,Reason} -> {reply,{error,Reason},Async};
        ok ->
            %FTP2=FTP#ftp{data= <<>>,block=?STOP},
            FTP2=FTP#ftp{meta=Idx,data= <<>>, status= <<"EOF">>, offset=Offset+Block },
            Path  = filename:join([?ROOT,RelPath]),            
            wf:send(Sid,{client,{ftpEOF, FieldName, Idx, Path, TotalSize, Offset}}),
            spawn(fun() -> n2o_async:stop(file,{Sid,filename:basename(FileName)}) end),
            {stop,normal,FTP2,Async#handler{state=FTP2}} end;

proc(#ftp{sid=Sid,meta={_,Idx},data=Data,block=Block}=FTP,
     #handler{state=#ftp{data=State,offset=Offset,filename=RelPath}}=Async) ->
    FTP2=FTP#ftp{meta=Idx,status= <<"send">>,offset=Offset+Block },
    wf:info(?MODULE,"Proc Process ~p",[FTP2#ftp{data= <<>>}]),
    case file:write_file(filename:join(?ROOT,RelPath),<<Data/binary>>,[append,raw]) of
        {error,Reason} -> {reply,{error,Reason},Async};
        ok -> {reply,FTP2#ftp{data= <<>>},Async#handler{state=FTP2#ftp{filename=RelPath}}} end.
