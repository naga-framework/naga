-module(naga_post_param).
-include_lib("naga.hrl").
-include_lib("n2o/include/wf.hrl").
-export([init/2,finish/2]).
-define(MAX_FILE_SIZE_LIMIT, 900 * 1024 * 1024). % 900MB

finish(State, Ctx) ->  {ok, State, Ctx}.
init(State, Ctx) ->
	App = case State of #route{application=A} -> A; _ -> naga end,
    Limit = wf:config(App, upload_size_limit, ?MAX_FILE_SIZE_LIMIT),
    Dir   = wf:config(naga, upload, "/tmp"),
	case cowboy_req:parse_header(<<"content-type">>, Ctx#cx.req) of
	 {ok, {<<"multipart">>, <<"form-data">>, _}, Req} ->
	 	    NewCtx = multipart(Ctx#cx{req=Req,form=[]}, App, Dir, Limit),
	 	    {ok, State, NewCtx};
     {ok, {<<"application">>, <<"x-www-form-urlencoded">>, _}, Req} -> 
            {Form,Req1} = wf:form(Req),
			{ok, State, Ctx#cx{form=Form,req=Req1}};
     {ok, _, Req} -> 
			{ok, State, Ctx#cx{req=Req}}
    end.

multipart(#cx{req=Req}=Ctx, App, Dir, MaxFileSizeLimit) when is_integer(MaxFileSizeLimit) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
			N = case cow_multipart:form_data(Headers) of
				    {data, FieldName} ->
				        {ok, Body, Req3} = cowboy_req:part_body(Req2),
				        #cx{form=Form}=Ctx,
				        Ctx#cx{req=Req3,form=Form++[{FieldName, Body}]};
				    {file, FieldName, Filename, CType, CTransferEncoding} ->
				        TempFilename = temp_filename(Dir),
				        {ok, IoDevice} = file:open(TempFilename, [raw, write]),
				        Rsf = stream_file(Req2, IoDevice, 0, MaxFileSizeLimit),
				        ok = file:close(IoDevice),
				        case Rsf of
				            {ok, FileSize, Req4} ->
				                File = {FieldName, {file, Filename, TempFilename, FileSize, CType}},
				                #cx{form=Form}=Ctx,
				                Ctx#cx{req=Req4,form=Form++[File]};
				            {error, Reason, Req4} ->
				                Form = Ctx#cx.form,
				                F = {FieldName,{error, [{reason, Reason},{filename,Filename}]}},
				                ok = file:delete(TempFilename),
				                Ctx#cx{form=Form++[F], req=Req4};				                
				            {limit, Reason, Req4} ->
				                Form = Ctx#cx.form,
				                F = {FieldName,{error, [{reason, Reason},{filename,Filename}]}},
				                ok = file:delete(TempFilename),
				                Ctx#cx{form=Form++[F], req=Req4}
				        end
				end,
            multipart(N, App, Dir, MaxFileSizeLimit);
        {done, Req2} ->
            Ctx#cx{req=Req2}
    end.

stream_file(Req, IoDevice, FileSize, MaxFileSizeLimit) ->
    {Control, Data, Req2} = cowboy_req:part_body(Req),
    NewFileSize = byte_size(Data) + FileSize,
    case NewFileSize > MaxFileSizeLimit of
        true -> {limit, file_size, Req2};
        false ->
            case file:write(IoDevice, Data) of 
            	ok -> case Control of
	                   ok -> {ok, NewFileSize, Req2};
	                   more -> stream_file(Req2, IoDevice, NewFileSize, MaxFileSizeLimit)
	                  end; 
            	{error, Raison} -> {error, Raison, Req2} 
        	end
    end.

temp_filename(Dir) ->
    list_to_binary(filename:join([Dir, "upload" ++ integer_to_list(erlang:phash2(make_ref()))])).
