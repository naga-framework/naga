-module(naga_static).
-description('N2O Static Bridge to files in LING image, MAD bundle or OS').
-author('Maxim Sokhatsky').
-author('Chanrotha Sisowath').
-compile(export_all).
-include_lib("kernel/include/file.hrl").

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

accept_gzip(<<"gzip,",_/binary>>) -> true;
accept_gzip(_)                    -> false.

rest_init(Req, {dir, Path, Extra}) when is_binary(Path) -> 
  rest_init(Req, {dir, binary_to_list(Path), Extra});

rest_init(Req, {dir, Path, Extra}) ->
	{PathInfo, Req2} = cowboy_req:path_info(Req),
	Info     = {ok, #file_info{type=regular,size=0}},
	Path1    = filename:join([Path|PathInfo]),
  FileName = wf:to_list(unicode:characters_to_binary(Path1,utf8,utf8)),
	{AE, _}  = cowboy_req:header(<<"accept-encoding">>,Req),
	case accept_gzip(AE) of 
		true  -> case ets:member(filesystem, FileName++".gz") of
							true  -> {ok, Req2, {FileName, {ok, #file_info{type={gz,ets_regular},size=0}}, Extra}};
							false -> which(FileName,Extra,Req2)
						 end;
		false -> which(FileName,Extra,Req2)
	end.

which(FileName,Extra,Req2) ->
	case ets:member(filesystem, FileName) of
		true  -> {ok, Req2, {FileName, {ok, #file_info{type=ets_regular,size=0}}, Extra}};
		false -> disk(FileName,Extra,Req2)
	end.

disk(FileName,Extra,Req) ->
	case filelib:is_file(FileName++".gz") of
		true -> {ok, Req, {FileName, {ok, #file_info{type={gz,regular},size=0}}, Extra}};
		false-> case filelib:is_file(FileName) of
							true -> {ok, Req, {FileName, {ok, #file_info{type=regular,size=0}}, Extra}};
							false-> {ok, Req, {FileName, {ok, #file_info{type=not_found}}, Extra}}
						end 
	 end.

malformed_request(Req, State) -> {State =:= error, Req, State}.

forbidden(Req, State={_, {ok, #file_info{type=directory}}, _}) -> {true, Req, State};
forbidden(Req, State={_, {error, eacces}, _}) -> {true, Req, State};
forbidden(Req, State={_, {error, not_found}, _}) -> {true, Req, State};
forbidden(Req, State={_, {ok, #file_info{access=Access}}, _}) when Access =:= write; Access =:= none -> {true, Req, State};
forbidden(Req, State) -> {false, Req, State}.

content_types_provided(Req, State={Path, _, Extra}) ->
	case lists:keyfind(mimetypes, 1, Extra) of
		false -> {[{cow_mimetypes:web(wf:to_binary(Path)), get_file}], Req, State};
		{mimetypes, Module, Function} -> {[{Module:Function(wf:to_binary(Path)), get_file}], Req, State};
		{mimetypes, Type} -> {[{Type, get_file}], Req, State}
	end.

resource_exists(Req, State={Path, {ok, #file_info{type={gz,T} }}, _}) -> 
	Req1 = cowboy_req:set_resp_header(<<"Content-Encoding">>,<<"gzip">>, Req),
	{true, Req1, State};
resource_exists(Req, State={Path, {ok, #file_info{type=ets_regular}}, _}) -> {true, Req, State};
resource_exists(Req, State={Path, {ok, #file_info{type=regular}}, _}) -> {true, Req, State};
resource_exists(Req, State) -> {false, Req, State}.

generate_etag(Req, State={Path, {ok, #file_info{size=Size, mtime=Mtime}}, Extra}) ->
	case lists:keyfind(etag, 1, Extra) of
		false -> {generate_default_etag(Size, Mtime), Req, State};
		{etag, Module, Function} -> {Module:Function(Path, Size, Mtime), Req, State};
		{etag, false} -> {undefined, Req, State}
	end.

generate_default_etag(Size, Mtime) ->
	{strong, list_to_binary(integer_to_list(
		erlang:phash2({Size, Mtime}, 16#ffffffff)))}.

last_modified(Req, State={_, {ok, #file_info{mtime=Modified}}, _}) -> {Modified, Req, State}.

get_file(Req, State={Path, {ok, #file_info{type={gz,regular} }}, _}) ->
	{ok, Raw} = file:read_file(Path ++ ".gz"),
	Sendfile  = fun (Socket, Transport) -> send(Transport,Socket,Raw) end,
	{{stream, size(Raw), Sendfile}, Req, State};

get_file(Req, State={Path, {ok, #file_info{type={gz,ets_regular} }}, _}) ->
	{ok, Raw} = mad_repl:load_file( Path ++ ".gz"),
	Sendfile  = fun (Socket, Transport) -> send(Transport,Socket,Raw) end,
	{{stream, size(Raw), Sendfile}, Req, State};

get_file(Req, State={Path, {ok, #file_info{type=regular}}, _}) ->
	{ok, Raw} = file:read_file(Path),
	Sendfile  = fun (Socket, Transport) -> send(Transport,Socket,Raw) end,
	{{stream, size(Raw), Sendfile}, Req, State};

get_file(Req, State={Path, {ok, #file_info{type=ets_regular}}, _}) ->
	{ok, Raw} = mad_repl:load_file(Path),
	Sendfile  = fun (Socket, Transport) -> send(Transport,Socket,Raw) end,
	{{stream, size(Raw), Sendfile}, Req, State}.

send(Transport,Socket,Raw) ->
	case Transport:send(Socket, Raw) of
		{ok, _} -> ok;
		{error, closed} -> ok;
		{error, etimedout} -> ok;
		_ -> ok end.

