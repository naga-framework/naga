%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
%% Copyright (c) 2015, Chanrotha Sisowath <chan.sisowath@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(naga_fcgi).
-author('Anthony Ramine <nox@dev-extend.eu>').
-author('chan sisowath <chan.sisowath@gmail.com>').
-behaviour(cowboy_http_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/3, handle/2, terminate/3]).
-export([start/1, stop/1, start_fcgi/5]).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_FCGI_TIMEOUT, 60000).
-define(DEFAULT_FCGI_PORT,    33000).
-define(DEFAULT_FCGI_URL,     <<"/fcgi">>).
-define(DEFAULT_FCGI_STATIC, [<<".jpg">>,
                              <<".jpeg">>,
                              <<".gif">>,
                              <<".css">>,
                              <<".png">>,
                              <<".js">>,
                              <<".ico">>,
                              <<".xml">>]).


-type uint32() :: 0..(1 bsl 32 - 1).

-type fold_k_stdout_fun(Acc, NewAcc) ::
        fun((Acc, Buffer::binary() | eof, fold_k_stdout_fun(Acc, NewAcc)) -> NewAcc).

-type option() :: {name, atom()}
                | {timeout, uint32()}
                | {script_dir, iodata()}
                | {fcgi_url, binary()}
                | {static_enabled, boolean()}
                | {static_exts, [binary()]}
                | {path_root, iodata()}.
-type req()    :: cowboy_req:req().

-export_type([option/0]).

-record(state, {server         :: atom(),
                timeout        :: uint32(),
                script_dir     :: iodata(),
                path_root      :: undefined | iodata(),
                https          :: boolean(),
                fcgi_url       :: iodata(),
                static_enabled :: boolean(),
                static_exts    :: [binary()]
               }).

-record(cgi_head, {status = 200 :: cowboy_http:status(),
                   type         :: undefined | binary(),
                   location     :: undefined | binary(),
                   headers = [] :: cowboy_http:headers()}).


start(App) -> 
    Exe        = wf:config(App, fcgi_exe,  'php-fpm'),
    Name       = wf:config(App, fcgi_name, 'php-fpm'),
    FcgiDir    = wf:config(App, fcgi_dir,  "fcgi-scripts/docroot"),
    FcgiConfig = wf:config(App, fcgi_conf, "fcgi-scripts/php-fpm.conf"),
    FcgiHost   = wf:config(App, fcgi_host, localhost),
    FcgiPort   = wf:config(App, fcgi_port, ?DEFAULT_FCGI_PORT),
    start_fcgi({Exe,Name}, FcgiConfig, FcgiDir, FcgiHost, FcgiPort).

start_fcgi({Exe, Name}, Conf, DocRoot, Host, Port) ->
   case os:find_executable(wf:to_list(Exe)) of
      false   -> wf:error(?MODULE, "EXE ~p not found\r",[Exe]);
      FpmPath -> 
                 application:start(ex_fcgi),
                 application:start(inets),
                 Env = [],
                 {ok, Cwd} = file:get_cwd(),
                 wf:info(?MODULE, "starting fcgi php-fpm ~p...",[FpmPath]),

                 {_,Status,X} = sh:run(FpmPath, ["-y", Conf], binary, Cwd, Env),
                 Command = {FpmPath, ["-y", Conf, "-p", DocRoot]},
                 wf:info(?MODULE, "starting fcgi php-fpm ~p...",[Command]), 

                 case Status == 0 of
                  true -> case ex_fcgi:start(Name, Host, Port) of
                           {ok, _Pid} -> ok;
                           {error, {already_started, _Pid}} -> ok;
                           E -> E
                          end;
                  false -> wf:error(?MODULE, "Shell Error ~p: ~s~n\r",
                          [Command, binary_to_list(X)]), 
                          {error,X}
                 end 
   end.

stop(App) -> 
    Name = wf:config(App, name, 'php-fpm'),
    %%FIXME: kill the php-fpm process, pid file ?
    ex_fcgi:stop(Name).

terminate(_Reason, _Req, _State) -> ok.

init(_, Req, FcgiOpts) ->
    %wf:info(?MODULE, "FCGI OPTS ~p",[FcgiOpts]),
   
    Name      = proplists:get_value(name, FcgiOpts, 'php-fpm'),
    Timeout   = proplists:get_value(timeout, FcgiOpts, 15000),

    ScriptDir = proplists:get_value(script_dir, FcgiOpts),
    PathRoot  = proplists:get_value(path_root, FcgiOpts, ScriptDir),
    FcgiUrl   = proplists:get_value(fcgi_path, FcgiOpts, ?DEFAULT_FCGI_URL),

    Https     = proplists:get_value(ssl_enabled, FcgiOpts, false),
StaticEnabled = proplists:get_value(fcgi_static_enabled,FcgiOpts,false),
   StaticExts = proplists:get_value(fcgi_static_exts, FcgiOpts, ?DEFAULT_FCGI_STATIC),  

  case whereis(Name) of
    Server when is_pid(Server) ->
      State = #state{server         = Server,
                     timeout        = Timeout,
                     script_dir     = ScriptDir,
                     path_root      = PathRoot,
                     fcgi_url       = FcgiUrl,
                     static_enabled = StaticEnabled,
                     static_exts    = StaticExts,
                     https          = Https},
          {ok, Req, State} end.


-spec handle(req(), #state{}) -> {ok, req(), #state{}}.
handle(Req, State) ->
    {Path, _}    = cowboy_req:path_info(Req),
    {RawPath, _} = cowboy_req:path(Req),
    %%FIXME: Path, PathInfo
    PathInfo = Path, 
    handle(Req, State, Path, PathInfo, RawPath).
    
handle(Req, State, Path, undefined, _RawPath) ->
  % php-fpm complains when PATH_TRANSLATED isn't set for /ping and
  % /status requests and it's not non standard to send a empty value
  % if PathInfo isn't defined (or empty).
  handle_scriptname(Req, State, [{<<"PATH_TRANSLATED">>, <<>>}], Path);
handle(Req, State = #state{path_root = undefined}, _, _, _) ->
  % A path info is here but the handler doesn't have a path root.
  {ok, cowboy_req:reply(500, [], [], Req), State};
handle(Req, State = #state{path_root = PathRoot}, Path, [], RawPath) ->
  case binary:last(RawPath) of
    $/ ->
      % Trailing slash means CGI path info is "/".
      CGIParams = [{<<"PATH_INFO">>, <<"/">>},
                   {<<"PATH_TRANSLATED">>, [PathRoot, $/]}],
      handle_scriptname(Req, State, CGIParams, Path);
    _ ->
      % Same as with undefined path info.
      handle_scriptname(Req, State, [{<<"PATH_TRANSLATED">>, <<>>}], Path) end;

handle(Req, State = #state{%path_root = PathRoot,
                           script_dir = ScriptDir,
                           fcgi_url = FcgiUrl, 
                           static_enabled = true,
                           static_exts = StaticExts}, Path, PathInfo, RawPath) ->
    Ext = filename:extension(lists:last(PathInfo)),
    case lists:member(Ext, StaticExts) of
        false ->
            handle(Req, State#state{static_enabled=false}, Path, PathInfo, RawPath);
        true ->
            Fcgi = tl(filename:split(FcgiUrl)),
            Filename = [ binary_to_list(X) || X <- (PathInfo -- Fcgi)],
            FilePath = filename:join(ScriptDir, filename:join(Filename)),
            Extra = [{mimetypes, cow_mimetypes, all}],
            case file:read_file_info(FilePath, [{time, universal}]) of
                {error, _} -> 
                    handle(Req, State#state{static_enabled=false}, Path, PathInfo, RawPath);
                Info ->
                    {fcgi_static, Req, {list_to_binary(FilePath), Info, Extra}}
            end
    end;

handle(Req, #state{path_root = PathRoot} = State, _Path, PathInfo, _) ->
    PathTranslated = [PathRoot, $/, filename:join(PathInfo)],
    CGIParams = [{<<"PATH_TRANSLATED">>, PathTranslated},
                 {<<"PATH_INFO">>, PathInfo}],
    handle_scriptname(Req, State, CGIParams, PathInfo).

-spec handle_scriptname(req(), #state{}, [{binary(), iodata()}],
                        cowboy_dispatcher:path_tokens()) ->
                         {ok, req(), #state{}}.
handle_scriptname(Req, State = #state{script_dir = undefined}, CGIParams, []) ->
  handle_req(Req, State, [{<<"SCRIPT_NAME">>, <<"/">>} | CGIParams]);
handle_scriptname(Req, State = #state{script_dir = undefined}, CGIParams,
                  ScriptName) ->
  CGIScriptName = [[$/, Segment] || Segment <- ScriptName],
  handle_req(Req, State, [{<<"SCRIPT_NAME">>, CGIScriptName} | CGIParams]);
handle_scriptname(Req, _State, _CGIParams, []) ->
  % The handler should send a SCRIPT_FILENAME param but there was no path
  % provided.
  {ok, cowboy_req:reply(500, [], [], Req)};
handle_scriptname(Req, State = #state{script_dir = Dir, fcgi_url=FcgiUrl}, CGIParams,
                  PathInfo) ->
    Fcgi = tl(filename:split(FcgiUrl)),
    CGIScriptName = [[$/, Segment] || Segment <- PathInfo],
    ScriptFileName = case PathInfo -- Fcgi of 
                         [] -> [list_to_binary(Dir), $/]; 
                         ScriptName -> [list_to_binary(Dir), $/, filename:join(ScriptName)] 
                     end,
    NewCGIParams = [{<<"SCRIPT_NAME">>, CGIScriptName},
                    {<<"SCRIPT_FILENAME">>, ScriptFileName} | CGIParams],
    handle_req(Req, State, NewCGIParams).

-spec handle_req(req(), #state{}, [{binary(), iodata()}]) ->
                  {ok, req(), #state{}}.
handle_req(Req, State = #state{server = Server,
                          timeout = Timeout,
                          https = Https}, CGIParams) ->
    {Method,_}  = cowboy_req:method(Req),
    {Version,_} = cowboy_req:version(Req),
    {RawQs,_}   = cowboy_req:qs(Req),
    {RawHost,_} = cowboy_req:host(Req),
    {Port,_}    = cowboy_req:port(Req),
    {Headers,_} = cowboy_req:headers(Req),    
    {{Address, _Port},_} = cowboy_req:peer(Req),
    AddressStr = inet_parse:ntoa(Address),
    %% @todo Implement correctly the following parameters:
    %% - AUTH_TYPE = auth-scheme token
    %% - REMOTE_USER = user-ID token
    CGIParams1 = case Https of
    true -> [{<<"HTTPS">>, <<"1">>}|CGIParams];
    false -> CGIParams end,
    CGIParams2 = [{<<"GATEWAY_INTERFACE">>, <<"CGI/1.1">>},
                  {<<"QUERY_STRING">>, RawQs},
                  {<<"REMOTE_ADDR">>, AddressStr},
                  {<<"REMOTE_HOST">>, AddressStr},
                  {<<"REQUEST_METHOD">>, Method},
                  {<<"SERVER_NAME">>, RawHost},
                  {<<"SERVER_PORT">>, integer_to_list(Port)},
                  {<<"SERVER_PROTOCOL">>, protocol(Version)},
                  {<<"SERVER_SOFTWARE">>, <<"Cowboy">>} |
                  CGIParams1],
    CGIParams3 = params(Headers, CGIParams2),
    %wf:info(?MODULE,"CGIParams: ~p~n",[CGIParams3]),
    case ex_fcgi:begin_request(Server, responder, CGIParams3, Timeout) of
        error ->
            wf:info(?MODULE,"ERROR FCGI", []),
            cowboy_req:reply(502, [], [], Req);
        {ok, Ref} ->
            wf:info(?MODULE,"FCGI >>>>>> OK.", []),
            Req3 = case cowboy_req:body(Req) of
                       {ok, Body, Req2} ->
                           ex_fcgi:send(Server, Ref, Body),
                           Req2;
                       {error, badarg} ->
                           Req end,
            Fun = fun decode_cgi_head/3,
            {ok, Req4} = case fold_k_stdout(#cgi_head{}, <<>>, Fun, Ref) of
                       {Head, Rest, Fold} ->
                           case acc_body([], Rest, Fold) of
                               error ->
                                   cowboy_req:reply(502, [], [], Req3);
                                     timeout ->
                                   cowboy_req:reply(504, [], [], Req3);
                               CGIBody ->
                                   send_response(Req3, Head, CGIBody) 
                           end;
                       error ->
                           cowboy_req:reply(502, [], [], Req3);
                       timeout ->
                           cowboy_req:reply(504, [], [], Req3) end,
            {ok, Req4, State} end.

-spec protocol(cowboy_http:version()) -> binary().
protocol('HTTP/1.0') -> <<"HTTP/1.0">>;
protocol('HTTP/1.1') -> <<"HTTP/1.1">>.

-spec params(cowboy_http:headers(), [{binary(), iodata()}]) -> [{binary(), iodata()}].
params(Params, Acc) ->
    F = fun ({Name, Value}, Acc1) ->
                case param(Name) of
                    ignore ->
                        Acc1;
                    ParamName ->
                        case Acc1 of
                            [{ParamName, AccValue} | Acc2] ->
                                %% Value is counter-intuitively prepended to AccValue
                                %% because Cowboy accumulates headers in reverse order.
                                [{ParamName, [Value, value_sep(Name) | AccValue]} | Acc2];
                            _ ->
                                [{ParamName, Value} | Acc1] end end end,
    lists:foldl(F, Acc, lists:keysort(1, Params)).

-spec value_sep(cowboy_http:header()) -> char().
value_sep(<<"cookie">>) ->
    %% Accumulate cookies using a semicolon because at least one known FastCGI
    %% implementation (php-fpm) doesn't understand comma-separated cookies.
    $;;
value_sep(_Header) ->
    $,.

-spec param(cowboy_http:header())-> binary() | ignore.
param(<<"host">>)                -> <<"HTTP_HOST">>;
param(<<"accept">>)              -> <<"HTTP_ACCEPT">>;
param(<<"accept-charset">>)      -> <<"HTTP_ACCEPT_CHARSET">>;
param(<<"accept-encoding">>)     -> <<"HTTP_ACCEPT_ENCODING">>;
param(<<"accept-language">>)     -> <<"HTTP_ACCEPT_LANGUAGE">>;
param(<<"cache-control">>)       -> <<"HTTP_CACHE_CONTROL">>;
param(<<"content-base">>)        -> <<"HTTP_CONTENT_BASE">>;
param(<<"content-encoding">>)    -> <<"HTTP_CONTENT_ENCODING">>;
param(<<"content-language">>)    -> <<"HTTP_CONTENT_LANGUAGE">>;
param(<<"content-length">>)      -> <<"CONTENT_LENGTH">>;
param(<<"content-md5">>)         -> <<"HTTP_CONTENT_MD5">>;
param(<<"content-range">>)       -> <<"HTTP_CONTENT_RANGE">>;
param(<<"content-type">>)        -> <<"CONTENT_TYPE">>;
param(<<"cookie">>)              -> <<"HTTP_COOKIE">>;
param(<<"etag">>)                -> <<"HTTP_ETAG">>;
param(<<"from">>)                -> <<"HTTP_FROM">>;
param(<<"if-modified-since">>)   -> <<"HTTP_IF_MODIFIED_SINCE">>;
param(<<"if-match">>)            -> <<"HTTP_IF_MATCH">>;
param(<<"if-none-match">>)       -> <<"HTTP_IF_NONE_MATCH">>;
param(<<"if-range">>)            -> <<"HTTP_IF_RANGE">>;
param(<<"ff-unmodified-since">>) -> <<"HTTP_IF_UNMODIFIED_SINCE">>;
param(<<"location">>)            -> <<"HTTP_LOCATION">>;
param(<<"pragma">>)              -> <<"HTTP_PRAGMA">>;
param(<<"range">>)               -> <<"HTTP_RANGE">>;
param(<<"referer">>)             -> <<"HTTP_REFERER">>;
param(<<"user-agent">>)          -> <<"HTTP_USER_AGENT">>;
param(<<"warning">>)             -> <<"HTTP_WARNING">>;
param(<<"x-forwarded-for">>)     -> <<"HTTP_X_FORWARDED_FOR">>;
param(Name) when is_atom(Name)   -> ignore;
param(Name) when is_binary(Name) -> <<"HTTP_", (<< <<(param_char(C))>> || <<C>> <= Name >>)/binary>>.

-spec param_char(char()) -> char().
param_char($a) -> $A;
param_char($b) -> $B;
param_char($c) -> $C;
param_char($d) -> $D;
param_char($e) -> $E;
param_char($f) -> $F;
param_char($g) -> $G;
param_char($h) -> $H;
param_char($i) -> $I;
param_char($j) -> $J;
param_char($k) -> $K;
param_char($l) -> $L;
param_char($m) -> $M;
param_char($n) -> $N;
param_char($o) -> $O;
param_char($p) -> $P;
param_char($q) -> $Q;
param_char($r) -> $R;
param_char($s) -> $S;
param_char($t) -> $T;
param_char($u) -> $U;
param_char($v) -> $V;
param_char($w) -> $W;
param_char($x) -> $X;
param_char($y) -> $Y;
param_char($z) -> $Z;
param_char($-) -> $_;
param_char(Ch) -> Ch.

-spec fold_k_stdout(Acc, binary(), fold_k_stdout_fun(Acc, NewAcc),
                    reference()) ->
                     NewAcc | error | timeout.
fold_k_stdout(Acc, Buffer, Fun, Ref) ->
  receive Msg -> fold_k_stdout(Acc, Buffer, Fun, Ref, Msg) end.

-spec fold_k_stdout(Acc, binary(), fold_k_stdout_fun(Acc, NewAcc),
                    reference(), term()) ->
                     NewAcc | error | timeout.
fold_k_stdout(Acc, Buffer, Fun, Ref, {ex_fcgi, Ref, Messages}) ->
  fold_k_stdout2(Acc, Buffer, Fun, Ref, Messages);
fold_k_stdout(_Acc, _Buffer, _Fun, Ref, {ex_fcgi_timeout, Ref}) ->
  timeout;
fold_k_stdout(Acc, Buffer, Fun, Ref, _Msg) ->
  fold_k_stdout(Acc, Buffer, Fun, Ref).

-spec fold_k_stdout2(Acc, binary(), fold_k_stdout_fun(Acc, NewAcc),
                     reference(), [ex_fcgi:message()]) ->
                      NewAcc | error | timeout.
fold_k_stdout2(Acc, Buffer, Fun, _Ref, [{stdout, eof} | _Messages]) ->
  fold_k_stdout2(Acc, Buffer, Fun);
fold_k_stdout2(Acc, Buffer, Fun, Ref, [{stdout, NewData} | Messages]) ->
  Cont = fun (NewAcc, Rest, NewFun) ->
    fold_k_stdout2(NewAcc, Rest, NewFun, Ref, Messages) end,
  Fun(Acc, <<Buffer/binary, NewData/binary>>, Cont);
fold_k_stdout2(Acc, Buffer, Fun, _Ref,
               [{end_request, _CGIStatus, _AppStatus} | _Messages]) ->
  fold_k_stdout2(Acc, Buffer, Fun);
fold_k_stdout2(Acc, Buffer, Fun, Ref, [_Msg | Messages]) ->
  fold_k_stdout2(Acc, Buffer, Fun, Ref, Messages);
fold_k_stdout2(Acc, Buffer, Fun, Ref, []) ->
  fold_k_stdout(Acc, Buffer, Fun, Ref).

-spec fold_k_stdout2(Acc, binary(), fold_k_stdout_fun(Acc, NewAcc)) ->
                      NewAcc | error | timeout.
fold_k_stdout2(Acc, <<>>, Fun) ->
  Cont = fun (_NewAcc, _NewBuffer, _NewFun) -> error end,
  Fun(Acc, eof, Cont);
fold_k_stdout2(_Acc, _Buffer, _Fun) ->
  error.

-spec decode_cgi_head(#cgi_head{}, binary() | eof,
                      fold_k_stdout_fun(#cgi_head{},
                                        #cgi_head{} | error | timeout)) ->
                       #cgi_head{} | error | timeout.
decode_cgi_head(_Head, eof, _More) ->
  error;
decode_cgi_head(Head, Data, More) ->
  case erlang:decode_packet(httph_bin, Data, []) of
    {ok, Packet, Rest} ->
      decode_cgi_head(Head, Rest, More, Packet);
    {more, _} ->
      More(Head, Data, fun decode_cgi_head/3);
    _ ->
      error end.

-define(decode_default(Head, Rest, More, Field, Default, Value),
  case Head#cgi_head.Field of
    Default ->
      decode_cgi_head(Head#cgi_head{Field = Value}, Rest, More);
    _ ->
      % Decoded twice the same CGI header.
      error end).

-spec decode_cgi_head(#cgi_head{}, binary(),
                      fold_k_stdout_fun(#cgi_head{},
                                        #cgi_head{} | error | timeout),
                      term()) -> #cgi_head{} | error | timeout.
decode_cgi_head(Head, Rest, More, {http_header, _, <<"Status">>, _, Value}) ->
  ?decode_default(Head, Rest, More, status, 200, Value);
decode_cgi_head(Head, Rest, More, {http_header, _, 'Content-Type', _, Value}) ->
  ?decode_default(Head, Rest, More, type, undefined, Value);
decode_cgi_head(Head, Rest, More, {http_header, _, 'Location', _, Value}) ->
  ?decode_default(Head, Rest, More, location, undefined, Value);
decode_cgi_head(Head, Rest, More, {http_header, _, << "X-CGI-", _NameRest >>, _, _Value}) ->
  % Dismiss any CGI extension header.
  decode_cgi_head(Head, Rest, More);
decode_cgi_head(Head = #cgi_head{headers = Headers}, Rest, More,
                {http_header, _, Name, _, Value}) ->
  NewHead = Head#cgi_head{headers = [{header_to_bin(Name), Value} | Headers]},
  decode_cgi_head(NewHead, Rest, More);
decode_cgi_head(Head, Rest, More, http_eoh) ->
  {Head, Rest, More};
decode_cgi_head(_Head, _Rest, _Name, _Packet) ->
  error.

-spec acc_body([binary()], binary() | eof,
               fold_k_stdout_fun([binary()], [binary()]) | error | timeout) ->
                [binary()] | error | timeout.
acc_body(Acc, eof, _More) ->
  lists:reverse(Acc);
acc_body(Acc, Buffer, More) ->
  More([Buffer | Acc], <<>>, fun acc_body/3).

-spec send_response(req(), #cgi_head{}, [binary()]) -> {ok, req()}.
send_response(Req, #cgi_head{location = <<$/, _/binary>>}, _Body) ->
  % @todo Implement 6.2.2. Local Redirect Response.
  cowboy_req:reply(502, [], [], Req);
send_response(Req, Head = #cgi_head{location = undefined}, Body) ->
  % 6.2.1. Document Response.
  send_document(Req, Head, Body);
send_response(Req, Head, Body) ->
  % 6.2.3. Client Redirect Response.
  % 6.2.4. Client Redirect Response with Document.
  send_redirect(Req, Head, Body).

-spec send_document(req(), #cgi_head{}, [binary()]) -> {ok, req()}.
send_document(Req, #cgi_head{type = undefined}, _Body) ->
  cowboy_req:reply(502, [], [], Req);
send_document(Req, #cgi_head{status = Status, type = Type, headers = Headers},
              Body) ->
  reply(Req, Body, Status, Type, Headers).

-spec send_redirect(req(), #cgi_head{}, [binary()]) -> {ok, req()}.
send_redirect(Req, #cgi_head{status = Status = <<$3, _/binary>>,
                             type = Type,
                             location = Location,
                             headers = Headers}, Body) ->
  reply(Req, Body, Status, Type, [{<<"Location">>, Location} | Headers]);
send_redirect(Req, #cgi_head{type = Type,
                             location = Location,
                             headers = Headers}, Body) ->  
  reply(Req, Body, 302, Type, [{<<"Location">>, Location} | Headers]).

-spec reply(req(), [binary()], cowboy_http:status(), undefined | binary(),
            cowboy_http:headers()) -> {ok, Req::req()}.
% @todo Filter headers like Content-Length.
reply(Req, Body, Status, undefined, Headers) ->
  cowboy_req:reply(Status, Headers, Body, Req);
reply(Req, Body, Status, Type, Headers) ->
    {PathInfo,_} = cowboy_req:path_info(Req),
    H = [{<<"Content-Type">>, content_type(lists:last(PathInfo), Type)} | Headers],
    cowboy_req:reply(Status, H, Body, Req).

header_to_bin(Name) ->
    case Name of
        'Set-Cookie'          -> <<"Set-Cookie">>;
        'Set-Cookie2'         -> <<"Set-Cookie2">>;
        'Content-Type'        -> <<"Content-Type">>;
        'Cookie'              -> <<"Cookie">>;
        'Cache-Control'       -> <<"Cache-Control">>;
        'Connection'          -> <<"Connection">>; 
        'Date'                -> <<"Date">>;
        'Pragma'              -> <<"Pragma">>;
        'Transfer-Encoding'   -> <<"Transfer-Encoding">>;
        'Upgrade'             -> <<"Upgrade">>;
        'Via'                 -> <<"Via">>;
        'Accept'              -> <<"Accept">>;
        'Accept-Charset'      -> <<"Accept-Charset">>;
        'Accept-Encoding'     -> <<"Accept-Encoding">>;
        'Accept-Language'     -> <<"Accept-Language">>;
        'Authorization'       -> <<"Authorization">>;
        'From'                -> <<"From">>;
        'Host'                -> <<"Host">>;
        'If-Modified-Since'   -> <<"If-Modified-Since">>;
        'If-Match'            -> <<"If-Match">>;
        'If-None-Match'       -> <<"If-None-Match">>;
        'If-Range'            -> <<"If-Range">>;
        'If-Unmodified-Since' -> <<"If-Unmodified-Since">>;
        'Max-Forwards'        -> <<"Max-Forwards">>;
        'Proxy-Authorization' -> <<"Proxy-Authorization">>;
        'Range'               -> <<"Range">>;
        'Referer'             -> <<"Referer">>;
        'User-Agent'          -> <<"User-Agent">>;
        'Age'                 -> <<"Age">>;
        'Location'            -> <<"Location">>;
        'Proxy-Authenticate'  -> <<"Proxy-Authenticate">>;
        'Public'              -> <<"Public">>;
        'Retry-After'         -> <<"Retry-After">>;
        'Server'              -> <<"Server">>;
        'Vary'                -> <<"Vary">>;
        'Warning'             -> <<"Warning">>;
        'Www-Authenticate'    -> <<"Www-Authenticate">>;
        'Allow'               -> <<"Allow">>;
        'Content-Base'        -> <<"Content-Base">>;
        'Content-Encoding'    -> <<"Content-Encoding">>;
        'Content-Language'    -> <<"Content-Language">>;
        'Content-Length'      -> <<"Content-Length">>;
        'Content-Location'    -> <<"Content-Location">>;
        'Content-Md5'         -> <<"Content-Md5">>;
        'Content-Range'       -> <<"Content-Range">>;
        'Etag'                -> <<"Etag">>;
        'Expires'             -> <<"Expires">>;
        'Last-Modified'       -> <<"Last-Modified">>;
        'Accept-Ranges'       -> <<"Accept-Ranges">>;
        'X-Forwarded-For'     -> <<"X-Forwarded-For">>;
        'Keep-Alive'          -> <<"Keep-Alive">>;
        'Proxy-Connection'    -> <<"Proxy-Connection">>;
        Else                  -> Else
    end.

content_type(Path, Type) ->
    case cow_mimetypes:web(Path) of
        {<<"application">>, <<"octet-stream">>, []} -> Type;
        {A,B,_}  -> <<A/binary,"/",B/binary>> end.
    
-ifdef(TEST).
param_test() ->
    ?assertEqual(<<"HTTP_X_NON_STANDARD_HEADER">>,
                 param(<<"X-Non-Standard-Header">>)).
-endif.
