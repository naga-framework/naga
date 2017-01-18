-ifndef(NAGA_HRL).
-define(NAGA_HRL, true).

-define(SPECIAL_FILES,             ["/favicon.ico",
                                    "/apple-touch-icon.png",
                                    "/robots.txt"]).
-define(DEFAULT_HTTP_PORT,          8001).
-define(DEFAULT_HTTPS_PORT,         8443).
-define(DEFAULT_SPDY_PORT,          8443).
-define(DEFAULT_ACCEPTOR_PROCESSES, 100).
-define(DEFAULT_LISTENER,           {http,[{ip, {0,0,0,0}},{port, ?DEFAULT_HTTP_PORT}]}).
-define(DEFAULT_FCGI_URL,           <<"/fcgi">>).
-define(DEFAULT_FCGI_STATIC,       [<<".jpg">>,
                                    <<".jpeg">>,
                                    <<".gif">>,
                                    <<".css">>,
                                    <<".png">>,
                                    <<".js">>,
                                    <<".ico">>,
                                    <<".xml">>]).

-define(DEFAULT_SSL_OPTS,         [{cacertfile, "./priv/ssl/cowboy-ca.crt"},
                                   {certfile, "./priv/ssl/server.crt"},
                                   {keyfile, "./priv/ssl/server.key"}]).   

-define(CTYPE_JS,                 [{<<"Content-Type">>,<<"application/javascript">>}]).
-define(CTYPE_HTML,               [{<<"Content-Type">>,<<"text/html">>}]).
-define(CTYPE_XML,                [{<<"Content-Type">>,<<"text/xml">>}]).
-define(CTYPE_CSS,                [{<<"Content-Type">>,<<"text/css">>}]).
-define(CTYPE_JSON,               [{<<"Content-Type">>,<<"application/json">>}]).
-define(CTYPE_PLAIN,              [{<<"Content-Type">>,<<"text/plain">>}]).
%-define(CTYPE_YAML,               [{<<"Content-Type">>,<<"text/yaml">>}]). ??
-define(CTYPE_YAML,               [{<<"Content-Type">>,<<"text/plain">>}]).

-record(route,{type        =[], 
               application =[],
               controller  =[],  %% controller == module
               view        =[],  %% view       == module 
               action      =index,
               arity       =3,	           
               want_session=true,
               is_steroid  =false,
               params      =undefined,
               bindding    =undefined,
               opts        =undefined,
               match       =undefined
              }).                           

-endif.