-compile({parse_transform, lager_transform}).
-module(naga_log).
-compile(export_all).

log_level() -> 
    log_level(lager_console_backend).
log_level(Backend) ->
    lager:get_loglevel(Backend).

log_modules() ->
    wf:config(naga,log_modules).

add_module(M) when is_atom(M)->
    case log_modules() of any -> application:set_env(naga, log_modules, [M]);
    Modules -> application:set_env(naga, log_modules, Modules ++ [M]) end.
rem_module(M) when is_atom(M)->
    case log_modules() of any -> application:set_env(naga, log_modules, []);
    Modules -> application:set_env(naga, log_modules, Modules -- [M]) end.

stop() -> 
    application:set_env(naga, log_modules, []).

set_modules(any) -> 
    application:set_env(naga, log_modules, any);
set_modules(List) when is_list(List) ->
    application:set_env(naga, log_modules, List).
    
set_loglevel(Level) ->    
    lager:set_loglevel(lager_console_backend, Level).
    
debug(Module, String, Args)     -> lager:debug(format_message(Module, String), Args).
info(Module, String, Args)      -> lager:info(format_message(Module, String), Args).
notice(Module, String, Args)    -> lager:notice(format_message(Module, String), Args).
warning(Module, String, Args)   -> lager:warning(format_message(Module, String), Args).
error(Module, String, Args)     -> lager:error(format_message(Module, String), Args).
critical(Module, String, Args)  -> lager:critical(format_message(Module, String), Args).
alert(Module, String, Args)     -> lager:alert(format_message(Module, String), Args).
emergency(Module, String, Args) -> lager:emergency(format_message(Module, String), Args).

format_message(Module, String) ->  wf:to_list([Module, ":", String]).
