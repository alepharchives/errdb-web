%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(errdb_web).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the errdb_web server.
start() ->
    errdb_web_deps:ensure(),
    ensure_started(crypto),
    application:start(errdb_web).

%% @spec stop() -> ok
%% @doc Stop the errdb_web server.
stop() ->
    Res = application:stop(errdb_web),
    application:stop(crypto),
    Res.
