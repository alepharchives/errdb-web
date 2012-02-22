%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(errdb_web).
-author('author <author@example.com>').
-export([start/0, stop/0, errdb_node/0]).

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
	init_elog(),
    errdb_web_deps:ensure(),
    ensure_started(crypto),
    application:start(errdb_web).

init_elog() ->
    {ok, [[LogLevel]]} = init:get_argument(log_level),
    {ok, [[LogPath]]} = init:get_argument(log_path),
	elog:init(list_to_integer(LogLevel), LogPath).

%% @spec stop() -> ok
%% @doc Stop the errdb_web server.
stop() ->
    Res = application:stop(errdb_web),
    application:stop(crypto),
    Res.

errdb_node() ->
	SNode =
	case net_kernel:longnames() of
	true ->
		"errdb@" ++ inet_db:gethostname() ++
			 "." ++ inet_db:res_option(domain);
	false ->
		"errdb@" ++ inet_db:gethostname()
	end,
	list_to_atom(SNode).

