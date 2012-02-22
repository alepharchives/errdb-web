%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the errdb_web application.

-module(errdb_web_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for errdb_web.
start(_Type, _StartArgs) ->
    errdb_web_deps:ensure(),
    errdb_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for errdb_web.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
