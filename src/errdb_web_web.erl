%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for errdb_web.

-module(errdb_web_web).
-author('author <ery.lee@gmail.com>').

-import(errdb_web, [errdb_node/0]).

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    Method = Req:get(method),
	Path = list_to_tuple(string:tokens(Req:get(path), "/")),
	handle(Method, Path, Req, DocRoot).

handle('GET', {"rrdb", Key, "last"}, Req, _DocRoot) ->
	case rpcall(errdb, last, [list_to_binary(Key)]) of
    {ok, Fields, Record} -> 
        Head = string:join(Fields, ","),
        Line = line(Record),
        Resp = list_to_binary(["time:", Head, "\n", Line]),
        Req:ok({"text/plain", Resp});
    {error, Reason} ->
        Req:respond({500, [], atom_to_list(Reason)});
    {badrpc, _Reason} ->
        Req:respond({500, [], "failed to connect errdb"})
	end;

handle('GET', {"rrdb", Key, Range}, Req, _DocRoot) ->
    [Begin,End|_] = string:tokens(Range, "-"),
	case rpcall(errdb, fetch, [list_to_binary(Key), 
        list_to_integer(Begin), list_to_integer(End)]) of
    {ok, Fields, Records} -> 
        Head = string:join(Fields, ","),
        Lines = string:join([line(Record) || Record <- Records], "\n"),
        Resp = list_to_binary(["time:", Head, "\n", Lines]),
        Req:ok({"text/plain", Resp});
    {error, Reason} ->
        Req:respond({500, [], atom_to_list(Reason)});
    {badrpc, _Reason} ->
        Req:respond({500, [], "failed to connect errdb"})
	end;

handle('GET', PathTuple, Req, DocRoot) ->
    Path = string:join(tuple_to_list(PathTuple), "/"),
	io:format("Path: ~p~n", [Path]),
    Req:serve_file(Path, DocRoot);

handle(_, _, Req, _DocRoot) ->
	Req:respond({501, [], <<"bad request">>}).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options),
	 proplists:delete(Option, Options)}.

rpcall(Mod, Fun, Args) ->
	rpc:call(errdb_node(), Mod, Fun, Args).

line({Time, Values}) when is_integer(Time) and is_list(Values) ->
    Line = string:join([extbif:to_list(V) || V <- Values], ","),
    string:join([extbif:to_list(Time), Line], ":").

