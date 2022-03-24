-module(python_erlang_exchange).

-export([initialize/0]).
-export([get_search_requests/0]).
-export([put_search_result/2]).

-export([test_get/0, test_put/0]).

%%% Macros
-define(DB_FILENAME, "python_erlang_exchange.sqlite3").

test_get() ->
    get_search_requests().

test_put() ->
    put_search_result(_PositionId = 123, _SearchResult = <<"esqlite3 again">>).

% TODO make it a process
% TODO subscribe to insert events in SQL

initialize() ->
    {ok, _Conn} = esqlite3:open(?DB_FILENAME).

get_search_requests() ->
    {ok, Conn} = esqlite3:open(?DB_FILENAME),
    Sql = "SELECT * from search_requests;",
    Result = esqlite3:map(fun as_map/1, Sql,  Conn),
    esqlite3:close(Conn),
    Result.

put_search_result(PositionId, SearchResult) ->
    {ok, Conn} = esqlite3:open(?DB_FILENAME),

    % FIXME protect against SQL injection
    Template =
        "UPDATE search_requests"
        " SET result = '~s', updated_at = DATETIME()"
        " WHERE position_id=~p;",
    Sql = io_lib:format(Template, [SearchResult, PositionId]),

    esqlite3:q(Sql, Conn),
    esqlite3:close(Conn),
    ok.

%%% Private functions

as_map({PositionId, Description, CreatedAt, Result, UpdatedAt}) ->
    #{
        position_id => PositionId,
        description => Description,
        created_at => CreatedAt,
        result => Result,
        updated_at => UpdatedAt
    }.

% WriteRead = fun(N) -> python_erlang_exchange:test_put(), io:format("written ~p~n", [N]), python_erlang_exchange:test_get(), io:format("read ~p~n", [N]), N end,
% [WriteRead(N) || N <- lists:seq(1, 1000)].
