-module(python_erlang_exchange).

-export([get_search_requests/0]).
-export([put_search_result/2]).

-export([test_get/0, test_put/0]).

test_get() ->
    get_search_requests().

test_put() ->
    put_search_result(123, <<"asdfghjk">>).

% TODO make it a process
% TODO subscribe to insert events

get_search_requests() ->
    {ok, Conn} = esqlite3:open("python_erlang_exchange.sqlite3"),
    Query = "SELECT * from search_requests;",
    Result = esqlite3:map(fun as_map/1, Query, Conn),
    esqlite3:close(Conn),
    Result.

put_search_result(PositionId, SearchResult) ->
    {ok, Conn} = esqlite3:open("python_erlang_exchange.sqlite3"),

    % FIXME protect against SQL injection
    Query = io_lib:format("UPDATE search_requests SET result = '~s', updated_at = TIME() WHERE position_id=~p;", [SearchResult, PositionId]),

    % io:format(Query).
    esqlite3:q(Query, Conn),
    esqlite3:close(Conn),
    ok.

as_map({PositionId, Description, CreatedAt, Result, UpdatedAt}) ->
    #{
        position_id => PositionId,
        description => Description,
        created_at => CreatedAt,
        result => Result,
        updated_at => UpdatedAt
    }.
