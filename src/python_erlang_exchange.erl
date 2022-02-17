-module(python_erlang_exchange).

-export([get_search_requests/0]).
-export([put_search_result/2]).

% TODO make it a process

% FIXME close DB
get_search_requests() ->
    {ok, Conn} = esqlite3:open("python_erlang_exchange.sqlite3"),
    esqlite3:q("SELECT * from search_requests;", Conn).

% FIXME protect against SQL injection
put_search_result(PositionId, SearchResult) ->
    {ok, Conn} = esqlite3:open("python_erlang_exchange.sqlite3"),
    % TODO don't change created_at
    Query = erlang:iolist_to_binary(io_lib:format("UPDATE search_requests SET result = '~s', updated_at = TIME() WHERE position_id=~p;",
                                                  [SearchResult, PositionId])),
    % io:format(Query).
    % FIXME close DB
    esqlite3:q(Query, Conn).




