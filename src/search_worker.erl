-module(search_worker).

-define(POOL_ID, 23077202).

-export([start/1]).

% % all worker-process needs to know
% -record(state, {query, search_task_id=undefined}).

start(Query) ->
    spawn(fun() -> worker(Query) end).

worker(Query) ->
    receive
    after 5000 ->
        io:format("~p~n", [Query])
    end.
