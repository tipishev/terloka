-module(search_worker).

-define(POOL_ID, <<"23077202">>).
-define(WAIT_TIMEOUT, 10000).
-define(QUOTE_DATA, [
    #{url => <<"ya.ru">>, screenshot => <<"123">>},
    #{url => <<"google.com">>, screenshot => <<"321">>}
]).

-export([start/1]).

% % all worker-process needs to know
% -record(state, {query, search_task_id=undefined}).

start(Query) ->
    spawn(fun() -> init(Query) end).

init(Query) ->
    TaskId = create_task(?POOL_ID, Query),
    io:format("Created a Toloka task in Pool ~p, Task Id ~p~n", [?POOL_ID, TaskId]),
    wait_for_search(TaskId, 2000).

wait_for_search(_TaskId, Wait) when Wait > ?WAIT_TIMEOUT ->
    io:format("I ain't gonna wait ~p seconds!~n", [Wait / 1000]),
    throw(error);
wait_for_search(TaskId, Wait) ->
    io:format("I will sleep for ~p seconds before checking Toloka...~n", [Wait / 1000]),
    receive
    after Wait ->
        case is_ready(TaskId) of
            true ->
                io:format("Hooray! Toloka is ready!~n"),
                download_solution(TaskId);
            false ->
                io:format("Not ready yet :-/~n"),
                wait_for_search(TaskId, Wait * 2)
        end
    end.

download_solution(_TaskId) ->
    QuoteData = ?QUOTE_DATA,
    io:format("There are ~p answers~n", [length(QuoteData)]),
    io:format("Downloading first..."),
    ok.

% @doc make file appear on yandex disk
% copy to Yandex.Disk without downloading!
% upload_to_yandex_disk(MeaningfulName) ->
%
    % download files
    % upload to yandex disk
    % create check pool

%%% Private Functions

%%% External Toloka
download_screenshot(_FileId) -> <<1, 2, 3>>.
create_task(_PoolId, _Query) -> task_id.
is_ready(_TaskId) -> rand:uniform() > 0.6.
% download_solution(_TaskId) -> [{url1, screenshot1_id}, {url2, screenshot2_id}].
