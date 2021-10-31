%%% @doc this module represents the logic of a worker as if he were a human who
%%% writes its state on a piece of paper -- the state record.

-module(search_worker_fsm).
-behaviour(gen_fsm).

-export([start/1, save/1, load/1, stop/1, pause/1, resume/1, status/1]).

start_link(Description) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Description, _Opts=[]).

init(Description) ->
    TolokaSearchId = search(Description),
    % FIXME set up a timer for query toloka
    {ok, expect_toloka_search, TolokaSearchId}.

%% States

% /2 for events, /3 for synchronous calls
expecting_toloka_search(query_toloka, TolokaSearchId) ->
    case is_search_ready(TolokaSearchId) of
        true ->
            TolokaCheckId = check(TolokaSearchId),
            {next_state, expecting_toloka_check, TolokaCheckId};
        false ->
            % FIXME set up a timer for query toloka
            {next_state, expecting_toloka_search, TolokaSearchId}
    end.

% Mocks
search(_Description) -> 123.
check(_TolokaSearchId) -> 456.
is_search_ready(_SearchId) -> false.
