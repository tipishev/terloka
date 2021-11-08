%%% @doc search worker as represented by FSM.
%%% hopefully will avoid carrying the full state around.

-module(search_worker_statem).
-behaviour(gen_statem).

% client API
-export([start_link/1, stop/0]).

% behaviour callbacks
-export([init/1, terminate/3, callback_mode/0]).

% states: /2 for events, /3 for synchronous calls
-export([expecting_toloka_search/2]).

%%% Client API

start_link(Description) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Description, _Opts=[]).

stop() ->
    ok.

%%% Behavior callbacks

callback_mode() ->
    state_functions.

init(Description) ->
    TolokaSearchId = toloka:search(Description),
    % FIXME set up a timer for query toloka
    {ok, expect_toloka_search, TolokaSearchId}.

terminate(_Reason, _State, _Data) ->
    ok.

%% States

expecting_toloka_search(query_toloka, TolokaSearchId) ->
    case toloka:is_search_ready(TolokaSearchId) of
        true ->
            TolokaCheckId = toloka:check(TolokaSearchId),
            {next_state, expecting_toloka_check, TolokaCheckId};
        false ->
            % FIXME set up a timer for query toloka
            {next_state, expecting_toloka_search, TolokaSearchId}
    end.
