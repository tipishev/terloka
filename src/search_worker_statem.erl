%%% @doc search worker as represented by a state machine.
%%% hopefully will avoid carrying the full state around.

-module(search_worker_statem).

-behaviour(gen_statem).

% client API
-export([start_link/1, stop/1]).

% behaviour callbacks
-export([init/1, terminate/3, callback_mode/0]).

% states
-export([expect_toloka_search/2]).

%%% Macros
-include_lib("kernel/include/logger.hrl").

% -define(TOLOKA, toloka).
-define(TOLOKA, toloka_mock).

%%% Client API

%% @doc Start-link searcher
-spec start_link(Description :: binary()) -> gen_statem:start_ret().
start_link(Description) ->
    gen_statem:start_link(?MODULE, Description, _Opts=[]).

%% @doc Stop searcher
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%%% Behavior callbacks

callback_mode() ->
    state_functions.

init(Description) ->
    ?LOG_INFO("Init, ~p~n", [Description]),
    TolokaSearchId = ?TOLOKA:search(Description),
    % FIXME set up a timer for query toloka
    {ok, expect_toloka_search, TolokaSearchId}.

terminate(_Reason, _State, _Data) ->
    ok.

%%% States

expect_toloka_search(query_toloka, TolokaSearchId) ->
    case ?TOLOKA:is_search_ready(TolokaSearchId) of
        true ->
            TolokaCheckId = ?TOLOKA:check(TolokaSearchId),
            {next_state, expect_toloka_check, TolokaCheckId};
        false ->
            % FIXME set up a timer for query toloka
            {next_state, expect_toloka_search, TolokaSearchId}
    end.
