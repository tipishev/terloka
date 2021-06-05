-module(search_worker).

-behaviour(gen_server).

-export([start/1, stop/1, pause/1, status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(SECONDS, 1000).
-define(MINUTES, 60 * ?SECONDS).

%%% Types

% copied from Search Sheet in README.MD
-record(state, {
    % Constants
    description :: binary(),
    quotes_required = 5 :: non_neg_integer(),

    % Variables
    searches_left = 3 :: non_neg_integer(),
    current_task = create_toloka_search :: current_task(),
    toloka_search_id = undefined :: undefined | binary(),
    toloka_check_id = undefined :: undefined | binary(),
    next_wakeup :: calendar:date_time(),

    % Result
    quotes = [] :: [quote()]
}).

-record(quote, {
    url :: binary(),
    % TODO non_neg_integer kopeyks
    price :: number(),
    screenshot :: binary()
}).

-type current_task() ::
    % intermediate states
    create_toloka_search
    | expect_toloka_search
    | create_toloka_check
    | expect_toloka_check
    % final states
    | success
    | fail.
-type quote() :: #quote{}.

%%% API

% TODO use Name, City, etc.
% TODO add budget cap
% TODO add ReportTo?
start(Description) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Description, []),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, stop).

pause(Pid) ->
    gen_server:call(Pid, pause).

% also acts as resume the pause
status(Pid) ->
    gen_server:call(Pid, status).

%%% OTP Callbacks

init(Description) ->
    log("I started looking for ~ts.", [Description]),
    SleepTime = 3 * ?SECONDS,
    NextWakeup = future(SleepTime),
    log("Sleeping until ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    State = #state{description = Description, next_wakeup = NextWakeup},
    {ok, State, _SleepTime = SleepTime}.

handle_call(stop, _From, State) ->
    log("I am stopping."),
    {stop, normal, ok, State};
handle_call(status, _From, State = #state{next_wakeup = NextWakeup}) ->
    log("Oh.. boss wants to know it all."),
    SleepTime = max(milliseconds_until(NextWakeup), 3000),
    log("Sleeping until ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    {reply, state_to_map(State), State, SleepTime};
handle_call(pause, _From, State) ->
    log("Ok, I will chill."),
    % note the absence of Timeout
    {reply, ok, State};
handle_call(_Mst, _From, State) ->
    log("Hm.. unknown call, I am ignoring it."),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Main State Logic                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create Search
handle_info(
    timeout,
    State = #state{
        current_task = create_toloka_search,
        searches_left = SearchesLeft,
        description = Description
    }
) when SearchesLeft > 0 ->
    log("Creating a search..."),
    TolokaSearchId = toloka:search(Description),
    log("Ok, created (id: ~p)", [TolokaSearchId]),
    SleepTime = 1 * ?MINUTES,
    NextWakeup = future(SleepTime),
    log("Will check it at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    NewState = State#state{
        current_task = expect_toloka_search,
        searches_left = SearchesLeft - 1,
        toloka_search_id = TolokaSearchId,
        next_wakeup = NextWakeup
    },
    {noreply, NewState, _SleepTime = SleepTime};
%% Expect Search
handle_info(
    timeout,
    State = #state{
        current_task = expect_toloka_search,
        toloka_search_id = TolokaSearchId
    }
) ->
    log("Ok, time to check if the search is ready..."),
    IsSearchReady = toloka:is_search_ready(TolokaSearchId),
    case IsSearchReady of
        false ->
            log("Search is not ready yet..."),
            % TODO exponential backoff with 1.5 coefficient and max of 30min.
            SleepTime = 1 * ?MINUTES,
            NextWakeup = future(SleepTime),
            log("I will check it again at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
            NewState = State#state{current_task = expect_toloka_search, next_wakeup = NextWakeup},
            {noreply, NewState, _SleepTime = SleepTime};
        true ->
            log("Hooray, it's ready!"),
            SleepTime = 3 * ?SECONDS,
            NextWakeup = future(SleepTime),
            log("I will create a check at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
            NewState = State#state{
                current_task = create_toloka_check,
                next_wakeup = NextWakeup
            },
            {noreply, NewState, _SleepTime = SleepTime}
    end;
%% Create Check
handle_info(
    timeout,
    State = #state{
        current_task = create_toloka_check,
        toloka_search_id = TolokaSearchId
    }
) ->
    log("Creating a check..."),
    TolokaCheckId = toloka:check(TolokaSearchId),
    log("Ok, created (id: ~p)", [TolokaCheckId]),
    SleepTime = 1 * ?MINUTES,
    NextWakeup = future(SleepTime),
    log("I will check this search at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    NewState = State#state{
        current_task = expect_check,
        toloka_check_id = TolokaCheckId,
        % don't need it, so reset.
        toloka_search_id = undefined,
        next_wakeup = NextWakeup
    },
    {noreply, NewState, _SleepTime = SleepTime};
%% Default
handle_info(timeout, State) ->
    log("I don't know what to do :("),
    {noreply, State, _SleepTime = 10 * ?SECONDS}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%% Helpers

% TODO use it to make snapshots
% TODO make an invese to load from snapshots
state_to_map(#state{
    description = Description,
    quotes_required = QuotesRequired,
    searches_left = SearchesLeft,
    current_task = CurrentTask,
    toloka_search_id = TolokaSearchId,
    toloka_check_id = TolokaCheckId,
    next_wakeup = NextWakeup,
    quotes = Quotes
}) ->
    #{
        description => Description,
        quotes_required => QuotesRequired,
        searches_left => SearchesLeft,
        current_task => CurrentTask,
        toloka_search_id => TolokaSearchId,
        toloka_check_id => TolokaCheckId,
        next_wakeup => NextWakeup,
        quotes => Quotes
    }.

% TODO map_to_state()

%%% Time stuff

milliseconds_until(Future) ->
    Now = calendar:local_time(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
    FutureSeconds = calendar:datetime_to_gregorian_seconds(Future),
    SecondsUntil = FutureSeconds - NowSeconds,
    ?SECONDS * SecondsUntil.

future(MilliSecondsFromNow) ->
    SecondsFromNow = MilliSecondsFromNow div ?SECONDS,
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(
            calendar:local_time()
        ) + SecondsFromNow
    ).

% Logging
% TODO use real logging
log(String) -> log(String, []).
log(String, Args) -> io:format("~p: " ++ String ++ "~n", [self() | Args]).
