%%% @doc this module represents the logic of a worker as if he were a human who
%%% writes its state on a piece of paper -- the state record.

-module(search_worker).

-behaviour(gen_server).

% module interface
-export([start/1, load/1, stop/1, pause/1, resume/1, status/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("kernel/include/logger.hrl").

% TODO remove
-define(SECONDS, 1000).

%%% Types

% copied from Search Sheet in README.MD
-record(state, {
    % Constants
    description :: binary(),
    quotes_required = 5 :: non_neg_integer(),

    % Variables
    searches_left = 3 :: non_neg_integer(),
    current_task :: current_task(),
    toloka_search_id = undefined :: undefined | binary(),
    toloka_check_id = undefined :: undefined | binary(),
    % TODO last_wait_time, max_wait_time

    % Result, they are all good.
    quotes = [] :: [quote()],

    % Transient variables
    timer_ref = undefined :: undefined | timer:tref()
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
    | extract_quotes
    % final states
    | success
    | fail.
-type quote() :: #quote{}.

%%% API

% TODO use Name, City, etc.
% TODO add budget cap
% TODO add ReportTo?

%% @doc creates a new worker searching Description.
start(Description) ->
    State = #state{description = Description, current_task = create_toloka_search},
    {ok, Pid} = gen_server:start_link(?MODULE, State, []),
    Pid.

%% @doc loads a worker from a StateMap.
load(StateMap) ->
    State = map_to_state(StateMap),
    {ok, Pid} = gen_server:start_link(?MODULE, State, []),
    Pid.

%% @doc stops a worker with Pid.
stop(Pid) ->
    gen_server:call(Pid, stop).

%% @doc pauses a worker with Pid.
pause(Pid) ->
    gen_server:call(Pid, pause).

%% @doc resumes a worker with Pid.
resume(Pid) ->
    gen_server:call(Pid, resume).

%% @doc requests the worker with Pid to report its status.
status(Pid) ->
    gen_server:call(Pid, status).

%%% OTP Callbacks

init(State = #state{description = Description, quotes_required = QuotesRequired}) ->
   ?LOG_INFO("My goal is to find ~p quotes for \"~ts\".", [QuotesRequired, Description]),
    SleepTime = timer:seconds(3),
    NextWakeup = future(SleepTime),
    ?LOG_INFO("Sleeping until ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    {ok, TimerRef} = timer:send_after(SleepTime, act),
    NewState = State#state{timer_ref = TimerRef},
    {ok, NewState}.

handle_call(stop, _From, State) ->
    ?LOG_INFO("I am stopping."),
    {stop, normal, ok, State};
handle_call(status, _From, State) ->
    ?LOG_INFO("Got request for status"),
    {reply, state_to_map(State), State};
handle_call(pause, _From, State = #state{timer_ref = TimerRef}) ->
    ?LOG_INFO("Ok, I take a break."),
    {ok, cancel} = timer:cancel(TimerRef),
    NewState = State#state{timer_ref = undefined},
    {reply, ok, NewState};
handle_call(resume, _From, State = #state{timer_ref = undefined}) ->
    ?LOG_INFO("Ok, I resume."),
    SleepTime = timer:seconds(1),
    NextWakeup = future(SleepTime),
    ?LOG_INFO("Sleeping until ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    {ok, TimerRef} = timer:send_after(SleepTime, act),
    NewState = State#state{timer_ref = TimerRef},
    {reply, ok, NewState};

handle_call(_Mst, _From, State) ->
    ?LOG_INFO("Hm.. unknown call, I am ignoring it."),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Main State Logic                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create Search
handle_info(
    act,
    State = #state{
        current_task = create_toloka_search,
        searches_left = SearchesLeft,
        description = Description
    }
) when SearchesLeft > 0 ->
    ?LOG_INFO("Creating a search..."),
    TolokaSearchId = toloka:search(Description),
    ?LOG_INFO("Ok, created a search (id: ~p)", [TolokaSearchId]),
    SleepTime = timer:minutes(1),
    NextWakeup = future(SleepTime),
    ?LOG_INFO("Will check it at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    {ok, TimerRef} = timer:send_after(SleepTime, act),
    NewState = State#state{
        current_task = expect_toloka_search,
        searches_left = SearchesLeft - 1,
        toloka_search_id = TolokaSearchId,
        timer_ref = TimerRef
    },
    {noreply, NewState};

%% Expect Search
handle_info(
    act,
    State = #state{
        current_task = expect_toloka_search,
        toloka_search_id = TolokaSearchId
    }
) ->
    ?LOG_INFO("Ok, time to check if the search is ready..."),
    IsSearchReady = toloka:is_search_ready(TolokaSearchId),
    case IsSearchReady of
        false ->
            ?LOG_INFO("Search is not ready yet..."),
            % TODO exponential backoff with 1.5 coefficient and max of 30min.
            SleepTime = timer:minutes(1),
            NextWakeup = future(SleepTime),
            ?LOG_INFO("I will check it again at ~p (~p seconds).", [NextWakeup, SleepTime div ?SECONDS]),
            {ok, TimerRef} = timer:send_after(SleepTime, act),
            NewState = State#state{current_task = expect_toloka_search, timer_ref = TimerRef},
            {noreply, NewState};
        true ->
            ?LOG_INFO("Hooray, it's ready!"),
            SleepTime = timer:seconds(3),
            NextWakeup = future(SleepTime),
            ?LOG_INFO("I will create a check at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
            {ok, TimerRef} = timer:send_after(SleepTime, act),
            NewState = State#state{
                current_task = create_toloka_check,
                timer_ref = TimerRef
            },
            {noreply, NewState}
    end;

%% Create Check
handle_info(
    act,
    State = #state{
        current_task = create_toloka_check,
        toloka_search_id = TolokaSearchId
    }
) ->
    ?LOG_INFO("Creating a check..."),
    TolokaCheckId = toloka:check(TolokaSearchId),
    ?LOG_INFO("Ok, created a check (id: ~p)", [TolokaCheckId]),
    SleepTime = timer:minutes(1),
    NextWakeup = future(SleepTime),
    ?LOG_INFO("I will check this search at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    {ok, TimerRef} = timer:send_after(SleepTime, act),
    NewState = State#state{
        current_task = expect_toloka_check,
        toloka_check_id = TolokaCheckId,
        % don't need it anymore, so reset.
        toloka_search_id = undefined,
        timer_ref = TimerRef
    },
    {noreply, NewState};

%% Expect Check
handle_info(
    act,
    State = #state{
        current_task = expect_toloka_check,
        toloka_check_id = TolokaCheckId
    }
) ->
    ?LOG_INFO("Ok, time to check if the check is checked..."),
    IsCheckReady = toloka:is_check_ready(TolokaCheckId),
    case IsCheckReady of
        false ->
            ?LOG_INFO("Check is not ready yet..."),
            % TODO exponential backoff with 1.5 coefficient and max of 30min.
            SleepTime = timer:minutes(1),
            NextWakeup = future(SleepTime),
            ?LOG_INFO("I will check the check again at ~p (~p seconds).",
                [NextWakeup, SleepTime div ?SECONDS]),
            {ok, TimerRef} = timer:send_after(SleepTime, act),
            NewState = State#state{current_task = expect_toloka_check, timer_ref = TimerRef},
            {noreply, NewState};
        true ->
            ?LOG_INFO("Hooray, the check is ready!"),
            SleepTime = timer:seconds(3),
            NextWakeup = future(SleepTime),
            ?LOG_INFO("I will extract check results at ~p. (~p seconds)",
                [NextWakeup, SleepTime div ?SECONDS]),
            {ok, TimerRef} = timer:send_after(SleepTime, act),
            NewState = State#state{
                current_task = extract_quotes,
                timer_ref = TimerRef
            },
            {noreply, NewState}
    end;

%% Extract Quotes

%% Catch-all
handle_info(act, State) ->
    SleepTime = timer:seconds(30),
    ?LOG_INFO("I don't know what to do :( I will sleep another ~p seconds.", [SleepTime div ?SECONDS]),
    {ok, TimerRef} = timer:send_after(SleepTime, act),
    NewState = State#state{timer_ref = TimerRef},
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%% Helpers

state_to_map(#state{
    description = Description,
    quotes_required = QuotesRequired,
    searches_left = SearchesLeft,
    current_task = CurrentTask,
    toloka_search_id = TolokaSearchId,
    toloka_check_id = TolokaCheckId,
    timer_ref = TimerRef,
    quotes = Quotes
}) ->
    #{
        description => Description,
        quotes_required => QuotesRequired,
        searches_left => SearchesLeft,
        current_task => CurrentTask,
        toloka_search_id => TolokaSearchId,
        toloka_check_id => TolokaCheckId,
        timerRef => TimerRef,
        quotes => Quotes
    }.

map_to_state(#{
    description := Description,
    quotes_required := QuotesRequired,
    searches_left := SearchesLeft,
    current_task := CurrentTask,
    toloka_search_id := TolokaSearchId,
    toloka_check_id := TolokaCheckId,
    quotes := Quotes
}) ->
    #state{
        description = Description,
        quotes_required = QuotesRequired,
        searches_left = SearchesLeft,
        current_task = CurrentTask,
        toloka_search_id = TolokaSearchId,
        toloka_check_id = TolokaCheckId,
        quotes = Quotes
    }.

%%% Time stuff

% TODO replace with generic
future(MilliSecondsFromNow) ->
    SecondsFromNow = MilliSecondsFromNow div ?SECONDS,
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(
            calendar:local_time()
        ) + SecondsFromNow
    ).
