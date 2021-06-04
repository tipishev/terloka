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
    current_task = create_search :: current_task(),
    search_id = undefined :: undefined | binary(),
    check_id = undefined :: undefined | binary(),

    % Technical
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
    create_search
    | expect_search.
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

status(Pid) ->
    gen_server:call(Pid, status).

%%% OTP Callbacks

init(Description) ->
    log("I started looking for ~ts.", [Description]),
    SleepTime = 3 * ?SECONDS,
    NextWakeup = future(SleepTime),
    log("Sleeping until ~p. (~p seconds)", [NextWakeup, SleepTime div 1000]),
    State = #state{description = Description, next_wakeup=NextWakeup},
    {ok, State, _SleepTime = SleepTime}.

handle_call(stop, _From, State) ->
    log("I am stopping."),
    {stop, normal, ok, State};
handle_call(status, _From, State = #state{next_wakeup=NextWakeup}) ->
    log("Oh.. boss wants to know it all."),
    SleepTime = max(1000 * seconds_until(NextWakeup), 3000),
    log("Sleeping until ~p. (~p seconds)", [NextWakeup, SleepTime div 1000]),
    {reply, state_to_dict(State), State, SleepTime};
handle_call(pause, _From, State) ->
    log("Ok, I will chill."),
    % note the absence of Timeout
    {reply, ok, State};
handle_call(_Mst, _From, State) ->
    log("Hm.. unknown call, ignoring."),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Main State Logic                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create Search
handle_info(timeout,
            State = #state{current_task = create_search,
                           searches_left = SearchesLeft,
                           description = Description}) when SearchesLeft > 0 ->
    log("Creating a search pool.."),
    SearchId = toloka:search(Description),
    log("Ok, created (id: ~p)", [SearchId]),
    SleepTime = 1 * ?MINUTES,
    NextWakeup = future(SleepTime),
    log("Will check it at ~p. (~p seconds)", [NextWakeup, SleepTime div 1000]),
    NewState = State#state{current_task=expect_search,
                           searches_left=SearchesLeft - 1,
                           search_id=SearchId,
                           next_wakeup=NextWakeup},
    {noreply, NewState, _SleepTime = SleepTime};

%% Expect Search
handle_info(timeout, State = #state{current_task = expect_search, search_id = SearchId}) ->
    log("Ok, time to check if search is ready..."),
    IsSearchReady = toloka:is_search_ready(SearchId),
    case IsSearchReady of
        true ->
            log("Hooray, it's ready"),
            SleepTime = 3 * ?SECONDS,
            NextWakeup = future(SleepTime),
            log("I'll check it at ~p. (~p seconds)", [NextWakeup, SleepTime div 1000]),
            NewState = State#state{current_task=create_check,
                                   search_id=undefined,
                                   next_wakeup=NextWakeup},
            {noreply, NewState, _SleepTime = SleepTime};
        false ->
            log("Search is not ready yet..."),
            SleepTime = 1 * ?MINUTES,
            NextWakeup = future(SleepTime),
            log("Will check it again at ~p. (~p seconds)", [NextWakeup, SleepTime div 1000]),
            NewState = State#state{next_wakeup=NextWakeup},
            {noreply, NewState, _SleepTime = SleepTime}
    end;

handle_info(timeout, State) ->
        log("I don't know what to do :("),
    {noreply, State, _SleepTime = 10 * ?SECONDS}.
% don't handle info messages other than timeout

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%% Helpers

state_to_dict(#state{
    description = Description,
    quotes_required = QuotesRequired,
    searches_left = SearchesLeft,
    current_task = CurrentTask,
    search_id = SearchId,
    check_id = CheckId,
    next_wakeup = NextWakeup,
    quotes = Quotes
}) ->
    #{
        description => Description,
        quotes_required => QuotesRequired,
        searches_left => SearchesLeft,
        current_task => CurrentTask,
        search_id => SearchId,
        check_id => CheckId,
        next_wakeup => NextWakeup,
        quotes => Quotes
    }.

%%% Time stuff

seconds_until(Future) ->
    Now = calendar:local_time(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
    FutureSeconds = calendar:datetime_to_gregorian_seconds(Future),
    FutureSeconds - NowSeconds.

future(MilliSecondsFromNow) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(
        calendar:local_time()) + MilliSecondsFromNow div 1000).

% Logging
% TODO use real logging
log(String) -> log(String, []).
log(String, Args) -> io:format("~p: " ++ String ++ "~n", [self() | Args]).
