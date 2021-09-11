%%% @doc this module represents the logic of a worker as if he were a human who
%%% writes its state on a piece of paper -- the state record.

-module(search_worker).

-behaviour(gen_server).

% module interface
-export([start/1, save/1, load/1, stop/1, pause/1, resume/1, status/1]).

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
    timer_ref = undefined :: undefined | reference()
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
    | evaluate_result
    % | TODO rename evaluate_result to check_completion
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
    {ok, Pid} = gen_server:start(?MODULE, State, []),
    Pid.

%% @doc writes current state to {Pid}.json in the current directory.
save(Pid) ->
    Filename = io_lib:format("~p.json", [Pid]),
    gen_server:call(Pid, {save, Filename}).

%% @doc loads a worker from a StateMap.
load(Filename) ->
    ?LOG_INFO("Loading worker's state from ~p~n", [Filename]),
    [StateMap] = jsx:consult(Filename, [return_maps, {labels, existing_atom}]),
    State = map_to_state(StateMap),
    {ok, Pid} = gen_server:start(?MODULE, State, []),
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
    TimerRef = erlang:send_after(SleepTime, self(), act),
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
    Time = erlang:cancel_timer(TimerRef),
    ?LOG_INFO("Canceled the timer ~p seconds before expiration.", [Time div ?SECONDS]),
    NewState = State#state{timer_ref = undefined},
    {reply, ok, NewState};
handle_call(resume, _From, State = #state{timer_ref = undefined}) ->
    ?LOG_INFO("Ok, I resume."),
    SleepTime = timer:seconds(1),
    NextWakeup = future(SleepTime),
    ?LOG_INFO("Sleeping until ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
    TimerRef = erlang:send_after(SleepTime, self(), act),
    NewState = State#state{timer_ref = TimerRef},
    {reply, ok, NewState};
handle_call({save, Filename}, _From, State) ->
    ?LOG_INFO("Saving my state to ~p~n", [Filename]),
    save_state(State, Filename),
    {reply, ok, State};
handle_call(_Mst, _From, State) ->
    ?LOG_INFO("Hm.. unknown call, I am ignoring it."),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(act, State) ->

    Filename = io_lib:format("~p.json", [Pid]),
    ?LOG_INFO("Backing up my state to ~p~n", [Filename]),
    save_state(State, Filename),

    NewState = act(State),
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Main State Logic                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create Search
act(
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
    TimerRef = erlang:send_after(SleepTime, self(), act),
    State#state{
        current_task = expect_toloka_search,
        searches_left = SearchesLeft - 1,
        toloka_search_id = TolokaSearchId,
        timer_ref = TimerRef
    };
%% Expect Search
act(
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
            AssignmentsInfo = toloka:assignments_info(TolokaSearchId),
            ?LOG_INFO("Search status: ~p~n", [AssignmentsInfo]),
            % TODO exponential backoff with 1.5 coefficient and max of 30min.
            SleepTime = timer:minutes(1),
            NextWakeup = future(SleepTime),
            ?LOG_INFO("I will check it again at ~p (~p seconds).", [NextWakeup, SleepTime div ?SECONDS]),
            TimerRef = erlang:send_after(SleepTime, self(), act),
            State#state{current_task = expect_toloka_search, timer_ref = TimerRef};
        true ->
            ?LOG_INFO("Hooray, it's ready!"),
            SleepTime = timer:seconds(3),
            NextWakeup = future(SleepTime),
            ?LOG_INFO("I will create a check at ~p. (~p seconds)", [NextWakeup, SleepTime div ?SECONDS]),
            TimerRef = erlang:send_after(SleepTime, self(), act),
            State#state{
                current_task = create_toloka_check,
                timer_ref = TimerRef
            }
    end;
%% Create Check
act(
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
    TimerRef = erlang:send_after(SleepTime, self(), act),
    State#state{
        current_task = expect_toloka_check,
        toloka_check_id = TolokaCheckId,
        % don't need it anymore, so reset.
        toloka_search_id = undefined,
        timer_ref = TimerRef
    };
%% Expect Check
act(
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
            AssignmentsInfo = toloka:assignments_info(TolokaCheckId),
            ?LOG_INFO("Check status: ~p~n", [AssignmentsInfo]),
            % TODO exponential backoff with 1.5 coefficient and max of 30min.
            SleepTime = timer:minutes(1),
            NextWakeup = future(SleepTime),
            ?LOG_INFO(
                "I will check the check again at ~p (~p seconds).",
                [NextWakeup, SleepTime div ?SECONDS]
            ),
            TimerRef = erlang:send_after(SleepTime, self(), act),
            State#state{current_task = expect_toloka_check, timer_ref = TimerRef};
        true ->
            ?LOG_INFO("Hooray, the check is ready!"),
            SleepTime = timer:seconds(10),
            NextWakeup = future(SleepTime),
            ?LOG_INFO(
                "I will extract check results at ~p. (~p seconds)",
                [NextWakeup, SleepTime div ?SECONDS]
            ),
            TimerRef = erlang:send_after(SleepTime, self(), act),
            State#state{
                current_task = extract_quotes,
                timer_ref = TimerRef
            }
    end;
%% Extract Quotes
act(
    State = #state{
        current_task = extract_quotes,
        toloka_check_id = TolokaCheckId,
        quotes = OldQuotes
    }
) ->
    ?LOG_INFO("I am extracting quotes"),
    NewQuotes = toloka:get_quotes(TolokaCheckId),
    Quotes = lists:append(OldQuotes, NewQuotes),
    ?LOG_INFO("I added ~p to my quotes, and now they are ~p.", [NewQuotes, Quotes]),
    SleepTime = timer:seconds(3),
    ?LOG_INFO("I will evaluate my results in ~p seconds.", [SleepTime div ?SECONDS]),
    TimerRef = erlang:send_after(SleepTime, self(), act),
    State#state{
        quotes = Quotes,
        % we used it, so it's not needed anymore
        toloka_check_id = undefined,
        current_task = evaluate_result,
        timer_ref = TimerRef
    };
%% Evaluate Result
act(
    State = #state{
        current_task = evaluate_result,
        quotes = Quotes,
        quotes_required = QuotesRequired,
        searches_left = SearchesLeft
    }
) ->
    ?LOG_INFO("I am evaluating result and deciding if I should go again."),
    QuotesLength = length(Quotes),
    NextTask =
        case QuotesLength >= QuotesRequired of
            true ->
                ?LOG_INFO("Hooray, enough quotes! I call it a success."),
                Filename = io_lib:format("success_~p.json", [self()]),
                ?LOG_INFO("Writing result to ~p.", [Filename]),
                save_state(State, Filename),
                success;
            false ->
                ?LOG_INFO("Not enough quotes :("),
                case SearchesLeft > 0 of
                    true ->
                        ?LOG_INFO("Let's do another search!"),
                        create_toloka_search;
                    false ->
                        ?LOG_INFO("Too bad, I cannot do another search. I failed."),
                        fail
                end
        end,

    SleepTime = timer:seconds(3),
    ?LOG_INFO("I will sleep another ~p seconds.", [SleepTime div ?SECONDS]),
    TimerRef = erlang:send_after(SleepTime, self(), act),
    State#state{
        current_task = NextTask,
        timer_ref = TimerRef
    };
%% Catch-all
act(State) ->
    SleepTime = timer:seconds(30),
    ?LOG_INFO("I don't know what to do :( I will sleep another ~p seconds.", [SleepTime div ?SECONDS]),
    TimerRef = erlang:send_after(SleepTime, self(), act),
    State#state{timer_ref = TimerRef}.

%%% Helpers

state_to_map(#state{
    description = Description,
    quotes_required = QuotesRequired,
    searches_left = SearchesLeft,
    current_task = CurrentTask,
    toloka_search_id = TolokaSearchId,
    toloka_check_id = TolokaCheckId,
    % TODO encode as seconds before it goes off?
    timer_ref = _TimerRef,
    quotes = Quotes
}) ->
    #{
        description => Description,
        quotes_required => QuotesRequired,
        searches_left => SearchesLeft,
        current_task => CurrentTask,
        toloka_search_id => TolokaSearchId,
        toloka_check_id => TolokaCheckId,
        quotes => [tuple_to_list(Quote) || Quote <- Quotes]
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
        current_task = binary_to_existing_atom(CurrentTask),
        toloka_search_id = undefined_or_binary(TolokaSearchId),
        toloka_check_id = undefined_or_binary(TolokaCheckId),
        quotes = [list_to_tuple(Quote) || Quote <- Quotes]
    }.

undefined_or_binary(<<"undefined">>) -> undefined;
undefined_or_binary(Binary) when is_binary(Binary) -> Binary.

save_state(State, Filename) ->
    Status = state_to_map(State),
    JsonStatus = jsx:encode(Status, [{indent, 4}, space]),
    file:write_file(Filename, JsonStatus),
    ok.

%%% Time stuff

% TODO replace with generic
future(MilliSecondsFromNow) ->
    SecondsFromNow = MilliSecondsFromNow div ?SECONDS,
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(
            calendar:local_time()
        ) + SecondsFromNow
    ).
