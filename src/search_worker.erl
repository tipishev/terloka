-module(search_worker).

-behaviour(gen_server).

-export([start/1, stop/1, status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(SECONDS, 1000).

%%% Types

% copied from Search Sheet in README.MD
-record(state, {
    % Constants
    description :: binary(),
    minimal_quotes = 5 :: non_neg_integer(),

    % Variables
    searches_left = 3 :: non_neg_integer(),
    current_task = create_search :: current_task(),
    search_id = undefined :: undefined | binary(),
    check_id = undefined :: undefined | binary(),

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
    {ok, Pid} = gen_server:start_link(?MODULE, #state{description = Description}, []),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, stop).

status(Pid) ->
    gen_server:call(Pid, status).

%%% OTP Callbacks

init(State = #state{description = Description}) ->
    log("I started looking for ~ts.", [Description]),
    {ok, State, _Sleep = 3 * ?SECONDS}.

handle_call(stop, _From, State) ->
    log("I am stopping."),
    {stop, normal, ok, State};
handle_call(status, _From, State) ->
    log("Oh.. boss wants to know it all."),
    {reply, report(State), State, 3 * ?SECONDS};
handle_call(_Mst, _From, State) ->
    log("Hm.. unknown call, ignore."),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout,
            State = #state{current_task = create_search,
                           searches_left = SearchesLeft,
                           description = Description}) when SearchesLeft > 0 ->
    log("I woke up to create a search pool"),
    SearchId = toloka:search(Description),
    NewState = State#state{current_task=expect_search,
                           searches_left=SearchesLeft - 1,
                           search_id=SearchId},
    {noreply, NewState, _Sleep = 2 * ?SECONDS};
handle_info(timeout, State) ->
        log("I woke up and don't know what to do :("),
    {noreply, State, _Sleep = 2 * ?SECONDS}.
% don't handle info messages other than timeout

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%% Helpers

report(#state{
    description = Description,
    minimal_quotes = MinimalQuotes,
    searches_left = SearchesLeft,
    current_task = CurrentTask,
    search_id = SearchId,
    check_id = CheckId,
    quotes = Quotes
}) ->
    #{
        description => Description,
        minimal_quotes => MinimalQuotes,
        searches_left => SearchesLeft,
        current_task => CurrentTask,
        search_id => SearchId,
        check_id => CheckId,
        quotes => Quotes
    }.

% Logging
% TODO real logging
log(String) -> log(String, []).
log(String, Args) -> io:format("~p: " ++ String ++ "~n", [self() | Args]).
