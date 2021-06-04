-module(search_worker).

-behaviour(gen_server).

-export([start/1, stop/1, status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(SECONDS, 1000).

% copied from Search Sheet in README.MD
-record(state, {
    % Constants
    description,
    search_pool_id = <<"23077202">>,
    check_pool_id = <<"24538798">>,
    minimal_quotes = 5,
    search_budget = 3,

    % Variables
    search_expenses = 0,
    current_task = create_search_pool,
    search_task_id = undefined,
    check_task_suite_id = undefined,

    % Result
    good_quotes = []
}).


%%% API

% TODO use Name, City, etc.
% TODO add budget cap
% TODO add ReportTo?
start(Description) ->
    {ok, Pid} = gen_server:start_link(?MODULE, #state{description=Description}, []),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, stop).

status(Pid) ->
    gen_server:call(Pid, status).

%%% OTP Callbacks

init(State=#state{description=Description}) ->
    log("I started looking for ~s.", [Description]),
    {ok, State, _Sleep = 3 * ?SECONDS}.

handle_call(stop, _From, State) ->
    log("I am stopping."),
    {stop, normal, ok, State};
handle_call(status, _From, State) ->
    log("Oh, an email from boss."),
    {reply, report(State), State, 3 * ?SECONDS};
handle_call(_Mst, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    log("I woke up."),
    {noreply, State, _Sleep = 2 * ?SECONDS}.
% don't handle info messages other than timeout

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%% Helpers
report(#state{current_task = CurrentTask, good_quotes = GoodQuotes, search_expenses = SearchExpenses}) ->
    lists:flatten(io_lib:format("My task is ~p. I have found ~p quotes. spent ~p searches.", [
        CurrentTask,
        length(GoodQuotes),
        SearchExpenses
    ])).

% Logging
% TODO real logging
log(String) -> log(String, []).
log(String, Args) -> io:format("~p: " ++ String ++ "~n", [self()| Args]).
