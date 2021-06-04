-module(search_worker).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, code_change/3, terminate/2]).

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
start_link(Description, ReportTo) ->
    gen_server:start_link(?MODULE, #state{description=Description}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%% OTP Callbacks

init({Description, ReportTo}) ->
    log("I started looking for ~s.", [Description]),
    {ok, {Description, ReportTo}, _Sleep = 3 * ?SECONDS}.

handle_call(stop, _From, State) ->
    log("I am stopping."),
    {stop, normal, ok, State};
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

% Logging
% TODO real logging
log(String) -> log(String, []).
log(String, Args) -> io:format("~p: " ++ String ++ "~n", [self()| Args]).
