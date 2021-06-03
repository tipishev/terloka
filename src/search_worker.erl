-module(search_worker).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, code_change/3, terminate/2]).
%%% API

% TODO use Name, City, etc.
% TODO add budget cap
start_link(Description, ReportTo) ->
    gen_server:start_link(?MODULE, {Description, ReportTo}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%% OTP Callbacks

init({Description, ReportTo}) ->
    {ok, {Description, ReportTo}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Mst, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

