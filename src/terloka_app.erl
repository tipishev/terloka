%%%-------------------------------------------------------------------
%% @doc terloka public API
%% @end
%%%-------------------------------------------------------------------

-module(terloka_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    terloka_sup:start_link().

stop(_State) ->
    ok.
