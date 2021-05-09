%%% Manage a collection of ongoing searches.
-module(search_server).


% -behaviour(gen_server).

% API
-export([shutdown/1]).

% gen_server callbacks

%% Synchronous Call
shutdown(Pid) ->
    gen_server:call(Pid, terminate).
