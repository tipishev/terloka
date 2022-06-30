%%% @doc this gen_server is a middleman between the API and searchers. It can receive 2 types
%%% of messages:
%%% - order creation from API
%%% - result from searchers

-module(dispatcher).

-behaviour(gen_server).

%% Module interface
-export([

    start_link/0,
    stop/0,

    create_order/2,
    order_ready/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-include_lib("kernel/include/logger.hrl").

%%% Types

-type order_id() :: pos_integer().

%% dispatcher's state
-record(state, {
    searchers = #{} :: #{pid() => order_id()}
}).

%%% Module Interface

%% @doc starts a link with a dispatcher
start_link() ->
    ?LOG_INFO("Starting Dispatcher"),
    gen_server:start_link(
        _ServerName = {local, ?MODULE},
        _Module = ?MODULE,
        _Options = [],
        _DebuggingOptions = []
    ).

%% @doc stops the dispatcher
stop() ->
    ?LOG_INFO("Stopping Dispatcher"),
    gen_server:call({local, ?MODULE}, stop).

%% @doc tells the dispatcher to create an order
create_order(OrderId, Description) ->
    ?LOG_INFO("Creating order ~p: ~p", [OrderId, Description]),
    % TODO include self() For a notify?
    gen_server:call(?MODULE, {create_order, OrderId, Description}).

%% @doc tells the dispatcher that an order is complete
order_ready(OrderId, Result) ->
    ?LOG_INFO("Completing order ~p: ~p", [OrderId, Result]),
    % FIXME should include call to self()
    gen_server:call(?MODULE, {order_ready, OrderId, Result}).

%%% GenServer callbacks

%% @doc initializes a dispatcher with an empty state
init([]) ->
    {ok, _State=#state{}}.

%% @doc handles a `create_order` or `order_ready` call.
handle_call({create_order, OrderId, Description}, _From, State) ->
    ?LOG_INFO("Handling create_order call ~p: ~p", [OrderId, Description]),
    ok = terloka_db:create_order(OrderId, Description),
    ?LOG_INFO("Created order ~p: ~p in the database", [OrderId, Description]),
    % TODO start the searcher and save its PID to the state
    Reply = order_created,
    {reply, Reply, _NewState=State};
% TODO check that the message is coming from correct worker
handle_call({order_ready, OrderId, Result}, _From, State) ->
    ?LOG_INFO("Handling order_ready call ~p: ~p", [OrderId, Result]),
    Reply = order_result_acknowledged,
    {reply, Reply, _NewState=State}.

handle_cast(Request, State) ->
    ?LOG_INFO("Ignoring cast ~p", [Request]),
    {noreply, _NewState=State}.
