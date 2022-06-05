-module(terloka_db).

-export([install/0]).
-export([set_order/2, get_order/1]).

-record(terloka_orders, {
    % external input
    position_id,
    description,

    % output
    result,

    % timestamps
    created_at,
    updated_at
}).

%% @doc initializes terloka_orders mnesia table
-spec install() -> ok.
install() ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    mnesia:create_table(
        terloka_orders,
        [
            {attributes, record_info(fields, terloka_orders)},
            {disc_copies, [Nodes]},
            {type, ordered_set}
        ]
    ),
    application:stop(mnesia).

%% @doc sets order
-spec set_order(PositionId, Description) -> ok when
      PositionId :: pos_integer(),
      Description :: binary().
set_order(PositionId, Description) ->
    F = fun() ->
        Now = calendar:local_time(),
        % TODO return {error, order_exists}
        mnesia:write(#terloka_orders{
            position_id = PositionId,
            description = Description,
            created_at = Now,
            updated_at = Now
        })
    end,
    mnesia:activity(transaction, F).

%% @doc gets order
-spec get_order(PositionId) -> {ok, Order} | {error, notfound} when
    PositionId :: pos_integer(),
    Order :: tuple().
get_order(PositionId) ->
    F = fun() ->
                case mnesia:read({terloka_orders, PositionId}) of
                    [#terloka_orders{description=Description,
                                     result=Result}] ->
                        {ok, {PositionId, Description, Result}};
                    [] -> {error, notfound}
                end
        end,
    mnesia:activity(transaction, F).
