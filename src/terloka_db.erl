-module(terloka_db).

-export([install/0]).
-export([add_order/2, order_by_position_id/1]).

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

add_order(PositionId, Description) ->
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

order_by_position_id(PositionId) ->
    F = fun() ->
                case mnesia:read({terloka_orders, PositionId}) of
                    [#terloka_orders{description=Description,
                                     result=Result}] ->
                        {ok, {PositionId, Description, Result}};
                    [] -> {error, notfound}
                end
        end,
    mnesia:activity(transaction, F).
