-module(toloka).

-export([
    create_task/2,
    get_quotes/1,
    copy_attachment_to_yadisk/1,
    open_pool/1
]).

-include("http_status_codes.hrl").

-define(TOLOKA_BASE_URI, <<"https://toloka.yandex.ru/api/v1">>).
-define(TOLOKA_TOKEN, <<"AQAAAAAZfxSeAACtpfveFFq4aEwOlvt1tH1YCeI">>).

% TODO create multiple tasks in one request
create_task(PoolId, Description) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/tasks">>, [#{
            input_values => #{description => Description},
            pool_id => PoolId,
            overlap => 1
        }]),
    maps:get(<<"id">>, maps:get(<<"0">>, maps:get(<<"items">>, Body))).

% TODO count the failed attempts by dropping status=ACCEPTED
get_quotes(TaskId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_id=", TaskId/binary, "&status=ACCEPTED">>),
    Body.

open_pool(PoolId) ->
    {?HTTP_STATUS_ACCEPTED, Body} = post(<<"/pools/", PoolId/binary, "/open">>),
    Body.

% fun fact: meta info is available at /attachments/{attachment_id}
copy_attachment_to_yadisk(AttachmentId) ->
    TolokaUrl = urlize(<<"/attachments/", AttachmentId/binary, "/download">>).

%%% Private Functions

post(Path) -> post(Path, <<>>).
post(Path, Json) ->
    Url = <<?TOLOKA_BASE_URI/binary, Path/binary>>,
    Headers = headers(),
    Payload = jsx:encode(Json),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:post(Url, Headers, Payload),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

get_(Path) ->
    Url = <<?TOLOKA_BASE_URI/binary, Path/binary>>,
    Headers = headers(),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:get(Url, Headers),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

headers() ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"OAuth ", ?TOLOKA_TOKEN/binary>>}
    ].

urlize(Path) ->
    <<?TOLOKA_BASE_URI/binary, Path/binary>>.
