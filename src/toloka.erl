-module(toloka).

-export([
    create_task/2,
    get_quotes/1,
    copy_attachment_to_yadisk/1,
    open_pool/1
]).

-include("http_status_codes.hrl").

-define(TOLOKA_BASE_URI, <<"https://toloka.yandex.ru/api/v1">>).
-define(TOLOKA_TOKEN, <<"AQAAAAAZfxSeAACtpcBhALKZ7k7YjgKe9rNIu5s">>).

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
    [AcceptedAssignment] = maps:get(<<"items">>, Body),
    [Solution] = maps:get(<<"solutions">>, AcceptedAssignment),
    OutputValues = maps:get(<<"output_values">>, Solution),
    [
     {maps:get(<<"URL-0">>, OutputValues), maps:get(<<"price-0">>, OutputValues),
      maps:get(<<"screenshot-0">>, OutputValues)},
     {maps:get(<<"URL-1">>, OutputValues), maps:get(<<"price-1">>, OutputValues),
      maps:get(<<"screenshot-1">>, OutputValues)},
     {maps:get(<<"URL-2">>, OutputValues), maps:get(<<"price-2">>, OutputValues),
      maps:get(<<"screenshot-2">>, OutputValues)}
    ].

open_pool(PoolId) ->
    {?HTTP_STATUS_ACCEPTED, Body} = post(<<"/pools/", PoolId/binary, "/open">>),
    Body.

copy_attachment_to_yadisk(AttachmentId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/attachments/",  AttachmentId/binary>>),
    FileNameFromUser = maps:get(<<"name">>, Body),
    FileExtension =  lists:last(binary:split(FileNameFromUser,  <<".">>, [global])),
    FileName = <<AttachmentId/binary, ".", FileExtension/binary>>,

    Bytes = download_attachment(AttachmentId),
    yadisk:upload_bytes(Bytes, FileName).

%%% Private Functions

download_attachment(AttachmentId) ->
    AttachmentDownloadPath = <<"/attachments/", AttachmentId/binary, "/download">>,
    {?HTTP_STATUS_OK, Bytes} = get_(binary, AttachmentDownloadPath),
    Bytes.

post(Path) -> post(Path, <<>>).
post(Path, Json) ->
    Url = <<?TOLOKA_BASE_URI/binary, Path/binary>>,
    Headers = headers(),
    Payload = jsx:encode(Json),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:post(Url, Headers, Payload),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

get_(Path) -> get_(json, Path).

get_(ResponseType, Path) ->
    Url = <<?TOLOKA_BASE_URI/binary, Path/binary>>,
    Headers = headers(),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:get(Url, Headers),
    {ok, Body} = hackney:body(BodyRef),
    DecodedBody =
        case ResponseType of
            json -> jsx:decode(Body, [return_maps]);
            binary -> Body
        end,
    {StatusCode, DecodedBody}.

headers() ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"OAuth ", ?TOLOKA_TOKEN/binary>>}
    ].

urlize(Path) ->
    <<?TOLOKA_BASE_URI/binary, Path/binary>>.
