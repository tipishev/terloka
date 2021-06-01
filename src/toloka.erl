-module(toloka).

-export([
    create_search_task/1,
    create_check_task/2,
    open_pool/1,
    get_quotes/1
]).

-include("http_status_codes.hrl").

-define(TOLOKA_BASE_URI, <<"https://toloka.yandex.ru/api/v1">>).
-define(TOLOKA_TOKEN, <<"AQAAAAAZfxSeAACtpcBhALKZ7k7YjgKe9rNIu5s">>).
-define(SEARCH_POOL_ID, <<"23077202">>).
-define(CHECK_POOL_ID, <<"24538798">>).

% TODO create multiple tasks in one request
create_search_task(Description) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/tasks">>, [#{
            input_values => #{description => Description},
            pool_id => ?SEARCH_POOL_ID,
            overlap => 1
        }]),
    #{<<"items">> := #{<<"0">> := #{<<"id">> := TaskId}}} = Body,
    TaskId.

open_pool(PoolId) ->
    {?HTTP_STATUS_ACCEPTED, Body} = post(<<"/pools/", PoolId/binary, "/open">>),
    Body.

create_check_task(Description, Screenshot) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/tasks">>, [
        #{
            input_values => #{description => Description, screenshot => Screenshot},
            pool_id => ?CHECK_POOL_ID,
            overlap => 3
        }
    ]),
    maps:get(<<"id">>, maps:get(<<"0">>, maps:get(<<"items">>, Body))).


% TODO count the failed attempts by counting statuses other than ACCEPTED
get_quotes(TaskId) ->
    % {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_id=", TaskId/binary, "&status=SUBMITTED">>),
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_id=", TaskId/binary, "&status=ACCEPTED">>),
    #{
        <<"items">> := [
            #{
                <<"solutions">> := [
                    #{
                        <<"output_values">> := #{
                            <<"URL-0">> := Url0,
                            <<"URL-1">> := Url1,
                            <<"URL-2">> := Url2,

                            <<"price-0">> := Price0,
                            <<"price-1">> := Price1,
                            <<"price-2">> := Price2,

                            <<"screenshot-0">> := AttachmentId0,
                            <<"screenshot-1">> := AttachmentId1,
                            <<"screenshot-2">> := AttachmentId2
                        }
                    }
                ]
            }
        ]
    } = Body,
    {Filename0, _OperationId0} = copy_to_yadisk(AttachmentId0),
    {Filename1, _OperationId1} = copy_to_yadisk(AttachmentId1),
    {Filename2, _OperationId2} = copy_to_yadisk(AttachmentId2),
    [
        {Url0, Price0, Filename0},
        {Url1, Price1, Filename1},
        {Url2, Price2, Filename2}
    ].

%%% Private Functions

%%% Attachments

download(AttachmentId) ->
    AttachmentDownloadPath = <<"/attachments/", AttachmentId/binary, "/download">>,
    {?HTTP_STATUS_OK, Bytes} = get_(binary, AttachmentDownloadPath),
    Bytes.

generate_filename(AttachmentId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/attachments/",  AttachmentId/binary>>),
    #{<<"name">> := FilenameFromUser} = Body,
    FileExtension =  lists:last(binary:split(FilenameFromUser,  <<".">>, [global])),
    Filename = <<AttachmentId/binary, ".", FileExtension/binary>>,
    Filename.

copy_to_yadisk(AttachmentId) ->
    log("Uploading ~p...~n", [AttachmentId]),
    FileName = generate_filename(AttachmentId),
    Bytes = download(AttachmentId),
    OperationId = yadisk:upload_bytes(Bytes, FileName),
    {FileName, OperationId}.

%%% HTTP wrappers

urlize(Path) -> <<?TOLOKA_BASE_URI/binary, Path/binary>>.

post(Path) -> post(Path, <<>>).
post(Path, Json) ->
    Url = urlize(Path),
    Headers = headers(),
    Payload = jsx:encode(Json),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:post(Url, Headers, Payload),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

get_(Path) -> get_(json, Path).

get_(ResponseType, Path) ->
    Url = urlize(Path),
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

% Logging

log(String, Args) ->
    io:format(String, Args).
