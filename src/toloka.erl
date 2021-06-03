-module(toloka).

-export([
    search/1,
    check/1,
    open_pool/1,
    % TODO is_ready
    get_quotes/1,
    create_check_task_suite/1
]).

-include("http_status_codes.hrl").

-define(TOLOKA_BASE_URI, <<"https://toloka.yandex.ru/api/v1">>).
-define(TOLOKA_TOKEN, <<"AQAAAAAZfxSeAACtpcBhALKZ7k7YjgKe9rNIu5s">>).
-define(SEARCH_POOL_ID, <<"23077202">>).
-define(CHECK_POOL_ID, <<"24538798">>).

% TODO create multiple tasks in one request
search(Description) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/tasks">>, [#{
            input_values => #{description => Description},
            pool_id => ?SEARCH_POOL_ID,
            overlap => 1
        }]),
    #{<<"items">> := #{<<"0">> := #{<<"id">> := TaskId}}} = Body,
    TaskId.

check(SearchTaskId) ->
    {SearchDescription, Quotes} = get_quotes(SearchTaskId),
    _DescriptionScreenshots = prepare_check_input(SearchDescription, Quotes).
    % create_check_task_suite(DescriptionScreenshots).

open_pool(PoolId) ->
    {?HTTP_STATUS_ACCEPTED, Body} = post(<<"/pools/", PoolId/binary, "/open">>),
    Body.

create_check_task_suite(DescriptionScreenshots) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/task-suites">>, #{
        <<"pool_id">> => ?CHECK_POOL_ID,
        <<"overlap">> => 3,
        <<"tasks">> => [
            #{
                <<"input_values">> => #{
                    <<"description">> => unicode:characters_to_binary(Description),
                    <<"screenshot">> => Screenshot
                }
            }
            || {Description, Screenshot} <- DescriptionScreenshots
        ]
    }),

    % TODO extract only relevant IDs from the Body
    Body.




%%% Private Functions

% FIXME rename to include Description
% TODO count the failed attempts by counting statuses other than ACCEPTED
get_quotes(SearchTaskId) ->
    % FIXME SUBMITTED vs ACCEPTED
    % {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_id=", TaskId/binary, "&status=SUBMITTED">>),
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_id=", SearchTaskId/binary, "&status=ACCEPTED">>),
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
                ],
                <<"tasks">> := [
                    #{
                        <<"input_values">> := #{
                            <<"description">> := SearchDescription
                        }
                    }
                ]
            }
        ]
    } = Body,

    % TODO move out
    {Filename0, _OperationId0} = copy_to_yadisk(AttachmentId0),
    {Filename1, _OperationId1} = copy_to_yadisk(AttachmentId1),
    {Filename2, _OperationId2} = copy_to_yadisk(AttachmentId2),

    {SearchDescription, [
        {Url0, Price0, Filename0},
        {Url1, Price1, Filename1},
        {Url2, Price2, Filename2}
    ]}.

%%% Descriptions

prepare_check_input(SearchDescription, Quotes) ->
    [
        {make_check_description(SearchDescription, Url, Price),
         <<"screenshots/", Screenshot/binary>>}
        || {Url, Price, Screenshot} <- Quotes
    ].

% TODO disarm user input
% TODO custom description template
make_check_description(SearchDescription, Url, Price) ->
    lists:flatten(
        io_lib:format(
            "Являются ли цена ~.2f р., <a href='~ts'>ссылка</a>, и скриншот ответом на задание~n<i>~ts</i>?",
            [Price, Url, SearchDescription]
        )
    ).

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

% TODO real logging
log(String, Args) -> io:format(String, Args).
