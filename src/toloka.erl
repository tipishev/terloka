-module(toloka).

-export([
    search/1,
    is_search_ready/1,
    check/1,
    % TODO combine create check and get results?
    is_check_ready/1,
    % TODO accept_search/1
    % TODO extract_good_quotes_from_check/1 <- uses passed in price,url context
    get_check_result/1
]).

-include("http_status_codes.hrl").

-define(TOLOKA_BASE_URI, <<"https://toloka.yandex.ru/api/v1">>).
-define(TOLOKA_TOKEN, <<"AQAAAAAZfxSeAACtpcBhALKZ7k7YjgKe9rNIu5s">>).
-define(SEARCH_POOL_ID, <<"23077202">>).
-define(CHECK_POOL_ID, <<"24743282">>).

% TODO create multiple tasks in one request
search(Description) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/tasks">>, [#{
            input_values => #{description => Description},
            pool_id => ?SEARCH_POOL_ID,
            overlap => 1
        }]),
    #{<<"items">> := #{<<"0">> := #{<<"id">> := TaskId}}} = Body,
    open_pool(?SEARCH_POOL_ID),
    TaskId.

is_search_ready(SearchTaskId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_id=", SearchTaskId/binary,
                                     % "&status=ACCEPTED">>),
                                     "&status=SUBMITTED">>),
    % implement pagination when this breaks
    #{<<"has_more">> := false,
      <<"items">> := SubmittedAssignments} = Body,
    case SubmittedAssignments of
        [_|_] -> true;
        [] -> false
    end.

is_check_ready(CheckTaskSuiteId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_suite_id=", CheckTaskSuiteId/binary,
                                     "&status=ACCEPTED">>),
    % implement pagination when this breaks
    #{<<"has_more">> := false,
      <<"items">> := Answers} = Body,
    case Answers of
        [_|_] -> true;
        [] -> false
    end.

check(SearchTaskId) ->
    {SearchDescription, Quotes} = get_search_result(SearchTaskId),
    CheckInput = prepare_check_input(SearchDescription, Quotes),
    create_check_task_suite(CheckInput).

open_pool(PoolId) ->
    {?HTTP_STATUS_ACCEPTED, _Body} = post(<<"/pools/", PoolId/binary, "/open">>).

%%% Private Functions

create_check_task_suite(CheckInput) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/task-suites">>, #{
        <<"pool_id">> => ?CHECK_POOL_ID,
        <<"overlap">> => 3,
        <<"tasks">> => [
            #{
                <<"input_values">> => #{
                    <<"description">> => unicode:characters_to_binary(Description),
                    <<"screenshot">> => Screenshot,

                    % keep for context
                    <<"url">> => Url,
                    <<"price">> => Price
                }
            }
            || #{description := Description,
                 screenshot :=  Screenshot,
                 url := Url,
                 price := Price} <- CheckInput
        ]
    }),

    open_pool(?CHECK_POOL_ID),

    #{<<"id">> := TaskSuiteId} = Body,
    TaskSuiteId.


% TODO count the failed attempts by counting statuses other than ACCEPTED
get_search_result(SearchTaskId) ->
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

get_check_result(CheckTaskSuiteId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_suite_id=", CheckTaskSuiteId/binary, "&status=ACCEPTED">>),
    #{
        % when this breaks, we need pagination
        <<"has_more">> := false,
        <<"items">> := [
            #{
                <<"user_id">> := _UserId1,
                <<"solutions">> := [
                    #{<<"output_values">> := #{<<"result">> := Answer1_1}},
                    #{<<"output_values">> := #{<<"result">> := Answer1_2}},
                    #{<<"output_values">> := #{<<"result">> := Answer1_3}}
                ],
                <<"tasks">> := [
                        #{
                            <<"input_values">> := #{
                                <<"url">> := Url1,
                                <<"price">> := Price1,
                                <<"screenshot">> := Screenshot1
                            }
                        },
                        #{
                            <<"input_values">> := #{
                                <<"url">> := Url2,
                                <<"price">> := Price2,
                                <<"screenshot">> := Screenshot2
                            }
                        },
                        #{
                            <<"input_values">> := #{
                                <<"url">> := Url3,
                                <<"price">> := Price3,
                                <<"screenshot">> := Screenshot3
                            }
                        }
                ]
            },

            #{
                <<"user_id">> := _UserId2,
                <<"solutions">> := [
                    #{<<"output_values">> := #{<<"result">> := Answer2_1}},
                    #{<<"output_values">> := #{<<"result">> := Answer2_2}},
                    #{<<"output_values">> := #{<<"result">> := Answer2_3}}
                ]
            },
            #{
                <<"user_id">> := _UserId3,
                <<"solutions">> := [
                    #{<<"output_values">> := #{<<"result">> := Answer3_1}},
                    #{<<"output_values">> := #{<<"result">> := Answer3_2}},
                    #{<<"output_values">> := #{<<"result">> := Answer3_3}}
                ]
            }
        ]
    } = Body,

    Votes = [
        vote(Answer1_1, Answer2_1, Answer3_1),
        vote(Answer1_2, Answer2_2, Answer3_2),
        vote(Answer1_3, Answer2_3, Answer3_3)
    ],

    Quotes = [
     {Price1, Url1, Screenshot1},
     {Price2, Url2, Screenshot2},
     {Price3, Url3, Screenshot3}
    ],

    [Quote || {Quote, Vote} <- lists:zip(Quotes, Votes), Vote =:= true].


% TODO just use https://yandex.ru/support/toloka-requester/concepts/mvote.html
vote(<<"yes">>, <<"yes">>, _) -> true;
vote(<<"yes">>, _, <<"yes">>) -> true;
vote(_, <<"yes">>, <<"yes">>) -> true;
vote(_, _, _) -> false.

prepare_check_input(SearchDescription, Quotes) ->
    [
        #{
            description => make_check_description(SearchDescription, Url, Price),
            screenshot => <<"screenshots/", Screenshot/binary>>,
            url => Url,
            price => Price
        }
        || {Url, Price, Screenshot} <- Quotes
    ].

%%% Descriptions

% TODO disarm user input
% TODO custom description template
make_check_description(SearchDescription, Url, Price) ->
    lists:flatten(
        io_lib:format(
            "Являются ли цена ~.2f р., <a href='~ts'>ссылка</a>, и скриншот ответом на задание~n~n<i>\"~ts\"</i>?",
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
log(String) -> log(String, []).
log(String, Args) -> io:format("~p: " ++ String ++ "~n", [self() | Args]).
