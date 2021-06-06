-module(toloka).

-compile(export_all).
% -export([
%     search/1, is_search_ready/1,

%     % TODO combine is_check_ready and get_quotes into get_quotes -> not_ready | quotes?
%     check/1, is_check_ready/1, get_quotes/1

%     % TODO accept_search/1
%     % TODO extract_good_quotes_from_check/1 <- uses passed in price,url context
% ]).

-include("http_status_codes.hrl").

-define(TOLOKA_BASE_URI, <<"https://toloka.yandex.ru/api/v1">>).
-define(TOLOKA_TOKEN, <<"AQAAAAAZfxSeAACtpcBhALKZ7k7YjgKe9rNIu5s">>).
-define(SEARCH_POOL_ID, <<"23077202">>).
-define(CHECK_POOL_ID, <<"24748405">>).

% TODO create multiple tasks in one request
search(Description) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/tasks">>, [
        #{
            input_values => #{description => Description},
            pool_id => ?SEARCH_POOL_ID,
            overlap => 1
        }
    ]),
    #{<<"items">> := #{<<"0">> := #{<<"id">> := TaskId}}} = Body,
    open_pool(?SEARCH_POOL_ID),
    TaskId.

is_search_ready(SearchTaskId) ->
    {?HTTP_STATUS_OK, Body} = get_(
        <<"/assignments?task_id=", SearchTaskId/binary,
            % "&status=ACCEPTED">>),
            "&status=SUBMITTED">>
    ),
    SubmittedAssignments = fake_unpaginate(Body),
    case SubmittedAssignments of
        [_ | _] -> true;
        [] -> false
    end.

check(SearchTaskId) ->
    % FIXME adapt to map
    SearchResult = get_search_result(SearchTaskId),
    % FIXME inject user_id and assignment_id
    CheckInput = prepare_check_input(SearchResult),
    create_check_task_suite(CheckInput).

is_check_ready(CheckTaskSuiteId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_suite_id=", CheckTaskSuiteId/binary, "&status=ACCEPTED">>),
    Answers = fake_unpaginate(Body),
    case Answers of
        [_ | _] -> true;
        [] -> false
    end.

open_pool(PoolId) ->
    % {ignored, PoolId}.
    {?HTTP_STATUS_ACCEPTED, _Body} = post(<<"/pools/", PoolId/binary, "/open">>).

%%% Private Functions

create_check_task_suite(CheckInput) ->
    {?HTTP_STATUS_CREATED, Body} = post(<<"/task-suites">>, #{
        <<"pool_id">> => ?CHECK_POOL_ID,

        % TODO check if necessary given pool overlap settings
        <<"overlap">> => 3,

        <<"tasks">> => [
            #{
                <<"input_values">> => #{
                    <<"description">> => unicode:characters_to_binary(Description),
                    <<"screenshot">> => Screenshot,

                    % keep for context
                    <<"url">> => Url,
                    <<"price">> => Price,
                    <<"assignment_id">> => AssignmentId,
                    <<"user_id">> => UserId
                }
            }
            || #{
                   description := Description,
                   screenshot := Screenshot,
                   url := Url,
                   price := Price,
                   assignment_id := AssignmentId,
                   user_id := UserId
               } <- CheckInput
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
                <<"id">> := AssignmentId,
                <<"user_id">> := UserId,
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
                            <<"description">> := Description
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
    % Filename0 = AttachmentId0,
    % Filename1 = AttachmentId1,
    % Filename2 = (AttachmentId2),

    #{
        description => Description,
        quotes => [
            #{
                url => Url0,
                price => Price0,
                filename => Filename0
            },
            #{
                url => Url1,
                price => Price1,
                filename => Filename1
            },
            #{
                url => Url2,
                price => Price2,
                filename => Filename2
            }
        ],
        assignment_id => AssignmentId,
        user_id => UserId
    }.

get_quotes(CheckTaskSuiteId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/assignments?task_suite_id=", CheckTaskSuiteId/binary, "&status=ACCEPTED">>),
    io:format("~p~n", [Body]),
    [
        #{
            <<"user_id">> := _CheckUserId1,
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
                        <<"screenshot">> := Screenshot1,
                        % <<"user_id">> := SearchUserId,
                        <<"assignment_id">> := SearchAssignmentId
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
            <<"user_id">> := _CheckUserId2,
            <<"solutions">> := [
                #{<<"output_values">> := #{<<"result">> := Answer2_1}},
                #{<<"output_values">> := #{<<"result">> := Answer2_2}},
                #{<<"output_values">> := #{<<"result">> := Answer2_3}}
            ]
        },
        #{
            <<"user_id">> := _CheckUserId3,
            <<"solutions">> := [
                #{<<"output_values">> := #{<<"result">> := Answer3_1}},
                #{<<"output_values">> := #{<<"result">> := Answer3_2}},
                #{<<"output_values">> := #{<<"result">> := Answer3_3}}
            ]
        }
    ] = fake_unpaginate(Body),

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

    GoodQuotes = [Quote || {Quote, Vote} <- lists:zip(Quotes, Votes), Vote =:= true],

    case (length(GoodQuotes) > 2) of
        true -> accept_assignment(SearchAssignmentId);
        false -> reject_assignment(SearchAssignmentId)
    end,

    GoodQuotes.

% TODO just use https://yandex.ru/support/toloka-requester/concepts/mvote.html
vote(<<"yes">>, <<"yes">>, _) -> true;
vote(<<"yes">>, _, <<"yes">>) -> true;
vote(_, <<"yes">>, <<"yes">>) -> true;
vote(_, _, _) -> false.

prepare_check_input(SearchResult) ->
    #{
        description := SearchDescription,
        quotes := Quotes,

        assignment_id := AssignmentId,
        user_id := UserId
    } = SearchResult,
    [
        #{
            % payload
            description => make_check_description(SearchDescription, Url, Price),
            screenshot => <<"screenshots/", Filename/binary>>,

            % context
            url => Url,
            price => Price,
            assignment_id => AssignmentId,
            user_id => UserId
        }
        || #{url := Url, price := Price, filename := Filename} <- Quotes
    ].

%%% Assignment Checking

accept_assignment(AssignmentId) ->
    patch(<<"/assignments/", AssignmentId/binary>>, #{<<"status">> => <<"ACCEPTED">>}).
reject_assignment(AssignmentId) ->
    patch(<<"/assignments/", AssignmentId/binary>>, #{<<"status">> => <<"REJECTED">>}).

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
    {?HTTP_STATUS_OK, Body} = get_(<<"/attachments/", AttachmentId/binary>>),
    #{<<"name">> := FilenameFromUser} = Body,
    FileExtension = lists:last(binary:split(FilenameFromUser, <<".">>, [global])),
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

post(Path) -> post(Path, <<>>).
post(Path, Json) ->
    Url = urlize(Path),
    Headers = headers(),
    Payload = jsx:encode(Json),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:post(Url, Headers, Payload),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

patch(Path, Json) ->
    Url = urlize(Path),
    Headers = headers(),
    Payload = jsx:encode(Json),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:patch(Url, Headers, Payload),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

headers() ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"OAuth ", ?TOLOKA_TOKEN/binary>>}
    ].

% Logging

% TODO add real logging
log(String) -> log(String, []).
log(String, Args) -> io:format("~p: " ++ String ++ "~n", [self() | Args]).

% Pagination
fake_unpaginate(#{<<"has_more">> := false, <<"items">> := Items}) -> Items.
