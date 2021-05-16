-module(yadisk).

-export([
    upload/2,
    is_uploaded/1
]).

-include("http_status_codes.hrl").

-define(YANDEX_DISK_TOKEN, <<"AQAAAAAZfxSeAADLW71qa7wwGE_6ruZTmmVBRKU">>).
-define(YANDEX_DISK_BASE_DIR, "/Приложения/Яндекс.Толока/screenshots/").
-define(YANDEX_DISK_BASE_URI, <<"https://cloud-api.yandex.net/v1/disk">>).


upload(FromUrl, FileName) ->
    Path = <<?YANDEX_DISK_BASE_DIR/utf8, FileName/bytes>>,
    YadiskUrl = hackney_url:make_url(
        <<"">>, <<"/resources/upload">>, [ {<<"path">>, Path}, {<<"url">>, FromUrl} ]),
    {?HTTP_STATUS_ACCEPTED, Body} = post(YadiskUrl),
    Url = maps:get(<<"href">>, Body),
    _OperationId = lists:last(binary:split(Url, <<"/">>, [global])).

is_uploaded(OperationId) ->
    operation_status(OperationId) =:= <<"success">>.

%%% Private Functions

operation_status(OperationId) ->
    {?HTTP_STATUS_OK, Body} = get_(<<"/operations/", OperationId/binary>>),
    maps:get(<<"status">>, Body).

get_(Path) ->
    Url = urlize(Path),
    Headers = headers(),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:get(Url, Headers),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

post(Path) ->
    Url = urlize(Path),
    Headers = headers(),
    {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:post(Url, Headers),
    {ok, Body} = hackney:body(BodyRef),
    {StatusCode, jsx:decode(Body, [return_maps])}.

urlize(Path) ->
    <<?YANDEX_DISK_BASE_URI/binary, Path/binary>>.

headers() ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"OAuth ", ?YANDEX_DISK_TOKEN/binary>>}
    ].
