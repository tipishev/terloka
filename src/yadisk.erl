-module(yadisk).

-export([
    upload/2,
    is_ready/1
]).

-include("http_status_codes.hrl").

-define(YANDEX_DISK_TOKEN, <<"AQAAAAAZfxSeAADLW71qa7wwGE_6ruZTmmVBRKU">>).
% -define(YANDEX_DISK_BASE_DIR, "/Приложения/Яндекс.Толока/screenshots/"/utf8>>).
-define(YANDEX_DISK_BASE_URI, <<"https://cloud-api.yandex.net/v1/disk">>).


upload(FromUrl, FileName) ->
    Path = <<"/Приложения/Яндекс.Толока/screenshots/"/utf8, FileName/bytes>>,
    YadiskUrl = hackney_url:make_url(
        <<"">>, <<"/resources/upload">>, [ {<<"path">>, Path}, {<<"url">>, FromUrl} ]),
    {?HTTP_STATUS_ACCEPTED, Body} = post(YadiskUrl),
    maps:get(<<"href">>, Body).

is_ready(Url) ->
    {ok, ?HTTP_STATUS_OK, _Headers, BodyRef} = hackney:get(Url, headers()),
    {ok, Body} = hackney:body(BodyRef),
    maps:get(<<"status">>, jsx:decode(Body, [return_maps])).

%%% Private Functions

get_(Path) ->
    Url = urlize(Path),
    io:format(Url),
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
% post(Path, Json) ->
%     Url = urlize(Path),
%     Headers = headers(),
%     Payload = jsx:encode(Json),
%     {ok, StatusCode, _ResponseHeaders, BodyRef} = hackney:post(Url, Headers, Payload),
%     {ok, Body} = hackney:body(BodyRef),
%     {StatusCode, jsx:decode(Body, [return_maps])}.

urlize(Path) ->
    <<?YANDEX_DISK_BASE_URI/binary, Path/binary>>.

headers() ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"OAuth ", ?YANDEX_DISK_TOKEN/binary>>}
    ].
