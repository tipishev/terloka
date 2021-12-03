%%% @doc this module simulates toloka

-module(toloka_simulator).

-export([
    search/1,
    is_search_ready/1,
    check/1,
    is_check_ready/1,
    get_quotes/1
]).

search(_Description) ->
    SearchId = 123,
    {ok, SearchId}.

is_search_ready(_SearchId) ->
    true.

check(_SearchId) ->
    CheckId = 456,
    {ok, CheckId}.

is_check_ready(_CheckId) ->
    true.

get_quotes(_SearchId) ->
    [
        {100, <<"ya.ru">>, <<"screenshot.jpg">>},
        {200, <<"google.com">>, <<"another_screenshot.jpg">>},
        {300, <<"rambler.ru">>, <<"yet_another_screenshot.png">>}
    ].
