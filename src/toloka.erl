-module(toloka).

-export([get_pools/0]).

get_pools() -> restc:request("https://api.github.com").


