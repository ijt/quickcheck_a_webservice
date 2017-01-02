%% Erlang wrapper for Go-based kv_store service, enabling PropEr testing.

-module(kv_store).

-export([start/0, stop/0]).
-export([put/2, get/1, erase/1, reset/0]).
-export([url/2, http_get/1]).

start() -> inets:start().
stop() -> inets:stop().

url(Cmd, Args) ->
	"http://localhost:1234/" ++ Cmd ++ "/" ++ string:join(Args, "/").

http_get(Url) ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	      httpc:request(get, {Url, []}, [], []),
	Body.

put(K, V) ->
	http_get(url("put", [K, V])).

get(K) ->
	http_get(url("get", [K])).

erase(K) ->
	http_get(url("erase", [K])).

reset() ->
	http_get(url("reset", [])).

