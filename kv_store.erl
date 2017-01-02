%% Erlang wrapper for Go-based kv_store service, enabling PropEr testing.

-module(kv_store).

-export([start/0, stop/0]).
-export([put/2, get/1, erase/1, reset/0]).
-export([url/2, http_get/1]).

start() -> inets:start().
stop() -> inets:stop().

-spec url(atom(), [atom()]) -> string().
url(Cmd, Args) ->
	Args2 = [atom_to_list(A) || A <- Args],
	"http://localhost:1234/" ++ Cmd ++ "/" ++ string:join(Args2, "/").

-spec http_get(string()) -> atom().
http_get(Url) ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	      httpc:request(get, {Url, []}, [], []),
	list_to_atom(Body).

-spec put(atom(), atom()) -> atom().
put(K, V) ->
	http_get(url("put", [K, V])).

-spec get(atom()) -> atom().
get(K) ->
	http_get(url("get", [K])).

-spec erase(atom()) -> atom().
erase(K) ->
	http_get(url("erase", [K])).

-spec reset() -> atom().
reset() ->
	http_get(url("reset", [])).

