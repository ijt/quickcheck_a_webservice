-module(restdict).

-export([put/2, get/1, erase/1]).

put(K, V) ->
	erlang:put(K, V).

get(K) ->
	erlang:get(K).

erase(K) ->
	erlang:erase(K).

