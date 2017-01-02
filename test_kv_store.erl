-module(test_kv_store).
-behaviour(proper_statem).

-export([test/0, test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	next_state/3]).

-include_lib("proper/include/proper.hrl").

-type key() :: string().
-type value() :: string().

-define(KEYS, ['a','b','c','d']).
-define(VALUES, ['-1', '0', '1', '2', '3']).
key() -> union(?KEYS).
value() -> union(?VALUES).

%% A simple statem test for a RESTful dict service; tests the
%% operations kv_store:put/2, kv_store:get/1, kv_store:erase/1.

test() ->
	test(100).

test(N) ->
	proper:quickcheck(?MODULE:prop_pdict(), N).

prop_kv_store() ->
	inets:start(),
	?FORALL(Cmds, commands(?MODULE),
		begin
		{H,S,Res} = run_commands(?MODULE, Cmds),
		clean_up(),
		?WHENFAIL(
			io:format("History: ~w\nState: ~w\nRes: ~w\n",
				[H, S, Res]),
			aggregate(command_names(Cmds), Res =:= ok))
		end).

clean_up() ->
	kv_store:reset(),
	lists:foreach(fun(Key) -> kv_store:erase(Key) end, ?KEYS).

initial_state() -> [].

-spec command([{key(),value()}]) -> proper_types:type().
command([]) ->
	{call,kv_store,put,[key(), value()]};
command(State) ->
	union([{call,kv_store,put,[key(),value()]},
		{call,kv_store,get,[key(State)]},
		{call,kv_store,erase,[key(State)]},
		{call,kv_store,reset,[]}]).

key(State) ->
	?LET({Key,_}, elements(State), Key).

precondition(_, _) ->
	true.

%% Set the right preconditions:
%% precondition(State, {call,kv_store,get,[Key]}) ->
%%	proplists:is_defined(Key, State);
%% precondition(State, {call,kv_store,erase,[Key]}) ->
%%	proplists:is_defined(Key, State);
%% precondition(_, {call,kv_store,put,[_K, _V]}) ->
%%	true.

postcondition(State, {call,kv_store,put,[Key,_]}, '') ->
	not proplists:is_defined(Key, State);
postcondition(State, {call,kv_store,put,[Key,_]}, Old) ->
	{Key,Old} =:= proplists:lookup(Key, State);
postcondition(State, {call,kv_store,get,[Key]}, Val) ->
	{Key,Val} =:= proplists:lookup(Key, State);
postcondition(State, {call,kv_store,erase,[Key]}, Val) ->
	{Key,Val} =:= proplists:lookup(Key, State);
postcondition(_State, {call,kv_store,reset,[]}, '') ->
	true.

next_state(State, _Var, {call,kv_store,put,[Key,Value]}) ->
	[{Key,Value}|proplists:delete(Key, State)];
next_state(State, _Var, {call,kv_store,erase,[Key]}) ->
	proplists:delete(Key, State);
next_state(State, _Var, {call,kv_store,get,[_]}) ->
	State;
next_state(_State, _Var, {call,kv_store,reset,[]}) ->
	[].
