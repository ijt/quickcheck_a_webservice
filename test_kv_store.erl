-module(test_kv_store).
-behaviour(proper_statem).

-export([test/0, test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	next_state/3]).
-export([main/1]).

-include_lib("proper/include/proper.hrl").

-type key() :: string().
-type value() :: string().

-define(KEYS, ["a","b","c","d"]).
-define(VALUES, ["-1", "0", "1", "2", "3"]).
key() -> union(?KEYS).
value() -> union(?VALUES).

%% A simple statem test for a RESTful dict service; tests the
%% operations restdict:put/2, restdict:get/1, restdict:erase/1.

test() ->
	test(100).

test(N) ->
	proper:quickcheck(?MODULE:prop_pdict(), N).

prop_kv_store() ->
	?FORALL(Cmds, commands(?MODULE),
		begin
		set_up(),
		{H,S,Res} = run_commands(?MODULE, Cmds),
		clean_up(),
		?WHENFAIL(
			io:format("History: ~w\nState: ~w\nRes: ~w\n",
				[H, S, Res]),
			aggregate(command_names(Cmds), Res =:= ok))
		end).

set_up() ->
	restdict:start().

clean_up() ->
	%% TODO(ijt): Figure out why this call to restdict:stop() causes the test to fail.
	%% restdict:stop(),
	lists:foreach(fun(Key) -> restdict:erase(Key) end, ?KEYS).

initial_state() -> [].

-spec command([{key(),value()}]) -> proper_types:type().
command([]) ->
	{call,restdict,put,[key(), value()]};
command(State) ->
	union([{call,restdict,put,[key(),value()]},
		{call,restdict,get,[key(State)]},
		{call,restdict,erase,[key(State)]}]).

key(State) ->
	?LET({Key,_}, elements(State), Key).

precondition(_, _) ->
	true.

%% Set the right preconditions:
%% precondition(State, {call,restdict,get,[Key]}) ->
%%	proplists:is_defined(Key, State);
%% precondition(State, {call,restdict,erase,[Key]}) ->
%%	proplists:is_defined(Key, State);
%% precondition(_, {call,restdict,put,[_K, _V]}) ->
%%	true.

postcondition(State, {call,restdict,put,[Key,_]}, "") ->
	not proplists:is_defined(Key, State);
postcondition(State, {call,restdict,put,[Key,_]}, Old) ->
	{Key,Old} =:= proplists:lookup(Key, State);
postcondition(State, {call,restdict,get,[Key]}, Val) ->
	{Key,Val} =:= proplists:lookup(Key, State);
postcondition(State, {call,restdict,erase,[Key]}, Val) ->
	{Key,Val} =:= proplists:lookup(Key, State).

next_state(State, _Var, {call,restdict,put,[Key,Value]}) ->
	[{Key,Value}|proplists:delete(Key, State)];
next_state(State, _Var, {call,restdict,erase,[Key]}) ->
	proplists:delete(Key, State);
next_state(State, _Var, {call,restdict,get,[_]}) ->
	State.

main(_) -> proper:module(?MODULE).
