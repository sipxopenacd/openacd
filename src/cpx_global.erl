-module(cpx_global).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	new/0,

	get/1,
	get/2,

	set/2,
	append/2
]).

-define(TBL, cpx_global).

new() ->
	try ets:new(?TBL, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]) of
		_ -> ok
	catch
		error:badarg ->
			ets:delete_all_objects(?TBL)
	end.

get(Key) ->
	get(Key, undefined).

get(Key, Default) ->
	try ets:lookup(?TBL, Key) of
		[] -> Default;
		[{Key, Val}] -> Val
	catch
		error:badarg ->
			%% not yet initialized
			Default
	end.

set(Key, Val) ->
	ets:insert(?TBL, {Key, Val}).

%% @todo -- not safe
append(Key, Val) ->
	L = get(Key, []),

	%% add to last
	case is_list(L) of
		true ->
			set(Key, L ++ [Val]);
		_ ->
			error(badarg)
	end.


%% eunit tests

-ifdef(TEST).
-define(M, ?MODULE).

get_on_noninitd_test() ->
	?assertEqual(99, ?M:get(key, 99)).

new_on_existing_clears_test() ->
	?M:new(),

	?M:set(key, hello),
	?assertEqual(hello, ?M:get(key)),
	?M:new(),
	?assertEqual(undefined, ?M:get(key)).

get_undefined_test() ->
	?M:new(),
	?assertEqual(undefined, ?M:get(key)).


get_default_test() ->
	?M:new(),
	?assertEqual(99, ?M:get(key, 99)).

set_get_test() ->
	?M:new(),

	?assertEqual(undefined, ?M:get(key)),
	?M:set(key, hello),
	?assertEqual(hello, ?M:get(key)).

append_test_() ->
	{foreach, fun() -> ?M:new() end, fun(_) -> ok end, [
		{"append on empty", fun() ->
			?assertEqual([], ?M:get(key, [])),
			?M:append(key, 99),
			?assertEqual([99], ?M:get(key))
		end},

		{"append on existing", fun() ->
			?M:append(key, 99),
			?M:append(key, 1024),
			?assertEqual([99, 1024], ?M:get(key))
		end},

		{"append on nonlist", fun() ->
			?M:set(key, nonlist),
			?assertError(badarg, ?M:append(key, 99))
		end}
	]}.

-endif.