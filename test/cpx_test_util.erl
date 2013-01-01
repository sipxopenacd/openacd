-module(cpx_test_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([t_passthrough/6]).

t_passthrough(Mod, MockMod, ApiFun, CbkFun, Args, Ret) ->
	ArgsN = length(Args),
	Name = lists:flatten(io_lib:format("~p/~b", [ApiFun, ArgsN])),

	{Name, fun() ->
		meck:expect(MockMod, CbkFun, ArgsN, Ret),
		?assertEqual(Ret, apply(Mod, ApiFun, Args)),
		?assert(meck:called(MockMod, CbkFun, Args, self()))
	end}.

-endif.