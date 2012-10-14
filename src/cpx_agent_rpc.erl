%%	The contents of this file are subject to the Common Public Attribution
%%	License Version 1.0 (the “License”); you may not use this file except
%%	in compliance with the License. You may obtain a copy of the License at
%%	http://opensource.org/licenses/cpal_1.0. The License is based on the
%%	Mozilla Public License Version 1.1 but Sections 14 and 15 have been
%%	added to cover use of software over a computer network and provide for
%%	limited attribution for the Original Developer. In addition, Exhibit A
%%	has been modified to be consistent with Exhibit B.
%%
%%	Software distributed under the License is distributed on an “AS IS”
%%	basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%	License for the specific language governing rights and limitations
%%	under the License.
%%
%%	The Original Code is OpenACD.
%%
%%	The Initial Developers of the Original Code is
%%	Andrew Thompson and Micah Warren.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2009 SpiceCSM.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Jan Vincent Liwanag <jvliwanag at ezuce dot com>
%%
-module(cpx_agent_rpc).

-include("agent.hrl").
-include("call.hrl").
-include("queue.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([logout/1,
	get_queues/1,
	get_clients/1,
	get_release_codes/1]).

logout(_St) ->
	send_exit(),
	{[{status, logged_out}]}.

get_queues(_St) ->
	Qs = call_queue_config:get_queues(),
	{[{queues, [l2b(Q#call_queue.name) || Q <- Qs]}]}.

get_clients(_St) ->
	ClientToEntry = fun(Cl) -> {[{id, l2b(Cl#client.id)},
		{name, l2b(Cl#client.label)}]}
	end,
	Clients = call_queue_config:get_clients(),
	{[{clients, [ClientToEntry(Cl) || Cl <- Clients]}]}.

get_release_codes(_St) ->
	Rs = agent_auth:get_releases(),
	RtoEntry = fun(#release_opt{id=Id, label=Label, bias=B}) ->
		Bias = case B of
			N when N < 0 -> negative;
			N when N > 0 -> positive;
			_ -> neutral
		end,
		{[{id, l2b(Id)}, {name, l2b(Label)}, {bias, Bias}]}
	end,
	{[{codes, [RtoEntry(R) || R <- Rs]}]}.

%% Internal

send_exit() ->
	self() ! {'$cpx_agent_rpc', exit}.

l2b(L) ->
	list_to_binary(L).

-ifdef(TEST).

t_st() ->
	cpx_conn_state:new().

assert_exit() ->
	Exit = receive
		{'$cpx_agent_rpc', exit} -> true
		after 0 -> false
	end,
	?assert(Exit).

logout_test() ->
	?assertEqual({[{status, logged_out}]},
		logout(t_st())),
	assert_exit().

call_queue_apis_test_() ->
	{setup, fun() ->
		meck:new(call_queue_config)
	end, fun(_) ->
		meck:unload(call_queue_config)
	end, [{"get_queues", fun() ->
		meck:expect(call_queue_config, get_queues, 0, [
			#call_queue{name="q1"}, #call_queue{name="q2"}]),

		?assertEqual({[{queues, [<<"q1">>, <<"q2">>]}]},
			get_queues(t_st()))
	end}, {"get_clients", fun() ->
		meck:expect(call_queue_config, get_clients, 0, [
			#client{id="cl1", label="Client1"},
			#client{id="cl2", label="Client2"}]),

		?assertEqual({[{clients,[{[{id, <<"cl1">>}, {name, <<"Client1">>}]},
			{[{id, <<"cl2">>}, {name, <<"Client2">>}]}]}]},
			get_clients(t_st()))
	end}]}.

agent_auth_apis_test_() ->
	{setup, fun() ->
		meck:new(agent_auth)
	end, fun(_) ->
		meck:unload(agent_auth)
	end, [{"get_release_codes", fun() ->
		meck:expect(agent_auth, get_releases, 0, [
			#release_opt{id="relopt1", label="Release 1", bias=-1},
			#release_opt{id="relopt2", label="Release 2", bias=0},
			#release_opt{id="relopt3", label="Release 3", bias=1}
		]),

		?assertEqual({[{codes, [{[{id, <<"relopt1">>}, {name, <<"Release 1">>}, {bias, negative}]},
			{[{id, <<"relopt2">>}, {name, <<"Release 2">>}, {bias, neutral}]},
			{[{id, <<"relopt3">>}, {name, <<"Release 3">>}, {bias, positive}]}]}]},
			get_release_codes(t_st()))
	end}]}.

-endif.
