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
	get_skills/1,
	get_node/1,
	get_release_codes/1,
	go_available/1,
	go_released/1,
	go_released/2]).

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

get_skills(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	Ss = agent:get_skills(APid),

	Es = lists:reverse(lists:foldl(
		fun(At, Acc) when is_atom(At) -> [At|Acc];
			({'_brand', Client}, Acc) -> [{[{client, l2b(Client)}]}|Acc];
			({'_node', Node}, Acc) -> [{[{node, Node}]}|Acc];
			({'_profile', Profile}, Acc) -> [{[{profile, l2b(Profile)}]}|Acc];
			({'_queue', Queue}, Acc) -> [{[{queue, l2b(Queue)}]}|Acc];
			(_, Acc) -> Acc
		end, [], Ss)),
	{[{skills, Es}]}.

get_node(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	{[{node, node(APid)}]}.

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

go_available(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	agent:set_release(APid, none),
	{[{state, available}]}.

go_released(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	agent:set_release(APid, default),
	%% TODO define default release in header
	{[{state, released}, {release, {[{id, default}, {name, default}, {bias, negative}]}}]}.

go_released(St, RelIdBin) ->
	Rs = agent_auth:get_releases(), %% Better if agent_auth has a get_release
	RelId = b2l(RelIdBin),
	case [X || X <- Rs, X#release_opt.id=:=RelId] of
		[R] ->
			APid = cpx_conn_state:get(St, agent_pid),
			agent:set_release(APid, {R#release_opt.id, R#release_opt.label, R#release_opt.bias}),
			{[{state, released}, {release, relopt_entry(R)}]};
		_ ->
			err(invalid_rel)
	end.

%% Internal

send_exit() ->
	self() ! {'$cpx_agent_rpc', exit}.

l2b(L) ->
	list_to_binary(L).

b2l(B) ->
	binary_to_list(B).

relopt_entry(#release_opt{id=Id, label=Label, bias=B}) ->
	Bias = case B of
		N when N < 0 -> negative;
		N when N > 0 -> positive;
		_ -> neutral
	end,
	{[{id, l2b(Id)}, {name, l2b(Label)}, {bias, Bias}]}.

%% Errors

err(invalid_rel) ->
	{error, 4001, <<"Invalid/Missing release code">>}.

-ifdef(TEST).

t_apid() ->
	erlang:list_to_pid("<0.1.2>").

t_st() ->
	cpx_conn_state:new(#agent{login="agent", source=t_apid()}).

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

release_change_test_() ->
	{setup, fun() ->
		meck:new(agent),
		meck:expect(agent, set_release, 2, ok),

		meck:new(agent_auth),
		meck:expect(agent_auth, get_releases, 0, [
			#release_opt{id="relopt1", label="Release 1", bias=-1},
			#release_opt{id="relopt2", label="Release 2", bias=0},
			#release_opt{id="relopt3", label="Release 3", bias=1}
		])
	end, fun(_) ->
		meck:unload(agent_auth),
		meck:unload(agent)
	end, [{"go_available", fun() ->
		?assertEqual({[{state, available}]},
			go_available(t_st())),
		?assert(meck:called(agent, set_release, [t_apid(), none]))
	end}, {"go_released/0", fun() ->
		?assertEqual({[{state, released}, {release, {[{id, default}, {name, default}, {bias, negative}]}}]},
			go_released(t_st())),
		?assert(meck:called(agent, set_release, [t_apid(), default]))
	end}, {"go_released/1 - existing state", fun() ->
		?assertEqual({[{state, released}, {release, {[{id, <<"relopt1">>}, {name, <<"Release 1">>}, {bias, negative}]}}]},
			go_released(t_st(), <<"relopt1">>)),
		?assert(meck:called(agent, set_release, [t_apid(), {"relopt1", "Release 1", -1}]))
	end}, {"go_released/1 - undefined state", fun() ->
		?assertEqual(err(invalid_rel),
			go_released(t_st(), <<"relopt99">>))
	end}]}.

agent_info_test_() ->
	{setup, fun() ->
		meck:new(agent),
		meck:expect(agent, get_skills, 1, [english, support, '_all',
			{'_brand', "Client 1"},
			{'_node', 'server1.openacd.com'},
			{'_profile', "tech_team"},
			{'_queue', "dsl_support"},
			{unknown, should, notbepresent}])
	end, fun(_) ->
		meck:unload(agent)
	end, [{"get_skills", fun() ->
		?assertEqual({[{skills, [english, support, '_all',
			{[{'client', <<"Client 1">>}]},
			{[{'node', 'server1.openacd.com'}]},
			{[{'profile', <<"tech_team">>}]},
			{[{'queue', <<"dsl_support">>}]}]}]},
			get_skills(t_st()))
	end}, {"get_node", fun() ->
		?assertEqual({[{node, node()}]},
			get_node(t_st()))
	end}]}.

-endif.
