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

-import(cpx_json_util, [l2b/1, b2l/1]).

-export([logout/1,
	ping/1,
	get_queues/1,
	get_clients/1,
	get_skills/1,
	get_all_skills/1,
	get_node/1,
	get_profile/1,
	get_connection_info/1,
	get_release_codes/1,
	go_available/1,
	go_released/1,
	go_released/2,
	hangup/2,
	end_wrapup/2,
	hold_channel/2,
	unhold_channel/2,
	transfer_to_queue/3
]).

logout(_St) ->
	send_exit(),
	{[{status, logged_out}]}.

ping(_St) ->
	{[{pong, util:now_ms()}]}.

get_queues(_St) ->
	{ok, Qs} = call_queue_config:get_queues(),
	{[{queues, [l2b(Q#call_queue.name) || Q <- Qs]}]}.

get_clients(_St) ->
	ClientToEntry = fun(Cl) -> {[{id, l2b(Cl#client.id)},
		{name, l2b(Cl#client.label)}]}
	end,
	{ok, Clients} = call_queue_config:get_clients(),
	{[{clients, [ClientToEntry(Cl) || Cl <- Clients]}]}.

get_skills(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	Ss = agent:get_skills(APid),

	Es = cpx_json_util:enc_skills(Ss),
	{[{skills, Es}]}.

get_all_skills(_St) ->
	{ok, Ss} = call_queue_config:get_skills(),

	Es = cpx_json_util:enc_skill_recs(Ss),
	{[{skills, Es}]}.

get_node(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	{[{node, node(APid)}]}.

get_connection_info(St) ->
	A = cpx_conn_state:get(St, agent),
	{[{username, l2b(A#agent.login)},
		{profile, l2b(A#agent.profile)},
		{skills, cpx_json_util:enc_skills(A#agent.skills)},
		{node, node(A#agent.source)},
		{time, util:now()}]}.

get_profile(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	{[{profile, l2b(agent:get_profile(APid))}]}.

get_release_codes(_St) ->
	{ok, Rs} = agent_auth:get_releases(),
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
	RelId = b2l(RelIdBin),
	case agent_auth:get_release(RelId) of
		{ok, R} ->
			APid = cpx_conn_state:get(St, agent_pid),
			agent:set_release(APid, {R#release_opt.id, R#release_opt.label, R#release_opt.bias}),
			{[{state, released}, {release, relopt_entry(R)}]};
		_ ->
			err(invalid_rel)
	end.

hangup(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:set_state(ChanPid, wrapup) of
			ok -> {[{state, wrapup}, {channel, ChanId}]};
			_ -> err(invalid_state_change)
		end
	end).

hold_channel(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:hold(ChanPid) of
			ok -> {ok, success};
			_ -> err(cannot_hold)
		end
	end).

unhold_channel(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:unhold(ChanPid) of
			ok -> {ok, success};
			_ -> err(cannot_unhold)
		end
	end).

end_wrapup(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:end_wrapup(ChanPid) of
			ok -> {[{state, stopped}, {channel, ChanId}]};
			_ -> err(invalid_state_change)
		end
	end).

transfer_to_queue(St, ChanId, QueueName) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:queue_transfer(ChanPid, QueueName) of
			ok -> {[{state, wrapup}, {channel, ChanId}]};
			Err -> 	lager:info("Error : ~p", [Err]),
					err(invalid_state_change)
		end
	end).

%% Internal

send_exit() ->
	self() ! {'$cpx_agent_rpc', exit}.

relopt_entry(#release_opt{id=Id, label=Label, bias=B}) ->
	Bias = case B of
		N when N < 0 -> negative;
		N when N > 0 -> positive;
		_ -> neutral
	end,
	{[{id, l2b(Id)}, {name, l2b(Label)}, {bias, Bias}]}.

with_channel_do(St, ChanId, Fun) ->
	case cpx_conn_state:get_channel_pid_by_id(St, ChanId) of
		ChanPid when is_pid(ChanPid) ->
			Fun(ChanPid);
		_ ->
			err(channel_not_found)
	end.

%% Errors

err(invalid_rel) ->
	{error, 4001, <<"Invalid/Missing release code">>};
err(channel_not_found) ->
	{error, 4002, <<"Channel not found">>};
err(invalid_state_change) ->
	{error, 4003, <<"Invalid state change">>}.

-ifdef(TEST).

t_apid() ->
	erlang:list_to_pid("<0.1.2>").

t_cpid() ->
	erlang:list_to_pid("<0.1.3>").

t_st() ->
	St = cpx_conn_state:new(#agent{login="agent", source=t_apid()}),
	{_, St2} = cpx_conn_state:store_channel(St, t_cpid()),
	St2.

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

ping_test_() ->
	{setup, fun() ->
		meck:new(util),
		meck:expect(util, now, 0, 123000),
		meck:expect(util, now_ms, 0, 123)
	end, fun(_) ->
		meck:unload()
	end, [{"ping", fun() ->
		?assertEqual({[{pong, 123}]}, ping(t_st()))
	end}]}.

call_queue_apis_test_() ->
	{setup, fun() ->
		meck:new(call_queue_config)
	end, fun(_) ->
		meck:unload(call_queue_config)
	end, [{"get_queues", fun() ->
		meck:expect(call_queue_config, get_queues, 0, {ok, [
			#call_queue{name="q1"}, #call_queue{name="q2"}]}),

		?assertEqual({[{queues, [<<"q1">>, <<"q2">>]}]},
			get_queues(t_st()))
	end}, {"get_clients", fun() ->
		meck:expect(call_queue_config, get_clients, 0, {ok, [
			#client{id="cl1", label="Client1"},
			#client{id="cl2", label="Client2"}]}),

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
		meck:expect(agent_auth, get_releases, 0, {ok, [
			#release_opt{id="relopt1", label="Release 1", bias=-1},
			#release_opt{id="relopt2", label="Release 2", bias=0},
			#release_opt{id="relopt3", label="Release 3", bias=1}
		]}),

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
		meck:expect(agent_auth, get_release, fun("relopt1") -> {ok,
			#release_opt{id="relopt1", label="Release 1", bias=-1}};
		(_) -> none
		end)
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
		meck:expect(agent, get_skills, 1, [english, support]),
		meck:expect(agent, get_profile, 1, "tech_team"),

		meck:expect(agent, dump_state, 1,
			#agent{login="agent",
				profile="tech_team",
				skills=[english, support, '_all'],
				source=t_apid()}),

		meck:new(util),
		meck:expect(util, now, 0, 123)
	end, fun(_) ->
		meck:unload(util),
		meck:unload(agent)
	end, [{"get_skills", fun() ->
		?assertEqual({[{skills, [english, support]}]},
			get_skills(t_st()))
	end}, {"get_node", fun() ->
		?assertEqual({[{node, node()}]},
			get_node(t_st()))
	end}, {"get_profile", fun() ->
		?assertEqual({[{profile, <<"tech_team">>}]},
			get_profile(t_st()))
	end}, {"get_connection_info", fun() ->
		?assertEqual({[
			{username, <<"agent">>},
			{profile, <<"tech_team">>},
			{skills, [english, support, '_all']},
			{node, node()},
			{time, 123}]},
		get_connection_info(t_st()))
	end}]}.

hangup_test_() ->
	{setup, fun() ->
		meck:new(agent_channel)
	end, fun(_) ->
		meck:unload(agent_channel)
	end, [{"success", fun() ->
		ChId = <<"ch1">>,
		St = t_st(),
		meck:expect(agent_channel, set_state, 2, ok),
		?assertEqual({[{state, wrapup}, {channel, ChId}]}, hangup(St, ChId)),
		?assert(meck:called(agent_channel, set_state, [t_cpid(), wrapup], self()))
	end}, {"invalid end wrapup", fun() ->
		meck:expect(agent_channel, set_state, 2, error),
		?assertEqual(err(invalid_state_change), hangup(t_st(), <<"ch1">>))
	end}, {"channel not found", fun() ->
		?assertEqual(err(channel_not_found), hangup(t_st(), <<"ch999">>))
	end}]}.

end_wrapup_test_() ->
	{setup, fun() ->
		meck:new(agent_channel)
	end, fun(_) ->
		meck:unload(agent_channel)
	end, [{"success", fun() ->
		ChId = <<"ch1">>,
		St = t_st(),
		meck:expect(agent_channel, end_wrapup, 1, ok),
		?assertEqual({[{state, stopped}, {channel, ChId}]}, end_wrapup(St, ChId)),
		?assert(meck:called(agent_channel, end_wrapup, [t_cpid()], self()))
	end}, {"invalid end wrapup", fun() ->
		meck:expect(agent_channel, end_wrapup, 1, invalid),
		?assertEqual(err(invalid_state_change), end_wrapup(t_st(), <<"ch1">>))
	end}, {"channel not found", fun() ->
		?assertEqual(err(channel_not_found), end_wrapup(t_st(), <<"ch999">>))
	end}]}.

hold_channel_test_() ->
	{setup, fun() ->
		meck:new(agent_channel),
		meck:expect(agent_channel, hold, 1, ok),
		meck:expect(agent_channel, unhold, 1, ok)
	end, fun(_) ->
		meck:unload(agent_channel)
	end, [{"hold channel", fun() ->
		ChId = <<"ch1">>,
		St = t_st(),
		?assertEqual({ok, success}, hold_channel(St, ChId)),
		?assert(meck:called(agent_channel, hold, [t_cpid()], self()))
	end}, {"unhold channel", fun() ->
		ChId = <<"ch1">>,
		St = t_st(),
		?assertEqual({ok, success}, unhold_channel(St, ChId)),
		?assert(meck:called(agent_channel, unhold, [t_cpid()], self()))
	end}]}.

get_all_skills_test_() ->
	{setup, fun() ->
		meck:new(call_queue_config),
		meck:expect(call_queue_config, get_skills, 0, {ok, [#skill_rec{atom=english, name="English"},
			#skill_rec{atom=support, name="Support"},
			#skill_rec{atom='_all', name="All"},
			#skill_rec{atom='_brand', name="Brand"},
			#skill_rec{atom='_node', name="Node"},
			#skill_rec{atom='_profile', name="Agent Profile"},
			#skill_rec{atom='_queue', name="Queue"}]})
	end, fun(_) ->
		meck:unload(call_queue_config)
	end, [{"get all skills", fun() ->
		?assertEqual({[{skills, [english, support, '_all', '_brand', '_node', '_profile', '_queue']}]},
			get_all_skills(t_st()))
	end}]}.

-endif.
