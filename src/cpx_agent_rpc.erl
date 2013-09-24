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
	sleep/2,
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
	transfer_to_agent/3,
	transfer_to_queue/4,
	transfer_outband/3,
	play/2,
	play/3,
	pause/2,

	conference_to_agent/3
]).

-type(cpx_conn_state() :: tuple()).
-type(rpc_success() :: {error, integer(), binary()}).
-type(rpc_error() :: {error, integer(), binary()}).

logout(_St) ->
	send_exit(),
	{[{status, logged_out}]}.

-spec(sleep/2 :: (St :: cpx_conn_state(), TimeoutMs :: integer()) -> rpc_success() | rpc_error()).
sleep(_St, TimeoutMs) ->
	self() ! {wsock_sleep, TimeoutMs},
	{[{status, sleep}]}.

ping(_St) ->
	{[{pong, util:now_ms()}]}.

get_queues(_St) ->
	{ok, Qs} = call_queue_config:get_queues(),
	Queues = [{[{name, l2b(Q#call_queue.name)},
				{skills, cpx_json_util:enc_skills(Q#call_queue.skills)}]} ||
				Q <- Qs],
	{[{queues, Queues}]}.

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

-spec(hold_channel/2 :: (St :: cpx_conn_state(), ChanId :: binary()) -> rpc_success() | rpc_error()).
hold_channel(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:hold(ChanPid) of
			ok -> {ok, success};
			_ -> err(cannot_hold)
		end
	end).

-spec(unhold_channel/2 :: (St :: cpx_conn_state(), ChanId :: binary()) -> rpc_success() | rpc_error()).
unhold_channel(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:unhold(ChanPid) of
			ok -> {ok, success};
			_ -> err(cannot_unhold)
		end
	end).

-spec(play/2 :: (St :: cpx_conn_state(), ChanId :: binary()) -> rpc_success() | rpc_error()).
play(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:play(ChanPid) of
			ok -> {ok, success};
			_ -> err(cannot_play)
		end
	end).

-spec(play/3 :: (St :: cpx_conn_state(), ChanId :: binary(), Opts :: json()) -> rpc_success() | rpc_error()).
play(St, ChanId, Opts) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:play(ChanPid, Opts) of
			ok -> {ok, success};
			_ -> err(cannot_play)
		end
	end).

-spec(pause/2 :: (St :: cpx_conn_state(), ChanId :: binary()) -> rpc_success() | rpc_error()).
pause(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:pause(ChanPid) of
			ok -> {ok, success};
			_ -> err(cannot_pause)
		end
	end).

end_wrapup(St, ChanId) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:end_wrapup(ChanPid) of
			ok -> {[{state, stopped}, {channel, ChanId}]};
			_ -> err(invalid_state_change)
		end
	end).

transfer_to_agent(St, ChanId, AgentLogin) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:agent_transfer(ChanPid, b2l(AgentLogin)) of
			ok -> {[{state, wrapup}, {channel, ChanId}]};
			Err -> 	lager:info("Error : ~p", [Err]),
					err(cannot_transfer)
		end
	end).

transfer_to_queue(St, ChanId, Queue, Opts) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		%% TODO write better conversion script from JSON -> proplist
		SkillBins = ej:get({"skills"}, Opts),
		Skills = [binary_to_existing_atom(SkillBin, utf8) || SkillBin <- SkillBins],
		NewOpts = [{skills, Skills}],
		case agent_channel:queue_transfer(ChanPid, b2l(Queue), NewOpts) of
			ok -> {[{state, wrapup}, {channel, ChanId}]};
			Err -> 	lager:info("Error : ~p", [Err]),
					err(invalid_state_change)
		end
	end).

transfer_outband(St, ChanId, Number) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:outband_transfer(ChanPid, b2l(Number)) of
			ok ->
				{[{state, wrapup}, {channel, ChanId}]};
			Err ->
				lager:info("Error : ~p", [Err]),
				err(cannot_transfer)
		end
	end).

conference_to_agent(St, ChanId, AgentLogin) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		case agent_channel:conference_to_agent(ChanPid, b2l(AgentLogin)) of
			ok ->
				{[{state, oncall}, {channel, ChanId}]};
			{error, agent_unavailable} ->
				err(agent_unavailable);
			{error, already_in_conference} ->
				err(already_in_conference);
			_ ->
				err(unable_to_start_conference)
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
	{error, 4003, <<"Invalid state change">>};
err(agent_unavailable) ->
	{error, 4004, <<"Agent unavailable">>};
err(already_in_conference) ->
	{error, 4005, <<"Channel already in conference">>};
err(unable_to_start_conference) ->
	{error, 4006, <<"Unable to start conference">>}.

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
			#call_queue{name="q1", skills=[english]},
			#call_queue{name="q2", skills=[technical]}]}),

		?assertEqual({[{queues, [{[{name, <<"q1">>},{skills, [english]}]},
								 {[{name, <<"q2">>},{skills, [technical]}]}
					]}]},
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

playback_control_test_() ->
	{setup, fun() ->
		meck:new(agent_channel),
		meck:expect(agent_channel, play, 1, ok),
		meck:expect(agent_channel, play, 2, ok),
		meck:expect(agent_channel, pause, 1, ok)
	end, fun(_) ->
		meck:unload(agent_channel)
	end, [{"play", fun() ->
		ChId = <<"ch1">>,
		St = t_st(),
		?assertEqual({ok, success}, play(St, ChId)),
		?assert(meck:called(agent_channel, play, [t_cpid()], self()))
	end}, {"play with location", fun() ->
		ChId = <<"ch1">>,
		Opts = {[{<<"location">>, 1000}]},
		St = t_st(),
		?assertEqual({ok, success}, play(St, ChId, Opts)),
		?assert(meck:called(agent_channel, play, [t_cpid(), Opts], self()))
	end}, {"pause", fun() ->
		ChId = <<"ch1">>,
		St = t_st(),
		?assertEqual({ok, success}, pause(St, ChId)),
		?assert(meck:called(agent_channel, pause, [t_cpid()], self()))
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

sleep_test() ->
	?assertEqual({[{status, sleep}]}, sleep(any, 123)),
	Rcvd = receive {wsock_sleep, 123} -> true after 10 -> false end,
	?assert(Rcvd).

-endif.
