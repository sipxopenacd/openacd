-module(agent_prop).
-compile(export_all).

-include("agent.hrl").

-define(AGENT_PROP, {p, l, cpx_online_agent}).

init(Agent, AgentState) ->
	UState = case AgentState of
		available -> idle;
		_ -> released
	end,
	#agent{
		login = Login,
		profile = Profile,
		firstname = FirstName,
		lastname = LastName} = Agent,
	Prop = #agent_prop_state{
		login = Login,
		profile = Profile,
		firstname = FirstName,
		lastname = LastName,
		ustate = UState,
		rstate = AgentState
	},
	gproc:reg(?AGENT_PROP, Prop),
	gproc:send({p, l, cpx_agent_ustate_change},
				{cpx_agent_ustate_change, offline, UState, Prop}),
	Prop.

set_release_state(_Agent, AgentState, Prop) ->
	UState = Prop#agent_prop_state.ustate,
	UState2 = case {AgentState, UState} of
		{released, idle} -> released;
		{available, released} -> idle;
		_ -> UState
	end,
	Prop2 = Prop#agent_prop_state{
		ustate = UState2,
		rstate = AgentState
	},
	gproc:set_value(?AGENT_PROP, Prop2),
	case UState =/= UState2 of
		true ->
			gproc:send({p, l, cpx_agent_ustate_change},
				{cpx_agent_ustate_change, UState, UState2, Prop2});
		_ ->
			ok
	end,
	Prop2.

set_channel_state(_Agent, _Channel, ChannelState, Prop) ->
	RState = Prop#agent_prop_state.rstate,
	UState = Prop#agent_prop_state.ustate,
	UState2 = case {ChannelState, RState} of
		{undefined, available} -> idle;
		{undefined, released} -> released;
		_ -> ChannelState
	end,
	Prop2 = Prop#agent_prop_state{
		ustate = UState2
	},
	gproc:set_value(?AGENT_PROP, Prop2),
	case UState =/= UState2 of
		true ->
			gproc:send({p, l, cpx_agent_ustate_change},
				{cpx_agent_ustate_change, UState, UState2, Prop2});
		_ ->
			ok
	end,
	Prop2.

set_offline(_Agent, Prop) ->
	UState = Prop#agent_prop_state.ustate,
	gproc:send({p, l, cpx_agent_offline}, {cpx_agent_offline, UState, Prop}).

list() ->
	gproc:lookup_values(?AGENT_PROP).

subscribe(ustate) ->
	catch gproc:add_local_property(cpx_agent_offline, subscribe),
	case catch gproc:add_local_property(cpx_agent_ustate_change, subscribe) of
		true -> true;
		_ -> false
	end.
