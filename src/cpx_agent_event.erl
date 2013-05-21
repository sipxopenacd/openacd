-module(cpx_agent_event).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("agent.hrl").

% api
-export([
	start/0,
	start_link/0,
	stop/0,
	agent_init/1,
	change_profile/3,
	change_release_state/3,
	agent_channel_init/5,
	change_agent_channel/5
]).

%% =====
%% API
%% =====

%% @doc starts the agent event server.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	gen_event:start({local, ?MODULE}).

%% @doc Starts teh agent event server linked.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	gen_event:start_link({local, ?MODULE}).

%% @doc Stops the agent event server.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_event:stop(?MODULE).

%% @doc Create a handler specifically for the given agent.  Handles profile
%% login, logout, profile changes, release, and idle states.
-spec(agent_init/1 :: (Agent :: #agent{}) -> 'ok' | {atom(), any()}).
agent_init(Agent) when is_record(Agent, agent) ->
	Now = ouc_time:now(),

	gen_event:notify(?MODULE, {agent_init, Agent, Now}).

%% @doc An agent has changed profile
change_profile(AgentId, Profile, Ts) ->
	gen_event:notify(?MODULE, {change_profile, AgentId, Profile, Ts}).

%% @doc An agent has changed state (idle &lt;-&gt; released)
-spec change_release_state(agent_id(), available | {released, release_code()}, ts()) -> ok.
change_release_state(AgentId, Release, Ts) ->
	gen_event:notify(?MODULE, {change_release_state, AgentId, Release, Ts}).

%% @doc Create a handler specifically for the given agent channel.
-spec(agent_channel_init/5 :: (Agent :: #agent{}, ChannelPid :: pid(),
Statename :: atom(), Statedata :: any(), ts()) -> 'ok' | {atom(), any()}).
agent_channel_init(Agent, ChannelPid, StateName, Call, Ts) ->
	gen_event:notify(?MODULE, {channel_init, Agent, ChannelPid, StateName, Call, Ts}).

%% @doc Alert the appropriate handler that an agent channel has changed
%% in some way (usually state).
-spec(change_agent_channel/5 :: (Agent :: #agent{}, ChannelPid :: pid(), Statename :: atom(),
Statedata :: any(), ts()) -> 'ok').
change_agent_channel(Agent, ChannelPid, Statename, Call, Ts) ->
	gen_event:notify(?MODULE, {channel_change, Agent, ChannelPid, Statename, Call, Ts}).
