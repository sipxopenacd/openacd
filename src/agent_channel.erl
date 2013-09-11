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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at lordnull dot com>
%%  Chris Case	<chris dot case at g33xnexus dot com>
%%

%% @doc A gen_fsm for an agent channel.  When an agent is to go ringing
%% for a media, if the agent fsm has a channel available, a new process
%% of this module is started.  Once the agent has gone through the flow,
%% this process can die.
-module(agent_channel).
-behaviour(gen_fsm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
%-include_lib("stdlib/include/qlc.hrl").


-record(state, {
	agent_rec :: #agent{},
	agent_fsm :: pid(),
	agent_connection :: pid(),
	agent_login :: string(),
	agent_profile :: string(),
	event_manager :: pid(),
	media_type = voice :: channel_category(),
	endpoint = inband :: any(),
	client :: undefined | #client{} | {Id :: string(), Opts :: [{atom(), any()}]} | (Id :: string()),
	callerid :: {string(), string()},
	state_data :: any()
}).

-type(state() :: #state{}).
-define(GEN_FSM, true).
-include("gen_spec.hrl").

-define(DEFAULT_REL, {"default", default, -1}).
-define(RING_FAIL_REL, {"Ring Fail", ring_fail, -1}).
-define(RING_LOCK_DURATION, 1000). % in ms
-define(WRAPUP_AUTOEND_KEY, autoend_wrapup).
-define(STATE_ATOMS, ['prering', 'ringing', 'precall', 'oncall',
	'warmtransfer_hold', 'warmtransfer_3rd_party', 'wrapup']).

%% gen_fsm exports
-export([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4%,
	%format_status/2
]).
%% defined state exports
-export([
	prering/3,
	ringing/3,
	precall/3,
	oncall/3,
	warmtransfer_hold/3,
	warmtransfer_3rd_party/3,
	wrapup/3
]).
%% defining async state exports
-export([
	prering/2,
	ringing/2,
	precall/2,
	oncall/2,
	warmtransfer_hold/2,
	warmtransfer_3rd_party/2,
	wrapup/2
]).

%% api
-export([
	% start/2,
	start/5,
	% start_link/2,
	start_link/5,
	stop/1,
	get_agent/1,
	get_media/1,
	set_state/2,
	set_state/3,
	async_set_state/2,
	async_set_state/3,
	end_wrapup/1,
	list_to_state/1,
	set_connection/2,
	queue_transfer/2,
	queue_transfer/3,
	agent_transfer/2,
	outband_transfer/2,
	media_call/2, % conn asking the media stuff
	media_cast/2, % conn telling media stuff
	media_push/3, % media telling conn stuff
	spy/2,
	has_successful_ring/1,
	has_failed_ring/1,
	url_pop/3,
	subscribe_events/2,
	subscribe_events/3,
	hold/1,
	unhold/1,
	play/1,
	play/2,
	pause/1
]).

% ======================================================================
% API
% ======================================================================

% -type(start_opts() :: [{atom(), any()}]).
% %% @doc start an fsm with the given options.
% -spec(start/2 :: (AgentRec :: #agent{}, Options :: start_opts()) -> {'ok', pid()}).
% start(AgentRec, Options) ->
% 	gen_fsm:start(?MODULE, [AgentRec, Options], []).

start(AgentRec, CallRec, EndpointData, InitState, EventManager) ->
	gen_fsm:start(?MODULE, [AgentRec, CallRec, EndpointData, InitState, EventManager], []).

% %% @doc Start an fsm linked to the calling process.
% -spec(start_link/2 :: (AgentRec :: #agent{}, Options :: start_opts()) -> {'ok', pid()}).
% start_link(AgentRec, Options) ->
% 	gen_fsm:start_link(?MODULE, [AgentRec, Options], []).

start_link(AgentRec, CallRec, EndpointData, InitState, EventManager) ->
	gen_fsm:start_link(?MODULE, [AgentRec, CallRec, EndpointData, InitState, EventManager], []).

%% @doc Stop the passed agent fsm `Pid'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(APid) ->
	gen_fsm:sync_send_event(APid, stop).

%% @doc link the given agent  `Pid' to the given connection `Socket'.
-spec(set_connection/2 :: (Pid :: pid(), Socket :: pid()) -> 'ok' | 'error').
set_connection(Pid, Socket) ->
	gen_fsm:sync_send_all_state_event(Pid, {set_connection, Socket}).

%% @doc The connection can request to call to the agent's media when oncall.
-spec(media_call/2 :: (Apid :: pid(), Request :: any()) -> any()).
media_call(Apid, Request) ->
	gen_fsm:sync_send_event(Apid, {mediacall, Request}).

%% @doc To cast to the media while oncall, use this.
-spec(media_cast/2 :: (Apid :: pid(), Request :: any()) -> 'ok').
media_cast(Apid, Request) ->
	gen_fsm:send_event(Apid, {mediacast, Request}).

%% @doc Returns the #agent{} from the current state.
-spec(get_agent/1 :: (Apid :: pid()) -> {ok, #agent{}}).
get_agent(Apid) ->
	gen_fsm:sync_send_all_state_event(Apid, get_agent).

%% @doc Returns the #call{} of the current state if there is on, otherwise
%% returns `invalid'.
-spec(get_media/1 :: (Apid :: pid()) -> {ok, #call{}} | 'invalid').
get_media(Apid) ->
	gen_fsm:sync_send_event(Apid, get_media).

%% @doc Attempt to set the state of agent at `Pid' to `State'.
-spec(set_state/2 :: (Pid :: pid(), State :: atom()) -> 'ok' | 'invalid').
set_state(Pid, State) ->
	gen_fsm:sync_send_event(Pid, State, infinity).

%% @doc Attempt to set the state of the agent at `Pid' to `State' with data `Data'.  `Data' is related to the `State' the agent is going into.
%% Often `Data' will be `#call{} or a callid of type `string()'.
-spec(set_state/3 :: (Pid :: pid(), State :: 'idle' | 'ringing' | 'precall' | 'oncall' | 'outgoing' | 'warmtransfer' | 'wrapup', Data :: any()) -> 'ok' | 'invalid';
                     (Pid :: pid(), State :: 'released', Data :: any()) -> 'ok' | 'invalid' | 'queued').
set_state(Pid, State, Data) ->
	gen_fsm:sync_send_event(Pid, {State, Data}, infinity).

async_set_state(Pid, State) ->
	gen_fsm:send_event(Pid, State).

async_set_state(Pid, State, Data) ->
	gen_fsm:send_event(Pid, {State, Data}).

%% @doc End the channel while in wrapup.
-spec(end_wrapup/1 :: (Pid :: pid()) -> 'ok' | 'invalid').
end_wrapup(Pid) ->
	stop(Pid).

%% @doc attmept to push data from the media connection to the agent.  It's up to
%% the agent connection to interpret this correctly.
-spec(media_push/3 :: (Pid :: pid(), Callrec :: #call{}, Data :: any()) -> any()).
media_push(Pid, Callrec, Data) ->
	S = self(),
	gen_fsm:send_event(Pid, {mediapush, S, Callrec, Data}).

%% @doc Make the give `pid() Spy' spy on `pid() Target'.
-spec(spy/2 :: (Spy :: pid(), Target :: pid()) -> 'ok' | 'invalid').
spy(Spy, Target) ->
	gen_fsm:sync_send_event(Spy, {spy, Target}).

%% @doc Make the agent connection try to pop a given url.
-spec(url_pop/3 :: (Pid :: pid(), Url :: string(), Name :: string()) -> 'ok').
url_pop(Pid, Url, Name) ->
	gen_fsm:send_all_state_event(Pid, {url_pop, Url, Name}).

%% @doc Translate the state `String' into the internally used atom.  `String' can either be the human readable string or a number in string form (`"1"').
-spec(list_to_state/1 :: (String :: string()) -> atom()).
list_to_state(String) ->
	Atom = try erlang:list_to_existing_atom(String) of
		A -> A
	catch
		error:badarg -> badarg
	end,
	case lists:member(Atom, ?STATE_ATOMS) of
		true -> Atom;
		false -> erlang:error(badarg)
	end.

%% @doc Start the queue_transfer procedure.  Gernally the media will handle it from here.
-spec(queue_transfer/2 :: (Pid :: pid(), Queue :: string()) -> 'ok' | 'invalid').
queue_transfer(Pid, Queue) ->
	queue_transfer(Pid, Queue, []).

-spec(queue_transfer/3 :: (Pid :: pid(), Queue :: string(), Opts :: any()) -> 'ok' | 'invalid').
queue_transfer(Pid, Queue, Opts) ->
	gen_fsm:sync_send_event(Pid, {queue_transfer, Queue, Opts}).

%% @doc Transfer call to an agent
-spec(agent_transfer/2 :: (Pid :: pid(), AgentLogin :: list()) -> ok | error).
agent_transfer(Pid, AgentLogin) ->
	gen_fsm:sync_send_event(Pid, {agent_transfer, AgentLogin}).

-spec(outband_transfer/2 :: (Pid :: pid(), AgentLogin :: list()) -> ok | error).
outband_transfer(Pid, Addr) ->
	gen_fsm:sync_send_event(Pid, {outband_transfer, Addr}).

%% @doc Inform the agent that it's failed a ring, usually an outbound.
%% Used by gen_media, prolly not anywhere else.
-spec(has_failed_ring/1 :: (Pid :: pid()) -> 'ok').
has_failed_ring(Pid) ->
	MediaPid = self(),
	gen_fsm:send_event(Pid, {failed_ring, MediaPid}).

%% @doc Media saying the ring worked afterall; useful to confirm outband rings.
-spec(has_successful_ring/1 :: (Pid :: pid()) -> 'ok').
has_successful_ring(Pid) ->
	MediaPid = self(),
	gen_fsm:send_event(Pid, {has_successful_ring, MediaPid}).

%% @doc Initialize and subscribe `Handler' to `Pid' events.
subscribe_events(Pid, Handler) ->
	subscribe_events(Pid, Handler, []).

%% @doc Initialize and subscribe `Handler' with initial `Args' to `Pid' events.
subscribe_events(Pid, Handler, Args) ->
	gen_fsm:send_all_state_event(Pid, {subscribe_events, Handler, Args}).

%% @doc Puts the channel on hold
-spec(hold/1 :: (Apid :: pid()) -> ok | error).
hold(Apid) ->
	gen_fsm:sync_send_event(Apid, hold).

%% @doc Puts the channel off hold
-spec(unhold/1 :: (Apid :: pid()) -> ok | error).
unhold(Apid) ->
	gen_fsm:sync_send_event(Apid, unhold).

%% @doc Starts or resumes the channel playback
-spec(play/1 :: (Apid :: pid()) -> ok | error).
play(Apid) ->
	play(Apid, {[]}).

%% @doc Starts or resumes the channel playback at a specified location
-spec(play/2 :: (Apid :: pid(), Opts :: json()) -> ok | error).
play(Apid, Opts) ->
	gen_fsm:sync_send_event(Apid, {play, Opts}).

%% @doc Pauses the channel playback
-spec(pause/1 :: (Apid :: pid()) -> ok | error).
pause(Apid) ->
	gen_fsm:sync_send_event(Apid, pause).

% ======================================================================
% INIT
% ======================================================================

%% @private
%-spec(init/1 :: (Args :: [#agent{}]) -> {'ok', 'released', #agent{}}).
%init([Agent, Options]) when is_record(Agent, agent) ->
%	%{ok, MaxRingouts} = cpx:get_env(max_ringouts, infinity),
%	ProtoState = #state{
%		agent_fsm = Agent#agent.source,
%		agent_connection = Agent#agent.connection
%	},
%	InitInfo = proplists:get_value(initial_state, Options, {prering, undefined}),
%	case InitInfo of
%		{prering, Call} when is_record(Call, call); Call =:= undefined ->
%			State = ProtoState#state{state_data = Call},
%			lager:debug("Starting in prering", []),
%			{ok, prering, State};
%		{ringing, Call} when is_record(Call, call) ->
%			State = ProtoState#state{state_data = Call},
%			lager:debug("Starting in ring directly", []),
%			{ok, ringing, State};
%		{precall, Client} when is_record(Client, client) ->
%			State = ProtoState#state{state_data = Client},
%			lager:debug("Starting in precall", []),
%			{ok, precall, State};
%		_ ->
%			lager:warning("Failed start:  ~p", [InitInfo]),
%			{stop, badstate}
%	end;

init([Agent, Call, Endpoint, StateName, EventManager]) ->
	process_flag(trap_exit, true),
	Now = util:now(),
	State = #state{
		agent_rec = Agent,
		agent_fsm = Agent#agent.source,
		agent_connection = Agent#agent.connection,
		agent_login = Agent#agent.login,
		agent_profile = Agent#agent.profile,
		media_type = Call#call.type,
		endpoint = Endpoint,
		client = Call#call.client,
		callerid = Call#call.callerid,
		state_data = Call,
		event_manager = EventManager
	},
	init_gproc_prop({State, init, StateName}),
	gen_event:notify(EventManager, {channel_feed, {initiated_channel, os:timestamp(), self(), Call}}),
	case StateName of
		prering when is_record(Call, call); Call =:= undefined ->
			case start_endpoint(Endpoint, Agent, Call) of
				{ok, Pid} ->
					lager:debug("Starting in prering", []),
					conn_cast(Agent, set_channel_msg(prering, Call)),
					cpx_agent_event:agent_channel_init(Agent, self(), prering, Call, Now),
					agent:set_channel(Agent#agent.source, self(), prering),
					{ok, prering, State#state{endpoint = Pid, state_data = update_state(prering, Call)}};
				{error, Error} ->
					{stop, {error, Error}}
			end;
		% precall when is_record(Call, client) ->
		% 	lager:debug("Starting in precall", []),
		% 	conn_cast(Agent, {set_channel, self(), precall, Call}),
		% 	% cpx_agent_event:agent_channel_init(Agent,self(),precall,Call),
		% 	{ok, precall, State#state{state_data = update_state(precall, Call)}};
		precall when is_record(Call, call) ->
			lager:debug("Starting in precall with media rather than client", []),
			conn_cast(Agent, set_channel_msg(precall, Call)),
			cpx_agent_event:agent_channel_init(Agent, self(), precall, Call, Now),
			agent:set_channel(Agent#agent.source, self(), precall),
			{ok, precall, State#state{state_data = update_state(precall, Call)}};
		ringing when is_record(Call, call) ->
			% TODO tell media to ring
			lager:debug("Starting in ringing", []),
			conn_cast(Agent, set_channel_msg(ringing, Call)),
			cpx_agent_event:agent_channel_init(Agent,self(),ringing, Call, Now),
			agent:set_channel(Agent#agent.source, self(), ringing),
			{ok, ringing, State#state{state_data = update_state(ringing, Call)}};
		_ ->
			lager:warning("Failed start:  ~p", [{StateName, Call}]),
			{stop, badstate}
	end.


% ======================================================================
% PRERING
% ======================================================================

prering({ringing, Call}, From, #state{agent_rec = Agent} = State) ->
	% TODO check if valid
	Now = ouc_time:now(),
	lager:debug("Moving from prering to ringing state request from ~p", [From]),
	conn_cast(State#state.agent_connection, set_channel_msg(ringing, Call)),
	cpx_agent_event:change_agent_channel(Agent, self(), ringing, Call, Now),
	agent:set_channel(Agent#agent.source, self(), ringing),
	set_gproc_prop({State, prering, ringing}),
	{reply, ok, ringing, State#state{state_data = update_state(ringing, Call)}};
prering(Msg, _From, State) ->
	lager:info("Msg ~p not understood", [Msg]),
	{reply, {error, invalid}, prering, State}.

%% -----

prering(_Msg, State) ->
	{next_state, prering, State}.

% ======================================================================
% RINGING
% ======================================================================

% ringing(oncall, {Conn, _}, #state{agent_connection = Conn, endpoint = inband} = State) ->
% 	#call{source = Media} = Call = State#state.state_data,
% 	case gen_media:oncall(Media) of
% 		ok ->
% 			conn_cast(Conn, {set_channel, self(), oncall, Call}),
% 			% cpx_agent_event:change_agent_channel(self(), oncall, Call),
% 			lager:debug("Moving from ringing to oncall state", []),
% 			set_gproc_prop({State, ringing, oncall}),
% 			{reply, ok, oncall, State#state{state_data = update_state(oncall, Call)}};
% 		Else ->
% 			lager:warning("Didn't go oncall:  ~p", [Else]),
% 			{reply, {error, Else}, ringing, State}
% 	end;

ringing(oncall, {Conn, _}, #state{agent_rec = Agent, agent_connection = Conn, endpoint = Ep, state_data = #call{ring_path = inband}} = State) ->
	#call{source = Media} = Call = State#state.state_data,
	Now = ouc_time:now(),
	case gen_media:oncall(Media) of
		ok ->
			conn_cast(Conn, set_channel_msg(oncall, Call)),
			cpx_agent_event:change_agent_channel(Agent, self(), oncall, Call, Now),
			agent:set_channel(Agent#agent.source, self(), oncall),
			NewEndpoint = case Call#call.media_path of
				inband ->
					cpx_endpoint:stop(Ep),
					undefined;
				_ ->
					Ep
			end,
			NewState = State#state{endpoint = NewEndpoint},
			lager:debug("Moving from ringing to oncall state", []),
			set_gproc_prop({State, ringing, oncall}),
			{reply, ok, oncall, NewState#state{state_data = update_state(oncall, Call)}};
		Else ->
			lager:warning("Didn't go oncall:  ~p", [Else]),
			{reply, {error, Else}, ringing, State}
	end;

ringing({oncall, #call{id = Id}}, _From, #state{agent_rec = Agent, state_data = #call{id = Id} = Call} = State) ->
	Now = ouc_time:now(),
	lager:debug("Moving from ringing to oncall state", []),
	conn_cast(State#state.agent_connection, set_channel_msg(oncall, Call)),
	cpx_agent_event:change_agent_channel(Agent, self(), oncall, Call, Now),
	agent:set_channel(Agent#agent.source, self(), oncall),
	set_gproc_prop({State, ringing, oncall}),
	{reply, ok, oncall, State#state{state_data = update_state(oncall, Call)}};

ringing(stop, _From, #state{endpoint = Ep, state_data = Call} = State) ->
	cpx_endpoint:hangup(Ep),
	{stop, normal, ok, State#state{state_data = update_state(hangup, Call)}};

ringing(_Msg, _From, State) ->
	lager:info("Msg ~p not understood", [_Msg]),
	{reply, {error, invalid}, ringing, State}.

%% -----

ringing(_Msg, State) ->
	{next_state, ringing, State}.

% ======================================================================
% PRECALL
% ======================================================================

precall({oncall, #call{client = Client} = Call}, _From, #state{agent_rec = Agent, state_data = Client} = State) ->
	Now = ouc_time:now(),
	lager:debug("Moving from precall to oncall state", []),
	conn_cast(State#state.agent_connection, set_channel_msg(oncall, Call)),
	cpx_agent_event:change_agent_channel(Agent, self(), oncall, Call, Now),
	agent:set_channel(Agent#agent.source, self(), oncall),
	set_gproc_prop({State, precall, oncall}),
	{reply, ok, oncall, State#state{state_data = update_state(oncall, Call)}};

precall({oncall, #call{id = Id} = Call}, _From, #state{agent_rec = Agent, state_data = #call{id = Id}} = State) ->
	Now = ouc_time:now(),
	lager:debug("Moving from precall to oncall", []),
	conn_cast(State#state.agent_connection, set_channel_msg(oncall, Call)),
	cpx_agent_event:change_agent_channel(Agent, self(), oncall, Call, Now),
	agent:set_channel(Agent#agent.source, self(), oncall),
	set_gproc_prop({State, precall, oncall}),
	{reply, ok, oncall, State#state{state_data = update_state(oncall, Call)}};

precall(_Msg, _From, State) ->
	lager:info("Msg ~p not understood", [_Msg]),
	{reply, {error, invalid}, precall, State}.

%% -----

precall({mediapush, From, Callrec, Data}, #state{state_data = #call{source = From}, agent_connection = Conn} = State) when is_pid(Conn) ->
	Self = self(),
	conn_cast(Conn, {mediapush, Self, Callrec, Data}),
	{next_state, precall, State};

precall(_Msg, State) ->
	{next_state, precall, State}.

% ======================================================================
% ONCALL
% ======================================================================

% TODO the two clauses below are no longer used as warmtransfer has been
% moved to a media specific set-up.
oncall(warmtransfer_hold, _From, #state{state_data = Call} = State) ->
	lager:debug("Moving from oncall to warmtransfer_hold", []),
	conn_cast(State#state.agent_connection, set_channel_msg(warmtransfer_hold, Call)),
	set_gproc_prop({State, oncall, warmtransfer_hold}),
	{reply, ok, warmtransfer_hold, State#state{state_data = update_state(warmtransfer_hold, Call)}};
oncall({warmtransfer_3rd_party, Data}, From, State) ->
	case oncall(warmtransfer_hold, From, State) of
		{reply, ok, warmtransfer_hold, NewState} ->
			warmtransfer_hold({warmtransfer_3rd_party, Data}, From, NewState);
		Else ->
			Else
	end;

%% -----
% oncall({queue_transfer, QueueBin}, From, State) when is_binary(QueueBin) ->
% 	oncall({queue_transfer, binary_to_list(QueueBin)}, From, State);

oncall({queue_transfer, Queue, Opts}, From,
	#state{state_data = #call{source = CallPid}} = State) ->
	QueueTransfer = fun() ->
		gen_media:queue(CallPid, Queue, Opts)
	end,
	handle_transfer_wrapup(QueueTransfer, From, State);

oncall({agent_transfer, AgentLogin}, From,
	#state{state_data = #call{source = CallPid}} = State) ->
	AgentTransfer = fun() ->
		gen_media:agent_transfer(CallPid, AgentLogin)
	end,
	handle_transfer_wrapup(AgentTransfer, From, State);

oncall({outband_transfer, AgentLogin}, From,
	#state{state_data = #call{source = CallPid}} = State) ->
	AgentTransfer = fun() ->
		gen_media:transfer_outband(CallPid, AgentLogin)
	end,
	handle_transfer_wrapup(AgentTransfer, From, State);

%% -----
oncall(wrapup, From, #state{state_data = Call} = State) ->
	oncall({wrapup, Call}, From, State);

oncall({wrapup, #call{id = Id}=Call}, {From, _Tag}, #state{agent_rec = Agent, state_data = #call{id = Id}} = State) ->
	Now = ouc_time:now(),

	% {ok, Queue} = call_queue_config:get_queue(Call#call.queue),
	% TestQueue = Queue#call_queue{wrapup_enabled = true, auto_wrapup = 5000},
	case Call#call.wrapup_enabled of
		true ->
			case Call#call.source of
				From ->
					%% hmm. should be avoided... this means gen_media called wrapup on agent_channel
					%% ideally, only agent_channel should be the one calling wrapup to gen_media

					lager:debug("Moving from oncall to wrapup", []),

					conn_cast(State#state.agent_connection, set_channel_msg(wrapup, Call)),
					cpx_agent_event:change_agent_channel(Agent, self(), wrapup, Call, Now),
					agent:set_channel(Agent#agent.source, self(), wrapup),
					prep_autowrapup(Call),
					set_gproc_prop({State, oncall, wrapup}),
					{reply, ok, wrapup, State#state{state_data = update_state(wrapup, Call)}};
				_CallSource ->
					{Rep, Next, State1} = try_wrapup(State, Now),
					{reply, Rep, Next, State1}
			end;
		false ->
			CallPid = Call#call.source,
			try gen_media:wrapup(CallPid) of
				ok ->
					lager:debug("Ending call; Skipping wrapup", []),
					{stop, normal, ok, State#state{state_data = update_state(stop, Call)}};
				_ ->
					{reply, ok, oncall, State}
			catch
				exit:{noproc, _} ->
					lager:info("gen_media: ~p is gone, proceeding anyway", [CallPid]),
					{stop, normal, ok, State#state{state_data = update_state(stop, Call)}}
			end
	end;

oncall(hold, _From, #state{state_data = Call} = State) ->
	MediaPid = Call#call.source,
	gen_media:hold(MediaPid),
	{reply, ok, oncall, State};

oncall(unhold, _From, #state{state_data = Call} = State) ->
	MediaPid = Call#call.source,
	gen_media:unhold(MediaPid),
	{reply, ok, oncall, State};

oncall({play, Opts}, _From, #state{state_data = Call} = State) ->
	MediaPid = Call#call.source,
	gen_media:play(MediaPid, Opts),
	{reply, ok, oncall, State};

oncall(pause, _From, #state{state_data = Call} = State) ->
	MediaPid = Call#call.source,
	gen_media:pause(MediaPid),
	{reply, ok, oncall, State};

oncall(_Msg, _From, State) ->
	lager:info("Msg ~p not understood", [_Msg]),
	{reply, {error, invalid}, oncall, State}.

%% -----

oncall({mediapush, From, Callrec, Data}, #state{state_data = #call{source = From}, agent_connection = Conn} = State) when is_pid(Conn) ->
	Self = self(),
	conn_cast(Conn, {mediapush, Self, Callrec, Data}),
	{next_state, oncall, State};

oncall(_Msg, State) ->
	{next_state, oncall, State}.

% ======================================================================
% WARMTRANSFER_HOLD
% ======================================================================

% TODO depricated state
warmtransfer_hold(oncall, _From, #state{state_data = Call} = State) ->
	lager:debug("Moving from warmtransfer_hold to oncall", []),
	conn_cast(State#state.agent_connection, set_channel_msg(oncall, Call)),
	set_gproc_prop({State, warmtransfer_hold, oncall}),
	{reply, ok, oncall, State#state{state_data = update_state(oncall, Call)}};
warmtransfer_hold({warmtransfer_3rd_party, Data}, _From, #state{state_data = Call} = State) ->
	lager:debug("Moving from warmtransfer_hold to warmtransfer_3rd_party", []),
	conn_cast(State#state.agent_connection, set_channel_msg(warmtransfer_3rd_party, Call)),
	{reply, ok, warmtransfer_3rd_party, State#state{state_data = {update_state(warmtransfer_3rd_party, Call), Data}}};
warmtransfer_hold(wrapup, _From, #state{state_data = Call} = State) ->
	lager:debug("Moving from warmtransfer_hold to wrapup", []),
	conn_cast(State#state.agent_connection, set_channel_msg(wrapup, Call)),
	set_gproc_prop({State, warmtransfer_hold, wrapup}),
	{reply, ok, wrapup, State#state{state_data = update_state(wrapup, State#state.state_data)}};
warmtransfer_hold(_Msg, _From, State) ->
	{reply, {error, invalid}, warmtransfer_hold, State}.

warmtransfer_hold(_Msg, State) ->
	{next_state, warmtransfer_hold, State}.

% ======================================================================
% WARMTRANSFER_3RD_PARTY
% ======================================================================

% TODO depricated state
warmtransfer_3rd_party(warmtransfer_hold, _From, #state{state_data = {Call, _}} = State) ->
	lager:debug("Moving from warmtransfer_3rd_party to warmtransfer_hold", []),
	conn_cast(State#state.agent_connection, set_channel_msg(warmtransfer_hold, Call)),
	set_gproc_prop({State, warmtransfer_3rd_party, warmtransfer_hold}),
	{reply, ok, warmtransfer_hold, State#state{state_data = update_state(warmtransfer_hold, Call)}};
warmtransfer_3rd_party(oncall, _From, #state{state_data = {Call, _}} = State) ->
	lager:debug("Moving from warmtransfer_3rd_party to oncall", []),
	conn_cast(State#state.agent_connection, set_channel_msg(oncall, Call)),
	set_gproc_prop({State, warmtransfer_3rd_party, oncall}),
	{reply, ok, oncall, State#state{state_data = update_state(oncall, Call)}};
warmtransfer_3rd_party(wrapup, _From, #state{state_data = {Call, _}} = State) ->
	lager:debug("Moving from warmtransfer_3rd_party to wrapup", []),
	conn_cast(State#state.agent_connection, set_channel_msg(wrapup, Call)),
	set_gproc_prop({State, warmtransfer_3rd_party, wrapup}),
	{reply, ok, wrapup, State#state{state_data = update_state(wrapup, Call)}};
warmtransfer_3rd_party(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

warmtransfer_3rd_party(_Msg, State) ->
	{next_state, warmtransfer_3rd_party, State}.

% ======================================================================
% WRAPUP
% ======================================================================

% no calls to the cpx_agent_event as monitoring should be enough.
wrapup(stop, _From, State) ->
	{stop, normal, ok, State};
wrapup(_Msg, _From, State) ->
	{reply, ok, wrapup, State}.

wrapup(stop, State) ->
	{stop, normal, State};
wrapup(_Msg, State) ->
	{next_state, wrapup, State}.


% ======================================================================
% HANDLE_EVENT
% ======================================================================
handle_event({subscribe_events, Handler, Args}, StateName, State) ->
	gen_event:add_handler(State#state.event_manager, Handler, Args),
	{next_state, StateName, State};

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

% ======================================================================
% HANDLE_SYNC_EVENT
% ======================================================================

handle_sync_event(get_agent, _From, StateName, State) ->
	{reply, {ok, State#state.agent_rec}, StateName, State};

handle_sync_event(query_state, _From, StateName, State) ->
	{reply, {ok, StateName}, StateName, State};

handle_sync_event({set_connection, Pid}, _From, StateName, #state{agent_connection = _AgentConn, state_data = Call} = State) ->
	conn_cast(Pid, set_channel_msg(StateName, Call)),
	case cpx_supervisor:get_value(motd) of
		{ok, Motd} ->
			conn_cast(Pid, {blab, Motd});
		_ ->
			ok
	end,
	{reply, ok, StateName, State#state{agent_connection = Pid}};

handle_sync_event({url_pop, URL, Name}, _From, StateName, #state{agent_connection = Connection} = State) when is_pid(Connection) ->
	conn_cast(Connection, {url_pop, URL, Name}),
	{reply, ok, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

% ======================================================================
% HANDLE_INFO
% ======================================================================

handle_info({'EXIT', Pid, Why}, _StateName, #state{agent_fsm = Pid} = State) ->
	lager:info("Exit of agent fsm due to ~p", [Why]),
	{stop, Why, State};
handle_info({'EXIT', Pid, Why}, StateName, #state{endpoint = Ep} = State) ->
	case cpx_endpoint:get_pid(Ep) of
		Pid ->
			handle_endpoint_exit(StateName, State, Why);
		_ ->
			{next_state, StateName, State}
	end;

handle_info(end_wrapup, wrapup, State) ->
	{stop, normal, State};

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

% ======================================================================
% TERMINATE
% ======================================================================

terminate(_Reason, StateName, State) ->
	set_gproc_prop({State, StateName, stop}),

	case StateName of
		wrapup -> cdr:endwrapup(State#state.state_data, State#state.agent_login);
		_ -> ok
	end,

	Agent = agent:dump_state(State#state.agent_fsm),
	Call = update_state(stop, State#state.state_data),
	gen_event:notify(State#state.event_manager, {channel_feed, {terminated_channel, os:timestamp(), Agent, Call}}),
	agent:set_channel(Agent#agent.source, self(), undefined),
	ok.

% ======================================================================
% CODE_CHANGE
% ======================================================================

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

% ======================================================================
% CODE_CHANGE
% ======================================================================

%format_status(normal, [PDict, State]) ->
%	[{data, [{"State", format_status(terminate, [PDict, State])}]}];
%format_status(terminate, [_PDict, #state{agent_rec = Agent} = _State]) ->
%	% prevent client data from being dumped
%	Newagent = case Agent#agent.statedata of
%		#call{client = Client} = Call when is_record(Call#call.client, client) ->
%			Client = Call#call.client,
%			Agent#agent{statedata = Call#call{client = Client#client{options = []}}};
%		{onhold, #call{client = Client} = Call, calling, ID} when is_record(Client, client) ->
%			Agent#agent{statedata = {onhold, Call#call{client = Client#client{options = []}}, calling, ID}};
%		_ ->
%			Agent
%	end,
%	[Newagent].


% ======================================================================
% INTERNAL FUNCTIONS
% ======================================================================

conn_cast(Agent, Msg) when is_record(Agent, agent) ->
	conn_cast(Agent#agent.connection, Msg);
conn_cast(undefined, _Msg) ->
	ok;
conn_cast(Conn, Msg) when is_pid(Conn) ->
	Conn ! {agent, Msg}.

% start_endpoint(Pid, Agent, Call) when is_pid(Pid) ->
% 	link(Pid),
% 	Pid ! {prering, {Agent, self()}, Call},
% 	{ok, Pid};
start_endpoint({EndpointCbk, Opts}, Agent, Call) ->
	% case apply(Mod, Func, [Agent, self(), Call | XtraArgs]) of
	Opts1 = [
		{agent, Agent},
		{agent_channel_pid, self()},
		{call, Call}
	| Opts],

	case cpx_endpoint:start(EndpointCbk, Opts1) of
		{ok, Ep} ->
			Pid = cpx_endpoint:get_pid(Ep),
			link(Pid),
			{ok, Ep};
		Else ->
			{error, Else}
	end;
start_endpoint(E, _, _) ->
	{error, {badendpoint, E}}.

prep_autowrapup(#call{auto_wrapup = AutoWrapup} = _Call) ->
	lager:debug("Prep AutoWrapup called with value: ~p", [AutoWrapup]),
	case AutoWrapup of
		N when is_integer(N) andalso N > 0 ->
			Self = self(),

			lager:debug("Sending EndWrapup after ~p, Time is: ~p",
				[AutoWrapup,util:now_ms()]),
			erlang:send_after(N, Self, end_wrapup);
		_ ->
			ok
	end.

init_gproc_prop({State, PrevChannelState, ChannelState}) ->
	Prop = get_agent_channel_prop(State, ChannelState),
	gproc:reg({p, l, cpx_agent_channel}, Prop),

	% TODO send cpx_agent_channel_init event
	Event = #cpx_agent_channel_state_update{pid=self(), agent_pid=State#state.agent_fsm, now=now(), state=ChannelState, old_state=PrevChannelState, prop=Prop},
	gproc:send({p, l, cpx_agent_channel_change}, Event).

set_gproc_prop({State, PreviousStateName, StateName}) ->
	Prop = get_agent_channel_prop(State, StateName),
	gproc:set_value({p, l, cpx_agent_channel}, Prop),

	Event = #cpx_agent_channel_state_update{pid=self(), agent_pid=State#state.agent_fsm, now=now(), state=StateName, old_state=PreviousStateName, prop=Prop},
	gproc:send({p, l, cpx_agent_channel_change}, Event).

-spec get_agent_channel_prop(#state{}, atom()) -> #cpx_agent_channel_prop{}.
get_agent_channel_prop(FsmState, ChannelState) ->
	Login = FsmState#state.agent_login,
	Profile = FsmState#state.agent_profile,
	Type = FsmState#state.media_type,
	Client = FsmState#state.client,
	CallerId = FsmState#state.callerid,
	CallId = get_call_id(FsmState#state.state_data),
	#cpx_agent_channel_prop{login=Login, profile=Profile, type=Type, client=Client, callid=CallId, callerid=CallerId, state=ChannelState}.

get_call_id(#call{id = Id}) ->
	Id;
get_call_id(_) ->
	undefined.

-spec update_state(NewSt :: atom(), Call :: #call{} | {#call{}, term()}) -> Call :: #call{} | {#call{}, term()}.
update_state(NewSt, #call{state_changes = Changes} = Call) ->
	UpdatedChanges = [{NewSt, os:timestamp()} | Changes],
	Call#call{state_changes = UpdatedChanges};
update_state(_, CallData) ->
	CallData.

-spec set_channel_msg(NewSt::atom(), Call::#call{}) ->
	{set_channel, pid(), atom(), #call{}}.
set_channel_msg(NewSt, Call) ->
	{set_channel, self(), NewSt, update_state(NewSt, Call)}.

try_wrapup(State, Now) ->
	Call = State#state.state_data,
	CallPid = Call#call.source,
	Agent = State#state.agent_rec,
	{Rep, Next} = try gen_media:wrapup(CallPid) of
		ok ->
			lager:debug("Moving from oncall to wrapup", []),
			{ok, wrapup};
		Else ->
			{Else, oncall}
	catch
		exit:{noproc, _} ->
			lager:info("gen_media: ~p is gone, proceeding anyway", [CallPid]),
			{ok, wrapup}
	end,

	State1 = case Next of
		wrapup ->
			conn_cast(State#state.agent_connection, set_channel_msg(wrapup, Call)),
			cpx_agent_event:change_agent_channel(Agent, self(), wrapup, Call, Now),
			agent:set_channel(Agent#agent.source, self(), wrapup),
			prep_autowrapup(Call),
			set_gproc_prop({State, oncall, wrapup}),
			State#state{state_data = update_state(wrapup, Call)};
		_ ->
			State
	end,

	{Rep, Next, State1}.

force_wrapup(State, Now) ->
	Call = State#state.state_data,
	Agent = State#state.agent_rec,
	lager:debug("Moving from oncall to wrapup", []),

	conn_cast(State#state.agent_connection, set_channel_msg(wrapup, Call)),
	cpx_agent_event:change_agent_channel(Agent, self(), wrapup, Call, Now),
	agent:set_channel(Agent#agent.source, self(), wrapup),
	prep_autowrapup(Call),
	set_gproc_prop({State, oncall, wrapup}),
	State1 = State#state{state_data = update_state(wrapup, Call)},

	{ok, wrapup, State1}.

handle_endpoint_exit(ringing, State, call_expired) ->
	lager:info("Exit of endpoint ~p due to ~p while ringing, setting agent to released before stopping", [State#state.endpoint, call_expired]),
	ConnMsg = {forced_release, ring_init_failed},
	{ok, AutoRelease} = cpx:get_env(release_on_ring_failure, true),
	case AutoRelease of
		true ->
			agent:set_release(State#state.agent_fsm, ?DEFAULT_RELEASE, ConnMsg);
		_ ->
			ok
	end,
	{stop, ring_init_failed, State};
handle_endpoint_exit(wrapup, State, Reason) ->
	State1 = State#state{endpoint = undefined},
	lager:debug("Exit of endpoint due to ~p while in wrapup; ignorable", [Reason]),
	{next_state, wrapup, State1};
handle_endpoint_exit(oncall, State, Reason) ->
	Now = ouc_time:now(),
	Call = State#state.state_data,
	CallPid = Call#call.source,
	lager:info("Exit of endpoint ~p due to ~p while oncall; moving ~p to wrapup.", [State#state.endpoint, Reason, CallPid]),
	{_Rep, Next, State1} = try_wrapup(State, Now),
	State2 = State1#state{endpoint = undefined},
	{next_state, Next, State2};
handle_endpoint_exit(StName, State, Reason) ->
	lager:info("Exit of endpoint due to ~p while ~p. exit", [StName, Reason]),
	{stop, Reason, State}.

handle_transfer_wrapup(TransferFun, From, #state{agent_rec=Agent,
	agent_connection=AgentConn, state_data=Call} = State) ->
	Now = ouc_time:now(),
	case TransferFun() of
		ok ->
			% lager:info("Moving from oncall to wrapup after call transfer"),
			% conn_cast(AgentConn, set_channel_msg(wrapup, Call)),
			% cpx_agent_event:change_agent_channel(Agent, self(), wrapup, Call, Now),
			% prep_autowrapup(Call),
			% set_gproc_prop({State, oncall, wrapup}),
			% {reply, ok, wrapup, State#state{state_data = update_state(wrapup, Call)}};
			case Call#call.wrapup_enabled of
				true ->
					case Call#call.source of
						From ->
							%% hmm. should be avoided... this means gen_media called wrapup on agent_channel
							%% ideally, only agent_channel should be the one calling wrapup to gen_media

							lager:debug("Moving from oncall to wrapup after call transfer", []),

							conn_cast(AgentConn, set_channel_msg(wrapup, Call)),
							cpx_agent_event:change_agent_channel(Agent, self(), wrapup, Call, Now),
							agent:set_channel(Agent#agent.source, self(), wrapup),
							prep_autowrapup(Call),
							set_gproc_prop({State, oncall, wrapup}),
							{reply, ok, wrapup, State#state{state_data = update_state(wrapup, Call)}};
						_CallSource ->
							{Rep, Next, State1} = force_wrapup(State, Now),
							{reply, Rep, Next, State1}
					end;
				false ->
					lager:debug("Ending channel; Skipping wrapup", []),
					{stop, normal, ok, State#state{state_data = update_state(stop, Call)}}
			end;
		Else ->
			lager:warning("Didn't queue transfer:  ~p", [Else]),
			{reply, {error, Else}, oncall, State}
	end.

% ======================================================================
% TESTS
% ======================================================================

-ifdef(TEST).

t_call_pid() ->
	erlang:list_to_pid("<0.1.2>").

t_call() ->
	#call{id = "callid", source = t_call_pid()}.

t_st() ->
	#state{state_data = t_call()}.

public_api_test_() ->
	{foreach, fun() ->
		meck:new(gen_fsm, [unstick])
	end,
	fun(_) ->
		meck:unload(gen_fsm)
	end, [

	% fun(_) -> {"start/2, simple_sucess", fun() ->
	% 	meck:expect(gen_fsm, start, fun(?MODULE, [agentrecord, options], []) ->
	% 		?assert(true)
	% 	end),

	% 	start(agentrecord, options),
	% 	?assertEqual(1, length(meck:history(gen_fsm))),
	% 	?assert(meck:validate(gen_fsm))
	% end} end,

	fun(_) -> {"start/5, simple_sucess", fun() ->
		meck:expect(gen_fsm, start, fun(?MODULE, [agentrecord, callrecord,
			endpointdata, initstate, ev_manager], []) ->
			?assert(true)
		end),

		start(agentrecord, callrecord, endpointdata, initstate, ev_manager),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))
	end} end,

	% fun(_) -> {"start_link/2, simple_sucess", fun() ->
	% 	meck:expect(gen_fsm, start_link, fun(?MODULE, [agentrecord, options], []) ->
	% 		?assert(true)
	% 	end),

	% 	start_link(agentrecord, options),
	% 	?assertEqual(1, length(meck:history(gen_fsm))),
	% 	?assert(meck:validate(gen_fsm))
	% end} end,

	fun(_) -> {"start_link/5, simple_sucess", fun() ->
		meck:expect(gen_fsm, start_link, fun(?MODULE, [agentrecord,
			callrecord, endpointdata, initstate, ev_manager], []) ->
			?assert(true)
		end),

		start_link(agentrecord, callrecord, endpointdata, initstate, ev_manager),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))
	end} end,

	fun(_) -> {"stop/1, simple_sucess", fun() ->
		meck:expect(gen_fsm, sync_send_event, fun(pid, stop) ->
			?assert(true)
		end),

		stop(pid),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))
	end} end

	]}.

handle_endpoint_exit_test_() ->
	{setup, fun() ->
		meck:new(agent),
		meck:expect(agent, set_release, 3, ok)
	end, fun(_) ->
		meck:unload()
	end, [
	{"call expired when ringing", fun() ->
		?assertEqual({stop, ring_init_failed, #state{}},
			handle_endpoint_exit(ringing, #state{}, call_expired))
	end}]}.

hold_test_() ->
	{setup, fun() ->
		meck:new(gen_media),
		meck:expect(gen_media, hold, 1, ok),
		meck:expect(gen_media, unhold, 1, ok)
	end, fun(_) ->
		meck:unload(gen_media)
	end, [{"hold", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall(hold, from, St)),
		?assert(meck:called(gen_media, hold, [t_call_pid()], self()))
	end}, {"unhold", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall(unhold, from, St)),
		?assert(meck:called(gen_media, unhold, [t_call_pid()], self()))
	end}]}.

playback_control_test_() ->
	{setup, fun() ->
		meck:new(gen_media),
		meck:expect(gen_media, play, 1, ok),
		meck:expect(gen_media, play, 2, ok),
		meck:expect(gen_media, pause, 1, ok)
	end, fun(_) ->
		meck:unload(gen_media)
	end, [{"play with opts", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall({play, {[]}}, from, St))
	end}, {"pause", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall(pause, from, St))
	end}]}.

-endif.
