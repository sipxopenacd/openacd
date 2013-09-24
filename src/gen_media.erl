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
%%

%% @doc Behaviour module for media types.  Gen_media uses gen_fsm as the
%% underlying framework for what it does.  It exposes a gen_server-esque
%% behavior for it's callback modules, however.  Any time gen_media
%% recieves an event it cannot handle wholly internally, it will call a
%% specific funciton of the callback module.
%%
%% Replies from handle_call, handle_cast, and handle_info are extended to
%% allow for specific events only media would need.
%%
%%	Callback functions:
%%
%%	The callback functions follow a general pattern for their arguments
%%	(aside from init).  It is:
%%		[Arg1, Arg2, Arg3, ..., ArgN, StateName, Call, InternalState, State]
%%
%%	Arg1 ... ArgN are arbitrary terms defined in the documenation for
%%	each callback.
%%
%%	StateName is the current state of the gen_media fsm.  The states most
%%	commonly used are inivr, inqueue, inqueue_ringing, oncall,
%%	oncall_ringing, and wrapup.
%%
%%	Call is the most recently #call{}.
%%
%%	InternalState is the internal state record of the gen_media fsm with
%%	the most pertinant data.  These are defined in gen_media.hrl.
%%
%%	State is the state the callback module last returned from a callback
%%	function.  It is used for implementation specific data for medias.
%%
%%	<b>init(Args) -> {ok, {State, InitPs}}</b>
%%		types:  Args = any()
%%				State = any()
%%              InitPs = init_p()
%%              init_p() = {id, string()}
%%                  | {type, call_type()}
%%                  | {client, client()}
%%                  | {skills, skills()}
%%                  | {caller_id, {string(), string()}},
%%                  | {dnis, string()},
%%                  | {ring_path, inband | outband},
%%                  | {media_path, inband | outband},
%%                  | {direction, inbound | outbound}
%%                  | {priority, pos_integer()},
%%                  | {info, [{atom(), atom() | number() | string()}]}
%%                  | {queue, string()}
%%
%%		When gen_media starts, this function is called.  It should
%%		initialize all required data.
%%
%%		Some media may not be able to provide a call record on start-up,
%%		thus allowing the media to finish prepping and then queue later.
%%		If a `queue' is provided then the media goes into inqueue directly.
%%
%%	<b>urlpop_getvars(State) -> UrlOptions</b>
%%		types:	State = any()
%%				UrlOptions = [{string(), string()}]
%%
%%		When a call rings to an agent, if a pop url is configured, get
%%		variables are appended to the end.  If this is set,
%%		get_url_getvars/1 will call it and merge the results to what will
%%		be used when ringing an agent.  Options set via set_url_getvars/2
%%		super-ceede those returned by the callback module.
%%
%%		At the time of this doc, the agent web interface prompts the agent
%%		to adjust the url pop options when doing a queue transfer.
%%
%%	<b>prepare_endpoint(Agent, Data) -> Result</b>
%%		types:	Agent = #agent{}
%%				Data = 'inband' | any()
%%				Result = {ok, NewData} | {error, Error}
%%					NewData = any()
%%					Error = any()
%%
%%		When an agent is given a new endpoint for the callback module, this
%%		function is called.  If the callback module returns {error, Error}
%% 		The agent does not store the given endpoint data.  If {ok, NewData}
%% 		is returned, NewData is stored for the endpoint.
%%
%%		The atom 'inband' indicates an agent will go ringing despite the
%%		presense or absence of a ring pid.  If this behavior is not desired,
%%		Module:prepare_endpoint/2 should return {error, any()}, preserving
%%		any settings in place already.  If there were no settings, the
%%		endpoint is no longer used, and any media requiring it will fail
%%		to ring to the agent.
%%
%%	<b>handle_ring(RingData, Agent, Call, State) -> Result</b>
%%		types:	RingData = any()
%%				Agent = pid()
%%				Call = #call{}
%%				State = any()
%%				Result = {ok, NewState} | {ok, UrlOptions, NewState} |
%%					{invalid, NewState}
%%					UrlOptions = [{string(), string()}]
%%					NewState = any()
%%
%%		When a call must ring to an agent (either due to out of queue or
%%		the start of a transfer), this is called.
%%
%%		RingData is the data the ring channel returned when confirming it is
%%		able to function.  Gen media does not alter or cache it.
%%
%%		Agent is the pid of the agent that will be set to ringing if
%%		Result is {ok, NewState} or {ok, UrlOptions, NewState}.
%%
%%		Call is the #call{} maintained by the gen_media and passed in for
%%		Reference.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState} or {ok, UrlOptions, NewState}, Agent
%%		is set to ringing, and execution continues with NewState.  A
%%		url_pop is sent to the agent is the client for the media is set to
%%		have one.  In the case of {ok, UrlOptions, NewState}, the
%%		UrlOptions are appened to the url as a query (get) string.
%%
%%		Note that UrlOptions can be set by the agent before a queue
%%		transfer occurs.  In this case, before the transfer is made,
%%		urlpop_getvars/1 should be used to present the agent with a chance
%%		to adjust the url pop.
%%
%%		If Result is {invalid, NewState}, Agent is set to idle, and
%%		execution continues with NewState.
%%
%%	<b>handle_ring_stop(StateName, Call, Internal, State) -> Result</b>
%%		types:	StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState}
%%					NewState = any()
%%
%%		When an agent should no longer be ringing, such as due to ringout,
%%		this function is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		Execution will continue with NewState.
%%
%%	<b>handle_answer({Agent, Apid}, StateName, Call, Internal, State) ->
%%		Result</b>
%%		types:	Agent = string()
%%				Apid = pid()
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState} | {error, Error, NewState}
%%					Error = NewState = any()
%%
%%		When an agent should be placed on call after ringing, this function
%%		is called.
%%
%%		Agent is the agent that will be set oncall if Result is
%%		{ok, NewState}.
%%
%%		Call is the #call{} the agent will be answering.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState} and the callpath is inband, it is
%%		assumed the agent has already set themselves oncall.  If it is out
%%		of band, the agent is set to oncall.  The callback module can
%%		always safely assume the agent is oncall.  Execution then
%%		continues with NewState.
%%
%%		If Result is {error, Error, NewState}, the agent's state is not
%%		changed and execution continues with NewState.
%%
%%	<b>handle_voicemail(StateName, Call, Internal, State) -> Result</b>
%%		types:	StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState} | {invalid, NewState}
%%
%%		This is an optional callback.
%%
%%		When a media should be removed from queue and moved to voicemail,
%%		this is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState}, the call is removed from queue and
%%		execution continues with NewState.
%%
%%		If Result is {invalid, NewState} execution continues with NewState.
%%
%%	<b>handle_annouce(Announce, StateName, Call, Internal, State) ->
%%		{ok, NewState}</b>
%%		types:	Announce = any()
%%				StateName = state_name()
%%				Call = any()
%%				Internal = internal_state()
%%				State = NewState = any()
%%
%%		This is an optional callback.
%%
%%		When a recipe calls for a call in queue to play an announcement, if
%%		this function is defined, it is called.  Execution then continues
%%		with NewState.
%%
%%	<b>handle_agent_transfer(Agent, Timeout, StateName, Call, Internal,
%%		State) -> Result</b>
%%		types:	Agent = pid()
%%				Timeout = pos_integer()
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState} | {error, Error, NewState}
%%					NewState = State = any()
%%					Error = any()
%%
%%		When a media should be transfered to another agent, this is the
%%		first step.  The target agent is set to prering, then this
%%		callback is used to verify that.  If the callback returns
%%		{ok, NewState}, execution continues with NewState, and gen_media
%%		handles with oncall or a ringout.
%%
%%		In the case of an outband ring, that process will send a takeover
%%		message to gen_media.
%%
%%	<b>handle_queue_transfer({Queue, Qpid}, StateName, Call, Internal,
%%		State) -> {ok, NewState}</b>
%%		types:	Queue = string()
%%				Qpid = pid
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = NewState = any()
%%
%%		When a media is placed back into queue from an agent, this is
%%		called to allow the media to do any required clean up or
%%		unbridging.  The Call is requeued at the priority it was initially
%%		queued at.  Execution then continues with NewState.
%%
%%		Queue is the name of the queue the media will be placed in; Qpid is
%%		the pid of said queue.
%%
%%	<b>handle_wrapup(From, StateName, Call, Internal, State) -> {Finality,
%%		NewState}</b>
%%		types:	From = {pid(), reference()}
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = NewState = any()
%%				Finality = ok | hangup
%%
%%		This callback is only used if the call record's media path is inband.
%%		When an agent goes to wrapup, this gives the callback a chance to
%%		do any clean-up needed.
%%
%%		If the media determines this is a hang-up (ie, no more
%%		can be done with the media), it can return {hangup, NewState}.  The
%%		gen_media then terminates with state NewState.
%%
%%		If {ok, NewState} is returned, execution continues with state
%%		NewState.
%%
%%	<b>handle_spy(Spy, StateName, Call, Internal, State) -> {ok, NewState}
%%		| {invalid, NewState} | {error, Error, NewState}</b>
%%		types:  Spy = {Spypid, AgentRec}
%%				Spypid = pid() | any()
%%				AgentRec = 'undefined' | #agent{}
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = NewState = any()
%%
%%		This callback is optional.
%%
%%		Spy can be a pid of an agent acting as a spy, or a generic term to
%%		be used by the media to allow spying.  When spy is an active, agent,
%%		They must be released.
%%
%%		This signals the callback that a supervisor is attempting to observe
%%		the agent that is oncall.  The other callbacks should take into
%%		account the possibility of a spy if 'ok' is returned.
%%
%%		Be aware that when calling this, gen_media does not have a
%%		reliable method to determine an agent's security level.  The agent
%%		connections, however, do.
%%
%%	<b>Extended gen_server Callbacks</b>
%%
%%	In addition to the usual replies gen_server expects from it's callback
%%	of handle_call/3, handle_cast/2, and handle_info/2, gen_media will
%%	take some action based on the following Returns.
%%
%%	{queue, Queue, Callrec, NewState}
%%		types:  Queue = string()
%%				Callrec = #call{}
%%				NewState = any()
%%
%%		This result is only valid if the callbacks init/1 returned
%%		undefined for the call record.  This sets the call record and
%%		queues the call.  Execution then continues on with NewState.  If
%%		this is replied to a call, ok is set as the reply.
%%
%%	{outbound, Agent, NewState}
%%	{outbound, Agent, Call, NewState}
%%		types:  Agent = pid()
%%				Call = #call{}
%%				NewState = any()
%%
%%		This result is valid only if the call is not queued.  The second
%%		form is only valid if init/1 retuned an undefined call.  This also
%%		assumes the agent at pid() is already in precall state.  If The
%%		agent can be set to outgoing, it will be.  Execution continues on
%%		with NewState.
%%
%%	{voicemail, NewState}
%%		types:	NewState = any()
%%
%%		This result is valid only if the call is queued.  Removes the media
%%		from queue and stops ringing to an agent it is.  Assumes the media
%%		has already	done what it needs to for a voicemail.  If done in a
%%		handle_call, the reply is 'ok'.
%%
%%	{Agentaction, NewState}
%%	{Agentaction, Reply, NewState}
%%		types:	Agentaction = stop_ring | {stop_ring, Data} | wrapup | hangup |
%%					{hangup, Data} | {mediapush, Data, Mode}
%%				Reply = any()
%%				NewState = any()
%%				Data = any()
%%				Mode = replace | append
%%
%%		This result is only valid if an agent has been associated with this
%%		media by ringing.  The second form is only valid if the request
%%		came in	as a gen_media:call.  This attempts to take the specified
%%		action on the agent, then continues execution with NewState.
%%
%%		{stop_ring, Data} is used to stop the gen_media from handling a
%%		ringout.  It does not change the agent's state.  Execution will
%%		continue with NewState.  This is useful if there is an error
%%		ringing to an agent that only becomes apparent at a later time.  A
%%		return of `stop_ring' is Equivalent to {stop_ring, undefined}.
%%
%%		wrapup is only valid if there is an agent associated with a media,
%%		and	that agent is oncall or outgoing.  This sets the agent to
%%		wrapup and continues execution with NewState.
%%
%%		{hangup, Data} is valid at any time.  This will unqueue the media,
%%		and set the appropriate state for any agents.  The cdr record will
%%		record Data as who hung up the call.  A return of hangup is
%%		equivalent to {hangup, undefined}.  Execution then coninues with
%%		NewState.
%%
%%		mediapush is only valid if there is an agent oncall with the media,
%%		and the media is inband.  The given Data is casted to the
%%		associaed agent as a media push.
%%
%%	{stop, hangup, NewState}
%%	{stop, {hangup, Data}, NewState}
%%		types:  NewState = any()
%%				Data = any()
%%
%%		This causes the media to take any action it would from an
%%		Agentaction return tuple of hangup, then stop.

% TODO Less agent oriented and more agent channel oriented.
-module(gen_media).
-author(micahw).

-behaviour(gen_fsm).

-include("call.hrl").
-include("queue.hrl").
-include("agent.hrl").
-include("gen_media.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	behaviour_info/1,
	start_link/2,
	start/2,
	stop/1
]).

%% gen_fsm callbacks
-export([
	init/1,  terminate/3, code_change/4,
	handle_event/3, handle_sync_event/4, handle_info/3,
	inivr/2, inivr/3,
	inqueue/2, inqueue/3,
	inqueue_ringing/2, inqueue_ringing/3,
	oncall/2, oncall/3,
	oncall_ringing/2, oncall_ringing/3,
	wrapup/2, wrapup/3
]).

%% gen_media api
-export([
	ring/4,
	ring/3,
	takeover_ring/2,
	get_call/1,
	voicemail/1,
	transfer_outband/2,
	announce/2,
	%% TODO added for testing only (implemented with focus on real Calls - no other media)
	end_call/1,
	stop_ringing/1,
	oncall/1,
	queue/2,
	queue/3,
	agent_transfer/2,
	call/2,
	call/3,
	cast/2,
	wrapup/1,
	spy/3,
	set_cook/2,
	set_queue/2,
	set_queue/3,
	set_url_getvars/2,
	get_url_getvars/1,
	add_skills/2,
	remove_skills/2,
	hold/1,
	unhold/1,
	play/1,
	play/2,
	pause/1,

	conference_to_agent/2
]).

% TODO - add these to a global .hrl, cpx perhaps?
-type(proplist_item() :: atom() | {any(), any()}).
-type(proplist() :: [proplist_item()]).

%% gen_media states
-define(states, [
	inivr, inqueue, inqueue_ringing, oncall, oncall_ringing, wrapup
]).

%% state changes
%% init -> inivr, inqueue, oncall (in case of outbound)
%% inivr -> inqueue
%% inqueue -> inqueue_ringing
%% inqueue_ringing -> inqueue, oncall
%% oncall -> oncall_ringing, wrapup, warm_transfer_hold, inqueue
%% oncall_ringing -> oncall (same state), oncall (new agent)
%% wrapup -> *
%% warm_transfer_hold -> warm_transfer_3rd_party, oncall, wrapup
%% warm_transfer_3rd_party -> warm_transfer_merged, warm_transfer_hold
%% warm_transfer_merged -> oncall, wrapup

-define(GM(E, V), {{'$gen_media', E}, V}).
-define(GM(E), ?GM(E, undefined)).

-define(get(K, Vs), proplists:get_value(K, Vs)).
-define(get(K, Vs, D), proplists:get_value(K, Vs, D)).


-record(base_state, {
	callback :: atom(),
	substate :: any(),
	callrec :: 'undefined' | #call{},
	init_callrec :: 'undefined' | #call{},
	queue_failover,
	url_pop_get_vars = [],
	state_changes = [],
	agent :: #agent{},
	conference_channel
}).

%% callback response
-record(cbkr, {
	zt, %% fsm_state
	cbk_res,
	reply,
	action :: reply | {stop, any()},
	base_state,
	internal_state
}).

-spec(behaviour_info/1 ::
	(Info :: 'callbacks' | any()) -> [{atom(), non_neg_integer()}] | 'undefined').
behaviour_info(callbacks) ->
	[
		{prepare_endpoint, 2},
		{init, 1},
		{handle_ring, 4},
		{handle_ring_stop, 4},
		{handle_answer, 5},
		%{handle_voicemail, 4},
		%{handle_announce, 5},
		{handle_agent_transfer, 6},
		{handle_queue_transfer, 5},
		{handle_wrapup, 5},
		{handle_call, 6},
		{handle_cast, 5},
		{handle_info, 5},
		{handle_hold, 2},
		{handle_unhold, 2},
		{terminate, 5},
		{code_change, 6}
	];
behaviour_info(_Other) ->
    undefined.

%% @doc Make the `pid() Genmedia' ring to `pid() Agent' based off of
%% `#queued_call{} Qcall' with a ringout of `pos_integer() Timeout'
%% miliseconds.
%% @deprecated Use ring/3 instead as timout is ignored.  The ringout is
%% determined by the client option "ringout", the default value being
%% 60000.
-spec(ring/4 :: (Genmedia :: pid(), Agent :: pid() | string() | {string(), pid()}, Qcall :: #queued_call{}, Timeout :: pos_integer())  -> 'ok' | 'invalid' | 'deferred').
ring(Genmedia, {_Agent, Apid} = A, Qcall, Timeout) when is_pid(Apid) ->
	lager:info("Ring invoked to: ~p from ~p", [_Agent, self()]),
	gen_fsm:sync_send_event(Genmedia, ?GM(ring, {A, Qcall, Timeout}), infinity);

ring(Genmedia, Apid, Qcall, Timeout) when is_pid(Apid) ->
	case agent_manager:find_by_pid(Apid) of
		notfound ->
			invalid;
		Agent ->
			ring(Genmedia, {Agent, Apid}, Qcall, Timeout)
	end;
ring(Genmedia, Agent, Qcall, Timeout) ->
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			ring(Genmedia, {Agent, Apid}, Qcall, Timeout);
		false ->
			invalid
	end.

%% @doc Have the given gen_media ring the given agent based on the given
%% queued call.
-spec ring(Genmedia :: pid(),
	Agent :: pid() | string() | {string(), pid()},
	Qcall :: #queued_call{}) -> 'ok' | 'invalid' | 'deferred'.
ring(Genmedia, {_Agent, _Apid}=A, Qcall) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(ring, {A, Qcall, undefined}), infinity);

ring(Genmedia, Apid, Qcall) when is_pid(Apid) ->
	case agent_manager:find_by_pid(Apid) of
		notfound ->
			invalid;
		Agent ->
			ring(Genmedia, {Agent, Apid}, Qcall)
	end;

ring(Genmedia, Agent, Qcall) ->
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			ring(Genmedia, {Agent, Apid}, Qcall);
		false ->
			invalid
	end.


-spec(takeover_ring/2 :: (Genmedia :: pid(), Agent :: pid() | string() | {string(), pid()}) -> 'ok' | 'invalid').
takeover_ring(Genmedia, {_, Apid} = Agent) when is_pid(Apid) ->
	Self = self(),
	gen_fsm:send_event(Genmedia, ?GM(takeover_ring, {Agent, Self}));

takeover_ring(Genmedia, Apid) when is_pid(Apid) ->
	case agent_manager:find_by_pid(Apid) of
		notfound -> invalid;
		Agent -> takeover_ring(Genmedia, {Agent, Apid})
	end;

takeover_ring(Genmedia, Agent) ->
	case agent_manager:query_agent(Agent) of
		{true, Apid} -> takeover_ring(Genmedia, {Agent, Apid});
		false -> invalid
	end.

%% @doc Get the call record associated with `pid() Genmedia'.
-spec(get_call/1 :: (Genmedia :: pid()) -> #call{}).
get_call(Genmedia) ->
	gen_fsm:sync_send_all_state_event(Genmedia, ?GM(get_call), infinity).

%% @doc Send the passed `pid() Genmedia' to voicemail.
-spec(voicemail/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
voicemail(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(voicemail)).

%% @doc Pass `any() Annouce' message to `pid() Genmedia'.
-spec(announce/2 :: (Genmedia :: pid(), Annouce :: any()) -> 'ok').
announce(Genmedia, Annouce) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(announce, Annouce)).

%% TODO added for testing only (implemented with focus on real Calls - no other media)
%% @doc End the Call for `pid() Genmedia'.
-spec(end_call/1 :: (Genmedia :: pid()) -> 'ok').
end_call(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_end_call').

%% @doc Sends the oncall agent associated with the call to wrapup; or, if it's
%% the oncall agent making the request, gives the callback module a chance to
%% handle it.
-spec(wrapup/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
wrapup(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(wrapup)).

%% @doc Send a stop ringing message to `pid() Genmedia'.
-spec(stop_ringing/1 :: (Genmedia :: pid()) -> 'ok').
stop_ringing(Genmedia) ->
	stop_ringing(Genmedia, undefined).

%% @doc Send a stop ringing message to `pid() Genmedia' with reason.
-spec(stop_ringing/2 :: (Genmedia :: pid(), Reason :: atom()) -> 'ok').
stop_ringing(Genmedia, Reason) ->
	gen_fsm:send_event(Genmedia, ?GM(stop_ringing, Reason)).

%% @doc Set the agent associated with `pid() Genmedia' to oncall.
-spec(oncall/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
oncall(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(agent_oncall), infinity).

%% doc Transfer the call from the agent it is associated with to a new agent.
% -spec(agent_transfer/3 :: (Genmedia :: pid(), Apid :: pid() | string() | {string(), pid()}, Timeout :: pos_integer()) -> 'ok' | 'invalid').
% agent_transfer(Genmedia, {_Login, Apid} = Agent, Timeout) when is_pid(Apid) ->
% 	gen_fsm:sync_send_event(Genmedia, ?GM(agent_transfer, {Agent, Timeout}));
% agent_transfer(Genmedia, Apid, Timeout) when is_pid(Apid) ->
% 	case agent_manager:find_by_pid(Apid) of
% 		notfound ->
% 			invalid;
% 		Agent ->
% 			agent_transfer(Genmedia, {Agent, Apid}, Timeout)
% 	end;
% agent_transfer(Genmedia, Agent, Timeout) ->
% 	case agent_manager:query_agent(Agent) of
% 		false ->
% 			invalid;
% 		{true, Apid} ->
% 			agent_transfer(Genmedia, {Agent, Apid}, Timeout)
% 	end.

%% @doc Transfer the passed media into the given queue.
-spec(queue/2 :: (Genmedia :: pid(), Queue :: string()) -> 'ok' | 'invalid').
queue(Genmedia, Queue) ->
	queue(Genmedia, Queue, []).

%% @doc Transfer the passed media into the given queue.
-spec(queue/3 :: (Genmedia :: pid(), Queue :: string(), Opts :: list()) -> 'ok' | 'invalid').
queue(Genmedia, Queue, Opts) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(queue, {Queue, Opts})).

-spec(agent_transfer/2 :: (Genmedia :: pid(), AgentLogin :: list()) -> ok | error).
agent_transfer(Genmedia, AgentLogin) ->
	Opts = [{new_skills, [{'_agent', AgentLogin}]}],
	queue(Genmedia, "transfer_queue", Opts).

%% @doc Attempt to spy on the agent oncall with the given media.  `Spy' is
%% the pid to send media events/load data to, and `AgentRec' is an
%% `#agent{}' used to hold the end point data.
-spec(spy/3 :: (Genmedia :: pid(), Spy :: pid(), AgentRec :: #agent{}) -> 'ok' | 'invalid' | {'error', any()}).
spy(Genmedia, Spy, AgentRec) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(spy, {Spy, AgentRec})).

-spec(set_cook/2 :: (Genmedia :: pid(), CookPid :: pid()) -> 'ok').
set_cook(Genmedia, CookPid) ->
	gen_fsm:send_event(Genmedia, ?GM(set_cook, CookPid)).

-spec(set_queue/2 :: (Genmedia :: pid(), Queue :: pid() | {string(), pid()}) -> 'ok').
set_queue(Genmedia, Queue) ->
	set_queue(Genmedia, Queue, false).

-spec(set_queue/3 :: (Genmedia :: pid(), Queue :: pid() | {string(), pid()}, Reset :: boolean()) -> 'ok').
set_queue(Genmedia, Queue, Reset) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(set_queue, {Queue, Reset})).


-spec(set_url_getvars/2 :: (Genmedia :: pid(), Vars :: [{string(), string()}]) -> 'ok').
set_url_getvars(Genmedia, Vars) ->
	gen_fsm:sync_send_all_state_event(Genmedia, ?GM(set_url_getvars, Vars)).

-spec(get_url_getvars/1 :: (Genmedia :: pid()) -> {'ok', [{string(), string()}]}).
get_url_getvars(Genmedia) ->
	gen_fsm:sync_send_all_state_event(Genmedia, ?GM(get_url_vars)).

-spec(add_skills/2 :: (Genmedia :: pid(), Skills :: [atom() | {atom(), any()}]) -> 'ok').
add_skills(Genmedia, Skills) ->
	gen_fsm:send_all_state_event(Genmedia, ?GM(add_skills, Skills)).

-spec(remove_skills/2 :: (Genmedia :: pid(), Skills :: [atom() | {atom(), any()}]) -> 'ok').
remove_skills(Genmedia, Skills) ->
	gen_fsm:send_all_state_event(Genmedia, ?GM(remove_skills, Skills)).

transfer_outband(Genmedia, Addr) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(transfer_outband, Addr)).

%% @doc Puts the media on hold
-spec(hold/1 :: (Genmedia :: pid()) -> 'ok' | 'error').
hold(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(hold)).

%% @doc Puts the media off hold
-spec(unhold/1 :: (Genmedia :: pid()) -> 'ok' | 'error').
unhold(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(unhold)).

%% @doc Starts or resumes the media playback
-spec(play/1 :: (Genmedia :: pid()) -> 'ok' | 'error').
play(Genmedia) ->
	play(Genmedia, {[]}).

%% @doc Starts or resumes the media playback at a specified location
-spec(play/2 :: (Genmedia :: pid(), Opts :: json()) -> 'ok' | 'error').
play(Genmedia, Opts) ->
	gen_fsm:sync_send_event(Genmedia, ?GM({play, Opts})).

%% @doc Puts the media off hold
-spec(pause/1 :: (Genmedia :: pid()) -> 'ok' | 'error').
pause(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, ?GM(pause)).

%% @doc Do the equivalent of a `gen_server:call/2'.
-spec(call/2 :: (Genmedia :: pid(), Request :: any()) -> any()).
call(Genmedia, Request) ->
	gen_fsm:sync_send_all_state_event(Genmedia, Request).

%% @doc Do the equivalent of `gen_server:call/3'.
-spec(call/3 :: (Genmedia :: pid(), Request :: any(), Timeout :: pos_integer()) -> any()).
call(Genmedia, Request, Timeout) ->
	gen_fsm:sync_send_all_state_event(Genmedia, Request, Timeout).

%% @doc Do the equivalent of `gen_server:cast/2'.
-spec(cast/2 :: (Genmedia :: pid(), Request:: any()) -> 'ok').
cast(Genmedia, Request) ->
	gen_fsm:send_all_state_event(Genmedia, Request).

%% @doc Puts the media off hold
-spec(conference_to_agent/2 :: (Genmedia :: pid(), AgentLogin :: string()) -> 'ok' | 'error').
conference_to_agent(Genmedia, AgentLogin) ->
	gen_fsm:sync_send_event(Genmedia, ?GM({conference_to_agent, AgentLogin})).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a gen_media linked to the calling process.
-spec(start_link/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Callback, Args) ->
	gen_fsm:start_link(?MODULE, [Callback, Args], []).

-spec(start/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Callback, Args) ->
	gen_fsm:start(?MODULE, [Callback, Args], []).

stop(Genmedia) ->
	gen_fsm:sync_send_all_state_event(Genmedia, ?GM(stop)).

%%====================================================================
%% init callbacks
%%====================================================================

%% @private
init([Callback, Args]) ->
	InitTime = os:timestamp(),
	StateChanges = [{init, InitTime}],

	case Callback:init(Args) of
		{ok, SubState, Ps} ->
			QueueN = ?get(queue, Ps),

			Call = ps_to_call(Ps, undefined, Callback, StateChanges),

			BaseSt = #base_state{
				callback = Callback,
				substate = SubState,
				init_callrec = Call,
				callrec = Call,
				state_changes = StateChanges
			},

			cdr:cdrinit(Call),
			cdr:loginit(Call#call{call_segment = 1}),
			cdr:inivr(Call#call{call_segment = 1}, Call#call.dnis),
			{NSt,NBaseSt, NIntSt} = case QueueN of
				undefined ->
					{inivr, BaseSt, #inivr_state{}};
				_ ->
					case enqueue(QueueN, Call, BaseSt) of
						{ok, {BaseSt1, IntSt}} ->
							{inqueue, BaseSt1, IntSt};
						_ ->
							lager:warning("Unable to queue to :~p", [QueueN]),
							{inivr, BaseSt, #inivr_state{}}
					end
			end,

			init_gproc_prop(NSt, NBaseSt),
			gproc:reg({n, l, {?MODULE, Call#call.id}}),
			{ok, NSt, {NBaseSt, NIntSt}};
		{stop, Reason} = O ->
			lager:warning("init aborted due to ~p", [Reason]),
			O;
		ignore ->
			lager:warning("init told to ignore", []),
			ignore
	end.

%%--------------------------------------------------------------------
%% inivr -> inqueue
%%--------------------------------------------------------------------

%% sync

inivr(Msg, From, State) ->
	fallback_sync(inivr, Msg, From, State).

%% async

inivr(Msg, State) ->
	fallback_async(inivr, Msg, State).

%%--------------------------------------------------------------------
%% inqueue -> inqueue_ringing
%%--------------------------------------------------------------------

%% sync

inqueue(?GM(ring, {{Agent, Apid}, #queued_call{
		cook = Requester, skills = ESkills} = _QCall, _Timeout}), {Requester, _Tag}, {
		#base_state{callrec = Call} = BaseState,
		Internal}) ->
	lager:debug("Queued call: ~p", [_QCall]),
	ClientOpts = Call#call.client#client.options,
	TimeoutSec = proplists:get_value("ringout", ClientOpts, 60),
	Timeout = TimeoutSec * 1000,
	{Queue, _QPid} = Internal#inqueue_state.queue_pid,
	StateChanges = BaseState#base_state.state_changes,
	Call1 = Call#call{skills = ESkills, queue=Queue, state_changes = StateChanges},
	BaseState1 = BaseState#base_state{callrec = Call1},
	lager:info("Trying to ring ~p with ~p with timeout ~p", [Agent, Call1#call.id, Timeout]),
	try agent:prering(Apid, Call1) of
		{ok, RPid} ->
			Rmon = erlang:monitor(process, RPid),
			Tref = gen_fsm:send_event_after(Timeout, ?GM(ringout)),
			#inqueue_state{ queue_pid = Qpid, queue_mon = Qmon,
				cook_mon = CookMon} = Internal,
			NewInternal = #inqueue_ringing_state{
				queue_pid = Qpid, queue_mon = Qmon, ring_pid = {Agent, RPid},
				ring_mon = Rmon, cook = Requester, cook_mon = CookMon,
				ringout = Tref
			},
			BaseState2 = BaseState1#base_state{state_changes = [{inqueue_ringing, os:timestamp()} | StateChanges]},
			set_gproc_prop(inqueue, inqueue_ringing, BaseState2),
			AgentInfo = [{agent_login, Agent}, {agent_pid, Apid}],
			cdr:ringing(Call1, AgentInfo),
			{reply, ok, inqueue_ringing, {BaseState2, NewInternal}};
		RingErr ->
			lager:info("Agent ~p prering response:  ~p for ~p", [Agent, RingErr, Call1#call.id]),
			{reply, invalid, inqueue, {BaseState1, Internal}}
	catch
		exit:{noproc, _} ->
			lager:warning("Agent ~p is a dead pid", [Apid]),
			{reply, invalid, inqueue, {BaseState1, Internal}};
		exit:{max_ringouts, _} ->
			lager:debug("Max ringouts reached for agent ~p", [Apid]),
			{reply, invalid, inqueue, {BaseState1, Internal}}
	end;

inqueue(?GM(ring, {{_Agent, Apid}, QCall, _Timeout}), _From, State) ->
	gen_server:cast(QCall#queued_call.cook, {ring_to, Apid, QCall}),
	{reply, deferred, inqueue, State};

inqueue(?GM(announce, Announce), _From, St) ->
	sync_call_cbk(inqueue, St, handle_announce, [Announce]);

inqueue(?GM(voicemail), _From, St) ->
	% lager:info("trying to send media ~p to voicemail", [Call#call.id]),
	sync_call_cbk(inqueue, St, handle_voicemail, [], fun(#cbkr{cbk_res=ok}=R) ->
			priv_voicemail(St),
			R;
		(R) ->
			R
	end);

inqueue(?GM(transfer_outband, Addr), _From, St) ->
	% lager:info("trying to transfer ~p to ~p", [Call#call.id, Addr]),
	sync_call_cbk(inqueue, St, handle_transfer_outband, [Addr],
		fun(#cbkr{cbk_res=ok}=R) ->
			R#cbkr{action={stop, normal}};
		(R) -> R end);


inqueue(?GM(end_call), {Cook, _}, {#base_state{
		callrec = #call{cook = Cook}} = BaseState, InqueueState}) ->
	#base_state{callback = Callback, substate = InSubstate,
		callrec = Call} = BaseState,
	case erlang:function_exported(Callback, handle_end_call, 4) of
		true ->
			case Callback:handle_end_call(inqueue, Call, InqueueState, InSubstate) of
				{ok, Substate} ->
					% stop agent ringing, kill self
					lager:info("Ending Call for ~p", [Call#call.id]),
					NewState0 = BaseState#base_state{substate = Substate},
					{Out, NewState} = handle_stop(hangup, inqueue, NewState0, InqueueState),
					{stop, Out, ok, NewState};
				{deferred, Substate} ->
					lager:info("Ending Call deferred for ~p", [Call#call.id]),
					% up to the media to kill self.
					NewBase = BaseState#base_state{substate = Substate},
					{reply, ok, {NewBase, InqueueState}};
				{error, Err, Substate} ->
					lager:info("Ending Call for ~p errored:  ~p", [Call#call.id, Err]),
					NewBase = BaseState#base_state{substate = Substate},
					{reply, invalid, {NewBase, InqueueState}}
			end;
		false ->
			{reply, invalid, {BaseState, InqueueState}}
	end;
inqueue(?GM(set_queue, {QPid, Reset}), From, St) when is_pid(QPid) ->
	QName = call_queue:get_name(QPid),
	inqueue(?GM(set_queue, {{QName, QPid}, Reset}), From, St);
inqueue(?GM(set_queue, {{QName, QPid}, Reset}), _From, {BaseState, Internal}) ->
	BaseState1 = update_basestate_queue(BaseState, QName, Reset),
	Call = BaseState1#base_state.callrec,

	lager:notice("Updating queue pid for ~p to ~p, reset: ~p", [Call#call.id, QPid, Reset]),
	case Internal#inqueue_state.queue_mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	Newmon = erlang:monitor(process, QPid),
	NewInternal = Internal#inqueue_state{
		queue_mon = Newmon,
		queue_pid = {QName, QPid}
	},
	{reply, ok, inqueue, {BaseState1, NewInternal}};

inqueue(?GM(get_url_vars), _From, {BaseState, Internal}) ->
	#base_state{url_pop_get_vars = GenPopopts, substate = Substate,
		callback = Callback} = BaseState,
	Cbopts = case erlang:function_exported(Callback, urlpop_getvars, 1) of
		true ->
			Callback:urlpop_getvars(Substate);
		false ->
			[]
	end,
	Out = lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Cbopts)),
	{reply, {ok, Out}, inqueue, {BaseState, Internal}};

inqueue(Msg, From, State) ->
	fallback_sync(inqueue, Msg, From, State).

%% async

inqueue(?GM(set_outband_ring_pid, Pid), {BaseState, Internal}) ->
	NewInternal = Internal#inqueue_state{outband_ring_pid = Pid},
	{next_state, inqueue, {BaseState, NewInternal}};

inqueue(?GM(set_cook, CookPid), {BaseState, Internal}) ->
	#base_state{callrec = Call} = BaseState,
	lager:notice("Updating cook pid for ~p to ~p", [Call#call.id, CookPid]),
	case Internal#inqueue_state.cook_mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	Newmon = erlang:monitor(process, CookPid),
	NewCall = Call#call{cook = CookPid},
	NewInternal = Internal#inqueue_state{cook_mon = Newmon, cook = CookPid},
	NewBase = BaseState#base_state{callrec = NewCall},
	{next_state, inqueue, {NewBase, NewInternal}};

inqueue(Msg, State) ->
	fallback_async(inqueue, Msg, State).

%%--------------------------------------------------------------------
%% inqueue_ringing -> inqueue, oncall
%%--------------------------------------------------------------------

%% sync

inqueue_ringing(?GM(announce, Announce), _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, substate = InSubstate,
		callrec = Call} = BaseState,
	lager:info("Doing announce for ~p", [Call#call.id]),
	Substate = case erlang:function_exported(Callback, handle_announce, 5) of
		true ->
			{ok, N} = Callback:handle_announce(Announce, inqueue_ringing, Call, Internal, InSubstate),
			N;
		false ->
			InSubstate
	end,
	{reply, ok, inqueue_ringing, {BaseState#base_state{substate = Substate}, Internal}};

inqueue_ringing(?GM(voicemail), _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call} = BaseState,
	lager:info("trying to send media ~p to voicemail", [Call#call.id]),
	case erlang:function_exported(Callback, handle_voicemail, 4) of
		false ->
			{reply, {error, invalid}, inqueue_ringing, {BaseState, Internal}};
		true ->
			case Callback:handle_voicemail(inqueue_ringing, Call, Internal, BaseState#base_state.substate) of
				{ok, Substate} ->
					priv_voicemail({BaseState, Internal}),
					NewInternal = #inqueue_state{
						queue_mon = Internal#inqueue_ringing_state.queue_mon,
						queue_pid = Internal#inqueue_ringing_state.queue_pid,
						cook = Internal#inqueue_ringing_state.cook,
						cook_mon = Internal#inqueue_ringing_state.cook_mon
					},
					StateChanges = [{inqueue, os:timestamp()} | BaseState#base_state.state_changes],
					NewBase = BaseState#base_state{substate = Substate, state_changes = StateChanges},
					set_gproc_prop(inqueue_ringing, inqueue, NewBase),
					{reply, ok, inqueue, {NewBase, NewInternal}};
				{invalid, Substate} ->
					{reply, {error, invalid}, inqueue_ringing, {BaseState#base_state{substate = Substate}, Internal}}
			end
	end;
inqueue_ringing(?GM(transfer_outband, Addr), _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call} = BaseState,
	lager:info("trying to transfer ~p to ~p", [Call#call.id, Addr]),
	case erlang:function_exported(Callback, handle_transfer_outband, 5) of
		false ->
			{reply, invalid, inqueue_ringing, {BaseState, Internal}};
		true ->
			case Callback:handle_transfer_outband(Addr, inqueue_ringing, Call, Internal, BaseState#base_state.substate) of
				{ok, Substate} ->
					lager:info("transferred ~p to ~p", [Call#call.id, Addr]),
					% almost same except for cdr part
					priv_voicemail({BaseState, Internal}),
					{stop, normal, ok, {BaseState#base_state{substate = Substate}, Internal}};
				{invalid, Substate} ->
					{reply, invalid, inqueue_ringing, {BaseState#base_state{substate = Substate}, Internal}}
			end
	end;
inqueue_ringing(?GM(agent_oncall), {Apid, _Tag},
		{#base_state{callrec = #call{ring_path = outband} = Call} = BaseState,
		#inqueue_ringing_state{ring_pid = {_, Apid}} = Internal}) ->
	lager:info("Cannot accept on call requests from agent (~p) unless ring_path is inband for ~p", [Apid, Call#call.id]),
	{reply, invalid, inqueue_ringing, {BaseState, Internal}};

inqueue_ringing(?GM(agent_oncall), {Apid, _Tag},
		{#base_state{callrec = #call{ring_path = inband} = Call} = BaseState,
		#inqueue_ringing_state{ring_pid = {Agent, Apid}} = Internal}) ->
	#base_state{callback = Callback} = BaseState,
	lager:info("oncall request from agent ~p for ~p", [Apid, Call#call.id]),
	case Callback:handle_answer(Apid, inqueue_ringing, Call, Internal, BaseState#base_state.substate) of
		{ok, NewState} ->
			kill_outband_ring({BaseState, Internal}),
			case Internal#inqueue_ringing_state.ringout of
				undefined -> ok;
				TimerRef -> gen_fsm:cancel_timer(TimerRef)
			end,
			unqueue(Internal#inqueue_ringing_state.queue_pid, self()),
			cdr:oncall(Call, Agent),
			StateChanges = [{oncall, os:timestamp()} | BaseState#base_state.state_changes],
			{ok, AgentRec} = agent_channel:get_agent(Apid),
			NewBase = BaseState#base_state{substate = NewState, state_changes = StateChanges, agent = AgentRec},
			set_gproc_prop(inqueue_ringing, oncall, NewBase),
			NewInternal = #oncall_state{
				oncall_pid = {Agent, Apid},
				oncall_mon = Internal#inqueue_ringing_state.ring_mon
			},
			set_cpx_mon({NewBase, NewInternal}, [{agent, Agent}]),
			erlang:demonitor(Internal#inqueue_ringing_state.queue_mon),
			{reply, ok, oncall, {NewBase, NewInternal}};
		{error, Reason, NewState} ->
			lager:error("Could not set ~p on call due to ~p for ~p", [Apid, Reason, Call#call.id]),
			NewBase = BaseState#base_state{substate = NewState},
			{reply, invalid, inqueue_ringing, {NewBase, Internal}}
	end;

inqueue_ringing(?GM(agent_oncall), From, {BaseState, Internal}=St) ->
	Zt = inqueue_ringing,

	#base_state{callback = Callback, callrec = Call} = BaseState,
	#inqueue_ringing_state{ring_pid = {Agent, Apid}, ring_mon = Mon} = Internal,
	lager:info("oncall request from ~p; agent to set on call is ~p for ~p", [From, Apid, Call#call.id]),
	%% TODO this will break w/ merge for multichannel; that uses a pre-ring
	%% state.
	case set_agent_state(Apid, [oncall, Call]) of
		{error, invalid} ->
			{reply, invalid, inqueue_ringing, {BaseState, Internal}};
		ok ->
			sync_call_cbk(Zt, St, handle_answer, [Apid], fun(#cbkr{cbk_res=ok}=R) ->
					cdr:oncall(Call, Agent),
					case Internal#inqueue_ringing_state.ringout of
						undefined -> ok;
						_ -> gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout)
					end,
					{_, Qpid} = Internal#inqueue_ringing_state.queue_pid,
					call_queue:remove(Qpid, self()),
					StateChanges = [{oncall, os:timestamp()} | BaseState#base_state.state_changes],
					{ok, AgentRec} = agent_channel:get_agent(Apid),
					NewBase = (R#cbkr.base_state)#base_state{state_changes = StateChanges, agent = AgentRec},
					set_gproc_prop(inqueue_ringing, oncall, NewBase),
					NewInternal = #oncall_state{
						oncall_pid = {Agent, Apid},
						oncall_mon = Mon
					},
					set_cpx_mon({NewBase, NewInternal}, []),
					R#cbkr{zt=oncall, base_state=NewBase, internal_state=NewInternal};
				(R) -> R
			end);
		badagent ->
			{ok, NewSubstate} = Callback:handle_ring_stop(inqueue_ringing, Call, Internal, BaseState#base_state.substate),
			kill_outband_ring({BaseState, Internal}),
			cdr:ringout(Call, {badagent, Agent}),
			gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout),
			erlang:demonitor(Internal#inqueue_ringing_state.ring_mon),
			StateChanges = [{inqueue, os:timestamp()} | BaseState#base_state.state_changes],
			NewBase = BaseState#base_state{substate = NewSubstate, state_changes = StateChanges},
			set_gproc_prop(inqueue_ringing, inqueue, NewBase),
			#inqueue_ringing_state{ queue_mon = Qmon, queue_pid = Qpid, cook = Cook, cook_mon = CookMon} = Internal,
			NewInternal = #inqueue_state{
				queue_mon = Qmon,
				queue_pid = Qpid,
				cook = Cook,
				cook_mon = CookMon
			},
			gen_server:cast(Cook, stop_ringing),
			{reply, invalid, inqueue, {NewBase, NewInternal}}
	end;
inqueue_ringing(?GM(set_queue, {QPid, Reset}), From, St) when is_pid(QPid) ->
	QName = call_queue:get_name(QPid),
	inqueue_ringing(?GM(set_queue, {{QName, QPid}, Reset}), From, St);
inqueue_ringing(?GM(set_queue, {{QName, QPid}, Reset}), _From, {BaseState, Internal}) ->
	BaseState1 = update_basestate_queue(BaseState, QName, Reset),
	Call = BaseState1#base_state.callrec,

	#inqueue_ringing_state{queue_mon = Mon} = Internal,
	lager:notice("Updating queue pid for ~p to ~p", [Call#call.id, QPid]),
	case Mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	NewMon = erlang:monitor(process, QPid),
	NewInternal = Internal#inqueue_ringing_state{
		queue_mon = NewMon,
		queue_pid = {QName, QPid}
	},
	{reply, ok, inqueue_ringing, {BaseState1, NewInternal}};

inqueue_ringing(?GM(ring, {{Agent, Apid}, _ChanType, takeover}),
		From, {_, #inqueue_ringing_state{ring_pid = {Agent, Apid}}} = State) ->
	lager:debug("~p said it's taking over ring", [From]),
	{BaseState, Internal} = State,
	gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout),
	agent_channel:set_state(Apid, ringing, BaseState#base_state.callrec),
	NewInternal = Internal#inqueue_ringing_state{ringout = undefined},
	{reply, ok, inqueue_ringing, {BaseState, NewInternal}};

inqueue_ringing(?GM(ring, {{_Agent, Apid}, QCall, _Timeout}),
		_From, {BaseState, Internal}) ->
	#inqueue_ringing_state{ringout = Ringout,
		ring_pid = RingAgent, ring_mon = RMon} = Internal,
	#base_state{callrec = Call} = BaseState,

	Cook = Call#call.cook,
	case Ringout of
		undefined -> ok;
		_ -> gen_fsm:cancel_timer(Ringout)
	end,
	cook:ring_to(Cook, Apid, QCall),

	case RingAgent of
		undefined -> ok;
		{Nom, PrevApid} ->
			erlang:demonitor(RMon),
			stop_agent_channel(PrevApid),
			cdr:ringout(Call, {forwarded, Nom})
	end,

	NewInternal = #inqueue_state{
		queue_mon = Internal#inqueue_ringing_state.queue_mon,
		queue_pid = Internal#inqueue_ringing_state.queue_pid,
		cook = Internal#inqueue_ringing_state.cook
	},
	StateChanges = [{inqueue, os:timestamp()} | BaseState#base_state.state_changes],
	NewBase = BaseState#base_state{state_changes = StateChanges},
	set_gproc_prop(inqueue_ringing, inqueue, NewBase),
	{reply, deferred, inqueue, {NewBase, NewInternal}};

inqueue_ringing(?GM(end_call), {Cook, _}, {#base_state{
		callrec = #call{cook = Cook}} = BaseState, InternalState}) ->
	#base_state{callback = Callback, substate = InSubstate,
		callrec = Call} = BaseState,
	case erlang:function_exported(Callback, handle_end_call, 4) of
		true ->
			case Callback:handle_end_call(inqueue_ringing, Call, InternalState, InSubstate) of
				{ok, Substate} ->
					% stop agent ringing, kill self
					lager:info("Ending Call for ~p", [Call#call.id]),
					NewState0 = BaseState#base_state{substate = Substate},
					{Out, NewState} = handle_stop(hangup, inqueue_ringing, NewState0, InternalState),
					{stop, Out, ok, NewState};
				{deferred, Substate} ->
					lager:info("Ending Call deferred for ~p", [Call#call.id]),
					% up to the media to kill self.
					NewBase = BaseState#base_state{substate = Substate},
					{reply, ok, {NewBase, InternalState}};
				{error, Err, Substate} ->
					lager:info("Ending Call for ~p errored:  ~p", [Call#call.id, Err]),
					NewBase = BaseState#base_state{substate = Substate},
					{reply, invalid, {NewBase, InternalState}}
			end;
		false ->
			{reply, invalid, {BaseState, InternalState}}
	end;

inqueue_ringing(?GM(Command), _From, State) ->
	lager:debug("Invalid command ~s while inqueue_ringing", [Command]),
	{reply, invalid, inqueue_ringing, State};

inqueue_ringing(Msg, From, State) ->
	fallback_sync(inqueue_ringing, Msg, From, State).

%% async

inqueue_ringing(?GM(set_outband_ring_pid, Pid), {BaseState, Internal}) ->
	NewInternal = Internal#inqueue_ringing_state{outband_ring_pid = Pid},
	{next_state, inqueue_ringing, {BaseState, NewInternal}};

inqueue_ringing(?GM(set_cook, CookPid), {BaseState, Internal}) ->
	Call = BaseState#base_state.callrec,
	Mon = Internal#inqueue_ringing_state.cook_mon,
	lager:notice("Updating cook pid for ~p to ~p", [Call#call.id, CookPid]),
	case Mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	NewMon = erlang:monitor(process, CookPid),
	NewInternal = Internal#inqueue_ringing_state{
		cook = CookPid, cook_mon = NewMon
	},
	NewCall = Call#call{cook = CookPid},
	NewBase = BaseState#base_state{callrec = NewCall},
	{next_state, inqueue_ringing, {NewBase, NewInternal}};

inqueue_ringing(?GM(takeover_ring, {{Agent, Apid}, OutbandRinger}), {BaseState,
		#inqueue_ringing_state{ring_pid = {Agent, Apid}} = Internal}) ->
	gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout),
	agent_channel:set_state(Apid, ringing, BaseState#base_state.callrec),
	NewInternal = Internal#inqueue_ringing_state{ringout = undefined, outband_ring_pid = OutbandRinger},
	{next_state, inqueue_ringing, {BaseState, NewInternal}};

inqueue_ringing(?GM(stop_ring, Reason), State) ->
	NewState = requeue_ringing(Reason, State),
	{next_state, inqueue, NewState};

inqueue_ringing(?GM(Command), State) ->
	lager:debug("Invalid command event ~s while inqueue_ringing", [Command]),
	{next_state, inqueue_ringing, State};

inqueue_ringing(Msg, State) ->
	fallback_async(inqueue_ringing, Msg, State).

%%--------------------------------------------------------------------
%% oncall -> oncall_ringing, wrapup, warm_transfer_hold, inqueue
%%--------------------------------------------------------------------

%% sync

oncall(?GM(queue, {Queue, Opts}), From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call} = BaseState,
	#oncall_state{oncall_pid = {Ocagent, Apid}, oncall_mon = Mon} = Internal,
	lager:info("Request to queue ~p from ~p", [Call#call.id, From]),
	NewSkills = proplists:get_value(skills, Opts, []),
	case enqueue(Queue, reprioritize_for_requeue(Call#call{skills=NewSkills}), BaseState#base_state{agent = undefined}) of
		{ok, {NewBase,
			  #inqueue_state{queue_pid = {_QN, Qpid}} = NewInternal}} ->
			async_set_agent_state(Apid, [wrapup, Call]),
			{ok, NewState} = Callback:handle_queue_transfer({Queue, Qpid}, oncall, Call, NewInternal, NewBase#base_state.substate),
			cdr:wrapup(Call, Ocagent),
			erlang:demonitor(Mon),
			% cdr:queue_transfer(Call, Queue),
			% cdr:queue_transfer(NewCall, Queue),
			FinalBase = NewBase#base_state{substate = NewState},
			set_gproc_prop(oncall, inqueue, NewBase),
			{reply, ok, inqueue, {FinalBase, NewInternal}};
		_ -> {reply, invalid, {BaseState, Internal}}
	end;

oncall(?GM(transfer_outband, Addr), _From, St) ->
	% lager:info("trying to transfer ~p to ~p", [Call#call.id, Addr]),
	sync_call_cbk(inqueue, St, handle_transfer_outband, [Addr],
		fun(#cbkr{cbk_res=ok}=R) ->
			R#cbkr{action={stop, normal}};
		(R) -> R end);

% oncall(?GM(agent_transfer, {{_Agent, Apid}, _Timeout}), _From, {BaseState, #oncall_state{oncall_pid = {_, Apid}}} = State) ->
% 	Call = BaseState#base_state.callrec,
% 	lager:notice("Can't transfer to yourself, silly ~p! ~p", [Apid, Call#call.id]),
% 	{reply, invalid, oncall, State};

% oncall(?GM(agent_transfer, {{Agent, Apid}, Timeout}), _From, {BaseState, Internal}) ->
% 	#base_state{callrec = Call, callback = Callback, url_pop_get_vars = GenPopopts} = BaseState,
% 	#oncall_state{oncall_pid = {OcAgent, Ocpid}, oncall_mon = Mon} = Internal,
% 	case set_agent_state(Apid, [ringing, Call]) of
% 		{ok, _RPid} ->
% 			case Callback:handle_agent_transfer(Apid, Timeout, oncall, BaseState#base_state.callrec, Internal, BaseState#base_state.substate) of
% 				Success when element(1, Success) == ok ->
% 					Popopts = case Success of
% 						{ok, Substate} ->
% 							[];
% 						{ok, Opts, Substate} ->
% 							lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Opts))
% 					end,
% 					% dummy is there because the structure of internally handled
% 					% messages is ?GM(command}, CommandData}.
% 					% Even if the command doesn't take any arguments, something
% 					% needs to be there to make it a 2 element tuple, thus dummy
% 					% here.
% 					Tref = gen_fsm:send_event_after(Timeout, ?GM(stop_ring, dummy)),
% 					cdr:agent_transfer(Call, {OcAgent, Agent}),
% 					AgentInfo = [{agent_login, Agent}, {agent_pid, Apid}],
% 					cdr:ringing(Call, AgentInfo),
% 					url_pop(Call, Apid, Popopts),
% 					RingMon = erlang:monitor(process, Apid),
% 					StateChanges = [{oncall_ringing, os:timestamp()} | BaseState#base_state.state_changes],
% 					NewBase = BaseState#base_state{substate = Substate, state_changes = StateChanges},
% 					NewInternal = #oncall_ringing_state{
% 						ring_pid = {Agent, Apid},
% 						ringout = Tref,
% 						ring_mon = RingMon,
% 						oncall_pid = {OcAgent, Ocpid},
% 						oncall_mon = Mon
% 					},
% 					set_gproc_prop(oncall, oncall_ringing, NewBase),
% 					{reply, ok, oncall_ringing, {NewBase, NewInternal}};
% 				{error, Error, NewState} ->
% 					lager:notice("Could not set agent ringing for transfer ~p due to ~p", [Error, Call#call.id]),
% 					set_agent_state(Apid, [idle]),
% 					NewBase = BaseState#base_state{substate = NewState},
% 					{reply, invalid, oncall, {NewBase, Internal}}
% 			end;
% 		invalid ->
% 			lager:notice("Could not ring ~p to target agent ~p", [Call#call.id, Apid]),
% 			{reply, invalid, oncall, {BaseState, Internal}}
% 	end;

oncall(?GM(warm_transfer_hold), _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call, substate = Substate} = BaseState,
	#oncall_state{oncall_pid = {_, Apid}} = Internal,
	case erlang:function_exported(Callback, handle_warm_transfer_hold, 4) of
		true ->
			case Callback:handle_warm_transfer_hold(oncall, Call, Internal, Substate) of
				{ok, CallerRef, NewState} ->
					set_agent_state(Apid, [warm_transfer_hold]),
					% cdr:warm_transfer_hold(Call, Apid),
					StateChanges = [{warm_transfer_hold, os:timestamp()} | BaseState#base_state.state_changes],
					NewBase = BaseState#base_state{substate = NewState, state_changes = StateChanges},
					set_gproc_prop(oncall, warm_transfer_hold, NewBase),
					NewInternal = #warm_transfer_hold_state{
						oncall_pid = Internal#oncall_state.oncall_pid,
						oncall_mon = Internal#oncall_state.oncall_mon,
						caller_ref = CallerRef
					},
					{reply, ok, warm_transfer_hold, {NewBase, NewInternal}};
				{error, Error, NewState} ->
					lager:debug("Callback module ~w errored for warm transfer hold from oncall:  ~p for ~p", [Callback, Error, Call#call.id]),
					NewBase = BaseState#base_state{ substate = NewState},
					{reply, invalid, oncall, {NewBase, Internal}}
			end;
		_ ->
			{reply, invalid, oncall, {BaseState, Internal}}
	end;

oncall(?GM(spy, {Spy, _}), _From, {_, #oncall_state{oncall_pid = {_Nom, Spy}}} = State) ->
	lager:debug("Can't spy on yourself", []),
	{reply, invalid, oncall, State};

oncall(?GM(spy, {Spy, AgentRec}), _From, {BaseState, Oncall} = State) ->
	Callback = BaseState#base_state.callback,
	Call = BaseState#base_state.callrec,
	case erlang:function_exported(Callback, handle_spy, 4) of
		false ->
			lager:debug("Callback ~p doesn't support spy for ~p", [Callback, Call#call.id]),
			{reply, invalid, oncall, State};
		true ->
			case Callback:handle_spy({Spy, AgentRec}, oncall, Call, Oncall, BaseState#base_state.substate) of
				{ok, Newstate} ->
					{reply, ok, oncall, {BaseState#base_state{substate = Newstate}, Oncall}};
				{invalid, Newstate} ->
					{reply, invalid, oncall, {BaseState#base_state{substate = Newstate}, Oncall}};
				{error, Error, Newstate} ->
					lager:info("Callback ~p errored ~p on spy for ~p", [Callback, Error, Call#call.id]),
					{reply, {error, Error}, oncall, {BaseState#base_state{substate = Newstate}, Oncall}}
			end
	end;

oncall(?GM(wrapup), {Ocpid, _Tag} = From,
		{#base_state{callrec = Call} = BaseState,
		#oncall_state{oncall_pid = {Ocagent, Ocpid}} = Oncall})
		when Call#call.media_path =:= inband ->
	lager:info("Request to end call ~p from agent", [Call#call.id]),
	Callback = BaseState#base_state.callback,
	cdr:wrapup(Call, Ocagent),
	case Callback:handle_wrapup(From, oncall, Call, Oncall, BaseState#base_state.substate) of
		{ok, NewState} ->
			erlang:demonitor(Oncall#oncall_state.oncall_mon),
			StateChanges = [{wrapup, os:timestamp()} | BaseState#base_state.state_changes],
			NewBase = BaseState#base_state{substate = NewState, state_changes = StateChanges},
			set_gproc_prop(oncall, wrapup, NewBase),
			{reply, ok, wrapup, {NewBase, #wrapup_state{}}};
		{hangup, NewState} ->
			cdr:hangup(BaseState#base_state.callrec, "agent"),
			erlang:demonitor(Oncall#oncall_state.oncall_mon),
			{stop, normal, ok, {BaseState#base_state{substate = NewState}, Oncall#oncall_state{oncall_mon = undefined}}}
	end;

oncall(?GM(wrapup), {Ocpid, _Tag}, {#base_state{callrec = Call}, #oncall_state{oncall_pid = {_Agent, Ocpid}}} = State) ->
	lager:error("Cannot do a wrapup directly unless mediapath is inband, and request is from agent oncall. ~p", [Call#call.id]),
	{reply, invalid, oncall, State};

oncall(?GM(queue, Queue), {Ocpid, _},
		{#base_state{callback = Callback, callrec = Call} = BaseState,
		#oncall_state{oncall_pid = {Ocagent, Ocpid}} = Oncall} = State) ->
	Internal = Oncall,
	lager:info("request to queue call ~p from agent", [Call#call.id]),
	% Decrement the call's priority by 5 when requeueing
	case priv_queue(Queue, reprioritize_for_requeue(Call), BaseState#base_state.queue_failover) of
		invalid ->
			{reply, invalid, State};
		{default, Qpid} ->
			{ok, NewState} = Callback:handle_queue_transfer({Queue, Qpid}, oncall, Call, Oncall, BaseState#base_state.substate),
			cdr:queue_transfer(Call, "default_queue"),
			cdr:inqueue(Call, "default_queue"),
			cdr:wrapup(Call, Ocagent),
			set_cpx_mon({BaseState#base_state{substate = NewState},#wrapup_state{}}, [{queue, "default_queue"}]),
			erlang:demonitor(Internal#oncall_state.oncall_mon),
			{reply, ok, {BaseState#base_state{substate = NewState}, #wrapup_state{}}};
		Qpid when is_pid(Qpid) ->
			{ok, NewState} = Callback:handle_queue_transfer({Queue, Qpid}, oncall, Call, Oncall, BaseState#base_state.substate),
			cdr:queue_transfer(Call, Queue),
			cdr:inqueue(Call, Queue),
			cdr:wrapup(Call, Ocagent),
			erlang:demonitor(Internal#oncall_state.oncall_mon),
			NewInternal = #inqueue_state{
				queue_pid = {Queue, Qpid},
				queue_mon = erlang:monitor(process, Qpid)
			},
			set_cpx_mon({BaseState#base_state{substate = NewState}, NewInternal}, [{queue, Queue}]),
			StateChanges = [{inqueue, os:timestamp()} | BaseState#base_state.state_changes],
			NewBase = BaseState#base_state{substate = NewState, state_changes = StateChanges},
			set_gproc_prop(oncall, inqueue, NewBase),
			{reply, ok, inqueue, {NewBase, NewInternal}}
	end;

oncall(?GM(hold), _From, {BaseState, Internal}) ->
	Callback = BaseState#base_state.callback,
	{Reply, NewState} = Callback:handle_hold(Internal, BaseState#base_state.substate),
	{reply, Reply, oncall, {BaseState#base_state{substate = NewState}, Internal}};

oncall(?GM(unhold), _From, {BaseState, Internal}) ->
	Callback = BaseState#base_state.callback,
	{Reply, NewState} = Callback:handle_unhold(Internal, BaseState#base_state.substate),
	{reply, Reply, oncall, {BaseState#base_state{substate = NewState}, Internal}};

oncall(?GM({play, JsonOpts}), _From, {BaseState, Internal}) ->
	Callback = BaseState#base_state.callback,
	Substate = BaseState#base_state.substate,
	Opts = case erlang:function_exported(Callback, from_json_opts, 1) of
		true ->
			Callback:from_json_opts(JsonOpts);
		false ->
			[]
	end,
	{Reply, NewState} = case erlang:function_exported(Callback, handle_play, 4) of
		true ->
			Callback:handle_play(Opts, BaseState#base_state.callrec, Internal, Substate);
		false ->
			{{error, not_supported}, Substate}
	end,
	{reply, Reply, oncall, {BaseState#base_state{substate = NewState}, Internal}};

oncall(?GM(pause), _From, {BaseState, Internal}) ->
	Callback = BaseState#base_state.callback,
	Substate = BaseState#base_state.substate,
	{Reply, NewState} = case erlang:function_exported(Callback, handle_pause, 3) of
		true ->
			Callback:handle_pause(BaseState#base_state.callrec, Internal, Substate);
		false ->
			{{error, not_supported}, Substate}
	end,
	{reply, Reply, oncall, {BaseState#base_state{substate = NewState}, Internal}};

oncall(?GM({conference_to_agent, AgentLogin}), _From, {BaseState, Internal}) ->
	Callback = BaseState#base_state.callback,
	Substate = BaseState#base_state.substate,
	Call = BaseState#base_state.callrec,
	{Reply, NewSub, ConferenceChannel} = case erlang:function_exported(Callback, handle_conference_to_agent, 4) of
		true ->
			offer_conference_to_agent(Callback, AgentLogin, Call, Internal, Substate);
		false ->
			{{error, not_supported}, Substate, undefined}
	end,
	{reply, Reply, oncall, {BaseState#base_state{substate = NewSub, conference_channel = ConferenceChannel}, Internal}};

oncall(Msg, From, State) ->
	fallback_sync(oncall, Msg, From, State).

offer_conference_to_agent(Callback, AgentLogin, Call, Internal, Substate) ->
	case agent_manager:query_agent(AgentLogin) of
		{true, AgentPid} ->
			case agent:offer_conference(AgentPid, Call) of
				{ok, ChannelPid} ->
					{Reply, NewSub} = Callback:handle_conference_to_agent(AgentLogin, Call, Internal, Substate),
					% agent_channel:set_state(ChannelPid, ringing, Call),
					{_Agent, Apid} = Internal#oncall_state.oncall_pid,
					agent_channel:set_conference(Apid),
					{Reply, NewSub, ChannelPid};
				_ ->
					{error, Substate, undefined}
			end;
		_ ->
			{error, Substate, undefined}
	end.

%% async

oncall(Msg, State) ->
	fallback_async(oncall, Msg, State).

%%--------------------------------------------------------------------
%% oncall_ringing -> oncall (same agent), oncall (new agent)
%%--------------------------------------------------------------------

%% sync

oncall_ringing(?GM(agent_oncall), {Apid, _},
		{#base_state{callrec = #call{ring_path = outband} = Call},
		#oncall_ringing_state{ring_pid = {_, Apid}}} = State) ->
	lager:info("Cannot accept on call requests from agent (~p) unless ring_path is inband for ~p", [Apid, Call#call.id]),
	{reply, invalid, oncall_ringing, State};

oncall_ringing(?GM(agent_oncall), {Rpid, _},
		{#base_state{callrec = #call{ring_path = inband} = Call} = BaseState,
		#oncall_ringing_state{ring_pid = {Ragent, Rpid}} = Internal}) ->
	#base_state{callback = Callback} = BaseState,
	#oncall_ringing_state{oncall_pid = {OcAgent, Ocpid},
		oncall_mon = Ocmon} = Internal,
	lager:info("oncall request during what looks like an agent transfer (inband) for ~p", [Call#call.id]),
	case Callback:handle_answer(Rpid, oncall_ringing, Call, Internal, BaseState#base_state.substate) of
		{ok, NewState} ->
			kill_outband_ring({BaseState, Internal}),
			cdr:oncall(Call, Ragent),
			gen_fsm:cancel_timer(Internal#oncall_ringing_state.ringout),
			set_agent_state(Ocpid, [wrapup, Call]),
			cdr:wrapup(Call, OcAgent),
			erlang:demonitor(Ocmon),
			StateChanges = [{oncall, os:timestamp()} | BaseState#base_state.state_changes],
			{ok, AgentRec} = agent_channel:get_agent(Rpid),
			NewBase = BaseState#base_state{substate = NewState, state_changes = StateChanges, agent = AgentRec},
			set_gproc_prop(oncall_ringing, oncall, NewBase),
			NewInternal = #oncall_state{
				oncall_pid = Internal#oncall_ringing_state.ring_pid,
				oncall_mon = Internal#oncall_ringing_state.ring_mon
			},
			set_cpx_mon({NewBase, NewInternal}, [{agent, Ragent}]),
			{reply, ok, oncall, {NewBase, NewInternal}};
		{error, Reason, NewState} ->
			lager:error("Cannot set ~p for ~p to oncall due to ~p", [Rpid, Call#call.id, Reason]),
			NewBase = BaseState#base_state{ substate = NewState},
			{reply, invalid, oncall_ringing, {NewBase, Internal}}
	end;

oncall_ringing(?GM(agent_oncall), _, State) ->
	{BaseState, Internal} = State,
	#base_state{callback = Callback, callrec = Call} = BaseState,
	#oncall_ringing_state{ring_pid = {Ragent, Rpid},
		oncall_pid = {OcAgent, Ocpid}, oncall_mon = Ocmon} = Internal,
	lager:info("oncall request during what looks like an agent transfer (outofband) to ~p for ~p", [Ragent, Call#call.id]),
	case set_agent_state(Rpid, [oncall, Call]) of
		invalid ->
			{reply, invalid, oncall_ringing, State};
		ok ->
			case Callback:handle_answer(Rpid, oncall_ringing, Call, Internal, BaseState#base_state.substate) of
				{ok, NewState} ->
					kill_outband_ring(State),
					cdr:oncall(Call, Ragent),
					gen_fsm:cancel_timer(Internal#oncall_ringing_state.ringout),
					set_agent_state(Ocpid, [wrapup, Call]),
					cdr:wrapup(Call, OcAgent),
					StateChanges = [{oncall, os:timestamp()} | BaseState#base_state.state_changes],
					{ok, AgentRec} = agent_channel:get_agent(Rpid),
					NewBase = BaseState#base_state{substate = NewState, state_changes = StateChanges, agent = AgentRec},
					set_gproc_prop(oncall_ringing, oncall, NewBase),
					NewInternal = #oncall_state{
						oncall_pid = {Ragent, Rpid},
						oncall_mon = Internal#oncall_ringing_state.ring_mon
					},
					set_cpx_mon({NewBase, NewInternal}, [{agent, Ragent}]),
					erlang:demonitor(Ocmon),
					{reply, ok, oncall, {NewBase, NewInternal}};
				{error, Reason, NewState} ->
					lager:error("Cannot set ~p to oncall due to ~p for ~p", [Rpid, Reason, Call#call.id]),
					NewBase = BaseState#base_state{ substate = NewState},
					{reply, invalid, oncall_ringing, {NewBase, Internal}}
			end
	end;

oncall_ringing(Msg, From, State) ->
	fallback_sync(oncall_ringing, Msg, From, State).

%% async

oncall_ringing(Msg, State) ->
	fallback_async(oncall_ringing, Msg, State).

%%--------------------------------------------------------------------
%% wrapup
%%--------------------------------------------------------------------

%% sync

wrapup(Msg, From, State) ->
	fallback_sync(wrapup, Msg, From, State).

%% async

wrapup(Msg, State) ->
	fallback_async(wrapup, Msg, State).

%%--------------------------------------------------------------------
%% handle_sync_event
%%--------------------------------------------------------------------

handle_sync_event(?GM(get_url_vars), _From, StateName, State) ->
	{BaseState, _Internal} = State,
	#base_state{
		substate = Substate,
		url_pop_get_vars = GenPopopts,
		callback = Callback
	} = BaseState,
	Cbopts = case erlang:function_exported(Callback, urlpop_getvars, 1) of
		true ->
			Callback:urlpop_getvars(Substate);
		false ->
			[]
	end,
	Out = lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Cbopts)),
	{reply, {ok, Out}, StateName, State};

handle_sync_event(?GM(get_call), _From, StateName, State) ->
	{BaseState, _} = State,
	#base_state{callrec = Reply} = BaseState,
	{reply, Reply, StateName, State};

handle_sync_event(?GM(stop), _From, StateName, {BaseState, Internal}) ->
	Reason = normal,
	{NewStop, {NewBase, NewInternal}} = handle_stop(Reason, StateName, BaseState, Internal),
	{stop, NewStop, ok, {NewBase, NewInternal}};

handle_sync_event(?GM(Command), _From, StateName, State) ->
	lager:debug("Invalid generic sync command ~p while in ~s", [Command, StateName]),
	{reply, invalid, StateName, State};

handle_sync_event(Msg, From, StateName, {#base_state{callback = Callback,
		callrec = Call} = BaseState, Extra} = State) ->
	Return = Callback:handle_call(Msg, From, StateName, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, StateName, reply, State).

%%--------------------------------------------------------------------
%% handle_event
%%--------------------------------------------------------------------

handle_event(?GM(set_url_getvars, Vars), StateName, State) ->
	{BaseState, Internal} = State,
	#base_state{url_pop_get_vars = Oldvars} = BaseState,
	Newvars = lists:ukeymerge(1, lists:ukeysort(1, Vars), lists:ukeysort(1, Oldvars)),
	lager:debug("Input:  ~p;  Old:  ~p;  new:  ~p", [Vars, Oldvars, Newvars]),
	NewBase = BaseState#base_state{url_pop_get_vars = Newvars},
	{next_state, StateName, {NewBase, Internal}};

handle_event(?GM(add_skills, Skills), StateName, State) ->
	{BaseState, Internal} = State,
	Call = BaseState#base_state.callrec,
	NewSkills = util:merge_skill_lists(Call#call.skills, Skills),
	NewCall = Call#call{skills = NewSkills},
	NewBase = BaseState#base_state{callrec = NewCall},
	set_gproc_prop(StateName, StateName, NewBase),
	{next_state, StateName, {NewBase, Internal}};

handle_event(?GM(remove_skills, Skills), StateName, State) ->
	{BaseState, Internal} = State,
	Call = BaseState#base_state.callrec,
	NewSkills = lists:subtract(Call#call.skills, Skills),
	NewCall = Call#call{skills = NewSkills},
	NewBase = BaseState#base_state{callrec = NewCall},
	set_gproc_prop(StateName, StateName, NewBase),
	{next_state, StateName, {NewBase, Internal}};

handle_event(?GM(Command), StateName, State) ->
	lager:debug("Invalid generic event command ~p while in state ~p", [Command, StateName]),
	{next_state, StateName, State};

handle_event(Msg, StateName, {BaseState, Internal} = State) ->
	#base_state{callback = Callback, callrec = Call, substate = Sub} = BaseState,
	Reply = Callback:handle_cast(Msg, StateName, Call, Internal, Sub),
	handle_custom_return(Reply, StateName, noreply, State).

%%--------------------------------------------------------------------
%% handle_info
%%--------------------------------------------------------------------

handle_info({'DOWN', Ref, process, Pid, Info}, inqueue, {BaseState,
		#inqueue_state{ queue_pid = {Q, Pid}, queue_mon = Ref} = Internal}) ->
	#base_state{callrec = Call} = BaseState,
	lager:warning("Queue ~p died due to ~p (I'm ~p)", [Q, Info, Call#call.id]),
	%% in theory, the cook will tell us when the queue is back up.
	NewInternal = Internal#inqueue_state{
		queue_pid = {Q, undefined},
		queue_mon = undefined
	},
	{next_state, inqueue, {BaseState, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, inqueue_ringing, {BaseState,
		#inqueue_ringing_state{ queue_pid = {Q, Pid}, queue_mon = Ref} =
		Internal}) ->
	#base_state{callrec = Call} = BaseState,
	lager:warning("Queue ~p died due to ~p (I'm ~p)", [Q, Info, Call#call.id]),
	%% in theory, the cook will tell us when the queue is back up.
	NewInternal = Internal#inqueue_ringing_state{
		queue_pid = {Q, undefined},
		queue_mon = undefined
	},
	{next_state, inqueue_ringing, {BaseState, NewInternal}};

handle_info({'DOWN', Ref, process, _Pid, Info}, inqueue_ringing, {BaseState,
		#inqueue_ringing_state{cook_mon = Ref, cook = Ref} = Internal}) ->
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	#inqueue_ringing_state{ring_pid = {Aname, Apid}} = Internal,
	lager:warning("Cook died due to ~p (I'm ~p)", [Info, Call#call.id]),
	case agent:query_state(Apid) of
		{ok, ringing} ->
			set_agent_state(Apid, [idle]);
		_ ->
			ok
	end,
	{ok, NewSub} = Callback:handle_ring_stop(inqueue_ringing, Call, Internal, Sub),
	kill_outband_ring({BaseState, Internal}),
	cdr:ringout(Call, {cook_death, Aname}),
	erlang:demonitor(Internal#inqueue_ringing_state.ring_mon),
	NewCall = Call#call{cook = undefined},
	NewBase = BaseState#base_state{substate = NewSub, callrec = NewCall},
	NewInternal = #inqueue_state{
		queue_mon = Internal#inqueue_ringing_state.queue_mon,
		queue_pid = Internal#inqueue_ringing_state.queue_pid
	},
	{next_state, inqueue, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, _Info}, inqueue, {BaseState,
		#inqueue_state{cook = Pid, cook_mon = Ref} = Internal}) ->
	% not much to do, a ressurected cook will tell us about itself.
	#base_state{callrec = Call} = BaseState,
	NewCall = Call#call{cook = undefined},
	NewBase = BaseState#base_state{callrec = NewCall},
	NewInternal = Internal#inqueue_state{
		cook = undefined,
		cook_mon = undefined
	},
	{next_state, inqueue, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, inqueue_ringing, {BaseState,
		#inqueue_ringing_state{ring_pid = {Aname, Pid}, ring_mon = Ref} = Internal}) ->
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	lager:warning("ringing Agent fsm ~p died due to ~p (I'm ~p)", [Aname, Info, Call#call.id]),
	% no need to modify agent state since it's already dead.
	gen_server:cast(Call#call.cook, stop_ringing),
	{ok, NewSub} = Callback:handle_ring_stop(inqueue_ringing, Call, Internal, Sub),
	kill_outband_ring({BaseState, Internal}),
	cdr:ringout(Call, {agent_fsm_death, Aname}),
	NewBase = BaseState#base_state{substate = NewSub},
	#inqueue_ringing_state{queue_mon = Qmon, queue_pid = Qpid, cook_mon = Cmon,
		cook = Cook} = Internal,
	NewInternal = #inqueue_state{
		queue_mon = Qmon,
		queue_pid = Qpid,
		cook = Cook,
		cook_mon = Cmon
	},
	{next_state, inqueue, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, _Info}, oncall_ringing, {BaseState,
		#oncall_ringing_state{oncall_pid = {_Agent, Pid}, oncall_mon = Ref} =
		Internal}) ->
	lager:warning("Oncall agent ~p died while in agent transfer", [Pid]),
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	% it is up to the media to do the messages to ultimately end ringing
	% if that actually matters.
	{default, Qpid} = priv_queue("default_queue", reprioritize_for_requeue(Call), true),
	{ok, NewSub} = Callback:handle_queue_transfer({"default_queue", Qpid}, oncall_ringing, Call, Internal, Sub),
	Qmon = erlang:monitor(process, Qpid),
	NewBase = BaseState#base_state{substate = NewSub},
	#oncall_ringing_state{ring_pid = Rpid, ring_mon = Rmon} = Internal,
	NewInternal = #inqueue_ringing_state{
		ring_pid = Rpid, ring_mon = Rmon, queue_pid = {"default_queue", Qpid},
		queue_mon = Qmon
	},
	{next_state, inqueue_ringing, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, _Info}, oncall_ringing, {BaseState,
		#oncall_ringing_state{ring_pid = {Agent, Pid}, ring_mon = Ref} =
		Internal}) ->
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	lager:warning("Ringing agent ~p died while oncall", [Pid]),
	{ok, NewSub} = Callback:handle_ring_stop({Agent, Pid}, oncall_ringing, Call, Internal, Sub),
	NewBase = BaseState#base_state{substate = NewSub},
	#oncall_ringing_state{oncall_pid = Ocpid, oncall_mon = OcMon} = Internal,
	NewInternal = #oncall_state{oncall_pid = Ocpid, oncall_mon = OcMon},
	{next_state, oncall, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, oncall, {BaseState,
		#oncall_state{oncall_pid = {_Agent, Pid}, oncall_mon = Ref} =
		Internal}) ->
	lager:warning("Oncall agent ~p died due to ~p", [Pid, Info]),
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	 case priv_queue("default_queue", reprioritize_for_requeue(Call), false) of
		invalid ->
			{stop, {agent_died, Info}, {BaseState, Internal}};
		Qpid ->
			{ok, NewSub} = Callback:handle_queue_transfer({"default_queue", Qpid}, oncall, Call, Internal, Sub),
			NewBase = BaseState#base_state{substate = NewSub},
			Qmon = erlang:monitor(process, Qpid),
			NewInternal = #inqueue_state{
				queue_pid = {"default_queue", Qpid},
				queue_mon = Qmon
			},
			{next_state, inqueue, {NewBase, NewInternal}}
	end;

handle_info(conference_accepted, oncall, {BaseState,
		#oncall_state{oncall_pid = {_Agent, Pid}, oncall_mon = Ref} =
		Internal}) ->
	ConferenceChannel = BaseState#base_state.conference_channel,
	agent_channel:set_state(ConferenceChannel, {oncall, BaseState#base_state.callrec}),
	{next_state, oncall, {BaseState, Internal}};

handle_info(third_party_hangup, oncall, {BaseState,
		#oncall_state{oncall_pid = {_Agent, Pid}, oncall_mon = Ref} =
		Internal}) ->
	ConferenceChannel = BaseState#base_state.conference_channel,
	agent_channel:set_state(ConferenceChannel, wrapup, BaseState#base_state.callrec),
	{next_state, oncall, {BaseState, Internal}};

handle_info(Msg, StateName, {#base_state{callback = Callback,
		callrec = Call} = BaseState, Extra} = State) ->
	Return = Callback:handle_info(Msg, StateName, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, StateName, noreply, State).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------

terminate(Reason, StateName, {#base_state{callback = Callback, callrec =
		Call} = BaseState, Extra} = State) ->
	set_cpx_mon(State, delete),
	set_gproc_prop(StateName, stop, BaseState),
	Callback:terminate(Reason, StateName, Call, Extra, BaseState#base_state.substate).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------

%% @private
code_change(OldVsn, StateName, State, Extra) ->
	{BaseState, Internal} = State,
	#base_state{callback = Callback, callrec = Call, substate = Sub} = BaseState,
	{ok, Newsub} = Callback:code_change(OldVsn, Call, StateName, Sub, Internal, Extra),
    {ok, StateName, {BaseState#base_state{substate = Newsub}, Internal}}.

%%====================================================================
%% Internal functions
%%====================================================================

sync_call_cbk(Zt, St, F, PreA) ->
	sync_call_cbk(Zt, St, F, PreA, fun(R) -> R end).

sync_call_cbk(Zt, {BaseSt, IntSt}, F, PreA, H) ->
	#base_state{callback = M, callrec = Call, substate = SubSt} = BaseSt,
	A = PreA ++ [Zt, Call, IntSt, SubSt],
	{Rep, SubSt1} = case erlang:function_exported(M, F, length(A)) of
		true ->
			case apply(M, F, A) of
				{ok, SubSt2} ->
					{ok, SubSt2};
				{ok, Rep1, SubSt2} ->
					{Rep1, SubSt2};
				{error, Err, SubSt2} ->
					{{error, Err}, SubSt2};
				_Resp ->
					lager:warning("unexpected response from ~p:~p, Args: ~p -> ~p", [M, F, A, _Resp]),
					{{error, invalid}, SubSt}
			end;
		_ ->
			{{error, invalid}, SubSt}
	end,
	BaseSt1 = BaseSt#base_state{substate=SubSt1},

	Cbkr0 = #cbkr{
		zt = Zt,
		cbk_res = Rep,
		reply = Rep, %% by default, cbk resp = reply
		action = reply,
		base_state = BaseSt1,
		internal_state = IntSt
	},

	case H(Cbkr0) of
		#cbkr{action={stop, Rsn}, reply=Rep9, base_state=BaseSt9, internal_state=IntSt9} ->
			{stop, Rsn, Rep9, {BaseSt9, IntSt9}};
		#cbkr{reply=Rep9, zt=Zt9, base_state=BaseSt9, internal_state=IntSt9} -> %% action=reply / default
			{reply, Rep9, Zt9, {BaseSt9, IntSt9}}
	end.

fallback_sync(S, ?GM(Command, _Args), _From, State) ->
	lager:debug("Invalid sync event ~s while ~s", [Command, S]),
	{reply, invalid, S, State};
fallback_sync(S, Msg, From, {#base_state{callback = Callback} = BaseState, Internal} = State) ->
	Return = Callback:handle_call(Msg, From, S, BaseState#base_state.callrec, Internal, BaseState#base_state.substate),
	handle_custom_return(Return, S, reply, State).

fallback_async(S, ?GM(Command, _Args), State) ->
	lager:debug("Invalid async event ~s while ~s", [Command, S]),
	{reply, invalid, S, State};
fallback_async(S, Msg, {#base_state{ callback = Callback, callrec = Call} = BaseState, Internal} = State) ->
	Return = Callback:handle_cast(Msg, S, Call, Internal, BaseState#base_state.substate),
	handle_custom_return(Return, S, noreply, State).

update_basestate_queue(BaseState, QName, Reset) ->
	C = case Reset of
		true -> BaseState#base_state.init_callrec;
		_ -> BaseState#base_state.callrec
	end,
	C1 = C#call{queue = QName},
	BaseState#base_state{callrec = C1}.

%%--------------------------------------------------------------------
%% handle_custom_return
%%--------------------------------------------------------------------

handle_custom_return({queue, QueueN, Ps, SubState}, inivr, Reply, {BaseSt, _IntSt}=JSt) ->
	Call0 = BaseSt#base_state.callrec,
	Callback = BaseSt#base_state.callback,
	StateChanges = BaseSt#base_state.state_changes,

	Call = ps_to_call(Ps, Call0, Callback, StateChanges),

	BaseSt0 = BaseSt#base_state{substate=SubState},
	{Ans, JSt1} = case enqueue(QueueN, Call, BaseSt0) of
		{ok, {BaseSt1, _IntSt1}=JSt2} ->
			set_gproc_prop(inivr, inqueue, BaseSt1),
			{ok, JSt2};
		_ ->
			{error, {noqueue, QueueN}, JSt}
	end,

	case Reply of
		reply ->
			{reply, Ans, inqueue, JSt1};
		_ ->
			{next_state, inqueue, JSt1}
	end;

	% case {Reply, QData} of
	% 	{noreply, {error, _}} ->
	% 		{next_state, inivr, {BaseState#base_state{substate = NewState}, CurState}};
	% 	{reply, {error, _}} ->
	% 		{reply, QData, inivr, {BaseState#base_state{substate = NewState}, CurState}};
	% 	{_, {ok, {QueueName, QPid}}} ->

	% 		set_gproc_prop(inivr, inqueue, NewBase),
	% 		case Reply of
	% 			reply ->
	% 				{reply, ok, inqueue, {NewBase, InternalState}};
	% 			noreply ->
	% 				{next_state, inqueue, {NewBase, InternalState}}
	% 		end
	% end;

handle_custom_return({voicemail, NewState}, StateName, Reply,
		{BaseState, InternalState}) when StateName =:= inqueue;
		StateName =:= inqueue_ringing ->
	priv_voicemail({BaseState, InternalState}),
	NewBase = BaseState#base_state{
		substate = NewState
	},
	case Reply of
		reply ->
			{reply, ok, wrapup, {NewBase, #wrapup_state{}}};
		noreply ->
			{next_state, wrapup, {NewBase, #wrapup_state{}}}
	end;

handle_custom_return({stop, Reason, Reply, NewState}, StateName, reply,
		{BaseState, Internal}) ->
	{NewStop, {NewBase, NewInternal}} = handle_stop(Reason, StateName, BaseState, Internal),
	{stop, NewStop, Reply, {NewBase#base_state{substate = NewState}, NewInternal}};

handle_custom_return({stop, Reason, NewState}, StateName, _Reply,
		{BaseState, Internal}) ->
	{NewStop, {NewBase, NewInternal}} = handle_stop(Reason, StateName, BaseState, Internal),
	{stop, NewStop, {NewBase#base_state{substate = NewState}, NewInternal}};

handle_custom_return({outgoing, AgentChannel, NewState}, StateName, Reply,
		{#base_state{callrec = Call} = BaseState, InternalState}) when
		is_record(BaseState#base_state.callrec, call) ->
	handle_custom_return({outgoing, AgentChannel, Call, NewState}, StateName, Reply, {BaseState, InternalState});

handle_custom_return({outgoing, {AgentName, AgentChannel}, Call, NewState}, _StateName, Reply,
		{BaseState, InternalState}) when is_record(Call, call) ->
	lager:info("Told to set ~s (~p) to outgoing for ~p", [AgentName, AgentChannel, Call#call.id]),
	Response = set_agent_state(AgentChannel, [oncall, Call]),
	State0 = case Response of
		ok ->
			cdr:oncall(Call, AgentName),
			NewBase = BaseState#base_state{
				substate = NewState,
				callrec = Call
			},
			NewInternal = #oncall_state{
				oncall_pid = {AgentName, AgentChannel},
				oncall_mon = erlang:monitor(process, AgentChannel)
			},
			set_cpx_mon({NewBase, NewInternal}, [{agent, AgentName}]),
			{NewBase, NewInternal};
		_Else ->
			{BaseState, InternalState}
	end,
	case {Response, Reply} of
		{ok, noreply} ->
			{next_state, oncall, State0};
		{ok, reply} ->
			{reply, ok, oncall, State0};
		{Err, noreply} ->
			lager:warning("Could not set ~p oncall:  ~p", [AgentName, Err]),
			{stop, Err, State0};
		{Err, reply} ->
			lager:warning("Could not set ~p oncall:  ~p", [AgentName, Err]),
			{stop, Err, invalid, State0}
	end;

handle_custom_return({reply, Reply, NewState}, State, reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{reply, Reply, State, {NewBase, Internal}};

% Timebernate:  Timeout or hibernate
handle_custom_return({reply, Reply, NewState, Timebernate}, State, reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{reply, Reply, State, {NewBase, Internal}, Timebernate};

handle_custom_return({noreply, NewState}, State, _Reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{next_state, State, {NewBase, Internal}};

handle_custom_return({noreply, NewState, Timebernate}, State, _Reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{next_state, State, {NewBase, Internal}, Timebernate};

handle_custom_return({hangup, NewSub}, inqueue, Reply, State) ->
	handle_custom_return({{hangup, undefined}, NewSub}, inqueue, Reply, State);

handle_custom_return({{hangup, Who}, NewSub}, inqueue, Reply,
		{#base_state{callrec = Callrec} = BaseState, Internal}) ->
	#inqueue_state{queue_mon = Qmon, queue_pid = {_, Qpid}} = Internal,
	lager:info("hang for ~p up when only queue is a pid", [Callrec#call.id]),
	unqueue(Qpid, self()),
	cdr:hangup(Callrec, Who),
	erlang:demonitor(Qmon),
	case Reply of
		reply ->
			{reply, ok, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}};
		noreply ->
			{next_state, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}}
	end;

handle_custom_return({{hangup, Who}, NewSub}, inivr, Reply,
		{#base_state{callrec = Callrec} = BaseState, _Internal}) ->
	case is_record(Callrec, call) of
		true ->
			lager:info("hangup for ~s while inivr", [Callrec#call.id]),
			cdr:hangup(Callrec, Who);
		_ ->
			lager:info("hangup nor a not yet defined call inivr", [])
	end,
	case Reply of
		reply ->
			{reply, ok, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}};
		noreply ->
			{next_state, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}}
	end;

handle_custom_return({{hangup, Who}, NewState}, inqueue_ringing, noreply,
		{#base_state{callrec = Callrec} = BaseState, Internal}) ->
	lager:info("hangup for ~s whiile inivr", [Callrec#call.id]),
	cdr:hangup(Callrec, Who),
	% for now just going to trust the agent connection hears this die.
	erlang:demonitor(Internal#inqueue_ringing_state.ring_mon),
	erlang:demonitor(Internal#inqueue_ringing_state.cook_mon),
	erlang:demonitor(Internal#inqueue_ringing_state.queue_mon),
	unqueue(Internal#inqueue_ringing_state.queue_pid,self()),
	case Internal#inqueue_ringing_state.ring_pid of
		{_, APid} ->
			stop_agent_channel(APid);
		_ ->
			ok
	end,
	{next_state, wrapup, {BaseState#base_state{substate = NewState}, #wrapup_state{}}};

handle_custom_return({{hangup, Who}, NewState}, wrapup, noreply,
		{#base_state{callrec = Callrec} = BaseState, Internal}) ->
	% this can occur when a media goes to voicemail.
	% TODO add a 'leaving voicemail' state?
	lager:info("hangup for ~s while in wrapup", [Callrec#call.id]),
	cdr:hangup(Callrec, Who),
	% leaving it up to the media whether it should stop or not.
	{next_state, wrapup, {BaseState#base_state{substate = NewState}, Internal}};

handle_custom_return({mutate, NewCallback, NewState}, State, noreply,
		{BaseState, Internal}) ->
	lager:info("mutating to ~p from ~p", [NewCallback, BaseState#base_state.callback]),
	NewBase = BaseState#base_state{callback = NewCallback, substate = NewState},
	{next_state, State, {NewBase, Internal}};

handle_custom_return({mutate, Reply, NewCallback, NewState}, State, reply,
		{BaseState, Internal}) ->
	lager:info("mutating to ~p from ~p", [NewCallback, BaseState#base_state.callback]),
	NewBase = BaseState#base_state{callback = NewCallback,
		substate = NewState},
	{reply, Reply, State, {NewBase, Internal}};

handle_custom_return({AgentInteract, Reply, NewState}, State, reply,
		StateTuple) when State =:= oncall;
		State =:= oncall_ringing;
		State =:= warm_transfer_hold;
		State =:= warm_transfer_3rd_party;
		State =:= warm_transfer_merged ->
	{NextState, {BaseState, Internal}} = agent_interact(AgentInteract, State, StateTuple),
	NewBase = BaseState#base_state{substate = NewState},
	{reply, Reply, NextState, {NewBase, Internal}};

handle_custom_return({AgentInteract, NewState}, StateName, _Reply, State)
		when StateName =:= oncall;
		StateName =:= oncall_ringing;
		StateName =:= inqueue_ringing;
		StateName =:= warm_transfer_hold;
		StateName =:= warm_transfer_3rd_party;
		StateName =:= warm_transfer_merged ->
	{NextState, {BaseState, Internal}} = agent_interact(AgentInteract, StateName, State),
	NewBase = BaseState#base_state{substate = NewState},
	{next_state, NextState, {NewBase, Internal}}.

set_agent_state(Apid, Args) ->
	try apply(agent_channel, set_state, [Apid | Args]) of
		ok ->
			ok;
		Res ->
			lager:error("Agent (~p) set state: ~p wasn't okay:  ~p", [Apid, Args, Res]),
			Res
	catch
		exit:{noproc, _} ->
			lager:warning("Agent ~p is a dead pid", [Apid]),
			badagent;
		exit:{max_ringouts, _} ->
			lager:debug("Max ringouts reached for agent ~p", [Apid]),
			badagent
	end.

async_set_agent_state(Apid, Args) ->
	try apply(agent_channel, async_set_state, [Apid | Args]) of
		ok ->
			ok;
		Res ->
			lager:error("Agent (~p) set state: ~p wasn't okay:  ~p", [Apid, Args, Res]),
			Res
	catch
		exit:{noproc, _} ->
			lager:warning("Agent ~p is a dead pid", [Apid]),
			badagent;
		exit:{max_ringouts, _} ->
			lager:debug("Max ringouts reached for agent ~p", [Apid]),
			badagent
	end.

stop_agent_channel(Apid) ->
	try agent_channel:stop(Apid) of
		_ -> ok
	catch
		exit:{noproc, _} ->
			lager:warning("Agent ~p is a dead pid", [Apid]),
			badagent
	end.


handle_stop(hangup, StateName, BaseState, Internal) ->
	handle_stop({hangup, undefined}, StateName, BaseState, Internal);

handle_stop(Reason, inqueue, #base_state{callrec = Call} = BaseState,
		#inqueue_state{queue_pid = {Queuenom, _}} = Internal) ->
	lager:debug("Once queued in ~p, assuming something else handles hangup.  ~p", [Queuenom, Call#call.id]),
	set_cpx_mon({BaseState, Internal}, delete),
	case Reason of
		{hangup, _} ->
			{normal, {BaseState, Internal}};
		_ ->
			{Reason, {BaseState, Internal}}
	end;

handle_stop(Reason, StateName, #base_state{callrec = Call} = BaseState,
		Internal) when StateName =:= inivr; StateName =:= wrapup ->
	set_cpx_mon({BaseState, Internal}, delete),
	case {is_record(Call, call), Reason} of
		{true, {hangup, Who}} ->
			cdr:hangup(Call, Who);
		_ ->
			ok
	end,
	case Reason of
		{hangup, _} -> {normal, {BaseState, Internal}};
		_ -> {Reason, {BaseState, Internal}}
	end;

handle_stop(Reason, StateName, BaseState, Internal) ->
	set_cpx_mon({BaseState, Internal}, delete),
	{Who, Reason} = case Reason of
		{hangup, W} -> {W, normal};
		_ -> {undefined, Reason}
	end,
	agent_interact({hangup, Who}, StateName, {BaseState, Internal}),
	Reason.

enqueue(QueueN, Call, BaseSt) ->
	%% TODO if invalid
	QNPid = case priv_queue(QueueN, Call, true) of
		{default, Pid} ->
			{"default_queue", Pid};
		Else when is_pid(Else) ->
			{QueueN, Else};
		_ ->
			error
	end,

	case QNPid of
		{QN, QPid} ->
			#queued_call{skills = QSkills} =
				call_queue:get_qcall(QPid, self()),

			QMon = erlang:monitor(process, QPid),

			StateChanges1 = [{inqueue, os:timestamp()}|
				BaseSt#base_state.state_changes],

			{ok, Queue} = call_queue_config:get_queue(QN),

			Call1 = Call#call{
				skills = lists:usort(Call#call.skills ++ QSkills),
				state_changes=StateChanges1,
				call_segment=Call#call.call_segment + 1,
				wrapup_enabled = Queue#call_queue.wrapup_enabled,
				wrapup_timer = Queue#call_queue.wrapup_timer,
				auto_wrapup = Queue#call_queue.auto_wrapup,
				url_vars = Call#call.url_vars,
				queue = QN
			},

			BaseSt1 = BaseSt#base_state{
				callrec = Call1,
				state_changes = StateChanges1
			},

			IntSt = #inqueue_state{
				queue_pid = {QN, QPid},
				queue_mon = QMon
			},

			set_cpx_mon({BaseSt1, IntSt}, [{queue, QN}]),
			cdr:loginit(Call1),
			cdr:inqueue(Call1, QN),

			{ok, {BaseSt1, IntSt}};
		_ ->
			{error, queue_failed}
	end.

ps_to_call(Ps, Base, Callback, StateChanges) when is_record(Base, call) ->
	QueueN = ?get(queue, Ps, Base#call.queue),

	CallId = ?get(id, Ps, Base#call.id),
	Type = ?get(type, Ps, Base#call.type),
	ClientId = ?get(client, Ps),
	ClientOpts = ?get(client_opts, Ps, []),
	Skills = ?get(skills, Ps, Base#call.skills),
	CallerId = ?get(caller_id, Ps, Base#call.callerid),
	Dnis = ?get(dnis, Ps, Base#call.dnis),
	RingPath = ?get(ring_path, Ps, Base#call.ring_path),
	MediaPath = ?get(media_path, Ps, Base#call.media_path),
	Direction = ?get(direction, Ps, Base#call.direction),
	Priority = ?get(priority, Ps, Base#call.priority),
	Info = ?get(info, Ps, Base#call.info),
	UrlVars = ?get(url_vars, Ps, Base#call.url_vars),

	Client = case (ClientId =:= undefined) andalso
		is_record(Base#call.client, client) of
			true ->
				Base#call.client;
			_ ->
				%% @todo does not use default val
				Z = get_client_by_id(ClientId),
				Z#client{options = ClientOpts}
		end,

	#call{
		id =  CallId,
		type = Type,
		callerid = CallerId,
		dnis = Dnis,
		source = self(),
		source_module = Callback,
		client = Client,
		skills = Skills,
		queue = QueueN,
		ring_path = RingPath,
		media_path = MediaPath,
		direction = Direction,
		priority = Priority,
		info = Info,
		url_vars = UrlVars,
		state_changes = StateChanges
	};
ps_to_call(Ps, _, Callback, StateChanges) ->
	ps_to_call(Ps, #call{id="",source=Callback}, Callback, StateChanges).

url_pop(#call{client = Client} = Call, Agent, Addedopts) ->
	#client{options = DefaultOptions} = correct_client_sub(undefined),
	String = case {proplists:get_value(url_pop, Client#client.options), proplists:get_value(url_pop, DefaultOptions)} of
		{undefined, undefined} ->
			undefined;
		{undefined, []} ->
			undefined;
		{undefined, L} ->
			L;
		{[], []} ->
			undefined;
		{[], L} ->
			L;
		{L, _} ->
			L
	end,
	case String of
		undefined ->
			ok;
		_ ->
			Words = [
				{"label", (case is_atom(Client#client.label) of true -> atom_to_list(Client#client.label); false -> Client#client.label end)},
				{"clientid", (case is_atom(Client#client.id) of true -> atom_to_list(Client#client.id);
					false -> Client#client.id end)},
				{"callerid", element(1, Call#call.callerid) ++ " " ++ element(2, Call#call.callerid)},
				{"calleridname", element(1, Call#call.callerid)},
				{"calleridnum", element(2, Call#call.callerid)},
				{"callid", Call#call.id},
				{"destination", ""},
				{"ivroption", ""},
				{"media_type", atom_to_list(Call#call.type)},
				{"direction", atom_to_list(Call#call.direction)}
			],
			BaseUrl = util:string_interpolate(String, Words),
			Appender = fun({_Key, undefined}, Midurl) ->
					Midurl;
				({Key, Value}, Midurl) ->
				lists:append([Midurl, [$& | Key], [$= | Value]])
			end,
			Url = lists:foldl(Appender, BaseUrl, Addedopts),
			agent_channel:url_pop(Agent, Url, "ring")
	end.

get_client_by_id(undefined) ->
	case catch call_queue_config:get_default_client() of
		{ok, C} -> C;
		_ -> #client{}
	end;
get_client_by_id(L) ->
	case catch call_queue_config:get_client_by_id(L) of
		{ok, C} -> C;
		_ -> get_client_by_id(undefined)
	end.

%% TODO: Will always fail
correct_client_sub(undefined) ->
	Client = try call_queue_config:get_default_client() of
		{ok, C} ->
			C
	catch
		error:{case_clause, {aborted, {node_not_running, _Node}}} ->
			#client{}
	end,
	Client;
correct_client_sub({Id,Opts}) ->
	#client{options = Defaults} = Client = try call_queue_config:get_client_by_id(Id) of
		{ok, C} ->
			C;
		none ->
			correct_client_sub(undefined)
	catch
		error:{case_clause, {aborted, {node_not_running, _Node}}} ->
			#client{}
	end,
	Opts0 = lists:sort(Opts),
	Defs0 = lists:sort(Defaults),
	Opts1 = merge_defaults(Opts0,Defs0),
	Client#client{options = Opts1}.

merge_defaults(Opts,Defaults) ->
	merge_defaults(Opts,Defaults,[]).

merge_defaults([],Rest,Acc) ->
	lists:append(Rest,Acc);
merge_defaults(Rest,[],Acc) ->
	lists:append(Rest,Acc);
merge_defaults([{Key,_Val} = H | OTail], [{Key,_Val1} | DTail], Acc) ->
	merge_defaults(OTail,DTail,[H|Acc]);
merge_defaults([{OKey,_} = H | OTail], [{DKey,_} | _] = Defs, Acc) when OKey > DKey ->
	merge_defaults(OTail,Defs,[H | Acc]);
merge_defaults(Opts, [H | Tail], Acc) ->
	merge_defaults(Opts, Tail, [H | Acc]).

-spec(set_cpx_mon/2 :: (State :: {#base_state{}, any()}, Action :: proplist() | 'delete') -> 'ok').
set_cpx_mon({#base_state{callrec = Call}, _}, delete) ->
	cpx_monitor:drop({media, Call#call.id});
set_cpx_mon({#base_state{callrec = _Call}, _} = State, Details) ->
	set_cpx_mon(State, Details, ignore).

set_cpx_mon({#base_state{callrec = Call}, _}, Details, Watch) ->
	Client = Call#call.client,
	MidBasedet = [
		{type, Call#call.type},
		{callerid, Call#call.callerid},
		{dnis, Call#call.dnis},
		{client, Client},
		{ring_path, Call#call.ring_path},
		{media_path, Call#call.media_path},
		{direction, Call#call.direction},
		{node, node()}
	],
	{_Hp, Basedet} = case {proplists:get_value(queue, Details), proplists:get_value(agent, Details)} of
		{undefined, undefined} ->
			{[], MidBasedet};
		{undefined, _A} ->
			{[{agent_link, {0, 60 * 5, 60 * 15, {time, util:now()}}}], MidBasedet};
		{_Q, _} ->
			{[{inqueue, {0, 60 * 5, 60 * 10, {time, util:now()}}}], [{queued_at, {timestamp, util:now()}}, {priority, Call#call.priority} | MidBasedet]}
	end,
	Fulldet = lists:append([Basedet, Details]),
	cpx_monitor:set({media, Call#call.id}, Fulldet, Watch).

priv_queue(Queue, Callrec, Failover) ->
	case queue_manager:get_queue(Queue) of
		undefined ->
			lager:warning("Uh oh, no queue of ~p, failover:  ~w for ~p", [Queue, Failover, Callrec#call.id]),
			case Failover of
				true ->
					Dqpid = queue_manager:get_queue("default_queue"),
					call_queue:add(Dqpid, self(), Callrec),
					%% yes, we do want this to die if the default queue can't be found
					{default, Dqpid};
				false ->
					invalid
			end;
		Qpid ->
			lager:debug("Trying to add ~p to queue...", [Callrec#call.id]),
			R = call_queue:add(Qpid, self(), Callrec),
			lager:debug("q response:  ~p for ~p", [R, Callrec#call.id]),
			lager:info("Queueing call ~s into ~s", [Callrec#call.id, Queue]),
			Qpid
	end.

priv_voicemail({BaseState, #inqueue_ringing_state{ring_mon = Rmon, ring_pid = {_, Rpid}} = Internal}) ->
	erlang:demonitor(Rmon),
	stop_agent_channel(Rpid),
	NewInternal = #inqueue_state{
		queue_mon = Internal#inqueue_ringing_state.queue_mon,
		queue_pid = Internal#inqueue_ringing_state.queue_pid,
		cook = Internal#inqueue_ringing_state.cook
	},
	priv_voicemail({BaseState, NewInternal});

priv_voicemail({BaseState, #inqueue_state{queue_mon = Mon, queue_pid = {QNom, _QPid}}}) ->
	erlang:demonitor(Mon),
	% call_queue:remove(QPid, self()),
	cdr:voicemail(BaseState#base_state.callrec, QNom),
	ok.

requeue_ringing(Reason, {BaseState, Internal}) ->
	#inqueue_ringing_state{ringout = Ringout,
		ring_pid = RingAgent, ring_mon = RMon} = Internal,
	#base_state{callrec = Call,
		state_changes = StateChanges} = BaseState,
	NewStateChanges = [{inqueue, os:timestamp()} | StateChanges],
	NewBase = BaseState#base_state{state_changes = NewStateChanges},
	set_gproc_prop(inqueue_ringing, inqueue, NewBase),

	case Call#call.cook of
		CookPid when is_pid(CookPid) ->
			gen_server:cast(CookPid, stop_ringing);
		_ ->
			ok
	end,
	case Ringout of
		undefined -> ok;
		_ -> gen_fsm:cancel_timer(Ringout)
	end,
	case RingAgent of
		undefined -> ok;
		{Nom, Apid} ->
			erlang:demonitor(RMon),
			stop_agent_channel(Apid),
			cdr:ringout(Call, {Reason, Nom})
	end,
	NewInternal = #inqueue_state{
		queue_mon = Internal#inqueue_ringing_state.queue_mon,
		queue_pid = Internal#inqueue_ringing_state.queue_pid,
		cook = Internal#inqueue_ringing_state.cook
	},
	{NewBase, NewInternal}.

% make the call higher priority in preparation for requeueing
reprioritize_for_requeue(Call) ->
	NewPriority = case Call#call.priority of
		P when P < 5 ->
			0;
		P ->
			P - 5
	end,
	Call#call{priority = NewPriority}.

unqueue(undefined, _Callpid) ->
	ok;
unqueue({_Qnom, Qpid}, Callpid) when is_pid(Qpid) ->
	unqueue(Qpid, Callpid);
unqueue(Qpid, Callpid) when is_pid(Qpid) ->
	call_queue:remove(Qpid, Callpid),
	ok.

kill_outband_ring({_, #inqueue_ringing_state{outband_ring_pid = P}}) when is_pid(P) ->
	freeswitch_ring:hangup(P);
kill_outband_ring(_) ->
	ok.

extract_agent(#inqueue_ringing_state{ring_pid = O, ring_mon = M}) ->
	{M, O};
extract_agent(#oncall_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#oncall_ringing_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#warm_transfer_hold_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#warm_transfer_3rd_party_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#warm_transfer_merged_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O}.

agent_interact(Action, StateName, {BaseState, Internal}) ->
	Agent = extract_agent(Internal),
	agent_interact(Action, StateName, BaseState, Internal, Agent).

agent_interact({mediapush, Data}, StateName, #base_state{
		callrec = Call} = BaseState, Internal, {_, {_, Ocpid}}) ->
	lager:debug("Shoving ~p from ~p", [Data, Call#call.id]),
	agent_channel:media_push(Ocpid, Call, Data),
	{StateName, {BaseState, Internal}};

agent_interact(stop_ring, StateName, BaseState, Internal, Agent) ->
	agent_interact({stop_ring, undefined}, StateName, BaseState, Internal, Agent);

agent_interact({stop_ring, Reason}, oncall_ringing, #base_state{
		callrec = Call} = BaseState, #oncall_ringing_state{ringout = Ringout,
		ring_pid = RingAgent, ring_mon = RMon} = Internal, _Agent) ->
	case Call#call.cook of
		CookPid when is_pid(CookPid) ->
			gen_server:cast(CookPid, stop_ringing);
		_ ->
			ok
	end,
	case Ringout of
		undefined -> ok;
		_ -> gen_fsm:cancel_timer(Ringout)
	end,
	case RingAgent of
		undefined -> ok;
		{Nom, Apid} ->
			set_agent_state(Apid, [idle]),
			cdr:ringout(Call, {Reason, Nom}),
			erlang:demonitor(RMon)
	end,
	NewInternal = #oncall_state{
		oncall_mon = Internal#oncall_ringing_state.oncall_mon,
		oncall_pid = Internal#oncall_ringing_state.oncall_pid
	},
	{oncall, {BaseState, NewInternal}};

agent_interact({stop_ring, Reason}, inqueue_ringing, BaseState, Internal, _Agent) ->
	NewState = requeue_ringing(Reason, {BaseState, Internal}),
	{inqueue, NewState};

agent_interact(wrapup, _StateName, #base_state{callrec = Call} = BaseState,
		_Internal, {Mon, {Agent, Apid}}) ->
	lager:info("Attempting to set agent at ~p to wrapup for ~p", [Apid, Call#call.id]),
	set_agent_state(Apid, [wrapup, Call]),
	cdr:wrapup(Call, Agent),
	erlang:demonitor(Mon),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact(hangup, StateName, BaseState, Internal, Agent) ->
	Hangup = {hangup, undefined},
	agent_interact(Hangup, StateName, BaseState, Internal, Agent);

agent_interact({hangup, Who}, oncall_ringing, #base_state{
		callrec = Callrec} = BaseState, #oncall_ringing_state{
		ring_mon = Rmon, ring_pid = Rpid} = Internal,
		{OcMonref, {OcName, Ocpid}}) ->
	lager:info("hangup for ~s when oncall_ringing", [Callrec#call.id]),
	set_agent_state(element(2, Rpid), [idle]),
	set_agent_state(Ocpid, [wrapup, Callrec]),
	cdr:wrapup(Callrec, OcName),
	cdr:hangup(Callrec, Who),
	kill_outband_ring(Internal),
	erlang:demonitor(Rmon),
	erlang:demonitor(OcMonref),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact({hangup, Who}, inqueue_ringing, #base_state{
		callrec = Call} = BaseState, Internal, {Rmon, {_Rname, Rpid}}) ->
	lager:info("hangup for ~p when both agent and queue are pid", [Call#call.id]),
	set_agent_state(Rpid, [idle]),
	cdr:hangup(Call, Who),
	kill_outband_ring(Internal),
	erlang:demonitor(Rmon),
	#inqueue_ringing_state{queue_pid = {_, Qpid}, queue_mon = Qmon} = Internal,
	unqueue(Qpid, self()),
	erlang:demonitor(Qmon),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact({hangup, Who}, oncall, #base_state{callrec = Callrec, conference_channel = ConferenceChannel} =
		BaseState, _Internal, {Mon, {Agent, Apid}}) ->
	lager:info("hangup by ~p for ~p when only oncall is a pid; skipping wrapup call to agent_channel", [Who, Callrec#call.id]),
	% set_agent_state(Apid, [wrapup, Callrec]),
	agent_channel:set_state(ConferenceChannel, wrapup, BaseState#base_state.callrec),
	cdr:wrapup(Callrec, Agent),
	cdr:hangup(Callrec, Who),
	erlang:demonitor(Mon),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact({hangup, _Who}, State, #base_state{callrec = Callrec} =
		BaseState, Internal, {_Mon, {_Agent, _Apid}}) when
		is_record(Callrec, call) ->
	lager:info("hangup for ~p while in state ~p; not taking action at this time.", [Callrec#call.id, State]),
	{State, {BaseState, Internal}}.

init_gproc_prop(MediaState, FsmState) ->
	Prop = get_gproc_prop(MediaState, FsmState),
	gproc:reg({p, l, cpx_media}, Prop),

	Event = #cpx_gen_media_init{pid = self(), now = now(), prop = Prop},
	gproc:send({p, l, cpx_media_change}, Event).

set_gproc_prop(OldMediaState, MediaState, FsmState) ->
	Prop = get_gproc_prop(MediaState, FsmState),
	gproc:set_value({p, l, cpx_media}, Prop),

	Event = #cpx_gen_media_update{pid = self(), now = now(), state = MediaState, old_state = OldMediaState, prop = Prop},
	gproc:send({p, l, cpx_media_change}, Event).

-spec get_gproc_prop(MediaState :: atom(), BaseState :: #base_state{}) -> #cpx_gen_media_prop{}.
get_gproc_prop(State, BaseState) ->
	CallRec = BaseState#base_state.callrec,
	AgentRec = BaseState#base_state.agent,
	Queue = case CallRec of
		#call{queue = Q} ->
			Q;
		_ ->
			undefined
	end,
	Client = case CallRec of
		#call{client = Cl} ->
			Cl;
		_ ->
			undefined
	end,
	{ALogin, AProfile} = case AgentRec of
		#agent{login = L, profile = P} ->
			{L, P};
		_ ->
			{undefined, undefined}
	end,
	#cpx_gen_media_prop{state = State, queue = Queue, call = CallRec, client = Client, state_changes = BaseState#base_state.state_changes, agent_login = ALogin, agent_profile = AProfile}.

-ifdef(TEST).

% dead_spawn() ->
% 	spawn(fun() -> ok end).

% url_pop_test_() ->
% 	{setup,
% 	fun() ->
% 		{ok, Agent} = gen_server_mock:new(),
% 		Call = #call{id = "testcall", source = dead_spawn()},
% 		{Call, Agent, undefined}
% 	end,
% 	fun({_, Agent, Conn}) ->
% 		gen_server_mock:stop(Agent)
% 	end,
% 	fun({BaseCall, Agent, Conn}) ->
% 		[{"no url pop defined in client",
% 		fun() ->
% 			Call = BaseCall#call{client = #client{label = "client", id = "client", options = []}},
% 			% if the mock (Conn) gets a cast, it'll error; that's the test.
% 			% if it error's, it's a fail.
% 			url_pop(Call, Agent, [])
% 		end},
% 		{"url is an empty list",
% 		fun() ->
% 			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, []}]}},
% 			% same as above.
% 			url_pop(Call, Agent, [])
% 		end},
% 		{"url is set",
% 		fun() ->
% 			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com"}]}},
% 			gen_server_mock:expect_info(Agent, fun({'$gen_all_state_event', {url_pop, "example.com", "ring"}}, _) -> ok end),
% 			url_pop(Call, Agent, []),
% 			gen_server_mock:assert_expectations(Agent)
% 		end},
% 		{"url is set with some additional options",
% 		fun() ->
% 			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com?a=b"}]}},
% 			gen_server_mock:expect_info(Agent, fun({'$gen_all_state_event', {url_pop, "example.com?a=b&addkey=addval", "ring"}}, _) -> ok end),
% 			url_pop(Call, Agent, [{"addkey", "addval"}]),
% 			gen_server_mock:assert_expectations(Agent)
% 		end},
% 		{"url is set with some additional options, some of which are blank",
% 		fun() ->
% 			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com?a=b"}]}},
% 			gen_server_mock:expect_info(Agent, fun({'$gen_all_state_event', {url_pop, "example.com?a=b&addkey=addval", "ring"}}, _) -> ok end),
% 			url_pop(Call, Agent, [{"addkey", "addval"}, {"foo", undefined}]),
% 			gen_server_mock:assert_expectations(Agent)
% 		end}
% 	]
% 	end}.

%% TODO Fix tests.
% init_test_d() ->
% 	util:start_testnode(),
% 	N = util:start_testnode(gen_media_init_tests),
% 	{spawn, N, {foreach,
% 	fun() ->
% 		{ok, QMmock} = gen_leader_mock:start(queue_manager),
% 		{ok, Qpid} = gen_server_mock:new(),
% 		Assertmocks = fun() ->
% 			gen_server_mock:assert_expectations(Qpid),
% 			gen_leader_mock:assert_expectations(QMmock)
% 		end,
% 		{QMmock, Qpid, Assertmocks}
% 	end,
% 	fun({QMmock, Qpid, _Assertmocks}) ->
% 		gen_leader_mock:stop(QMmock),
% 		gen_server_mock:stop(Qpid),
% 		timer:sleep(10)
% 	end,
% 	[
% 	fun({_, _, Assertmocks}) ->
% 			{"call rec returned, but no queue",
% 			fun() ->
% 				Args = [[{id, "dummy"}, {queues, none}], success],
% 				Res = init([dummy_media, Args]),
% 				?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}}}, Res),
% 				Assertmocks()
% 			end}
% 		end,
% 		fun({QMmock, Qpid, Assertmocks}) ->
% 			{"call rec and queue name returned",
% 			fun() ->
% 				Args = [[{queues, ["testqueue"]}, {id, "dummy"}], success],
% 				gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
% 					{ok, Qpid, State}
% 				end),
% 				gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
% 				Res = init([dummy_media, Args]),
% 				?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = {"testqueue", Qpid}}}, Res),
% 				#state{monitors = Mons} = element(2, Res),
% 				?assertNot(undefined =:= Mons#monitors.queue_pid),
% 				Assertmocks()
% 			end}
% 		end,
% 		fun({QMmock, Qpid, Assertmocks}) ->
% 			{"call rec and queue name returned, but queue doesn't exist",
% 			fun() ->
% 				Args = [[{queues, ["testqueue"]}, {id, "dummy"}], success],
% 				gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
% 					{ok, undefined, State}
% 				end),
% 				gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
% 					{ok, Qpid, State}
% 				end),
% 				gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
% 				Res = init([dummy_media, Args]),
% 				?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = {"default_queue", Qpid}}}, Res),
% 				Assertmocks()
% 			end}
% 		end
% ]}}.

% -record(state_changes_mocks, {
% 	queue_manager,
% 	queue,
% 	agent_manager,
% 	cdr,
% 	assert,
% 	make_state
% }).

% handle_state_changes_test_d() ->
% 	util:start_testnode(),
% 	N = util:start_testnode(gen_media_state_changes_tests),
% 	{spawn, N, {setup,
% 	fun() ->
% 		{ok, QMmock} = gen_leader_mock:start(queue_manager),
% 		{ok, Qpid} = gen_server_mock:new(),
% 		{ok, Ammock} = gen_leader_mock:start(agent_manager),
% 		gen_event:start({local, cdr}),
% 		Assertmocks = fun() ->
% 			gen_server_mock:assert_expectations(Qpid),
% 			gen_leader_mock:assert_expectations(QMmock),
% 			gen_leader_mock:assert_expectations(Ammock),
% 			gen_event_mock:assert_expectations(cdr)
% 		end,
% 		Makestate = fun() ->
% 			{ok, {#base_state{callrec = Callrec} = Out, _}} = init([dummy_media, [[{queues, none}], success]]),
% 			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
% 			Out
% 		end,
% 		#state_changes_mocks{ queue_manager = QMmock, queue = Qpid,
% 			agent_manager = Ammock, assert = Assertmocks, make_state = Makestate
% 		}
% 	end,
% 	fun(Mocks) ->
% 		gen_server_mock:stop(Mocks#state_changes_mocks.queue_manager),
% 		gen_leader_mock:stop(Mocks#state_changes_mocks.queue),
% 		gen_leader_mock:stop(Mocks#state_changes_mocks.agent_manager),
% 		gen_event:stop(cdr)
% 	end, [
% 	]}}.





%handle_call_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_handle_call_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		{ok, QMmock} = gen_leader_mock:start(queue_manager),
%		{ok, Qpid} = gen_server_mock:new(),
%		{ok, Ammock} = gen_leader_mock:start(agent_manager),
%		gen_event:start({local, cdr}),
%		Assertmocks = fun() ->
%			gen_server_mock:assert_expectations(Qpid),
%			gen_leader_mock:assert_expectations(QMmock),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end,
%		Makestate = fun() ->
%			{ok, #state{callrec = Callrec} = Out} = init([dummy_media, [[{queues, none}], success]]),
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			Out
%		end,
%		{Makestate, QMmock, Qpid, Ammock, Assertmocks}
%	end,
%	fun({_Makestate, QMmock, Qpid, Ammock, _Assertmocks}) ->
%		gen_server_mock:stop(Qpid),
%		gen_leader_mock:stop(QMmock),
%		gen_leader_mock:stop(Ammock),
%		gen_event:stop(cdr),
%		timer:sleep(10)
%	end,
%	[fun({Makestate, _QMock, _Qpid, _Ammock, Assertmocks}) ->
%		{"spying when there's no agent oncall fails",
%		fun() ->
%			Seedstate = Makestate(),
%			?assertMatch({reply, invalid, Seedstate}, handle_call({'$gen_media', spy, "Pid", "AgentRec"}, "from", Seedstate)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
%		{"Spy is not the pid making the request",
%		fun() ->
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Spy} = agent:start(#agent{login = "testagent"}),
%			Seedstate = Makestate(),
%			State = Seedstate#state{oncall_pid = {"testagent", Spy}},
%			?assertMatch({reply, invalid, State}, handle_call({'$gen_media', spy, Spy, "AgentRec"}, {self(), "tag"}, State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMock, _Qpid, _Ammock, _Assertmocks}) ->
%		{"Can't spy on yourself",
%		fun() ->
%			Seedstate = Makestate(),
%			Spy = dead_spawn(),
%			State = Seedstate#state{oncall_pid = {"testagent", Spy}},
%			?assertMatch({reply, invalid, State}, handle_call({'$gen_media', spy, Spy, "AgentRec"}, {Spy, "tag"}, State))
%		end}
%	end,
%	fun({_Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
%		{"Spy valid, callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			Ocpid = dead_spawn(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			SpyRec = #agent{login = "testagent"},
%			{ok, Spy} = agent:start(SpyRec),
%			?assertMatch({reply, invalid, _Newstate}, handle_call({'$gen_media', spy, Spy, SpyRec}, {Spy, "tag"}, Seedstate#state{oncall_pid = {"testagent", Ocpid}})),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
%		{"Spy valid, callback says ok",
%		fun() ->
%			Seedstate = Makestate(),
%			Ocpid = dead_spawn(),
%			State = Seedstate#state{oncall_pid = {"ocagent", Ocpid}},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			SpyRec = #agent{login = "testagent"},
%			{ok, Spy} = agent:start(SpyRec),
%			?assertMatch({reply, ok, _Newstate}, handle_call({'$gen_media', spy, Spy}, {Spy, "tag"}, State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _, _, Ammock, Assertmocks}) ->
%		{"oncall_pid requests wrapup",
%		fun() ->
%			Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrer, _Time, "agent"}, _State) -> ok end),
%			Monref = make_ref(),
%			Mons = #monitors{oncall_pid = Monref},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			Out = handle_call('$gen_media', wrapup, {Agent, "tag"}, State),
%			?assertMatch({stop, normal, ok, _State}, Out),
%			#state{monitors = Newmon} = element(4, Out),
%			?assertEqual(undefined, Newmon#monitors.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall_pid can't request wrapup when media_path is outband",
%		fun() ->
%			#state{callrec = Oldcall} = Seedstate = Makestate(),
%			Callrec = Oldcall#call{media_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, callrec = Callrec},
%			?assertMatch({reply, invalid, _State}, handle_call('$gen_media', wrapup, {Agent, "tag"}, State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"sending to queue requested by oncall pid, all works",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
%				Mpid = Callrec#call.source,
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, {Agent, "tag"}, State),
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall pid sends call to queue, but falls back to default queue",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, MState, _Elec) ->
%				{ok, undefined, MState}
%			end),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, MState, _Elec) ->
%				{ok, Qpid, MState}
%			end),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
%				Mpid = Callrec#call.source,
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, {Agent, "tag"}, State),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall pid sends call to queue, but falls back to nowhere w/ fallback set to false",
%		fun() ->
%			Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, queue_failover = false, monitors = Mons},
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, MState, _Elec) ->
%				{ok, undefined, MState}
%			end),
%			Out = handle_call({'$gen_media', queue, "testqueue"}, {Agent, "tag"}, State),
%			?assertMatch({reply, invalid, _State}, Out),
%			Newstate = element(3, Out),
%			?assertEqual(Mons, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"sent to queue by something else, and alls well.",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
%				Mpid = Callrec#call.source,
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_leader_mock:expect_leader_call(queue_manager, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, "from", State),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"sent to queue by something else, but falling back",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _state) ->
%				Mpid = Callrec#call.source,
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, undefined, State}
%			end),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, "from", State),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_ring setting agent successful, as is the callback module.",
%		fun() ->
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_event_mock:expect_event(cdr, fun({ringing, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
%			{reply, ok, Newstate} = handle_call({'$gen_media', ring, {"testagent", Agent}, Qcall, 100}, {Cook, "tag"}, Seedstate),
%			receive
%				{'$gen_media', stop_ring, Cook} ->
%					ok
%			after 150 ->
%				erlang:error(timer_timeout)
%			end,
%			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
%			?assertNot(false =:= Newstate#state.ringout),
%			Mons = Newstate#state.monitors,
%			?assertNot(undefined =:= Mons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_ring setting the agent fails.",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = "whatever"}),
%			Out = handle_call({'$gen_media', ring, {"testagent", Agent}, Qcall, 100}, {Cook, "tag"}, Seedstate),
%			?assertMatch({reply, invalid, _State}, Out),
%			receive
%				{'$gen_media', stop_ring, Cook} ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			Newstate = element(3, Out),
%			?assertEqual(Seedstate#state.monitors, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_ring callback module fails",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
%			%gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({now_avail, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
%			{reply, invalid, Newstate} = handle_call({'$gen_media', ring, {"testagent", Agent}, Qcall, 150}, {Cook, "tag"}, Seedstate),
%			receive
%				{'$gen_media', stop_ring, Cook} ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual({ok, idle}, agent:query_state(Agent)),
%			?assertEqual(Seedstate#state.monitors, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"can't transfer to yourself, silly!",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			?assertEqual({reply, invalid, State}, handle_call({'$gen_media', agent_transfer, {"testagent", Agent}}, "from", State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent transfer, target agent can't change state",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Target} = agent:start(#agent{login = "targetagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			?assertEqual({reply, invalid, State}, handle_call({'$gen_media', agent_transfer, {"targetagent", Target}, 100}, "from", State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent transfer, all is well",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			%% cdr makes 2 call outs to this, but that will be tested in cdr
%			gen_event_mock:expect_event(cdr, fun({agent_transfer, _Callrec, _Time, {"testagent", "targetagent"}}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({ringing, _Callrec, _Time, "targetagent"}, _State) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "targetagent"}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Target} = agent:start(#agent{login = "targetagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', agent_transfer, {"targetagent", Target}, 100}, "from", State),
%			receive
%				{'$gen_media', stop_ring, _Cook} ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertEqual({"targetagent", Target}, Newstate#state.ring_pid),
%			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
%			?assertNot(false =:= Newstate#state.ringout),
%			?assertEqual({ok, ringing}, agent:query_state(Target)),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(Mons#monitors.oncall_pid, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent transfer, callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Target} = agent:start(#agent{login = "testagent"}),
%			Agent = spawn(fun() -> ok end),
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}},
%			{reply, invalid, Newstate} = handle_call({'$gen_media', agent_transfer, Target, 100}, "from", State),
%			receive
%				{'$gen_media', ring_stop, _Cook} ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual({ok, idle}, agent:query_state(Target)),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, _Ammock, Assertmocks}) ->
%		{"gen_media_announce",
%		fun() ->
%			{ok, #state{callrec = Call} = Seedstate} = init([dummy_media, [[{queues, none}], success]]),
%			gen_event_mock:supplant(cdr, {{cdr, Call#call.id}, []}),
%			{reply, ok, Newstate} = handle_call({'$gen_media', announce, "doesn't matter"}, "from", Seedstate),
%			?debugFmt("~p", [Seedstate]),
%			?debugFmt("~p", [Newstate]),
%			?assertEqual({reply, ok, Seedstate}, handle_call({'$gen_media', announce, "doesn't matter"}, "from", Seedstate)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
%		{"gen_media_voicemail works",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			Mons = #monitors{queue_pid = make_ref()},
%			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, monitors = Mons},
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({voicemail, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			Out = handle_call('$gen_media', voicemail, "from", State),
%			?assertMatch({reply, ok, _State}, Out),
%			#state{monitors = Newmons} = element(3, Out),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_voicemail while an agent's ringing",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({now_avail, "testagent"}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{queue_pid = make_ref(), ring_pid = make_ref()},
%			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, ring_pid = {"testagent", Agent}, monitors = Mons},
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({voicemail, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			{reply, ok, NewState} = handle_call('$gen_media', voicemail, "from", State),
%			Newmons = NewState#state.monitors,
%			?assertEqual(undefined, NewState#state.ring_pid),
%			?assertEqual({ok, idle}, agent:query_state(Agent)),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
%		{"gen_media_voicemail callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			#state{callrec = Call} = State = Seedstate#state{queue_pid = {"default_queue", Qpid}},
%			gen_event_mock:supplant(cdr, {{cdr, Call#call.id}, []}),
%			?assertMatch({reply, invalid, _State}, handle_call('$gen_media', voicemail, "from", State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, _Ammock, Assertmocks}) ->
%		{"Agent can't request oncall if ring_path is outband",
%		fun() ->
%			#state{callrec = Seedcall} = Seedstate = Makestate(),
%			Agent = spawn(fun() -> ok end),
%			Callrec = Seedcall#call{ring_path = outband},
%			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}},
%			?assertEqual({reply, invalid, State}, handle_call('$gen_media', agent_oncall, {Agent, "tag"}, State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ringing"}),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "ringing"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "oncall"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ring_pid = {"ringing", Ring}, ringout = Tref, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, {Ring, "tag"}, State),
%			receive
%				timer_lives ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"ringing", Ring}, Newstate#state.oncall_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual({ok, wrapup}, agent:query_state(Oncall)),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ring"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ring_pid = {"ring", Ring}, ringout = Tref, monitors = Mons},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, {Ring, "tag"}, State),
%			receive
%				timer_lives ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertEqual(State, Newstate),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall during transfer with outband media",
%		fun() ->
%			#state{callrec = Seedcall} = Seedstate = Makestate(),
%			Callrec = Seedcall#call{ring_path = outband},
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "ring"}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ring"}),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "ring"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "oncall"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{
%				oncall_pid = make_ref(),
%				ring_pid = make_ref()
%			},
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ringout = Tref, ring_pid = {"ring", Ring}, callrec = Callrec, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertEqual({ok, oncall}, agent:query_state(Ring)),
%			?assertEqual({ok, wrapup}, agent:query_state(Oncall)),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"ring", Ring}, Newstate#state.oncall_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall during transfer with outband media, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Seedcall = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
%			Callrec = Seedcall#call{ring_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "ring"}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ring"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ringout = Tref, ring_pid = {"ring", Ring}, callrec = Callrec},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			% TODO Two agents oncall due to why going on call is handled :/
%			% since there's no way to roll back an oncall, if the handle_answer
%			% callback fails, we have a f*cked state.
%			?assertEqual({ok, oncall}, agent:query_state(Ring)),
%			?assertEqual({ok, oncall}, agent:query_state(Oncall)),
%			?assertEqual({"ring", Ring}, Newstate#state.ring_pid),
%			?assertEqual(Tref, Newstate#state.ringout),
%			?assertEqual({"oncall", Oncall}, Newstate#state.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall queue to agent requested by agent with inband media",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{ring_pid = make_ref(), queue_pid = make_ref()},
%			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, ring_pid = {"testagent", Agent}, ringout = Tref, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, {Agent, "tag"}, State),
%			receive
%				timer_lives ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual("default_queue", Newstate#state.queue_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall quee to agent request by agent, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ringout = Tref, ring_pid = {"testagent", Agent}, queue_pid = {"default_queue", Qpid}, monitors = Mons},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, {Agent, "tag"}, State),
%			receive
%				timer_lives ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(Tref, Newstate#state.ringout),
%			?assertEqual(Mons, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall queue to agent requst by whoever with outband media",
%		fun() ->
%			#state{callrec = Seedcall} = Seedstate = Makestate(),
%			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "testagent"}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{ring_pid = make_ref(), queue_pid = make_ref()},
%			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}, queue_pid = {"default_queue", Qpid}, ringout = Tref, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
%			?assertEqual("default_queue", Newstate#state.queue_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall queue to agent request by whoever with outband media, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Seedcall = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
%			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{
%				oncall_pid = make_ref(),
%				queue_pid = make_ref()
%			},
%			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}, ringout = Tref, queue_pid = {"default_queue", Qpid}, monitors = Mons},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertNot(false =:= Newstate#state.ringout),
%			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(Mons, Newstate#state.monitors),
%			Assertmocks()
%%		end}
%%	end,
%%	fun({Makestate, _QMmock, _Qpid, _Ammock, _Assertmocks}) ->
%%		{"late oncall request (ring_pid is undefined)",
%%		fun() ->
%%			Seedstate = Makestate(),
%%			State = Seedstate#state{ring_pid = undefined},
%%			?assertMatch({reply, invalid, State}, handle_call('$gen_media', agent_oncall, "from", State))
%%		end}
%%	end,
%%	fun({_Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
%%		{"oncall request to agent that is no longer running",
%%		fun() ->
%%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
%%			Seedcall = Seedstate#state.callrec,
%%			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
%%			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
%%			Agent = spawn(fun() -> ok end),
%%			{ok, Tref} = timer:send_after(100, timer_lives),
%%			Mon = #monitors{ring_pid = make_ref()},
%%			State = Seedstate#state{callrec = Callrec, ring_pid = {"deadagent", Agent}, ringout = Tref, queue_pid = {"default_queue", Qpid}, monitors = Mon},
%%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, _Data}, _) -> ok end),
%%			Out = handle_call('$gen_media', agent_oncall, "from", State),
%%			?assertMatch({reply, invalid, _}, Out),
%%			{reply, invalid, Newstate} = Out,
%%			receive
%%				timer_lives ->
%%					erlang:error(timer_lives)
%%			after 150 ->
%%				ok
%%			end,
%%			?assert(false == Newstate#state.ringout),
%%			?assertEqual(undefined, Newstate#state.ring_pid),
%%			?assertEqual({"default_queue", Qpid}, State#state.queue_pid),
%%			Assertmocks()
%%		end}
%%	end]}}.
%
%handle_cast_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_handle_cast),
%	{spawn, N, {foreach,
%	fun() ->
%		Call = #call{
%			id = "testcall",
%			source = self()
%		},
%		{#state{callrec = Call}}
%	end,
%	fun(_) ->
%		ok
%	end,
%	[fun({Seedstate}) ->
%		{"setting outband ring pid",
%		fun() ->
%			P = dead_spawn(),
%			{noreply, #state{outband_ring_pid = NewP}} = handle_cast({'$gen_media', set_outband_ring_pid, P}, Seedstate),
%			?assertEqual(P, NewP)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"setting the cook with no previous mons",
%		fun() ->
%			P = spawn(fun() ->
%				receive
%					done ->
%						ok
%				end
%			end),
%			{noreply, #state{callrec = Newcall, monitors = Mons}} = handle_cast({'$gen_media', set_cook, P}, Seedstate),
%			?assert(Mons#monitors.cook =/= undefined andalso is_reference(Mons#monitors.cook)),
%			?assertEqual(P, Newcall#call.cook),
%			P ! done
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"setting additional url pop opts",
%		fun() ->
%			{noreply, #state{url_pop_getvars = Urlget}} = handle_cast({'$gen_media', set_url_getvars, [{"key", "val"}]}, Seedstate),
%			?assertEqual([{"key", "val"}], Urlget)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"setting new url pop opts doen't nix old ones",
%		fun() ->
%			State = Seedstate#state{url_pop_getvars = [{"oldkey", "oldval"}]},
%			{noreply, #state{url_pop_getvars = Newget}} = handle_cast({'$gen_media', set_url_getvars, [{"newkey", "newval"}]}, State),
%			?assertEqual("oldval", proplists:get_value("oldkey", Newget)),
%			?assertEqual("newval", proplists:get_value("newkey", Newget))
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"Adding new skills",
%		fun() ->
%			{noreply, #state{callrec = Callrec}} = handle_cast({'$gen_media', add_skills, [cookskill, {'_agent', "anagent"}]}, Seedstate),
%			?assertEqual([cookskill, {'_agent', "anagent"}], Callrec#call.skills)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"Adding existing skills",
%		fun() ->
%			Call = Oldcall#call{skills = [cookskill]},
%			State = Seedstate#state{callrec = Call},
%			{noreply, #state{callrec = Newcall}} = handle_cast({'$gen_media', add_skills, [cookskill]}, State),
%			?assertEqual([cookskill], Newcall#call.skills)
%		end}
%	end]}}.
%
%handle_info_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_handle_info_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
%		{Seedstate}
%	end,
%	fun(_) ->
%		ok
%	end,
%	[fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"agent requests a ring stop",
%		fun() ->
%			{ok, Apid} = agent:start(#agent{login = "testagent"}),
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
%			Callrec = Oldcall#call{cook = Cook},
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Apid}, State),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_server_mock:assert_expectations(Cook),
%			gen_server_mock:stop(Cook),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with no ring_pid defined",
%		fun() ->
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, "doesn't matter"}, Seedstate),
%			?assertEqual(Seedstate, Newstate)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with no ringout handled",
%		fun() ->
%			Pid = spawn(fun() -> ok end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = Pid, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, "doesn't matter"}, State),
%			?assertEqual(State, Newstate)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with a live agent ringing",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
%			gen_leader_mock:expect_cast(Am, fun({now_avail, _}, _, _) -> ok end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"testagent", Agent}, ringout = true, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Cook}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertEqual({ok, idle}, agent:query_state(Agent)),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with a live agent in wrong state",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"testagent", Agent}, ringout = true, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Cook}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertEqual({ok, oncall}, agent:query_state(Agent)),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with a dead agent",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "doesn't matter", State} end),
%			Agent = spawn(fun() -> ok end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"deadagent", Agent}, ringout = true, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Cook}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"queue pid goes down",
%		fun() ->
%			Qpid = dead_spawn(),
%			Qref = make_ref(),
%			Mons = #monitors{queue_pid = Qref},
%			State = Seedstate#state{monitors = Mons, queue_pid = {"testqueue", Qpid}},
%			{noreply, Newstate} = handle_info({'DOWN', Qref, process, Qpid, testdeath}, State),
%			?assertEqual(#monitors{}, Newstate#state.monitors),
%			?assertEqual({"testqueue", undefined}, Newstate#state.queue_pid)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"Cook pid goes down with no agent ringing",
%		fun() ->
%			Qpid = dead_spawn(),
%			Cook = dead_spawn(),
%			Qref = make_ref(),
%			CookRef = make_ref(),
%			Mons = #monitors{queue_pid = Qref, cook = CookRef},
%			State = Seedstate#state{queue_pid = {"testqueue", Qpid}, callrec = Oldcall#call{cook = Cook}, monitors = Mons},
%			{noreply, Newstate} = handle_info({'DOWN', CookRef, process, Cook, testdeath}, State),
%			?assertEqual(#monitors{cook = undefined, queue_pid = Qref}, Newstate#state.monitors),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"Cook pid goes down with an agent ringing",
%		fun() ->
%			Qpid = dead_spawn(),
%			Cook = dead_spawn(),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Qref = make_ref(),
%			CookRef = make_ref(),
%			Aref = make_ref(),
%			Mons = #monitors{queue_pid = Qref, cook = CookRef, ring_pid = Aref},
%			State = Seedstate#state{queue_pid = {"testqueue", Qpid}, ring_pid = {"testagent", Agent}, callrec = Oldcall#call{cook = Cook}, monitors = Mons},
%			{noreply, Newstate} = handle_info({'DOWN', CookRef, process, Cook, testdeath}, State),
%			?assertEqual(#monitors{queue_pid = Qref}, Newstate#state.monitors),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"ringing agent dies",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			Agent = dead_spawn(),
%			AgentRef = make_ref(),
%			Qref = make_ref(),
%			Cookref = make_ref(),
%			Mons = #monitors{queue_pid = Qref, cook = Cookref, ring_pid = AgentRef},
%			Call = Oldcall#call{cook = Cook},
%			State = Seedstate#state{monitors = Mons, queue_pid = {"testqueue", dead_spawn()}, ring_pid = {"testagent", Agent}, callrec = Call},
%			{noreply, Newstate} = handle_info({'DOWN', AgentRef, process, Agent, testdeath}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertEqual(#monitors{queue_pid = Qref, cook = Cookref}, Newstate#state.monitors),
%			?assertEqual(undefined, Newstate#state.ring_pid)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"oncall agent dies with no ringing agent",
%		fun() ->
%			OncallRef = make_ref(),
%			Agent = dead_spawn(),
%			Mons = #monitors{
%				oncall_pid = OncallRef
%			},
%			{ok, Newqueue} = gen_server_mock:new(),
%			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
%				ok
%			end),
%			{ok, Mock} = gen_leader_mock:start(queue_manager),
%			gen_leader_mock:expect_leader_call(Mock, fun(_Msg, _From, State, _Elec) ->
%				{ok, Newqueue, State}
%			end),
%			State = Seedstate#state{monitors = Mons, oncall_pid = {"testagent", Agent}},
%			{noreply, #state{monitors = Newmons} = Newstate} = handle_info({'DOWN', OncallRef, process, Agent, testdeath}, State),
%			?assertMatch({"default_queue", _}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			gen_leader_mock:stop(Mock)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"oncall agent dies with a ringing agent",
%		fun() ->
%			OncallRef = make_ref(),
%			Ocagent = dead_spawn(),
%			Ringref = make_ref(),
%			{ok, Ragent} = agent:start(#agent{login = "ringagent"}),
%			Mons = #monitors{
%				oncall_pid = OncallRef,
%				ring_pid = Ringref
%			},
%			{ok, Newqueue} = gen_server_mock:new(),
%			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
%				ok
%			end),
%			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
%				ok
%			end),
%			{ok, Mock} = gen_leader_mock:start(queue_manager),
%			gen_leader_mock:expect_leader_call(Mock, fun(_Msg, _From, State, _Elec) ->
%				{ok, Newqueue, State}
%			end),
%			State = Seedstate#state{monitors = Mons, oncall_pid = {"deadagent", Ocagent}, ring_pid = {"ringagent", Ragent}},
%			{noreply, #state{monitors = Newmons} = Newstate} = handle_info({'DOWN', OncallRef, process, Ocagent, testdeath}, State),
%			?assertMatch({"default_queue", _}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			gen_leader_mock:stop(Mock)
%		end}
%	end]}}.
%
%agent_interact_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_agent_interact_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		Callrec = #call{id = "testcall", source = self(), client = #client{}},
%		{ok, Mock} = gen_leader_mock:start(agent_manager),
%		gen_leader_mock:expect_leader_call(Mock, fun(_Data, _From, State, _Elec) -> {ok, "testagent", State} end),
%		gen_event:start({local, cdr}),
%		gen_event:add_handler(cdr, gen_event_mock, []),
%		{#agent{login = "testagent"}, Callrec}
%	end,
%	fun({_Arec, _Callrec}) ->
%		Mock = whereis(agent_manager),
%		gen_leader_mock:stop(Mock),
%		gen_event:stop(cdr),
%		timer:sleep(10), % because mocks don't like to die quickly.
%		ok
%	end,
%	[%fun({Arec, Callrecbase}) ->
%%		{"mediapush",
%%		fun() ->
%%			Callrec = Callrecbase#call{media_path = inband},
%%			{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = oncall}),
%%			State = #state{oncall_pid = Apid, callrec = Callrec},
%%			Expected = State,
%%			?assertEqual(Expected, agent_interact({mediapush, "data", append}, State)),
%%			agent:stop(Apid),
%%			gen_event_mock:assert_expectations(cdr)
%%		end}
%%	end,
%	%fun({Arec, Callrec}) ->
%		%{"media push when media_path doesn't match",
%		%fun() ->
%			%{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = oncall}),
%			%State = #state{oncall_pid = Apid, callrec = Callrec},
%			%Expected = State,
%			%agent:stop(Apid),
%			%?assertEqual(Expected, agent_interact({mediapush, "data", append}, State)),
%			%gen_event_mock:assert_expectations(cdr)
%			%ok
%		%end}
%	%end,
%	fun({Arec, Callrec}) ->
%		{"stop_ring with a ringout timer going",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			{ok, Tref} = timer:send_interval(1000, <<"timer">>),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = #state{ring_pid = {"testagent", Apid}, ringout = Tref, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _State) ->
%				ok
%			end),
%			Res = agent_interact(stop_ring, State),
%			agent:stop(Apid),
%			receive
%				<<"timer">> ->
%					 erlang:error(timer_lives)
%			after 1500 ->
%				ok
%			end,
%			?assertEqual(false, Res#state.ringout),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"stop_ring with no ringout or ring_pid defined",
%		fun() ->
%			State = #state{ring_pid = undefined, ringout = false, callrec = Callrec},
%			Res = agent_interact(stop_ring, State),
%			?assertEqual(State, Res),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"stop_ring with only ring_pid defined",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = #state{ring_pid = {"testagent", Apid}, ringout = false, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _State) -> ok end),
%			Res = agent_interact(stop_ring, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"stop_ring with only ringout defined",
%		fun() ->
%			{ok, Tref} = timer:send_interval(1000, <<"timer">>),
%			State = #state{ringout = Tref, callrec = Callrec},
%			Res = agent_interact(stop_ring, State),
%			receive
%				<<"timer">>	->
%					 erlang:error(timer_lives)
%			after 1500 ->
%				ok
%			end,
%			?assertEqual(false, Res#state.ringout),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"wrapup",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = #state{oncall_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			Res = agent_interact(wrapup, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.oncall_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hangup when both oncall and ring are pids",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(Arec#agent{}),
%			{ok, Ringing} = agent:start(Arec#agent{login = "ringing"}),
%			Mons = #monitors{ring_pid = make_ref(), oncall_pid = make_ref()},
%			State = #state{oncall_pid = {"testagent", Oncall}, ring_pid = {"ring", Ringing}, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Res = agent_interact(hangup, State),
%			agent:stop(Oncall),
%			agent:stop(Ringing),
%			?assertEqual(undefined, Res#state.oncall_pid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hang up when only oncall is a pid",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = #state{oncall_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Res = agent_interact(hangup, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.oncall_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hang up when only ringing is a pid",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{ring_pid = make_ref()},
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			State = #state{ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			Res = agent_interact(hangup, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"hang up when only queue is a pid",
%		fun() ->
%			{ok, Qpid} = gen_server_mock:new(),
%			gen_server_mock:expect_call(Qpid, fun({remove, _Incpid}, _From, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Mons = #monitors{queue_pid = make_ref()},
%			State = #state{queue_pid = {"testqueue", Qpid}, callrec = Callrec, monitors = Mons},
%			#state{monitors = Newmons} = Res = agent_interact(hangup, State),
%			?assertEqual("testqueue", Res#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			gen_server_mock:assert_expectations(Qpid),
%			gen_server_mock:stop(Qpid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hang up when both queue and ring is a pid",
%		fun() ->
%			{ok, Qpid} = gen_server_mock:new(),
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			gen_server_mock:expect_call(Qpid, fun({remove, _Incpid}, _From, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Mons = #monitors{queue_pid = make_ref(), ring_pid = make_ref()},
%			State = #state{queue_pid = {"testqueue", Qpid}, ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			#state{monitors = Newmons} = Res = agent_interact(hangup, State),
%			agent:stop(Apid),
%			?assertEqual("testqueue", Res#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_server_mock:assert_expectations(Qpid),
%			gen_server_mock:stop(Qpid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, _Callrec}) ->
%		{"orphaned call, or just not yet queued",
%		fun() ->
%			Res = agent_interact(hangup, #state{}),
%			?assertEqual(#state{}, Res),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"dead agent pid doesn't cause crash",
%		fun() ->
%			Mon = #monitors{ring_pid = make_ref()},
%			State = #state{ring_pid = {"testagent", spawn(fun() -> ok end)}, callrec = Callrec, monitors = Mon},
%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _) -> ok end),
%			Res = agent_interact(stop_ring, State),
%			?assertEqual(undefined, Res#state.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end]}}.

%outgoing_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_outgoing_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		{ok, Apid} = agent:start(#agent{login = "testagent"}),
%		{ok, Ammock} = gen_leader_mock:start(agent_manager),
%		gen_event:start({local, cdr}),
%		gen_event:add_handler(cdr, gen_event_mock, []),
%		{Apid, Ammock}
%	end,
%	fun({Apid, Ammock}) ->
%		agent:stop(Apid),
%		gen_leader_mock:stop(Ammock),
%		gen_event:stop(cdr),
%		timer:sleep(10)
%	end,
%	[fun({Apid, Ammock}) ->
%		{"set agent outbound with known call, and agent exists",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, {true, Apid}, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			State = #state{callrec = #call{id = "testcall", source = self()}},
%			{ok, Res} = outgoing({outbound, "testagent", "newsubstate"}, State),
%			?assertEqual({"testagent", Apid}, Res#state.oncall_pid),
%			?assertEqual("newsubstate", Res#state.substate),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Apid, Ammock}) ->
%		{"set agent outbound with known call, but agent doesn't exist",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, false, State}
%			end),
%			State = #state{callrec = #call{id = "testcall", source = self()}},
%			Res = outgoing({outbound, "testagent", "newsubstate"}, State),
%			?assertMatch({{error, {noagent, "testagent"}}, _Newstate}, Res),
%			{_, Newstate} = Res,
%			?assertEqual("newsubstate", Newstate#state.substate),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Apid, Ammock}) ->
%		{"set agent outbound with a new callrec",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, {true, Apid}, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			Callrec = #call{id = "testcall", source = self()},
%			State = #state{},
%			{ok, Res} = outgoing({outbound, "testagent", Callrec, "newsubstate"}, State),
%			?assertEqual({"testagent", Apid}, Res#state.oncall_pid),
%			?assertEqual(Callrec, Res#state.callrec),
%			?assertEqual("newsubstate", Res#state.substate),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Apid, Ammock}) ->
%		{"set agent outbound iwth a new call rec, but agent doesn't exist",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, false, State}
%			end),
%			Callrec = #call{id = "testcall", source = self()},
%			State = #state{},
%			Res = outgoing({outbound, "testagent", Callrec, "newsubstate"}, State),
%			?assertMatch({{error, {noagent, "testagent"}}, _State}, Res),
%			{_, Newstate} = Res,
%			?assertEqual("newsubstate", Newstate#state.substate),
%			?assertEqual(Callrec, Newstate#state.callrec),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end]}}.

agent_interact_test_() ->
	{setup, fun() ->
		cpx_dummy_pid:start_link()
	end, fun(Pid) ->
		cpx_dummy_pid:stop(Pid)
	end,
	fun(Pid) -> [
		fun() ->
			Mon = erlang:monitor(process, Pid),
			BaseState = #base_state{callrec = #call{id="call", source=source}},
			?assertEqual({wrapup, {BaseState, #wrapup_state{}}},
				agent_interact({hangup, "caller"}, oncall, BaseState, internal,
					{Mon, {#agent{login="agent"}, Pid}}))
		end]
	end}.

dpid() -> spawn(fun() -> ok end).

priv_queue_test_() ->
	{setup, fun() ->
		meck:new(queue_manager),
		meck:new(call_queue),
		Callrec = #call{id = "testcall", source = self()},
		Validator = fun() ->
			?assert(meck:validate(queue_manager)),
			?assert(meck:validate(call_queue))
		end,
		{Callrec, Validator}
	end,
	fun(_) ->
		meck:unload(queue_manager),
		meck:unload(call_queue)
	end,
	fun({Callrec, Validator}) -> [

		{"All is well", fun() ->
			Qpid = dpid(),
			meck:expect(queue_manager, get_queue, fun(_) -> Qpid end),
			meck:expect(call_queue, add, fun(_, _, _) -> ok end),
			?assertEqual(Qpid, priv_queue("testqueue", Callrec, "doesn't matter")),
			Validator()
		end},

		{"failover is false", fun() ->
			meck:expect(queue_manager, get_queue, fun(_) -> undefined end),
			?assertEqual(invalid, priv_queue("testqueue", Callrec, false)),
			Validator()
		end},

		{"failover is true", fun() ->
			Qpid = dpid(),
			meck:expect(queue_manager, get_queue, fun(QuNom) ->
				case QuNom of
					"testqueue" -> undefined;
					"default_queue" -> Qpid
				end
			end),
			meck:expect(call_queue, add, fun(_, _, _) -> ok end),
			?assertEqual({default, Qpid}, priv_queue("testqueue", Callrec, true)),
			Validator()
		end}

	] end}.

mutate_return_test_() ->
	{setup, fun() ->
		meck:new(first_module),
		meck:new(second_module),
		#base_state{callback = first_module}
	end,
	fun(_) ->
		meck:unload(first_module),
		meck:unload(second_module)
	end,
	fun(Base) -> [

		{"mutate from handle info", fun() ->
			meck:expect(first_module, handle_info, fun(mutate, "gm state", undefined, _GMStateRec, undefined) ->
				{mutate, second_module, "new callback state"}
			end),
			{next_state, "gm state", {NewBase, "gm state rec"}} = handle_info(mutate, "gm state", {Base, "gm state rec"}),
			?assertEqual(second_module, NewBase#base_state.callback),
			?assertEqual("new callback state", NewBase#base_state.substate),
			meck:validate(first_module),
			meck:validate(second_module)
		end},

		{"mutate from call", fun() ->
			meck:expect(first_module, handle_call, fun(mutate, "from", "gm statename", undefined, "gm state data", undefined) ->
				{mutate, {ok, "goober"}, second_module, "new callback state"}
			end),
			{reply, {ok, "goober"}, "gm statename", {NewBase, "gm state data"}} = handle_sync_event(mutate, "from", "gm statename", {Base, "gm state data"}),
			?assertEqual(second_module, NewBase#base_state.callback),
			?assertEqual("new callback state", NewBase#base_state.substate)
		end}

	] end}.

outbound_call_flow_test_() ->
	{setup, fun() ->
		application:start(gproc),
		meck:new(media_callback),
		meck:new(agent_channel),
		meck:expect(media_callback, init, fun(_) ->
			{ok, undefined, []}
		end),
		cpx_hooks:start_link(),
		{ok, InitState, GmState} = init([media_callback, undefined]),
		lager:debug("initstate:  ~p, ~p", [InitState, GmState]),
		Validator = fun() ->
			meck:validate(media_callback),
			meck:validate(agent_channel)
		end,
		{GmState, Validator}
	end,
	fun(_) ->
		cpx_hooks:stop(),
		meck:unload(media_callback),
		meck:unload(agent_channel),
		application:stop(gproc)
	end,
	fun({GmState, Validator}) -> [

		{"callback is a success (handle_info)", fun() ->
			Call = #call{source = dpid(), id = "testcall"},
			meck:expect(media_callback, handle_info, fun(doit, _, _, _, _) ->
				{outgoing, {"agent", dpid()}, Call, undefined}
			end),
			meck:expect(agent_channel, set_state, fun(_, _, _) -> ok end),
			Out = handle_info(doit, inivr, GmState),
			?assertMatch({next_state, oncall, _Whatever}, Out),
			Validator()
		end},

		{"callback is a success (handle_call)", fun() ->
			Call = #call{source = dpid(), id = "testcall"},
			meck:expect(media_callback, handle_call, fun(doit, _, _, _, _, _) ->
				{outgoing, {"agent", dpid()}, Call, undefined}
			end),
			meck:expect(agent_channel, set_state, fun(_, _, _) -> ok end),
			Out = handle_sync_event(doit, {dpid(), "from"}, inivr, GmState),
			?assertMatch({reply, ok, oncall, _Whatever}, Out),
			Validator()
		end}

	] end}.

update_skills_test_() ->
	Source = spawn(fun() -> ok end),
	Call = #call{id = "call", source = Source, skills = [english, sales]},
	Call2 = Call#call{skills = [english, german, sales]},
	Call3 = Call#call{skills = [english, german]},
	BaseState = #base_state{callrec = Call},
	InternalState = internal_state,
	{setup, fun() ->
		application:start(gproc)
	end,
	fun(_) ->
		application:stop(gproc)
	end, [{"add skills", fun() ->
		init_gproc_prop(init, BaseState),

		?assertEqual({next_state, ringing, {BaseState#base_state{callrec = Call2}, InternalState}}, handle_event(?GM(add_skills, [german]), ringing, {BaseState, InternalState})),
		?assertEqual(#cpx_gen_media_prop{state=ringing, call=Call2}, gproc:get_value({p,l,cpx_media}))
	end}, {"remove skills", fun() ->
		?assertEqual({next_state, ringing, {BaseState#base_state{callrec = Call3}, InternalState}}, handle_event(?GM(remove_skills, [sales]), ringing, {BaseState#base_state{callrec = Call2}, InternalState})),
		?assertEqual(#cpx_gen_media_prop{state=ringing, call=Call3}, gproc:get_value({p,l,cpx_media}))
	end}]}.

t_st() ->
	{#base_state{callback = cpx_media_dummy}, #oncall_state{}}.

hold_test_() ->
	{setup, fun() ->
		ok
	end, fun(_) ->
		ok
	end, [{"hold", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall(?GM(hold), from, St))
	end}, {"unhold", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall(?GM(unhold), from, St))
	end}]}.

playback_control_test_() ->
	{setup, fun() ->
		ok
	end, fun(_) ->
		ok
	end, [{"play with opts", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall(?GM({play, []}), from, St))
	end}, {"pause", fun() ->
		St = t_st(),
		?assertEqual({reply, ok, oncall, St}, oncall(?GM(pause), from, St))
	end}]}.

-endif.
