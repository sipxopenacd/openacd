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
%%  Jan Vincent Liwanag <jvliwanag at gmail dot com>
%%

-module(cpx_media_dummy).

-behaviour(gen_media).
-behaviour(gen_media_playable).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("agent.hrl").
-include("call.hrl").

%% api
-export([
	start/0,
	stop/1
]).

%% gen_media callbacks
-export([
	init/1,
	prepare_endpoint/2,
	handle_ring/4,
	handle_ring_stop/4,
	handle_answer/5,
	handle_agent_transfer/6,
	handle_queue_transfer/5,
	handle_wrapup/5,

	handle_call/6,
	handle_cast/5,
	handle_info/5,

	handle_hold/2,
	handle_unhold/2,

	terminate/5,
	code_change/6
]).

-export([
	handle_play/4,
	handle_pause/3,
	from_json_opts/1
]).

%% api

start() ->
	gen_media:start(?MODULE, []).

stop(Pid) ->
	gen_media:stop(Pid).

%% gen_media callbacks

init(_Args) ->
	{ok, [], []}.

prepare_endpoint(_Agent, _Data) ->
	{ok, {cpx_ring_dummy, start, []}}.

handle_ring(_RingData, _Agent, _Call, St) ->
	{ok, St}.

handle_ring_stop(_StName, _Call, _GMInt, St) ->
	{ok, St}.

handle_answer({_Agent, _APid}, _StName, _Call, _GMInt, St) ->
	{ok, St}.

handle_agent_transfer(_Agent, _Timeout, _StName, _Call,
		_GmInt, St) ->
	{ok, St}.

handle_queue_transfer({_Queue, _QPid}, _StName, _Call, _GMInt, St) ->
	{ok, St}.

handle_wrapup(_From, _StName, _Call, _GmInt, St) ->
	{ok, St}.

handle_call(_Msg, _From, _StName, _Call, _Extra, St) ->
	Reply = ok,
	{reply, Reply, St}.

handle_cast(_Msg, _StName, _Call, _Extra, St) ->
	{noreply, St}.

handle_info(_Msg, _StName, _Call, _Extra, St) ->
	{noreply, St}.

handle_hold(_GMInt, St) ->
	{ok, St}.

handle_unhold(_GMInt, St) ->
	{ok, St}.

terminate(_Msg, _StName, _Call, _Extra, _St) ->
	ok.

code_change(_OldVsn, _Call, _StName, St, _GmInt, _Extra) ->
	{ok, St}.

handle_play(_Opts, _Call, _GMInt, St) ->
	{ok, St}.

handle_pause(_Call, _GMInt, St) ->
	{ok, St}.

from_json_opts(_) ->
	[].

-ifdef(TEST).

-define(M, ?MODULE).

%% callbacks tests
init_test() ->
	?assertEqual({ok, [], []},
		?M:init([])).

prepare_endpoint_test() ->
	?assertEqual({ok, {cpx_ring_dummy, start, []}},
		?M:prepare_endpoint(t_agent(), [])).

handle_ring_test() ->
	?assertEqual({ok, []},
		?M:handle_ring([], t_agent(),
			t_call(), [])).

handle_ring_stop_test() ->
	?assertEqual({ok, []},
		?M:handle_ring_stop(inqueue_ringing,
			t_call(), internal_state, [])).

handle_answer_test() ->
	?assertEqual({ok, []},
		?M:handle_answer({t_agent(), self()},
			inqueue_ringing, t_call(), t_internal(), [])).

handle_agent_transfer_test() ->
	?assertEqual({ok, []},
		?M:handle_agent_transfer(t_agent(), 10,
			inqueue_ringing, t_call(), t_internal(), [])).

handle_queue_transfer_test() ->
	?assertEqual({ok, []},
		?M:handle_queue_transfer({"queue", self()},
			inqueue_ringing, t_call(), t_internal(), [])).

handle_wrapup_test() ->
	?assertEqual({ok, []},
		?M:handle_wrapup(from, oncall, t_call(), t_internal(), [])).

handle_call_test() ->
	?assertEqual({reply, ok, []},
		?M:handle_call(msg, from, inqueue, t_call(), extra, [])).

handle_cast_test() ->
	?assertEqual({noreply, []},
		?M:handle_cast(msg, inqueue, t_call(), t_internal(), [])).

handle_info_test() ->
	?assertEqual({noreply, []},
		?M:handle_info(msg, inqueue, t_call(), t_internal(), [])).

handle_hold_test() ->
	?assertEqual({ok, []},
		?M:handle_hold(t_internal(), [])).

handle_unhold_test() ->
	?assertEqual({ok, []},
		?M:handle_unhold(t_internal(), [])).

terminate_test() ->
	?M:terminate(msg, inqueue, t_call(), t_internal(), []).

code_change_test() ->
	?assertEqual({ok, []},
		?M:code_change(oldvsn, t_call(), inqueue, [], t_internal(), [])).

handle_play_test() ->
	?assertEqual({ok, []},
		?M:handle_play([], t_call(), t_internal(), [])).

handle_pause_test() ->
	?assertEqual({ok, []},
		?M:handle_pause(t_call(), t_internal(), [])).

from_json_opts_test() ->
	?assertEqual([],
		?M:from_json_opts({[]})).

%% API

start_stop_test_() ->
	{setup, fun() ->
		application:start(gproc),
		cpx_hooks:start_link()
	end, fun(_) ->
		cpx_hooks:stop()
	end, fun() ->
		{ok, Pid} = ?M:start(),
		?assert(is_process_alive(Pid)),
		?M:stop(Pid),
		?assertNot(is_process_alive(Pid))
	end}.

%% internal

t_internal() ->
	internal_state.

t_agent() ->
	#agent{login="agent0"}.

t_call() ->
	#call{id="call", source=self()}.
-endif.