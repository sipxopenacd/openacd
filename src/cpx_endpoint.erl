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

-module(cpx_endpoint).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("agent.hrl").
-include("call.hrl").

-callback start(any()) -> {ok, pid()}.
-callback stop(pid()) -> ok.

-export([
	start/2,
	stop/1,

	get_pid/1,

	prering/1,
	ringout/1,
	hangup/1
]).

-record(ep, {
	cbk :: atom(),
	pid :: pid()
}).

start(Cbk, Opts) ->
	{ok, Pid} = Cbk:start(Opts),
	{ok, #ep{cbk=Cbk, pid=Pid}}.

stop(#ep{cbk=Cbk, pid=Pid}) ->
	Cbk:stop(Pid).

get_pid(#ep{pid=Pid}) ->
	Pid.

prering(Ep) ->
	send_endpoint_event(Ep, prering).

ringout(Ep) ->
	send_endpoint_event(Ep, ringout).

hangup(Ep) ->
	send_endpoint_event(Ep, hangup).

%% internal

send_endpoint_event(#ep{pid=Pid}, Event) ->
	Pid ! {cpx_endpoint, Event}.

-ifdef(TEST).

-define(M, ?MODULE).

start_test_() ->
	Opts = [a, b, c],

	Pid = spawn(fun() -> ok end),

	{setup, fun() ->
		meck:new(cbk),
		meck:expect(cbk, start, 1, {ok, Pid})
	end, fun(_) ->
		meck:unload()
	end, [fun() ->
		?assertEqual({ok, #ep{cbk=cbk, pid=Pid}}, ?M:start(cbk, Opts)),
		?assert(meck:called(cbk, start, [Opts]))
	end]}.

stop_test_() ->
	with_ep(fun(Ep) ->
		meck:expect(cbk, stop, 1, ok),
		?M:stop(Ep),
		?assert(meck:called(cbk, stop, [Ep#ep.pid]))
	end).

get_pid_test_() ->
	with_ep(fun(Ep) ->
		?assertEqual(self(), ?M:get_pid(Ep))
	end).

prering_test_() ->
	with_ep(fun(Ep) ->
		?M:prering(Ep),
		Rcvd = receive {cpx_endpoint, prering} -> true after 10 -> false end,
		?assert(Rcvd)
	end).

ringout_test_() ->
	with_ep(fun(Ep) ->
		?M:ringout(Ep),
		Rcvd = receive {cpx_endpoint, ringout} -> true after 10 -> false end,
		?assert(Rcvd)
	end).

hangup_test_() ->
	with_ep(fun(Ep) ->
		?M:hangup(Ep),
		Rcvd = receive {cpx_endpoint, hangup} -> true after 10 -> false end,
		?assert(Rcvd)
	end).

with_ep(Test) ->
	{setup, fun() ->
		meck:new(cbk)
	end, fun(_) ->
		meck:unload()
	end, fun() ->
		meck:expect(cbk, start, 1, {ok, self()}),

		{ok, Ep} = ?M:start(cbk, []),
		Test(Ep)
	end}.

-endif.
