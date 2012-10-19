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
-module(cpx_conn_state).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("agent.hrl").

-export([new/1, get/2, set/3]).

-record(state, {
	agent_pid :: pid(),
	agent_login :: erlang:error({undefined, login}) | string(),
	channels = dict:new() :: dict()
}).

-type state() :: #state{}.
-export_type([state/0]).

new(#agent{source=APid, login=Login}) ->
	#state{agent_pid = APid, agent_login=Login}.

get(#state{agent_pid=APid}, agent_pid) ->
	APid;
get(#state{agent_login=ALogin}, agent_login) ->
	ALogin;
get(#state{agent_pid=APid}, agent) ->
	agent:dump_state(APid);
get(#state{channels=Channels}, channels) ->
	Channels.

set(State, channels, Channels) ->
	State#state{channels = Channels}.

-ifdef(TEST).

new_test() ->
	APid = spawn(fun() -> ok end),
	ALogin = "agent",
	St = new(#agent{login=ALogin, source=APid}),

	?assertEqual(APid, get(St, agent_pid)),
	?assertEqual(ALogin, get(St, agent_login)).

get_agent_rec_test_() ->
	APid = spawn(fun() -> ok end),
	A = #agent{login="agent", profile="tech", source=APid},
	A2 = A#agent{profile="corp"},
	{setup, fun() ->
		meck:new(agent),
		meck:expect(agent, dump_state, 1, A2)
	end, fun(_) ->
		meck:unload(agent)
	end,[fun() ->
		St = new(A),
		?assertEqual(A2, get(St, agent))
	end]}.

-endif.