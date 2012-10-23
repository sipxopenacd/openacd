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

-export([new/1, get/2, set/3,
	get_channel_pid_by_id/2, get_id_by_channel_pid/2,
	store_channel/2]).

-record(state, {
	agent_pid :: pid(),
	agent_login :: erlang:error({undefined, login}) | string(),
	channels = [] :: list(),
	chan_count = 0 :: non_neg_integer()
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

-spec get_id_by_channel_pid(state(), pid()) -> binary() | none.
get_id_by_channel_pid(St, Pid) ->
	case lists:keyfind(Pid, 2, St#state.channels) of
		{Id, _} -> Id;
		_ -> none
	end.

-spec get_channel_pid_by_id(state(), binary()) -> pid() | none.
get_channel_pid_by_id(St, Id) ->
	case lists:keyfind(Id, 1, St#state.channels) of
		{_, Ch} -> Ch;
		_ -> none
	end.

-spec store_channel(state(), pid()) -> binary().
store_channel(St, Pid) ->
	Channels = St#state.channels,
	case lists:keyfind(Pid, 2, Channels) of
		{Id, _} ->
			{Id, St};
		_ ->
			NextCount = St#state.chan_count + 1,
			Id = count_to_id(NextCount),
			Chs = [{Id, Pid}|Channels],
			St1 = St#state{channels=Chs, chan_count=NextCount},
			{Id, St1}
	end.

%% Internal
-spec count_to_id(non_neg_integer()) -> binary().
count_to_id(Id) ->
	IdBin = list_to_binary(integer_to_list(Id)),
	<<"ch", IdBin/binary>>.

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

t_st() ->
	APid = spawn(fun() -> ok end),
	ALogin = "agent",
	new(#agent{login=ALogin, source=APid}).

channel_test() ->
	St = t_st(),
	Pid1 = spawn(fun() -> ok end),
	Pid2 = spawn(fun() -> ok end),

	?assertEqual(none, get_id_by_channel_pid(St, Pid1)),
	?assertEqual(none, get_channel_pid_by_id(St, <<"ch1">>)),

	{Id1, St1} = store_channel(St, Pid1),
	{Id2, St2} = store_channel(St1, Pid2),

	?assertEqual(<<"ch1">>, Id1),
	?assertEqual(<<"ch2">>, Id2),

	?assertEqual(Id1, get_id_by_channel_pid(St2, Pid1)),
	?assertEqual(Pid2, get_channel_pid_by_id(St2, Id2)).


-endif.