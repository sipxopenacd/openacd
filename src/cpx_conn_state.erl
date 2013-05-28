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

-export([
	% new
	new/1,

	% fixed properties
	get/2, set/3,

	%% arbitrary properties
	get_prop/2, set_prop/3, unset_prop/2,

	% channels
	get_channel_pid_by_id/2, get_id_by_channel_pid/2,
	store_channel/2, remove_channel/2
]).

-record(state, {
	agent_pid :: pid(),
	agent_login :: string(),
	security_level :: security_level(),
	channels = [] :: list(),
	chan_count = 0 :: non_neg_integer(),
	skills = [] :: skills(),
	%% arbitrary properties
	props = dict:new()
}).

-type state() :: #state{}.
-export_type([state/0]).

new(#agent{source=APid, login=Login, securitylevel=Level, skills=Skills}) ->
	#state{agent_pid = APid, agent_login=Login,
		security_level=Level, skills=Skills}.

get(#state{agent_pid=APid}, agent_pid) ->
	APid;
get(#state{agent_login=ALogin}, agent_login) ->
	ALogin;
get(#state{agent_pid=APid}, agent) ->
	agent:dump_state(APid);
get(#state{channels=Channels}, channels) ->
	Channels;
get(#state{security_level=Level}, security_level) ->
	Level;
get(#state{skills=Skills}, skills) ->
	Skills;
get(#state{skills=Skills}, clients) ->
	[Cl || {'_brand', Cl} <- Skills].

set(State, channels, Channels) ->
	State#state{channels = Channels}.

-spec get_prop(state(), K::any()) -> {ok, V::any()} | error.
get_prop(#state{props=Props}, K) ->
	dict:find(K, Props).

-spec set_prop(state(), K::any(), V::any()) -> state().
set_prop(#state{props=Props}=St, K, V) ->
	Props1 = dict:store(K, V, Props),
	St#state{props=Props1}.

-spec unset_prop(state(), K::any()) -> state().
unset_prop(#state{props=Props}=St, K) ->
	Props1 = dict:erase(K, Props),
	St#state{props=Props1}.

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

-spec remove_channel(state(), pid()) -> {binary() | none, state()}.
remove_channel(St, Pid) ->
	case lists:keytake(Pid, 2, St#state.channels) of
		{value, {ChanId, _}, Chans} ->
			{ChanId, St#state{channels = Chans}};
		_ ->
			{none, St}
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
	ALevel = supervisor,
	Skills = [english, {'_brand', "client1"}, {'_brand', "client2"}],

	St = new(#agent{login=ALogin, source=APid,
		securitylevel=ALevel, skills=Skills}),

	?assertEqual(APid, get(St, agent_pid)),
	?assertEqual(ALogin, get(St, agent_login)),
	?assertEqual(ALevel, get(St, security_level)),
	?assertEqual(Skills, get(St, skills)),
	?assertEqual(["client1", "client2"], get(St, clients)).

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

	Pid3 = spawn(fun() -> ok end),

	?assertEqual(none, get_id_by_channel_pid(St, Pid1)),
	?assertEqual(none, get_channel_pid_by_id(St, <<"ch1">>)),

	{Id1, St1} = store_channel(St, Pid1),
	{Id2, St2} = store_channel(St1, Pid2),

	?assertEqual(<<"ch1">>, Id1),
	?assertEqual(<<"ch2">>, Id2),

	?assertEqual(Id1, get_id_by_channel_pid(St2, Pid1)),
	?assertEqual(Pid2, get_channel_pid_by_id(St2, Id2)),

	{Id1, St3} = remove_channel(St2, Pid1),
	{none, St4} = remove_channel(St3, Pid3),

	?assertEqual(none, get_id_by_channel_pid(St4, Pid1)).

props_test() ->
	St = t_st(),
	?assertEqual(error, get_prop(St, key)),

	St1 = set_prop(St, key, value),
	?assertEqual({ok, value}, get_prop(St1, key)),

	St2 = unset_prop(St1, key),
	?assertEqual(error, get_prop(St2, key)).

-endif.
