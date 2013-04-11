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

-module(agent_auth_ets).

-include("call.hrl").
-include("agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(cpx_auth, {
	login :: string(),
	profile = "Default" :: string(),
	skills = [] :: skills(),
	security_level = agent :: security_level(),
	endpoints = [] :: [endpoint()]
}).
-behaviour(agent_auth).

-export([
	start/0,
	setup/1
]).

%% Callbacks
-export([
	get_agent_by_login/1,
	get_agent_by_id/1,
	get_agents_by_profile/1,
	auth/2,
	get_profile/1,
	get_default_profile/0,
	get_profiles/0,
	get_release/1,
	get_releases/0
]).

start() ->
	%% TODO put in a process
	S = self(),
	spawn(fun() -> loop0(S) end),
	receive agent_auth_init -> ok end,
	ok.


setup(Opts) ->
	application:set_env(openacd, agent_auth_storage, ?MODULE),
	start(),

	Agents = proplists:get_value(agents, Opts, []),
	load_agents(Agents, []),

	Profiles = proplists:get_value(profiles, Opts, []),
	load_profiles(Profiles, []),

	RelOpts = proplists:get_value(rel_opts, Opts, []),
	load_releases(RelOpts, []),

	ok.


get_agent_by_id(Id) ->
	get_agent_by_login(Id).

get_agent_by_login(Login) ->
	case lookup_login(Login) of
		{ok, E} ->
			{ok, entry_to_agent_auth(E)};
		_ ->
			none
	end.

get_agents_by_profile(Profile) ->
	Entries = ets:match_object(cpx_auth, #cpx_auth{profile=Profile, _='_'}),
	Auths = [entry_to_agent_auth(E) || E <- Entries],
	{ok, Auths}.



auth(Login, "pwd" ++ Login) ->
	case lookup_login(Login) of
		{ok, Entry} ->
			{ok, entry_to_agent_auth(Entry)};
		_ ->
			{error, deny}
	end;
auth(_, _) ->
	{error, deny}.

get_profile(Name) ->
	case ets:lookup(cpx_profile, Name) of
		[E] ->
			{ok, E};
		_ ->
			none
	end.

get_default_profile() -> get_profile("Default").

get_profiles() ->
	{ok, ets:tab2list(cpx_profile)}.

get_release(Label) ->
	case ets:lookup(cpx_relopt, Label) of
		[E] ->
			{ok, E};
		_ ->
			none
	end.

get_releases() ->
	{ok, DefaultRel} = get_release("Default"),
	{ok, [DefaultRel]}.

load_agents([], Acc) ->
	ets:delete_all_objects(cpx_auth),
	ets:insert(cpx_auth, Acc);
load_agents([Login|T], Acc) when is_list(Login) ->
	load_agents([{Login, []}|T], Acc);
load_agents([{Login, Opts}|T], Acc) when is_list(Login), is_list(Opts) ->
	Skills = get_opt(skills, Opts),
	Profile = get_opt(profile, Opts),
	SecurityLevel = get_opt(security_level, Opts),

	Entry = #cpx_auth{
		login = Login,
		profile = Profile,
		skills = Skills,
		security_level= SecurityLevel
	},

	load_agents(T, [Entry|Acc]).

load_releases([], Acc) ->
	Acc1 = case lists:any(fun(#release_opt{label="Default"}) -> true;
			(_) -> false end, Acc) of
		true ->
			Acc;
		_ ->
			[#release_opt{id="Default", label="Default"}|Acc]
	end,

	ets:delete_all_objects(cpx_relopt),
	ets:insert(cpx_relopt, Acc1);
load_releases([Name|T], Acc) ->
	RelOpt = #release_opt{id=Name, label=Name},
	load_releases(T, [RelOpt|Acc]).

load_profiles([], Acc) ->
	%% non-atomic

	Acc1 = case [1 || #agent_profile{name="Default"} <- Acc] of
		[_] ->
			Acc;
		_ ->
			[?DEFAULT_PROFILE|Acc]
	end,

	ets:delete_all_objects(cpx_profile),
	ets:insert(cpx_profile, Acc1);
load_profiles([Name|T], Acc) when is_list(Name) ->
	load_profiles([{Name, []}|T], Acc);
load_profiles([{Name, Opts}|T], Acc) when is_list(Name) ->
	Skills = proplists:get_value(skills, Opts, []),

	Profile = #agent_profile{id=Name, name=Name, skills=Skills},
	load_profiles(T, [Profile|Acc]).


%% internal
loop0(P) ->
	try
		ets:new(cpx_auth, [{keypos, #cpx_auth.login}, named_table, public]),
		ets:new(cpx_profile, [{keypos, #agent_profile.name}, named_table, public]),
		ets:new(cpx_relopt, [{keypos, #release_opt.label}, named_table, public]),

		P ! agent_auth_init,
		loop()
	catch _:_ ->
		P ! agent_auth_init,
		ok
end.

loop() ->
	receive
		die -> ok;
		_ -> loop()
	end.

lookup_login(Login) ->
	case ets:lookup(cpx_auth, Login) of
		[E] ->
			{ok, E};
		_ ->
			none
	end.

entry_to_agent_auth(E) ->
	#cpx_auth{
		login = Login,
		profile = Profile,
		skills = Skills,
		security_level = SecurityLevel,
		endpoints = Endpoints
	} = E,

	#agent_auth{
		id = Login,
		login = Login,
		skills = Skills,
		security_level = SecurityLevel,
		profile = Profile,
		endpoints = Endpoints
	}.

get_opt(K, L) ->
	Default = get_default(K),
	proplists:get_value(K, L, Default).

get_default(profile) -> "Default";
get_default(endpoints) -> [];
get_default(security_level) -> agent;
get_default(skills) -> [english, '_agent', '_node'].


-ifdef(TEST).

setup_reset_test() ->
	ok = agent_auth_ets:setup([{agents, ["agent1"]}]),
	?assertMatch({ok, _}, agent_auth:get_agent_by_id("agent1")),

	ok = agent_auth_ets:setup([{agents, ["agent3"]}]),
	?assertEqual(none, agent_auth:get_agent_by_id("agent1")).

%% agent tests
agent_defaults_test_() ->
	ok = agent_auth_ets:setup([{agents, ["agent1", "agent2", "agent3"]}]),
	[
		{"ok, by id", ?_assertMatch({ok, #agent_auth{id="agent1", login="agent1"}},
			agent_auth:get_agent_by_id("agent1"))},
		{"none, by id", ?_assertEqual(none, agent_auth:get_agent_by_id("agent9"))},


		{"ok, by login", ?_assertMatch({ok, #agent_auth{id="agent2", login="agent2"}},
			agent_auth:get_agent_by_login("agent2"))},
		{"none, by login", ?_assertEqual(none, agent_auth:get_agent_by_login("agent9"))},

		{"profile is Default",
			?_assertMatch({ok, #agent_auth{profile="Default"}}, agent_auth:get_agent_by_id("agent1"))
		},

		{"security level is agent",
			?_assertMatch({ok, #agent_auth{security_level=agent}}, agent_auth:get_agent_by_id("agent1"))
		},

		{"default skills",
			?_assertMatch({ok, #agent_auth{skills=[english, '_agent', '_node']}}, agent_auth:get_agent_by_id("agent1"))
		},

		{"password is 'pwd' + login",
			?_assertMatch({ok, #agent_auth{}}, agent_auth:auth("agent1", "pwdagent1"))
		},
		{"wrong password",
			?_assertMatch({error, deny}, agent_auth:auth("agent1", "wrong"))
		},
		{"no user auth",
			?_assertMatch({error, deny}, agent_auth:auth("agent9", "pwdagent9"))
		}

		%% TODO endpoints
	].

agent_options_test_() ->
	ok = agent_auth_ets:setup([{agents, [
		{"agent1", [{profile, "Profile1"}, {skills, [romanian]}, {security_level, supervisor}]},
		"agent2", "agent3"]}]),
	[
		{"profile",
			?_assertMatch({ok, #agent_auth{profile="Profile1"}}, agent_auth:get_agent_by_id("agent1"))
		},

		{"skills",
			?_assertMatch({ok, #agent_auth{skills=[romanian]}}, agent_auth:get_agent_by_id("agent1"))
		},

		{"security level",
			?_assertMatch({ok, #agent_auth{security_level=supervisor}}, agent_auth:get_agent_by_id("agent1"))
		}
	].

%% Profiles
profile_defaults_test_() ->
	ok = agent_auth_ets:setup([{profiles, ["profile1", "profile2"]}]),
	[
		{"profile 'Default' is present",
			?_assertMatch({ok, #agent_profile{id="Default", name="Default"}}, agent_auth:get_profile("Default"))
		},

		{"profile by name",
			?_assertMatch({ok, #agent_profile{id="profile1", name="profile1"}}, agent_auth:get_profile("profile1"))
		},
		{"non-existing profile",
			?_assertEqual(none, agent_auth:get_profile("profile9"))
		},
		{"get profiles", fun() ->
			{ok, Profiles} = agent_auth:get_profiles(),
			?assertEqual(["Default", "profile1", "profile2"], lists:sort([Name || #agent_profile{name=Name} <- Profiles]))
		end},

		{"skills is []",
			?_assertMatch({ok, #agent_profile{skills=[]}}, agent_auth:get_profile("profile2"))
		}
	].

profile_options_test_() ->
	ok = agent_auth_ets:setup([{profiles, [{"profile1", [{skills, [japanese]}]}]}]),
	[
		{"skills",
			?_assertMatch({ok, #agent_profile{skills=[japanese]}}, agent_auth:get_profile("profile1"))
		}
	].

default_profile_override_test_() ->
	ok = agent_auth_ets:setup([{profiles, [{"Default", [{skills, [japanese]}]}]}]),
	{"skills",
		?_assertMatch({ok, #agent_profile{skills=[japanese]}}, agent_auth:get_default_profile())
	}.

%% Release
release_defaults_test_() ->
	ok = agent_auth_ets:setup([{rel_opts, ["release1", "release2"]}]),
	[
		{"default release",
			?_assertMatch({ok, #release_opt{id="Default", label="Default"}}, agent_auth:get_release("Default"))
		},
		{"get release",
			?_assertMatch({ok, #release_opt{id="release1", label="release1"}}, agent_auth:get_release("release1"))
		},
		{"non-existing release",
			?_assertEqual(none, agent_auth:get_release("release9"))
		}
	].

default_release_override_test() ->
	ok = agent_auth_ets:setup([{rel_opt, ["Default"]}]),
	?assertMatch({ok, [#release_opt{label="Default"}]}, agent_auth:get_releases()).

-endif.