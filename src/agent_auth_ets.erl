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

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(TAB, agent_auth_entry).

-type agent_entry() :: {Login::string(), Password::string(), Skills::skills(), [agent_entry_opt()]}.
-type agent_entry_opt() :: {profile, string()} |
	{security_level, security_level()} |
	{endpoints, [endpoint()]}.

-record(agent_auth_entry, {
	login :: string(),
	pwd_hash :: binary(),
	profile = "Default" :: string(),
	skills = [] :: skills(),
	security_level = agent :: security_level(),
	endpoints = [] :: [endpoint()]
}).
-behaviour(agent_auth).

-export([
	start/0,
	load_agents/1
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
	ets:new(?TAB, [{keypos, #agent_auth_entry.login}, named_table, public]),
	ok.

get_agent_by_login(Login) ->
	case lookup_login(Login) of
		{ok, E} ->
			{ok, entry_to_agent_auth(E)};
		_ ->
			none
	end.

get_agents_by_profile(Profile) ->
	Entries = ets:match_object(?TAB, #agent_auth_entry{profile=Profile, _='_'}),
	Auths = [entry_to_agent_auth(E) || E <- Entries],
	{ok, Auths}.

get_agent_by_id(Id) ->
	get_agent_by_login(Id).

auth(Login, Password) ->
	PwdHash = erlang:md5(Password),
	case lookup_login(Login) of
		{ok, Auth = #agent_auth_entry{pwd_hash=PwdHash}} ->
			{ok, entry_to_agent_auth(Auth)};
		_ ->
			{error, deny}
	end.

get_profile("Default") -> get_default_profile();
get_profile(_) -> none.

get_default_profile() -> {ok, ?DEFAULT_PROFILE}.

get_profiles() -> {ok, [?DEFAULT_PROFILE]}.

get_release("Default") -> {ok, #release_opt{id="Default", label="Default"}};
get_release(_) -> none.

get_releases() ->
	{ok, DefaultRel} = get_release("Default"),
	{ok, [DefaultRel]}.


-spec load_agents([agent_entry() | any()]) ->
	{ok, [agent_entry()]}.
load_agents(Agents) ->
	ets:delete_all_objects(?TAB),
	Entries = read_entries(Agents, []),
	ets:insert(?TAB, Entries).

lookup_login(Login) ->
	case ets:lookup(?TAB, Login) of
		[E] ->
			{ok, E};
		_ ->
			none
	end.

entry_to_agent_auth(E) ->
	#agent_auth_entry{
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

read_entries([], Acc) ->
	Acc;
read_entries([E|T], Acc) ->
	case catch read_entry(E) of
		{ok, R} -> read_entries(T, [R|Acc]);
		_ -> Acc
	end.

read_entry({Login, Pwd, Skills, Opts}) ->
	Profile = get_opt(profile, Opts),
	Endpoints = get_opt(endpoints, Opts),
	SecurityLevel = get_opt(security_level, Opts),

	%% TODO salt
	PwdHash = erlang:md5(Pwd),

	{ok, #agent_auth_entry{
		login = Login,
		pwd_hash = PwdHash,
		profile = Profile,
		skills = Skills,
		security_level = SecurityLevel,
		endpoints = Endpoints
	}};
read_entry(_) ->
	error.

get_opt(K, L) ->
	Default = get_default(K),
	proplists:get_value(K, L, Default).

get_default(profile) -> "default";
get_default(endpoints) -> [];
get_default(security_level) -> agent.
