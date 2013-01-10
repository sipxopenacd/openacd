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
%%  Jan Vincent Liwanag <jvliwanag at ezuce com>
%%

-module(agent_auth).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_STORAGE, agent_auth_ets).

%% API
-export([
	start/0,
	auth/2
]).
-export([
	get_agent/1,
	get_agent_by_login/1,
	get_agent_by_id/1,
	get_agents_by_profile/1,
	get_profile/1,
	get_default_profile/0,
	get_profiles/0,
	get_release/1,
	get_releases/0
]).

-callback start() -> ok.

%% Agent
-callback get_agent_by_login(string()) -> {ok, #agent_auth{}} | none | {error, any()}.
-callback get_agent_by_id(string()) -> {ok, #agent_auth{}} | none | {error, any()}.
-callback get_agents_by_profile(string()) -> {ok, [#agent_auth{}]} | none | {error, any()}.

%% Auth
-callback auth(Login::string(), Password::string()) -> {ok, #agent_auth{}} | {error, deny}.

%% Profile
-callback get_profile(string()) -> {ok, #agent_profile{}} | none | {error, any()}.
-callback get_profiles() -> {ok, [#agent_profile{}]}.
-callback get_default_profile() -> {ok, #agent_profile{}} | {error, any()}.

%% Release
-callback get_release(Id::string()) -> {ok, #release_opt{}} | none | {error, any()}.
-callback get_releases() -> {ok, [#release_opt{}]} | {error, any()}.

-define(STORE,
	{ok, _Store} = application:get_env(openacd, agent_auth_storage),
	_Store).

%%====================================================================
%% API
%%====================================================================

start() ->
	case application:get_env(openacd, agent_auth_storage) of
		{ok, St} ->
			St;
		_ ->
			St = ?DEFAULT_STORAGE,
			application:set_env(openacd, agent_auth_storage, St),
			St
	end,
	St:start().

%% Agent

%% @doc Gets `#agent_auth{}' associated with `string() Login'.
-spec get_agent(Login :: string()) -> {ok, #agent_auth{}} | none.
get_agent(Login) ->
	get_agent_by_login(Login).

%% @doc Get an agent by login
-spec get_agent_by_login(Login :: string()) -> {ok, #agent_auth{}} | none.
get_agent_by_login(Login) ->
	?STORE:get_agent_by_login(Login).

%% @doc Get an agent by id
-spec get_agent_by_id(Id :: string()) -> {ok, #agent_auth{}} | none.
get_agent_by_id(Id) ->
	?STORE:get_agent_by_id(Id).

%% @doc Gets all the agents associated with `string() Profile'.
-spec get_agents_by_profile(Profile :: string()) -> {ok, [#agent_auth{}]} | {error, noprofile}.
get_agents_by_profile(Profile) ->
	?STORE:get_agents_by_profile(Profile).

%% @doc Take the plaintext username and password and attempt to
%% authenticate the agent.
-spec auth(Username :: string(), Password :: string()) -> {ok, #agent_auth{}} | {error, deny}.
auth(Username, Password) ->
	?STORE:auth(Username, Password).

%% Profile

%% @doc Gets the profile `string() Name'
-spec get_profile(Name :: string()) -> {ok, #agent_profile{}} | none.
get_profile(Name) ->
	?STORE:get_profile(Name).

%% @doc Gets the profile `string() Name'
-spec get_default_profile() -> {ok, #agent_profile{}} | none.
get_default_profile() ->
	?STORE:get_default_profile().

%% @doc Return all agent profiles.
-spec get_profiles() -> {ok, [#agent_profile{}]} | {error, any()}.
get_profiles() ->
	?STORE:get_profiles().

%% Release

%% @doc Get all `#release_opt'.
-spec get_release(Id::string()) -> {ok, [#release_opt{}]} | {error, any()}.
get_release(Id) ->
	?STORE:get_release(Id).

%% @doc Get all `#release_opt'.
-spec get_releases() -> {ok, [#release_opt{}]} | {error, any()}.
get_releases() ->
	?STORE:get_releases().

%% Internal

-ifdef(TEST).

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------

t_agent() ->
	#agent_auth{id="a0", login="agent0"}.

t_agents() ->
	[#agent_auth{id="a0", login="agent0"},
	#agent_auth{id="a1", login="agent1"}].

t_profile() ->
	#agent_profile{id="prof1", name="profile1"}.

t_profiles() ->
	[#agent_profile{id="prof0", name="profile0"},
	#agent_profile{id="prof1", name="profile1"}].

t_release() ->
	#release_opt{id="rel0", label="release0"}.

t_releases() ->
	[#release_opt{id="rel0", label="release0"},
	#release_opt{id="rel1", label="release1"}].

passthrough_test_() ->
	{foreach, fun() ->
		meck:new(mock_auth),
		application:set_env(openacd, agent_auth_storage, mock_auth)
	end, fun(_) ->
		application:unset_env(openacd, agent_auth_storage),
		meck:unload()
	end, [
		t_passthrough(get_agent, get_agent_by_login, ["agent0"], {ok, t_agent()}),

		t_passthrough(get_agent_by_login, ["agent0"], {ok, t_agent()}),
		t_passthrough(get_agent_by_id, ["a0"], {ok, t_agent()}),
		t_passthrough(get_agents_by_profile, ["Default"], {ok, t_agents()}),
		t_passthrough(auth, ["agent0", "pwd"], {ok, t_agent()}),

		t_passthrough(get_profile, ["Default"], {ok, t_profile()}),
		t_passthrough(get_profiles, [], {ok, t_profiles()}),
		t_passthrough(get_default_profile, [], {ok, t_profile()}),

		t_passthrough(get_release, ["rel0"], {ok, t_release()}),
		t_passthrough(get_releases, [], {ok, t_releases()})
	]}.


t_passthrough(ApiFun, Args, Ret) ->
	t_passthrough(ApiFun, ApiFun, Args, Ret).

t_passthrough(ApiFun, CbkFun, Args, Ret) ->
	cpx_test_util:t_passthrough(?MODULE, mock_auth, ApiFun, CbkFun, Args, Ret).


-endif.
