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
	get_profiles/0,
	get_releases/0
]).

%%====================================================================
%% API
%%====================================================================

start() ->
	ok.

%% @doc Gets `#agent_auth{}' associated with `string() Login'.
-spec get_agent(Login :: string()) -> {ok, #agent_auth{}} | none.
get_agent(Login) ->
	get_agent_by_login(Login).

%% @doc Get an agent by login
-spec get_agent_by_login(Login :: string()) -> {ok, #agent_auth{}} | none.
get_agent_by_login(Login) ->
	none.

%% @doc Get an agent by id
-spec get_agent_by_id(Id :: string()) -> {ok, #agent_auth{}} | none.
get_agent_by_id(Id) ->
	none.

%% @doc Get all `#release_opt'.
-spec get_releases() -> {ok, [#release_opt{}]} | {error, any()}.
get_releases() ->
	{ok, []}.

%% @doc Gets the profile `string() Name'
-spec get_profile(Name :: string()) -> {ok, #agent_profile{}} | none.
get_profile(Name) ->
	none.

%% @doc Return all agent profiles.
-spec get_profiles() -> {ok, [#agent_profile{}]} | {error, any()}.
get_profiles() ->
	{ok, []}.

%% @doc Gets all the agents associated with `string() Profile'.
-spec get_agents_by_profile(Profile :: string()) -> {ok, [#agent_auth{}]} | {error, noprofile}.
get_agents_by_profile(Profile) ->
	{error, noprofile}.

%% @doc Take the plaintext username and password and attempt to
%% authenticate the agent.
-spec auth(Username :: string(), Password :: string()) -> {allow, #agent_auth{}} | deny.
auth(Username, Password) ->
	deny.

-ifdef(TEST).

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------

-endif.
