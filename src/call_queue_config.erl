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

%% @doc The helper module to config the call_queues, skills, clients, and
%% queue groups.
%%
%% <h3>Call Queue</h3>
%% A Call Queue has a name, weight, skills, hold_music, group, and recipe.
%% The name is a unique identifier of the queue.  There can be only one
%% queue with a given name in a cluster.
%%
%% The wieght is how important media in the queue are relative to other
%% queues.  A higher wieght indicates more importance.
%%
%% Skills is a list of skills to add to media that is placed in the queue.
%% Note that media automatically get the magic '_node' skill assigned to
%% them.
%%
%% Hold music really only matters to freeswitch or voice media.  It is a
%% string that defines the file the hold music is located in.
%%
%% Group is which Queue Group the queue is a member of.  The skills and
%% recipe of a queue is combined with it's group on start up.
%%
%% Finally, there is the recipe.  A reciepe is a list of recipe steps.
%% A recipe step is a list of contions, what actions to take when those
%% conditions are met, whether to run multiple times or only once, and
%% finally a comment describing the step.
%%
%% The full description of a recipe:
%% 	recipe_runs() :: run_once | run_many
%% 	recipe_comparison() :: &lt; | &gt; | =
%% 	recipe_condition() ::
%%		{ticks, pos_integer()} |
%%		{eligible_agents, recipe_comparison(), non_neg_integer()} |
%%		{available_agents, recipe_comparison(), non_neg_integer()} |
%%		{queue_position, recipe_comparison(), non_neg_integer()} |
%%		{calls_queued, recipe_comparison(), non_neg_integer()}).
%%
%%	recipe_operation() ::
%%		{add_skills, [atom(), ...]} |
%%		{remove_skills, [atom(), ...]} |
%%		{set_priority, integer()} |
%%		{prioritize, []} |
%%		{deprioritize, []} |
%%		{voicemail, []} |
%%		{announce, string()} |
%%		{add_recipe, recipe_step()}
%%
%%	recipe_comment() :: binary()
%%
%%	recipe_step() ::
%%		{[recipe_condition(), ...], [recipe_operation(), ...],
%%			recipe_runs(), recipe_comment()}
%%
%%	recipe() :: [recipe_step()]
%%
%% <h3>Skills</h3>
%%
%% A skill configuration has an atom, name, protected, description, and
%% group.
%%
%% The atom is the key, and is used when routing.  Certain skills are
%% 'magic' in that they expand to {atom(), string()} values under the
%% correct conditions.  A magic skill is denoted by an atom starting with
%% an underscore by convetion, such as '_queue'.  It is an error to assign
%% an unexpanded magic skill under conditions when it cannot expand.  The
%% magic skills are:
%% <table style="border:black solid 1px">
%% <tr><th>Atom</th><th>Expands When</th><th>Other Notes</th></tr>
%% <tr><td>_agent</td><td>Assigned to an agent</td>
%% 	<td>Expands to the agent's login</td></tr>
%% <tr><td>_profile</td><td>Assigned to an agent</td><td>Expands to the
%% 	name of the agent's profile.</td></tr>
%% <tr><td>_queue></td><td>Assigned to a media that is in a queue</td>
%% 	<td>Expands to the name of the queue media is in</td></tr>
%% <tr><td>_brand</td><td>Assigned to a media</td><td>Expands to the
%% 	client's label.</td></tr>
%% <tr><td>_node</td><td>Assigned to an agent or media</td><td>Expands to
%% 	the node the agent fsm or gen_media process is running on.</td></tr>
%% <tr><td>_all</td><td>Always</td><td>Does not actually expand, but
%% 	overrides other skills, making that agent able to take any media, or
%% 	the media answerable by any agent.</td></tr>
%% </table>
%%
%% The remaining configuration options only matter to the configuration for
%% human's sake.  The only one that's not obvious is protected.  If
%% protected is set to true, the skill cannot be edited or deleted.  The
%% magic skills are protected.
%%
%% <h3>Queue Group</h3>
%%
%% A queue group is a recipe and list of skills that each queue in the
%% group shares.
%%
%% <h3>Clients</h3>
%%
%% A client is an id, label, and list of options.  Currently the only two
%% options available are url_pop and autoend_wrapup.

-module(call_queue_config).
-author("Micah").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_STORAGE, call_queue_config_mnesia).

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
	start/0
]).
-export([
	get_queue/1,
	get_merged_queue/1,
	get_queues/0,
	get_queues_by_group/1
]).
-export([
	get_queue_group/1,
	get_default_queue_group/0,
	get_queue_groups/0
]).
-export([
	get_skill_key/1,
	get_skill/1, %% equiv get_skill_by_key
	get_skill_by_key/1,
	get_skill_by_name/1,
	get_skills/0,
	get_skills_by_group/1
]).
-export([
	get_client/1, %% equiv get_client_by_id
	get_client_by_id/1,
	get_client_by_name/1,
	get_default_client/0,
	get_clients/0
]).

-callback get_queue(Name::string()) -> {ok, #call_queue{}} | none | {error, any()}.
-callback get_merged_queue(Queue::string()) -> {ok, #call_queue{}} | none | {error, any()}.
-callback get_queues_by_group(Group::string()) -> {ok, [#call_queue{}]} | none | {error, any()}.
-callback get_queue_group(Name::string()) -> {ok, #queue_group{}} | none | {error, any()}.
-callback get_default_queue_group() -> {ok, #queue_group{}} | {error, any()}.
-callback get_queue_groups() -> {ok, [#queue_group{}]} | {error, any()}.
-callback get_skill_key(Name::string()) -> {ok, atom()} | none | {error, any()}.
-callback get_skill_by_key(Key::atom()) -> {ok, #skill_rec{}} | none | {error, any()}.
-callback get_skill_by_name(Name::string()) -> {ok, #skill_rec{}} | none | {error, any()}.
-callback get_skills() -> {ok, [#skill_rec{}]} | none | {error, any()}.
-callback get_skills_by_group(Group::string()) -> {ok, [#skill_rec{}]} | none | {error, any()}.
-callback get_client_by_id(Id::string()) -> {ok, #client{}} | none | {error, any()} .
-callback get_client_by_name(Id::string()) -> {ok, #client{}} | none | {error, any()}.
-callback get_default_client() -> {ok, #client{}} | {error, any()}.
-callback get_clients() -> {ok, [#client{}]} | {error, any()}.

-define(STORE,
	{ok, _Store} = application:get_env(oacd_core, call_queue_config_storage),
	_Store).

-spec(start/0 :: () -> any()).
start() ->
	Store = case application:get_env(oacd_core, call_queue_config_storage) of
		{ok, St} -> St;
		undefined -> ?DEFAULT_STORAGE
	end,
	Store:start().

%% =====
%% Call queue
%% =====

%% @doc Get the configuration for the passed `string()' `Queue' name.
-spec get_queue(Name::string()) -> {ok, #call_queue{}} | none | {error, any()}.
get_queue(Name) ->
	?STORE:get_queue(Name).

%% @doc Get the configureation for the passed `string()' `Queue' name and
%% merge it with the queue group's skills and recipe.
-spec get_merged_queue(Queue::string()) -> {ok, #call_queue{}} | none | {error, any()}.
get_merged_queue(Queue) ->
	?STORE:get_merged_queue(Queue).

%% @doc Get all the queue configurations (`[#call_queue{}]').
-spec get_queues() -> {ok, [#call_queue{}]} | {error, any()}.
get_queues() ->
	?STORE:get_queues().

%% @doc Get all the queues that are members of the specified Group (`string()').
-spec get_queues_by_group(Group::string()) -> {ok, [#call_queue{}]} | none | {error, any()}.
get_queues_by_group(Group) ->
	?STORE:get_queues_by_group(Group).


%% =====
%% call queue groups Configs
%% =====


%% @doc get a `#queue_group{}' named `Name'
-spec get_queue_group(Name::string()) -> {ok, #queue_group{}} | none | {error, any()}.
get_queue_group(Name) ->
	?STORE:get_queue_group(Name).

-spec get_default_queue_group() -> {ok, #queue_group{}} | {error, any()}.
get_default_queue_group() ->
	?STORE:get_default_queue_group().

%% @doc Gets all `#queue_group{}' in a list sorted by group.
-spec get_queue_groups() -> {ok, [#queue_group{}]} | {error, any()}.
get_queue_groups() ->
	?STORE:get_queue_groups().

%% =====
%% Skill Configs
%% =====

%% @doc Check if the given `string()' `Skillname' exists.
%% Returns the `atom()' of `Skillname' or `undefined'
-spec get_skill_key(Name::string()) -> {ok, atom()} | none | {error, any()}.
get_skill_key(Name) ->
	?STORE:get_skill_key(Name).

%% @doc get a single `#skill_rec{}'
-spec get_skill(Key::atom()) -> {ok, #skill_rec{}} | none | {error, any()}.
get_skill(Key) ->
	get_skill_by_key(Key).

-spec get_skill_by_key(Key::atom()) -> {ok, #skill_rec{}} | none | {error, any()}.
get_skill_by_key(Key) ->
	?STORE:get_skill_by_key(Key).

-spec get_skill_by_name(Name::string()) -> {ok, #skill_rec{}} | none | {error, any()}.
get_skill_by_name(Key) ->
	?STORE:get_skill_by_name(Key).

%% @doc Return `[#skill_rec{}]' in the system sorted by group
-spec get_skills() -> {ok, [#skill_rec{}]} | none | {error, any()}.
get_skills() ->
	?STORE:get_skills().

%% @doc Returns `[#skill_rec{}]' in the system which have a group of `string()' `Group'.
-spec get_skills_by_group(Group::string()) -> {ok, [#skill_rec{}]} | none | {error, any()}.
get_skills_by_group(Group) ->
	?STORE:get_skills_by_group(Group).

%% =====
%% Client configs
%% =====

%% @doc Get the `#client{}' associated with the id `Id'
-spec get_client(Id::string()) -> {ok, #client{}} | none | {error, any()}.
get_client(Id) ->
	get_client_by_id(Id).

-spec get_client_by_id(Id::string()) -> {ok, #client{}} | none | {error, any()}.
get_client_by_id(Id) ->
	?STORE:get_client_by_id(Id).

-spec get_client_by_name(Id::string()) -> {ok, #client{}} | none | {error, any()}.
get_client_by_name(Name) ->
	?STORE:get_client_by_name(Name).

-spec get_default_client() -> {ok, #client{}} | {error, any()}.
get_default_client() ->
	?STORE:get_default_client().

-spec get_clients() -> {ok, [#client{}]} | {error, any()}.
get_clients() ->
	?STORE:get_clients().

%% =====
%% Tests
%% =====

-ifdef(TEST).

t_queue() ->
	#call_queue{name="queue0", skills=[english]}.

t_merged_queue() ->
	#call_queue{name="queue0", skills=[english, chinese]}.

t_queues() ->
	[#call_queue{name="queue0", skills=[english], group="qgroupA"},
	#call_queue{name="queue1", skills=[japanese], group="qgroupA"},
	#call_queue{name="queue2", skills=[japanese], group="qgroupB"}].

t_queues_group_a() ->
	[#call_queue{name="queue0", skills=[english], group="qgroupA"},
	#call_queue{name="queue1", skills=[japanese], group="qgroupA"}].

t_queue_group() ->
	#queue_group{name="qgroupA", skills=[chinese]}.

t_default_queue_group() ->
	#queue_group{name="qgroupDef", skills=[japanese]}.

t_queue_groups() ->
	[#queue_group{name="qgroupA", skills=[chinese]},
	#queue_group{name="qgroupB", skills=[]}].

t_skill() ->
	#skill_rec{atom=chinese, name="Chinese"}.

t_skills() ->
	[#skill_rec{atom=chinese, name="Chinese", group="Language"},
	#skill_rec{atom=japanese, name="Japanese", group="Language"},
	#skill_rec{atom=admin, name="Administration", group="Department"}].

t_skills_group_lang() ->
	[#skill_rec{atom=chinese, name="Chinese", group="Language"},
	#skill_rec{atom=japanese, name="Japanese", group="Language"}].

t_client() ->
	#client{id="cl0", label="Client0"}.

t_default_client() ->
	#client{id="cldef", label="DefClient"}.

t_clients() ->
	[t_default_client(), t_client()].

passthrough_test_() ->
	{setup, fun() ->
		meck:new(mock_cqueue),
		application:set_env(oacd_core, call_queue_config_storage, mock_cqueue),

		meck:expect(mock_cqueue, get_queue, 1, {ok, t_queue()}),
		meck:expect(mock_cqueue, get_merged_queue, 1, {ok, t_merged_queue()}),
		meck:expect(mock_cqueue, get_queues, 0, {ok, t_queues()}),
		meck:expect(mock_cqueue, get_queues_by_group, 1, {ok, t_queues_group_a()}),

		meck:expect(mock_cqueue, get_queue_group, 1, {ok, t_queue_group()}),
		meck:expect(mock_cqueue, get_default_queue_group, 0, {ok, t_default_queue_group()}),
		meck:expect(mock_cqueue, get_queue_groups, 0, {ok, t_queue_groups()}),
		meck:expect(mock_cqueue, get_skill_key, 1, {ok, chinese}),

		meck:expect(mock_cqueue, get_skill_by_key, 1, {ok, t_skill()}),
		meck:expect(mock_cqueue, get_skill_by_name, 1, {ok, t_skill()}),
		meck:expect(mock_cqueue, get_skills, 0, {ok, t_skills()}),
		meck:expect(mock_cqueue, get_skills_by_group, 1, {ok, t_skills_group_lang()}),

		meck:expect(mock_cqueue, get_client, 1, {ok, t_client()}),
		meck:expect(mock_cqueue, get_client_by_id, 1, {ok, t_client()}),
		meck:expect(mock_cqueue, get_client_by_name, 1, {ok, t_client()}),
		meck:expect(mock_cqueue, get_default_client, 0, {ok, t_default_client()}),
		meck:expect(mock_cqueue, get_clients, 0, {ok, t_clients()})

	end, fun(_) ->
		application:unset_env(oacd_core, call_queue_config_storage),
		meck:unload()
	end, [
		?_assertEqual({ok, t_queue()}, get_queue("queue0")),
		?_assertEqual({ok, t_merged_queue()}, get_merged_queue("queue0")),
		?_assertEqual({ok, t_queues()}, get_queues()),
		?_assertEqual({ok, t_queues_group_a()}, get_queues_by_group("qgroupA")),

		?_assertEqual({ok, t_queue_group()}, get_queue_group("qgroupA")),
		?_assertEqual({ok, t_default_queue_group()}, get_default_queue_group()),
		?_assertEqual({ok, t_queue_groups()}, get_queue_groups()),

		?_assertEqual({ok, chinese}, get_skill_key("Chinese")),
		?_assertEqual({ok, t_skill()}, get_skill(chinese)), %% equiv get_skill_by_key
		?_assertEqual({ok, t_skill()}, get_skill_by_key(chinese)),
		?_assertEqual({ok, t_skill()}, get_skill_by_name("Chinese")),
		?_assertEqual({ok, t_skills()}, get_skills()),
		?_assertEqual({ok, t_skills_group_lang()}, get_skills_by_group("Language")),

		?_assertEqual({ok, t_client()}, get_client("cl0")),
		?_assertEqual({ok, t_client()}, get_client_by_id("cl0")),
		?_assertEqual({ok, t_client()}, get_client_by_name("Client0")),
		?_assertEqual({ok, t_default_client()}, get_default_client()),
		?_assertEqual({ok, t_clients()}, get_clients())
	]}.

-endif.
