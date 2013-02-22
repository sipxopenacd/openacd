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

-module(call_queue_config_ets).

-include("queue.hrl").
-include("call.hrl").

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
	get_skill_by_key/1,
	get_skill_by_name/1,
	get_skills/0,
	get_skills_by_group/1
]).
-export([
	get_client_by_id/1,
	get_client_by_name/1,
	get_default_client/0,
	get_clients/0
]).
-export([
	load_queues/1,
	load_skills/1,
	load_clients/1
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

-define(CALL_QUEUE_TAB, call_queue_entry).
-define(SKILLS_TAB, skill_entry).
-define(CLIENTS_TAB, client_entry).

-record(call_queue_entry, {
	name = erlang:error({undefined, name}) :: string(),
	weight = 1 :: non_neg_integer(),
	skills = [english, '_node'] :: [atom()],
	recipe = ?DEFAULT_RECIPE :: recipe(),
	hold_music :: string() | undefined,
	group = "Default" :: string()
}).
-record(skill_entry, {
	atom :: atom(),
	name = "New Skill" :: string(),
	protected = false :: 'true' | 'false',
	description = "Default description" :: string(),
	group = "Misc" :: string()
}).
-record(client_entry, {
	id :: string() | 'undefined',
	label :: string() | 'undefined',
	options = [] :: client_opts(),
	last_integrated :: 'undefined' | pos_integer()
}).

start() ->
	%% TODO put in a process
	ets:new(?CALL_QUEUE_TAB, [{keypos, #call_queue_entry.name}, named_table, public]),
	ets:new(?SKILLS_TAB, [{keypos, #skill_entry.atom}, named_table, public]),
	ets:new(?CLIENTS_TAB, [{keypos, #client_entry.id}, named_table, public]),
	ok.

get_queue(Q) ->
	case ets:lookup(?CALL_QUEUE_TAB, Q) of
		[E] ->
			{ok, entry_to_call_queue(E)};
		_ ->
			none
	end.

get_merged_queue(Name) ->
	cpx_queue_util:simple_get_merged_queue(?MODULE, Name).

get_queues() ->
	Es = ets:tab2list(?CALL_QUEUE_TAB),
	Qs = [entry_to_call_queue(E) || E <- Es],
	{ok, Qs}.
get_queues_by_group(Group) ->
	Es = ets:match_object(?CALL_QUEUE_TAB, #call_queue_entry{group=Group, _='_'}),
	Qs = [entry_to_call_queue(E) || E <- Es],
	{ok, Qs}.

get_queue_group(Q) ->
	case get_queue(Q) of
		{ok, #call_queue{group = Group}} ->
			{ok, Group};
		_ ->
			none
	end.

get_default_queue_group() ->
	{ok, #queue_group{name="Default"}}.
get_queue_groups() ->
	{ok, D} = get_default_queue_group(),
	{ok, [D]}.

get_skill_key(Name) ->
	case get_skill_by_name(Name) of
		{ok, #skill_rec{atom=Key}} ->
			{ok, Key};
		_ ->
			none
	end.
get_skill_by_key(Key) ->
	case ets:lookup(?SKILLS_TAB, Key) of
		[E] ->
			{ok, entry_to_skill(E)};
		_ ->
			none
	end.
get_skill_by_name(Name) ->
	case ets:match_object(?SKILLS_TAB, #skill_entry{name=Name, _='_'}) of
		[E] ->
			{ok, entry_to_skill(E)};
		_ ->
			none
	end.
get_skills() ->
	Es = ets:tab2list(?SKILLS_TAB),
	Ss = [entry_to_skill(E) || E <- Es],
	{ok, Ss}.

get_skills_by_group(Group) ->
	Es = ets:match_object(?SKILLS_TAB, #skill_entry{group=Group, _='_'}),
	Ss = [entry_to_skill(E) || E <- Es],
	{ok, Ss}.

get_client_by_id(Id) ->
	case ets:lookup(?CLIENTS_TAB, Id) of
		[E] ->
			{ok, entry_to_client(E)};
		_ ->
			none
	end.

get_client_by_name(Name) ->
	case ets:match_object(?CLIENTS_TAB, #client_entry{label=Name, _='_'}) of
		[E] ->
			{ok, entry_to_client(E)};
		_ ->
			none
	end.

get_default_client() ->
	{ok, #client{id="Default", label="Default"}}.
get_clients() ->
	Es = ets:tab2list(?CLIENTS_TAB),
	Cs = [entry_to_client(E) || E <- Es],
	{ok, Cs}.

-spec load_queues(Qs :: list()) -> {ok, [#call_queue_entry{}]}.
load_queues(Qs) ->
	ets:delete_all_objects(?CALL_QUEUE_TAB),
	Entries = lists:map(fun(Q) ->
		#call_queue_entry{name = Q}
	end, Qs),
	ets:insert(?CALL_QUEUE_TAB, Entries),
	{ok, Entries}.

-spec load_skills(Ss :: list()) -> {ok, [#skill_entry{}]}.
load_skills(Ss) ->
	ets:delete_all_objects(?SKILLS_TAB),
	Entries = lists:map(fun(S) ->
		Key = S,
		#skill_entry{atom = Key, name = atom_to_list(Key)}
	end, Ss),
	ets:insert(?SKILLS_TAB, Entries),
	{ok, Entries}.

-spec load_clients(Cs :: list()) -> {ok, [#client_entry{}]}.
load_clients(Cs) ->
	ets:delete_all_objects(?CLIENTS_TAB),
	Entries = lists:map(fun(C) ->
		Id = C,
		#client_entry{id = Id, label = Id}
	end, Cs),
	ets:insert(?CLIENTS_TAB, Entries),
	{ok, Entries}.

entry_to_call_queue(E) ->
	#call_queue_entry{
		name = N,
		weight = W,
		skills = Ss,
		recipe = R,
		hold_music = M,
		group = G
	} = E,
	#call_queue{
		name = N,
		weight = W,
		skills = Ss,
		recipe = R,
		hold_music = M,
		group = G
	}.

entry_to_skill(E) ->
	#skill_entry{
		atom = A,
		name = N,
		protected = P,
		description = D,
		group = G
	} = E,
	#skill_rec{
		atom = A,
		name = N,
		protected = P,
		description = D,
		group = G
	}.

entry_to_client(E) ->
	#client_entry{
		id = Id,
		label = Label,
		options = Opts,
		last_integrated = LastInt
	} = E,
	#client{
		id = Id,
		label = Label,
		options = Opts,
		last_integrated = LastInt
	}.
