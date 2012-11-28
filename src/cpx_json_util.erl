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

-module(cpx_json_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("agent.hrl").
-include("queue.hrl").

%% encode
-export([enc_skills/1, enc_state_changes/1, enc_agent_state/1, enc_skill_recs/1]).
%% binutils
-export([l2b/1, b2l/1, nob/1, nol/1]).

-spec enc_skills(skills()) -> json().
enc_skills(Skills) ->
	lists:reverse(lists:foldl(
		fun(At, Acc) when is_atom(At) -> [At|Acc];
			({'_brand', Client}, Acc) -> [{[{client, l2b(Client)}]}|Acc];
			({'_node', Node}, Acc) -> [{[{node, Node}]}|Acc];
			({'_profile', Profile}, Acc) -> [{[{profile, l2b(Profile)}]}|Acc];
			({'_queue', Queue}, Acc) -> [{[{queue, l2b(Queue)}]}|Acc];
			(_, Acc) -> Acc
		end, [], Skills)).

-spec enc_skill_recs([#skill_rec{}]) -> json().
enc_skill_recs(SkillRecs) ->
	[At || #skill_rec{atom=At} <- SkillRecs].

-type state_changes() :: [{atom(), tuple()}].
-spec enc_state_changes(state_changes()) -> json().
enc_state_changes(Changes) ->
	lists:reverse(lists:map(
		fun({State, Timestamp}) ->
			{[{State, util:now_ms(Timestamp)}]}
		end, Changes)).

-spec enc_agent_state(available | {released, tuple()}) -> json().
enc_agent_state({released, {_, Label, _}}) ->
	LabelEnc = case Label of
		L when is_list(L) -> l2b(L);
		L when is_atom(L) -> L;
		_ -> undefined
	end,
	{[{released, LabelEnc}]};
enc_agent_state(State) when is_atom(State) ->
	State.

-spec b2l(binary()) -> list().
b2l(B) ->
	binary_to_list(B).

-spec l2b(list()) -> binary().
l2b(L) ->
	list_to_binary(L).

-spec nob(null | undefined | list()) -> null | binary().
nob(null) ->
	null;
nob(undefined) ->
	null;
nob(L) when is_list(L) ->
	list_to_binary(L).

-spec nol(null | undefined | binary()) -> null | list().
nol(null) ->
	null;
nol(undefined) ->
	null;
nol(L) when is_binary(L) ->
	binary_to_list(L).

-ifdef(TEST).

binary_utils_test_() ->
	[?_assertEqual(b2l(<<"hello">>), "hello"),
	?_assertEqual(l2b("hello"), <<"hello">>)].

enc_skills_test() ->
	?assertEqual([english, support, '_all',
			{[{'client', <<"Client 1">>}]},
			{[{'node', 'server1.openacd.com'}]},
			{[{'profile', <<"tech_team">>}]},
			{[{'queue', <<"dsl_support">>}]}],
		enc_skills([english, support, '_all',
			{'_brand', "Client 1"},
			{'_node', 'server1.openacd.com'},
			{'_profile', "tech_team"},
			{'_queue', "dsl_support"},
			{unknown, should, notbepresent}])).

enc_skill_recs_test() ->
	?assertEqual([english, support, '_all', '_brand', '_node', '_profile', '_queue'],
		enc_skill_recs([#skill_rec{atom=english, name="English"},
			#skill_rec{atom=support, name="Support"},
			#skill_rec{atom='_all', name="All"},
			#skill_rec{atom='_brand', name="Brand"},
			#skill_rec{atom='_node', name="Node"},
			#skill_rec{atom='_profile', name="Agent Profile"},
			#skill_rec{atom='_queue', name="Queue"}])).

enc_state_changes_test() ->
	?assertEqual([{[{init, 1352161996526}]},
			{[{inqueue, 1352161998317}]}],
		enc_state_changes([{inqueue, {1352,161998,317840}},
			{init, {1352,161996,526276}}])).

enc_agent_state_test_() ->
	[?_assertEqual({[{released, <<"label">>}]}, enc_agent_state({released, {rel, "label", -1}})),
	?_assertEqual({[{released, label}]}, enc_agent_state({released, {rel1, label, 0}})),
	?_assertEqual(available, enc_agent_state(available))].

-endif.
