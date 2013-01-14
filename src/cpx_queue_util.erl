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

-module(cpx_queue_util).

-include("queue.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([merge_queue_with_group/2,
	simple_get_merged_queue/2]).

-spec merge_queue_with_group(#call_queue{}, #queue_group{}) -> #call_queue{}.
merge_queue_with_group(Q, QG) ->
	#call_queue{skills=QSkills, recipe=QRecipe} = Q,
	#queue_group{skills=QGSkills, recipe=QGRecipe} = QG,

	MergedSkills = util:merge_skill_lists(QSkills, QGSkills),
	MergedRecipe = QRecipe ++ QGRecipe,

	Q#call_queue{skills=MergedSkills, recipe=MergedRecipe}.

-spec simple_get_merged_queue(Mod::atom(), QName::string()) -> {ok, #call_queue{}} | none.
simple_get_merged_queue(Mod, QName) ->
	case Mod:get_queue(QName) of
		{ok, Q} ->
			QG = find_qgroup(Mod, Q),
			{ok, merge_queue_with_group(Q, QG)};
		_ ->
			none
	end.

%% Internal
find_qgroup(Mod, Q) ->
	QGName = Q#call_queue.group,
	case Mod:get_queue_group(QGName) of
		{ok, V} ->
			V;
		_ ->
			lager:warning("Queue group ~p for queue ~p not found. Using default",
				[QGName, Q#call_queue.name]),
			{ok, V} = Mod:get_default_queue_group(),
			V
	end.

-ifdef(TEST).

t_queue() ->
	QSkills = [english],
	QRecipe = [{[{ticks, 5}], [], run_once, <<"blank qrecipe">>}],
	#call_queue{name="q0", group="qgA", skills=QSkills, recipe=QRecipe}.

t_qgroup() ->
	QGSkills = [chinese],
	QGRecipe = [{[{ticks, 2}], [], run_many, <<"blank qgrecipe">>}],
	#queue_group{name="qgA", skills=QGSkills, recipe=QGRecipe}.

t_default_qgroup() ->
	QGSkills = [japanese],
	QGRecipe = [{[{ticks, 3}], [], run_many, <<"blank def qgrecipe">>}],
	#queue_group{name="qgDef", skills=QGSkills, recipe=QGRecipe}.

merge_queue_with_group_test() ->
	Q = t_queue(),
	QSkills = Q#call_queue.skills,
	QRecipe = Q#call_queue.recipe,

	QG = t_qgroup(),
	QGSkills = QG#queue_group.skills,
	QGRecipe = QG#queue_group.recipe,

	MergedSkills = util:merge_skill_lists(QSkills, QGSkills),
	MergedRecipe = QRecipe ++ QGRecipe,

	?assertEqual(Q#call_queue{skills=MergedSkills, recipe=MergedRecipe},
		merge_queue_with_group(Q, QG)).

simple_get_merged_queue_test_() ->
	{setup, fun() ->
		meck:new(mock_cqueue)
	end, fun(_) ->
		meck:unload()
	end, [{"normal", fun() ->
		meck:expect(mock_cqueue, get_queue, 1, {ok, t_queue()}),
		meck:expect(mock_cqueue, get_queue_group, fun("qgA") -> {ok, t_qgroup()} end),

		?assertEqual({ok, merge_queue_with_group(t_queue(), t_qgroup())},
			simple_get_merged_queue(mock_cqueue, "q0"))
	end},
	{"missing qgroup", fun() ->
		meck:expect(mock_cqueue, get_queue, 1, {ok, t_queue()}),
		meck:expect(mock_cqueue, get_queue_group, fun("qgA") -> none end),
		meck:expect(mock_cqueue, get_default_queue_group, 0, {ok, t_default_qgroup()}),

		?assertEqual({ok, merge_queue_with_group(t_queue(), t_default_qgroup())},
			simple_get_merged_queue(mock_cqueue, "q0"))
	end},
	{"missing queue", fun() ->
		meck:expect(mock_cqueue, get_queue, 1, none),
		?assertEqual(none, simple_get_merged_queue(mock_cqueue, "q1"))
	end}]}.

-endif.
