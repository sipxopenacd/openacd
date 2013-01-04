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

start() -> ok.
get_queue(_) -> none.
get_merged_queue(_) -> none.

get_queues() -> {ok, []}.
get_queues_by_group(_) -> none.

get_queue_group("Default") -> get_default_queue_group();
get_queue_group(_) -> none.

get_default_queue_group() -> {ok, #queue_group{name="Default"}}.
get_queue_groups() ->
	{ok, D} = get_default_queue_group(),
	{ok, [D]}.

get_skill_key(_) -> none.
get_skill_by_key(_) -> none.
get_skill_by_name(_) -> none.
get_skills() -> {ok, []}.

get_skills_by_group(_) -> none.

get_client_by_id("Default") -> get_default_client();
get_client_by_id(_) -> none.

get_client_by_name("Default") -> get_default_client();
get_client_by_name(_) -> none.

get_default_client() ->
	{ok, #client{id="Default", label="Default"}}.
get_clients() ->
	{ok, D} = get_default_client(),
	{ok, [D]}.
