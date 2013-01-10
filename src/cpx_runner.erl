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
-module(cpx_runner).

-export([run/0]).

-define(PRINT(Fmt), io:format(Fmt, [])).
-define(PRINT(Fmt, Data), io:format(Fmt, Data)).


run() ->
	Exit = case init:get_plain_arguments() of
		[NodeStr|Args] ->
			Node = list_to_atom(NodeStr),
			% ?PRINT("Node: ~p, Args: ~p~n", [Node, Args]),
			call_ctl(Node, Args);
		_ ->
			print_usage(),
			1
	end,
	init:stop(Exit).

call_ctl(Node, Args) ->
	case rpc:call(Node, cpx_ctl, process, [Args]) of
		{badrpc, Reason} ->
			print_error(Node, Reason),
			2;
		{_, E} when is_integer(E) ->
			E;
		_ ->
			%% Unknown exit code
			3
	end.

print_usage() ->
	?PRINT("ERROR: Invalid arguments\n"),
	?PRINT("erl -[s]name NAME -s cpx_runner run -extra NODE FUN [ARGS]\n").

print_error(Node, Reason) ->
	?PRINT("ERROR: Unable to reach ~p, reason: ~p~n", [Node, Reason]).