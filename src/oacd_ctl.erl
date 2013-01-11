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

-module(oacd_ctl).

-export([start/0]).

start() ->
	St = process(init:get_plain_arguments()),
	halt(St).

process([Node, "stop"]) ->
	rpc_call(Node, init, stop, []);

process([Node, "restart"]) ->
	rpc_call(Node, init, restart, []);


process(_) ->
	usage().

usage() ->
	io:format("Usage: openacd start|live|stop|restart|status~n"),
	1.

rpc_call(NodeStr, Mod, Fun, Args) ->
	Node = list_to_atom(NodeStr),
	case rpc:call(Node, Mod, Fun, Args) of
		{badrpc, Reason} ->
			io:format("Unable to connect to node ~p: ~p~n", [Node, Reason]),
			1;
		_ ->
			0
	end.

%% Internal
