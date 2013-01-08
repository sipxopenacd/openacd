-module(cpx_ctl).
-export([process/1]).

-define(RET_SUCCESS, {ok, 0}).
-define(RET_INVALID_COMMAND, {error, 1}).

-define(PRINT(Fmt), io:format(Fmt, [])).
-define(PRINT(Fmt, Data), io:format(Fmt, Data)).

process(["stop"]) ->
	io:format("Stopping openacd~n"),
	init:stop(),
	?RET_SUCCESS;

process(["restart"]) ->
	io:format("Restarting openacd~n"),
	init:restart(),
	?RET_SUCCESS;

process(["pid"]) ->
	io:format("~s~n", [os:getpid()]),
	?RET_SUCCESS;

process(["status"]) ->
	{ok, Uptime} = application:get_env(oacd_core, uptime),
	AgentCount = length(agent_manager:list()),
	{ok, Queues} = call_queue_config:get_queues(),
	QueueCount = length(Queues),
	Plugins = cpx:plugins_running(),
	?PRINT("Uptime: ~p~n", [Uptime]),
	?PRINT("Number of queues: ~p~n", [QueueCount]),
	?PRINT("Number of agents logged in: ~p~n", [AgentCount]),
	?PRINT("~nPlugins running:~n"),
	[?PRINT("~p~n", [P]) || {P, running} <- Plugins],
	?RET_SUCCESS;

process(["list-agents"]) ->
	?RET_SUCCESS;

process(["list-queues"]) ->
	?RET_SUCCESS;

process(["list-calls"]) ->
	?RET_SUCCESS;

process(["show-agent", _Agent]) ->
	?RET_SUCCESS;

process(["show-queue", _Queue]) ->
	?RET_SUCCESS;

process(["trace-agent", _Agent]) ->
	?RET_SUCCESS;

process(["kick-agent", _Agent]) ->
	?RET_SUCCESS;

process(_) ->
	?RET_INVALID_COMMAND.
