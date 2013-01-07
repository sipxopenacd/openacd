-module(cpx_ctl).
-export([process/1]).

-define(RET_SUCCESS, {ok, 0}).
-define(RET_INVALID_COMMAND, {error, 1}).

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
	io:format("Uptime: ~p~n", [Uptime]),
	io:format("Number of queues: ~p~n", [QueueCount]),
	io:format("Number of agents logged in: ~p~n", [AgentCount]),
	io:format("~nPlugins running:~n"),
	[io:format("~p~n", [P]) || {P, running} <- Plugins],
	?RET_SUCCESS;

process(_) ->
	?RET_INVALID_COMMAND.
