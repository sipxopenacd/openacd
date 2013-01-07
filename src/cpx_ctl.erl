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

process(_) ->
	?RET_INVALID_COMMAND.
