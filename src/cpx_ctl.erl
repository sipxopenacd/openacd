-module(cpx_ctl).
-export([process/1]).

-define(RET_INVALID_COMMAND, {error, 1}).

process(_) ->
	?RET_INVALID_COMMAND.
