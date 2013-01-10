-module(openacd).

-export([start/0]).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, _}} ->
			ok
	end.

start() ->
	lists:foreach(fun ensure_started/1, my_apps()).

my_apps() -> [
	kernel,
	stdlib,
	sasl,
	mnesia,
	crypto,
	public_key,

	compiler,
	syntax_tools,
	xmerl,
	inets,
	ssl,

	ejrpc2,
	gproc,

	openacd
].

