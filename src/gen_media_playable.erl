-module(gen_media_playable).

-include("agent.hrl").
-include("call.hrl").

-type(gen_media_state() :: tuple()).
-type(state() :: tuple()).

-callback handle_play(Opts::list(), Call::#call{},
	GenMediaState::gen_media_state(), State::state()) ->
		{ok | {error, atom()}, NewState :: state()}.
-callback handle_pause(Call::#call{}, InternalState::gen_media_state(),
	State::state()) ->
		{ok | {error, atom()}, NewState::state()}.
-callback from_json_opts(JsonOpts::json()) -> list().
