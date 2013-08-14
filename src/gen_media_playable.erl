-module(gen_media_playable).
-export([behaviour_info/1]).

-spec(behaviour_info/1 ::
	(Info :: 'callbacks' | any()) -> [{atom(), non_neg_integer()}] | 'undefined').
behaviour_info(callbacks) ->
	[
		{handle_play, 2},
		{handle_pause, 2}
	];
behaviour_info(_Other) ->
    undefined.
