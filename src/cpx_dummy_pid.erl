-module(cpx_dummy_pid).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	start/0,
	start_link/0,
	stop/1,
	stop/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {}).

%% API

start() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	Pid.

start_link() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	Pid.

stop(Pid) ->
	stop(Pid, normal).

stop(Pid, Reason) ->
	gen_server:call(Pid, {stop, Reason}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({stop, Reason}, _From, St) ->
	{stop, Reason, ok, St};
handle_call(_Request, _From, St) ->
    Reply = ok,
    {reply, Reply, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% tests
-ifdef(TEST).

-define(M, ?MODULE).

start_stop_test() ->
	Pid = ?M:start(),
	?assert(is_process_alive(Pid)),

	?M:stop(Pid),
	?assertNot(is_process_alive(Pid)).

start_link_test() ->
	Pid = ?M:start_link(),
	{_, Links} = erlang:process_info(self(), links),
	?assert(lists:member(Pid, Links)).

exit_test() ->
	process_flag(trap_exit, true),
	Pid = ?M:start_link(),

	?M:stop(Pid, bla),
	?assertNot(is_process_alive(Pid)),
	?assert(receive {'EXIT', Pid, bla} -> true after 0 -> false end).


-endif.