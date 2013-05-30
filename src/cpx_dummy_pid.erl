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
	stop/2,

	do/2,
	do_async/2,
	has_received/2,
	watch_for/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	rcv_log = []
}).

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

do(Pid, F) ->
	gen_server:call(Pid, {do, F}).

do_async(Pid, F) ->
	gen_server:cast(Pid, {do, F}).

has_received(Pid, Msg) ->
	gen_server:call(Pid, {has_received, Msg}).

watch_for(Pid, Msg) ->
	gen_server:call(Pid, {watch_for, Msg}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({stop, Reason}, _From, St) ->
	{stop, Reason, ok, St};
handle_call({do, F}, _From, St) ->
	{reply, F(), St};
handle_call({has_received, Msg}, _From, St) ->
	Reply = lists:member(Msg, St#state.rcv_log),
	{reply, Reply, St};
handle_call({watch_for, Msg}, From, St) ->
	case lists:member(Msg, St#state.rcv_log) of
		true -> {reply, true, St};
		_ ->
			timer:send_after(500, {watch_for_req, Msg, From}),
			{noreply, St}
	end;

handle_call(_Request, _From, St) ->
    Reply = ok,
    {reply, Reply, St}.

handle_cast({do, F}, St) ->
	F(),
	{noreply, St};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({watch_for_req, Msg, From}, St) ->
	Reply = lists:member(Msg, St#state.rcv_log),
	gen_server:reply(From, Reply);
handle_info(M, St) ->
	Log = St#state.rcv_log,
	St1 = St#state{rcv_log=[M|Log]},
    {noreply, St1}.

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

do_test() ->
	Pid = ?M:start_link(),
	From = self(),
	?assertEqual(5, cpx_dummy_pid:do(Pid, fun() -> From ! hello, 5 end)),
	?assert(receive hello -> true after 0 -> false end).

do_async_test() ->
	Pid = ?M:start_link(),
	From = self(),
	?assertEqual(ok, cpx_dummy_pid:do_async(Pid, fun() -> From ! hello end)),
	?assert(receive hello -> true after 5 -> false end).

has_received_test() ->
	Pid = ?M:start_link(),
	Pid ! a,
	Pid ! b,
	timer:sleep(10),
	?assert(has_received(Pid, a)),
	?assert(has_received(Pid, b)),
	?assertNot(has_received(Pid, x)).

watch_for_test_() ->
	{foreach, fun() -> ?M:start_link() end, fun(Pid) -> catch ?M:stop(Pid) end, [
		fun(Pid) -> {"already received", fun() ->
			Pid ! a,
			timer:sleep(10),
			?assert(watch_for(Pid, a))
		end} end,
		fun(Pid) -> {"receive within", fun() ->
			spawn_link(fun() -> timer:sleep(100), Pid ! b end),
			?assert(watch_for(Pid, b))
		end} end
	]}.


-endif.