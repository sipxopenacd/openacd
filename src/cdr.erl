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
%%	The Original Code is Spice Telephony.
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
%%	Andrew Thompson <athompson at spicecsm dot com>
%%	Micah Warren <mwarren at spicecsm dot com>
%%

%% @doc A gen_event based module to create and record Call Data Records to mnesia.

-module(cdr).
-behaviour(gen_event).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	start/0,
	cdrinit/1,
	inqueue/2,
	ringing/2,
	oncall/2,
	hangup/2,
	wrapup/2,
	endwrapup/2,
	transfer/2,
	agent_transfer/2,
	status/1,
	recover/1,
	recoveryinit/1,
	merge/3
]).

-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).


-type(transaction_type() :: 'inqueue' | 'ringing' | 'oncall' | 'wrapup').
-type(callid() :: string()).
-type(time() :: integer()).
-type(datalist() :: [{atom(), string()}]).
-type(proplist() :: [{any(), any()}]).
-type(transaction() :: {transaction_type(), callid(), time(), datalist()}).
-type(transactions() :: [transaction()]).
-type(raw_transaction() :: {transaction_type(), time(), any()}).
%-type(proto_transactions() :: [proto_transaction()]).
%	-type(proto_transaction() :: {transaction_type(), callid(), time(), datalist()}).

-record(state, {
	id :: callid(),
	transactions = [] :: transactions(),
	unterminated = [] :: [raw_transaction()],
	limbo :: raw_transaction() | 'undefined',
	hangup = false :: bool(),
	wrapup = false :: bool()}).
	
-record(cdr_rec, {
	id :: callid(),
	summary = inprogress :: 'inprogress' | proplist(),
	transactions = inprogress :: 'inprogress' | transactions()
}).

-record(cdr_raw, {
	id :: callid(),
	transaction :: raw_transaction()
}).

-type(state() :: #state{}).
-define(GEN_EVENT, true).
-include("gen_spec.hrl").

%event -> terminated by
%inqueue -> ringing
%ringing -> ringing, oncall
%oncall -> wrapup
%wrapup -> endwrapup
%
%event -> initialted by
%inqueue ->
%ringing -> inqueue, ringing
%oncall -> ringing, transfer
%wrapup -> oncall
%endwrapup -> wrapup
%
%event -> branched by
%oncall -> transfer
	
%% API

%% @doc starts the cdr event server.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	build_tables(),
	gen_event:start({local, cdr}).

%% @doc Create a handler specifically for `#call{} Call' with default options.
-spec(cdrinit/1 :: (Call :: #call{}) -> 'ok' | 'error').
cdrinit(Call) ->
	try gen_event:add_handler(cdr, {?MODULE, Call#call.id}, [Call]) of
		ok ->
			ok;
		Else ->
			?ERROR("Initializing CDR for ~s erred with: ~p", [Call#call.id, Else]),
			error
	catch
		What:Why ->
			?ERROR("Initializing CDR for ~s erred with: ~p:~p", [Call#call.id, What, Why]),
			error
	end.

%% @doc Create a handler for `#call{} Call', then use the recovery database.
-spec(recoveryinit/1 :: (Call :: #call{}) -> 'ok' | 'error').
recoveryinit(Call) ->
	case cdrinit(Call) of
		ok ->
			recover(Call);
		error ->
			error
	end.
	
%% @doc Notify cdr handler that `#call{} Call' is now in queue `string() Queue'.
-spec(inqueue/2 :: (Call :: #call{}, Queue :: string()) -> 'ok').
inqueue(Call, Queue) ->
	catch gen_event:notify(cdr, {inqueue, Call, nowsec(now()), Queue}).

%% @doc Notify cdr handler that `#call{} Call' is now ringing to `string() Agent'.
-spec(ringing/2 :: (Call :: #call{}, Agent :: string()) -> 'ok').
ringing(Call, Agent) when is_pid(Agent) ->
	ringing(Call, agent_manager:find_by_pid(Agent));
ringing(Call, Agent) ->
	catch gen_event:notify(cdr, {ringing, Call, nowsec(now()), Agent}).

%% @doc Notify cdr handler that `#call{} Call' is currently oncall with `string() Agent'.
-spec(oncall/2 :: (Call :: #call{}, Agent :: string()) -> 'ok').
oncall(Call, Agent) when is_pid(Agent) ->
	oncall(Call, agent_manager:find_by_pid(Agent));
oncall(Call, Agent) ->
	catch gen_event:notify(cdr, {oncall, Call, nowsec(now()), Agent}).

%% @doc Notify cdr handler that `#call{} Call' has been hungup by `string() | agent By'.
-spec(hangup/2 :: (Call :: #call{}, By :: string() | 'agent') -> 'ok').
hangup(Call, By) ->
	catch gen_event:notify(cdr, {hangup, Call, nowsec(now()), By}).

%% @doc Notify cdr handler that `#call{} Call' has been put in wrapup by `string() Agent'.
-spec(wrapup/2 :: (Call :: #call{}, Agent :: string()) -> 'ok').
wrapup(Call, Agent) when is_pid(Agent) ->
	wrapup(Call, agent_manager:find_by_pid(Agent));
wrapup(Call, Agent) ->
	catch gen_event:notify(cdr, {wrapup, Call, nowsec(now()), Agent}).

%% @doc Notify cdr handler that `#call{} Call' has had a wrapup ended by `string() Agent'.
-spec(endwrapup/2 :: (Call :: #call{}, Agent :: string()) -> 'ok').
endwrapup(Call, Agent) ->
	catch gen_event:notify(cdr, {endwrapup, Call, nowsec(now()), Agent}).

%% @doc Notify cdr handler that `#call{} Call' is to be transfered to `string() Transferto'.
-spec(transfer/2 :: (Call :: #call{}, Transferto :: string()) -> 'ok').
transfer(Call, Transferto) ->
	catch gen_event:notify(cdr, {transfer, Call, nowsec(now()), Transferto}).

%% @doc Notify cdr handler that `#call{} Call' is being offered by `string() Offerer'
%% to `string() Recipient'.
-spec(agent_transfer/2 :: (Call :: #call{}, {Offerer :: string(), Recipient :: string()}) -> 'ok').
agent_transfer(Call, {Offerer, Recipient}) when is_pid(Offerer) ->
	agent_transfer(Call, {agent_manager:find_by_pid(Offerer), Recipient});
agent_transfer(Call, {Offerer, Recipient}) when is_pid(Recipient) ->
	agent_transfer(Call, {Offerer, agent_manager:find_by_pid(Recipient)});
agent_transfer(Call, {Offerer, Recipient}) ->
	catch gen_event:notify(cdr, {agent_transfer, Call, nowsec(now()), {Offerer, Recipient}}).
	
%% @doc Return the completed and partial transactions for `#call{} Call'.
-spec(status/1 :: (Call :: #call{}) -> {[tuple()], [tuple()]}).
status(Call) ->
	gen_event:call(cdr, {?MODULE, Call#call.id}, status).

%% @doc Pulls the transactions from the cdr_raw (recovery) database.
-spec(recover/1 :: (Call :: #call{}) -> 'ok').
recover(Call) ->
	catch gen_event:notify(cdr, {recover, Call}).

%% Gen event callbacks

%% @private
init([Call]) ->
	?NOTICE("Starting new CDR handler for ~s", [Call#call.id]),
	Cdrrec = #cdr_rec{id = Call#call.id},
	mnesia:transaction(fun() -> mnesia:write(Cdrrec) end),
	{ok, #state{id=Call#call.id}}.

%% @private
handle_event({inqueue, #call{id = CallID}, Time, Queuename}, #state{id = CallID} = State) ->
	?NOTICE("~s has joined queue ~s", [CallID, Queuename]),
	Unterminated = [{inqueue, Time, Queuename} | State#state.unterminated],
	push_raw(CallID, {inqueue, Time, Queuename}),
	{ok, State#state{unterminated = Unterminated}};
handle_event({ringing, #call{id = CallID}, Time, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is ringing to ~s", [CallID, Agent]),
	push_raw(CallID, {ringing, Time, Agent}),
	case find_initiator_limbo({ringing, Time, Agent}, State#state.unterminated, State#state.limbo) of
		{_Limboevent, _Limbotime, _Limbodata} = Limbo ->
			{ok, State#state{limbo = Limbo}};
		{{Event, Oldtime, Data}, Midunterminated} ->
			Newuntermed = [{ringing, Time, Agent} | Midunterminated],
			Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
			{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}};
		{[{Event1, Otime1, Data1}, {Event2, Otime2, Data2}], Midunterminated} ->
			{_Levent, Ltime, _Ldata} = State#state.limbo,
			Unlimboed = [
				{Event2, Otime2, Ltime, Ltime - Otime2, Data2},
				{Event1, Otime1, Otime2, Otime2 - Otime1, Data1}
			],
			Newtrans = lists:append(Unlimboed, State#state.transactions),
			{ok, State#state{unterminated = [State#state.limbo | Midunterminated], limbo = undefined, transactions = Newtrans}}
%	
%	
%	try find_initiator({ringing, Time, Agent}, State#state.unterminated) of
%		{{Event, Oldtime, Data}, Midunterminated} ->
%			Newuntermed = [{ringing, Time, Agent} | Midunterminated],
%			Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
%			case State#state.limbo of
%				undefined ->
%					{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}};
%				{Levent, Ltime, Ldata} = Limbo ->
%					try find_initiator(Limbo, Newuntermed) of
%						{{Limboevent, Limbotime, Limbodata}, Limboeduntermed} ->
%							
%	catch
%		error:split_check_fail ->
%			case State#state.limbo of
%				undefined ->
%					{ok, State#state{limbo = {ringing, Time, Agent}};
%				_Else ->
%					erlang:error(split_check_fail)
%			end
	end;
handle_event({oncall, #call{id = CallID}, Time, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is on call with ~s.", [Agent, CallID]),
	push_raw(CallID, {oncall, Time, Agent}),
	try find_initiator({oncall, Time, Agent}, State#state.unterminated) of
		{{Event, Oldtime, Data}, Midunterminated} ->
			Newuntermed = [{oncall, Time, Agent} | Midunterminated],
			Newtrans = case Event of
				transfer ->
					State#state.transactions;
				_Else ->
					[{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions]
			end,
			{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}}
	catch
		error:split_check_fail ->
			case State#state.limbo of
				undefined ->
					{ok, State#state{limbo = {oncall, Time, Agent}}};
				_Else ->
					erlang:error(split_check_fail)
			end
	end;
handle_event({hangup, _Callrec, _Time, _By}, #state{hangup = true} = State) ->
	% already got a hangup, so we don't care.
	{ok, State};
handle_event({hangup, #call{id = CallID}, Time, agent}, #state{id = CallID} = State) ->
	?NOTICE("hangup for ~s", [CallID]),
	push_raw(CallID, {hangup, Time, agent}),
	Newtrans = [{hangup, Time, Time, 0, agent} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, hangup = true}};
handle_event({hangup, #call{id = CallID}, Time, By}, #state{id = CallID} = State) ->
	?NOTICE("Hangup for ~s", [CallID]),
	push_raw(CallID, {hangup, Time, By}),
	Newtrans = [{hangup, Time, Time, 0, By} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, hangup = true}};
handle_event({wrapup, #call{id = CallID}, Time,  Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s wrapup for ~s", [Agent, CallID]),
	push_raw(CallID, {wrapup, Time, Agent}),
	{{Event, Oldtime, Data}, Midunterminated} = find_initiator({wrapup, Time, Agent}, State#state.unterminated),
	Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
	Newuntermed = [{wrapup, Time, Agent} | Midunterminated],
	{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}};
handle_event({endwrapup, #call{id = CallID}, Time, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s ended wrapup for ~s", [Agent, CallID]),
	{{Event, Oldtime, Data}, Midunterminated} = find_initiator({endwrapup, Time, Agent}, State#state.unterminated),
	push_raw(CallID, {endwrapup, Time, Agent}),
	Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
	case {State#state.hangup, Midunterminated} of
		{true, []} ->
			Summary = summarize(Newtrans),
			F = fun() ->
				mnesia:delete({cdr_rec, CallID}),
				mnesia:write(#cdr_rec{
					id = CallID,
					summary = Summary,
					transactions = Newtrans
				})
			end,
			mnesia:transaction(F),
			?DEBUG("Summarize complete, now to remove the handler...", []),
			remove_handler;
		_Else ->
			{ok, State#state{unterminated = Midunterminated, transactions = Newtrans}}
	end;
handle_event({transfer, #call{id = CallID}, Time, Transferto}, #state{id = CallID} = State) ->
	?NOTICE("~s has gotten a transfer of ~s", [Transferto, CallID]),
	push_raw(CallID, {transfer, Time, Transferto}),
	Newtrans = [{transfer, Time, Time, 0, Transferto} | State#state.transactions],
	{ok, State#state{transactions = Newtrans}};
handle_event({recover, #call{id = CallID}}, #state{id = CallID} = State) ->
	?NOTICE("Doing a recovery for ~s", [CallID]),
	{Unterminated, Termed, Hangup} = load_recover(CallID),
	{ok, State#state{unterminated = Unterminated, transactions = Termed, hangup = Hangup}};
handle_event({agent_transfer, #call{id = CallID}, Time, {Offerer, Recipient}}, #state{id = CallID} = State) ->
	push_raw(CallID, {agent_transfer, Time, {Offerer, Recipient}}),
	push_raw(CallID, {transfer, Time, Recipient}),
	{{Event, Oldtime, Data}, Midunterminated} = find_initiator({agent_transfer, Time, {Offerer, Recipient}}, State#state.unterminated),
	Miduntermed = [{agent_transfer, Time, {Offerer, Recipient}} | Midunterminated],
	Newuntermed = [{transfer, Time, Recipient} | Miduntermed],
	Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}};
handle_event(_Event, State) ->
	{ok, State}.

%% @private
handle_call(status, State) ->
	{ok, {State#state.transactions, State#state.unterminated}, State};
handle_call(_Request, State) ->
	{ok, ok, State}.

%% @private
handle_info(_Info, State) ->
	{ok, State}.

%% @private
terminate(Args, _State) ->
	?NOTICE("terminating with args ~p", [Args]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%% @doc Using `fun() Fun' as the search function, extract the first item that 
%% causes `Fun' to return false from `[any()] List'.  Returns `{Found, Remaininglist}'.
-spec(check_split/2 :: (Fun :: fun(), List :: [tuple()]) -> {tuple(), [tuple()]}).
check_split(Fun, List) ->
	?DEBUG("checking split with list ~p", [List]),
	case lists:splitwith(Fun, List) of
		{List, []} ->
			?ERROR("Split check erred on list ~p", [List]),
			erlang:error(split_check_fail);
		{Head, [Tuple | Tail]} ->
			?DEBUG("Head:  ~p;  Tuple:  ~p;  Tail:  ~p", [Head, Tuple, Tail]),
			{Tuple, lists:append(Head, Tail)}
	end.

%% When returning 2 events to be transactionsized, the older one is first
find_initiator_limbo(Rawtrans, Unterminated, undefined) ->
	try find_initiator(Rawtrans, Unterminated)
	catch
		error:split_check_fail ->
			Rawtrans
	end;
find_initiator_limbo(Rawtrans, Unterminated, Limbo) ->
	?INFO("Attempting to reconcile limbo'ed event ~p", [Limbo]),
	{{_Event, _Time, _Data} = Event1, Newunterminated} = N = find_initiator(Rawtrans, Unterminated),
	?DEBUG("N:  ~p", [N]),
	try find_initiator(Limbo, [Rawtrans | Newunterminated]) of
		{Event2, Moreuntermed} ->
			{[Event1, Event2], Moreuntermed}
	catch
		error:split_check_fail ->
			?DEBUG("Caught fall through", []),
			{Event1, Newunterminated}
	end.
	
%% @doc Find the most recent transaction that can be used as an opening pair
%% for the passed event.
-spec(find_initiator/2 :: ({Event :: atom(), Time :: integer(), Datalist :: any()}, Unterminated :: [tuple()]) -> {tuple(), [tuple()]}).
find_initiator({agent_transfer, _Time, {Offerer, _Recipient}} = H, Unterminated) ->
	?DEBUG("~p", [H]),
	F = fun(I) ->
		case I of
			{oncall, _Oldtime, Offerer} ->
				false;
			_Else ->
				true
		end
	end,
	check_split(F, Unterminated);
find_initiator({ringing, _Time, _Datalist} = H, Unterminated) ->
	?DEBUG("~p", [H]),
	F = fun(I) ->
		case I of
			{ringing, _Oldtime, _Data} ->
				false;
			{inqueue, _Oldtime, _Data} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated);
find_initiator({oncall, _Time, Agent} = H, Unterminated) ->
	?DEBUG("~p", [H]),
	F = fun(I) ->
		case I of
			{ringing, _Oldtime, Agent} ->
				false;
			{transfer, _Oldtime, Agent} ->
				false;
			{agent_transfer, _Oldtime, {Agent, _Whoever}} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated);
find_initiator({wrapup, _Time, Agent} = H, Unterminated) ->
	?DEBUG("~p", [H]),
	F = fun(I) ->
		case I of
			{oncall, _Oldtime, Agent} ->
				false;
			{agent_transfer, _Oldtime, {Agent, _Whoever}} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated);
find_initiator({endwrapup, _Time, Agent} = H, Unterminated) ->
	?DEBUG("~p", [H]),
	F = fun(I) ->
		case I of
			{wrapup, _Oldtime, Agent} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated).

build_tables() ->
	util:build_table(cdr_rec, [
		{attributes, record_info(fields, cdr_rec)},
		{disc_copies, [node() | nodes()]}
	]),
	util:build_table(cdr_raw, [
		{attributes, record_info(fields, cdr_raw)},
		{disc_copies, [node() | nodes()]},
		{type, bag}
	]),
	ok.
	
%% Need to keep in mind that not all agents are to be billed the same,
%% so there does need to be some form of pair checking.
% pair checking is done as the transactions are built up.
% All this needs to do is sum up the data.
%% @doc Given a list of `[tuple()] Transactions' summarize how long the call was
%% in each state, and a break down of each agent involved.
-spec(summarize/1 :: (Transactions :: [tuple()]) -> [tuple()]).
summarize(Transactions) ->
	?DEBUG("Summarizing ~p", [Transactions]),
	Acc = dict:from_list([
		{total, {0, 0, 0, 0}}
	]),
	Count = fun({Event, _State, _End, Duration, Data}, Dict) ->
		{ok, {Inqueue, Ringing, Oncall, Wrapup}} = dict:find(total, Dict),
		case Event of
			inqueue ->
				dict:store(total, {Inqueue + Duration, Ringing, Oncall, Wrapup}, Dict);
			ringing ->
				{_Aqueue, Aring, Aoncall, Awrapup} = case dict:find(Data, Dict) of
					error ->
						{0, 0, 0, 0};
					{ok, Tuple} when is_tuple(Tuple) ->
						Tuple
				end,
				Dict2 = dict:store(total, {Inqueue, Ringing + Duration, Oncall, Wrapup}, Dict),
				dict:store(Data, {0, Aring + Duration, Aoncall, Awrapup}, Dict2);
			oncall ->
				{_Aqueue, Aring, Aoncall, Awrapup} = case dict:find(Data, Dict) of
					error ->
						{0, 0, 0, 0};
					{ok, Tuple} when is_tuple(Tuple) ->
						Tuple
				end,
				Dict2 = dict:store(total, {Inqueue, Ringing, Oncall + Duration, Wrapup}, Dict),
				dict:store(Data, {0, Aring, Aoncall + Duration, Awrapup}, Dict2);
			wrapup ->
				{_Aqueue, Aring, Aoncall, Awrapup} = case dict:find(Data, Dict) of
					error ->
						{0, 0, 0, 0};
					{ok, Tuple} when is_tuple(Tuple) ->
						Tuple
				end,
				Dict2 = dict:store(total, {Inqueue, Ringing, Oncall, Wrapup + Duration}, Dict),
				dict:store(Data, {0, Aring, Aoncall, Awrapup + Duration}, Dict2);
			_Other ->
				?DEBUG("Can't summarize ~s", [Event]),
				Dict
		end
	end,
	SummaryDict = lists:foldl(Count, Acc, Transactions),
	dict:to_list(SummaryDict).

load_recover(Callid) ->
	?DEBUG("Starting recovery for ~s", [Callid]),
	F = fun() ->
		QH = qlc:q([X#cdr_raw.transaction || X <- mnesia:table(cdr_raw), X#cdr_raw.id =:= Callid]),
		qlc:e(QH)
	end,
	{atomic, Transactions} = mnesia:transaction(F),
	Sort = fun({_Atrans, Atime, _Adata}, {_Btrans, Btime, _Bdata}) ->
		Btime >= Atime
	end,
	Sorted = lists:sort(Sort, Transactions),
	?DEBUG("Recovery list:  ~p", [Sorted]),
	recover(Sorted, [], [], false).

recover([], Untermed, Termed, Hangup) ->
	{Untermed, Termed, Hangup};
recover([{inqueue, _Time, _Details} = Head | Tail], Untermed, Termed, Hangup) ->
	recover(Tail, [Head | Untermed], Termed, Hangup);
recover([{hangup, Time, Details} = _Head | Tail], Untermed, Termed, _Hangup) ->
	recover(Tail, Untermed, [{hangup, Time, Time, 0, Details} | Termed], true);
recover([{transfer, Time, Details} = _Head | Tail], Untermed, Termed, Hangup) ->
	recover(Tail, [{transfer, Time, Details} | Untermed], Termed, Hangup);
recover([{Event, Time, _Details} = Head | Tail], Untermed, Termed, Hangup) ->
	{{Priorevent, Oldtime, Olddetails}, Miduntermed} = find_initiator(Head, Untermed),
	Newtermed = [{Priorevent, Oldtime, Time, Time - Oldtime, Olddetails} | Termed],
	Newuntermed = case Event of
		endwrapup ->
			Miduntermed;
		_Else ->
			[Head | Miduntermed]
	end,
	recover(Tail, Newuntermed, Newtermed, Hangup).

%% @doc Given the list of nodes, merge the cdrs.  Since this can take a long
%% time, it can help to just spawn a new process for it.
-spec(merge/3 :: (Nodes :: [atom()], Time :: pos_integer(), Replyto :: pid()) -> 'ok').
merge(Nodes, Time, Replyto) ->
	Raws = get_raws(Nodes, Time),
	Touchedcalls = get_ids(Raws),
	Sums = get_summaries(Nodes, Touchedcalls),
	Mergedraws = merge_raw(Raws),
	MergedSums = merge_sum(Sums),
	Replyto ! {merge_complete, ?MODULE, lists:append([Mergedraws, MergedSums])}.
	
%% @private
get_raws(Nodes, Time) ->
	get_raws(Nodes, Time, []).

%% @private
get_raws([], _Time, Acc) ->
	Acc;
get_raws([Node | Tail], Time, Acc) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cdr_raw), element(2, X#cdr_raw.transaction) =< Time]),
		qlc:e(QH)
	end,
	Out = rpc:call(Node, mnesia, transaction, [F]),
	get_raws(Tail, Time, [Out | Acc]).

%% @private
get_ids(Raws) ->
	get_ids(Raws, []).

%% @private
get_ids([], Acc) ->
	Acc;
get_ids([{atomic, Raws} | Tail], Acc) ->
	Newacc = get_ids_sub(Raws, Acc),
	get_ids(Tail, Newacc).

get_ids_sub([], Acc) ->
	Acc;
get_ids_sub([#cdr_raw{id = ID} | Tail], Acc) ->
	case lists:member(ID, Acc) of
		true ->
			get_ids_sub(Tail, Acc);
		false ->
			get_ids_sub(Tail, [ID | Acc])
	end.

%% @private
get_summaries(Nodes, Ids) ->
	get_summaries(Nodes, Ids, []).

%% @private
get_summaries([], _Ids, Acc) ->
	Acc;
get_summaries([Node | Nodes], Ids, Acc) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cdr_rec), lists:member(X#cdr_rec.id, Ids)]),
		qlc:e(QH)
	end,
	case rpc:call(Node, mnesia, transaction, [F]) of
		{atomic, _Rows} = Rez ->
			get_summaries(Nodes, Ids, [Rez | Acc]);
		Else ->
			?WARNING("Could not get cdr_rec from ~w", [Node]),
			get_summaries(Nodes, Ids, Acc)
	end.

%% @private
merge_raw(Recs) ->
	merge_raw(Recs, []).

merge_raw([], Acc) ->
	Acc;
merge_raw([{atomic, Raws} | Tail], Acc) ->
	Newacc = lists:append([Raws, Acc]),
	merge_raw(Tail, Newacc).

merge_sum(Sums) ->
	merge_sum(Sums, []).

merge_sum([], Acc) ->
	Acc;
merge_sum([{atomic, Sums} | Tail], Acc) ->
	Newacc = diff_sum(Sums, Acc),
	merge_sum(Tail, Newacc).

diff_sum(Left, Right) ->
	Sort = fun(A, B) ->
		A#cdr_rec.id < B#cdr_rec.id
	end,
	Sleft = lists:sort(Sort, Left),
	Sright = lists:sort(Sort, Right),
	diff_sum(Sleft, Sright, []).

diff_sum([], Right, Acc) ->
	lists:append([Right, Acc]);
diff_sum(Left, [], Acc) ->
	lists:append([Left, Acc]);
diff_sum([#cdr_rec{summary = inprogress, id = Id} = Hleft | Tleft], [#cdr_rec{summary = inprogress, id = Id} = _Hright | Tright], Acc) ->
	diff_sum(Tleft, Tright, [Hleft | Acc]);
diff_sum([#cdr_rec{summary = inprogress, id = Id} | Tleft], [#cdr_rec{id = Id} = Hright | Tright], Acc) ->
	diff_sum(Tleft, Tright, [Hright | Acc]);
diff_sum([#cdr_rec{id = Id} = Hleft | Tleft], [#cdr_rec{id = Id} = Hright | Tright], Acc) ->
	diff_sum(Tleft, Tright, [Hleft | Acc]);
diff_sum([Hleft | Tleft] = Left, [Hright | Tright] = Right, Acc) ->
	case Hleft#cdr_rec.id < Hright#cdr_rec.id of
		true ->
			diff_sum(Tleft, Right, [Hleft | Acc]);
		false ->
			diff_sum(Left, Tright, [Hright | Acc])
	end.

%% @private Get a standarized unix epoch integer from `now()'.
-spec(nowsec/1 :: ({Mega :: non_neg_integer(), Sec :: non_neg_integer(), Micro :: non_neg_integer()}) -> non_neg_integer()).
nowsec({Mega, Sec, _Micro}) ->
	(Mega * 1000000) + Sec.

%% @private Push the raw transaction into the cdr_raw table.
-spec(push_raw/2 :: (Callid :: string(), Trans :: tuple()) -> {'atomic', 'ok'}).
push_raw(Callid, Trans) ->
	mnesia:transaction(fun() -> mnesia:write(#cdr_raw{id = Callid, transaction = Trans}) end).


-ifdef(EUNIT).

check_split_test_() ->
	[{"List ends up all on the left",
	fun() ->
		F = fun(_I) ->
			true
		end,
		List = [{a}, {b}, {c}, {d}],
		?assertError(split_check_fail, check_split(F, List))
	end},
	{"List ends up all on the right",
	fun() ->
		F = fun(_I) ->
			false
		end,
		List = [{a}, {b}, {c}, {d}],
		?assertEqual({{a}, [{b}, {c}, {d}]}, check_split(F, List))
	end},
	{"Get the correct split from a b b b c",
	fun() ->
		F = fun(I) ->
			case I of
				{b} ->
					false;
				_Else ->
					true
			end
		end,
		List = [{a}, {b}, {b}, {b}, {c}],
		?assertEqual({{b}, [{a}, {b}, {b}, {c}]}, check_split(F, List))
	end}].

find_initiator_test_() ->
	[{"ringing with only inqueue before it",
	fun() ->
		Unterminated = [{inqueue, 10, "queuename"}],
		?assertEqual({{inqueue, 10, "queuename"}, []}, find_initiator({ringing, 15, "agent"}, Unterminated))
	end},
	{"ringing with only ringing before it",
	fun() ->
		Unterminated = [{ringing, 10, "agent1"}],
		?assertEqual({{ringing, 10, "agent1"}, []}, find_initiator({ringing, 15, "agent2"}, Unterminated))
	end},
	{"oncall with a ringing before it",
	fun() ->
		Unterminated = [{ringing, 10, "agent"}],
		?assertEqual({{ringing, 10, "agent"}, []}, find_initiator({oncall, 15, "agent"}, Unterminated))
	end},
	{"oncall with a transfer before it",
	fun() ->
		Unterminated = [{transfer, 10, "newagent"}],
		?assertEqual({{transfer, 10, "newagent"}, []}, find_initiator({oncall, 10, "newagent"}, Unterminated))
	end},
	{"wraupup with an oncall before it",
	fun() ->
		Unterminated = [{oncall, 10, "agent"}],
		?assertEqual({{oncall, 10, "agent"}, []}, find_initiator({wrapup, 15, "agent"}, Unterminated))
	end},
	{"endwrapup with a wrapup before it",
	fun() ->
		Unterminated = [{wrapup, 10, "agent"}],
		?assertEqual({{wrapup, 10, "agent"}, []}, find_initiator({endwrapup, 15, "agent"}, Unterminated))
	end},
	{"endwrapup with two wrapups before it",
	fun() ->
		Unterminated = [{wrapup, 10, "agent1"}, {wrapup, 5, "agent2"}],
		Res = find_initiator({endwrapup, 15, "agent1"}, Unterminated),
		?CONSOLE("res:  ~p", [Res]),
		?assertEqual({{wrapup, 10, "agent1"}, [{wrapup, 5, "agent2"}]}, Res)
	end},
	{"endwrapup with wrapup in the middle of the list",
	fun() ->
		Unterminated = [{oncall, 10, "ignore"}, {wrapup, 10, "catch"}, {wrapup, 5, "alsoignore"}],
		Res = find_initiator({endwrapup, 15, "catch"}, Unterminated),
		?CONSOLE("res:  ~p", [Res]),
		?assertEqual({{wrapup, 10, "catch"}, [{oncall, 10, "ignore"}, {wrapup, 5, "alsoignore"}]}, Res)
	end},
	{"agent_transfer ends an oncall",
	fun() ->
		Unterminated = [{oncall, 10, "offerer"}],
		Res = find_initiator({agent_transfer, 15, {"offerer", "recipient"}}, Unterminated),
		?CONSOLE("res:  ~p", [Res]),
		?assertEqual({{oncall, 10, "offerer"}, []}, Res)
	end},
	{"oncall of the offerer ends an agent_transfer.",
	fun() ->
		Unterminated = [{agent_transfer, 10, {"offerer", "recipient"}}],
		Res = find_initiator({oncall, 15, "offerer"}, Unterminated),
		?CONSOLE("res:  ~p", [Res]),
		?assertEqual({{agent_transfer, 10, {"offerer", "recipient"}}, []}, Res)
	end},
	{"wrapup of the offerer ends an agent_transfer.",
	fun() ->
		Unterminated = [{agent_transfer, 10, {"offerer", "recipient"}}],
		Res = find_initiator({wrapup, 15, "offerer"}, Unterminated),
		?CONSOLE("res:  ~p", [Res]),
		?assertEqual({{agent_transfer, 10, {"offerer", "recipient"}}, []}, Res)
	end},
	{"oncall of the recipient does not terminate the agent_transfer",
	fun() ->
		Unterminated = [{agent_transfer, 10, {"offerer", "recipient"}}],
		?assertError(split_check_fail, find_initiator({oncall, 15, "recipient"}, Unterminated))
	end}].

find_initiator_limbo_test_() ->
	[{"No limbo to test, and no errors occur",
	fun() ->
		Unterminated = [{ringing, 5, "agent"}],
		Res = find_initiator_limbo({oncall, 10, "agent"}, Unterminated, undefined),
		?assertEqual({{ringing, 5, "agent"}, []}, Res)
	end},
	{"No limbo to test, but ends up with one",
	fun() ->
		Unterminated = [{inqueue, 5, "testqueue"}],
		Res = find_initiator_limbo({oncall, 15, "agent"}, Unterminated, undefined),
		?assertEqual({oncall, 15, "agent"}, Res)
	end},
	{"A limbo to test, and it's cleared",
	fun() ->
		Unterminated = [{inqueue, 5, "testqueue"}],
		Limbo = {oncall, 15, "agent"},
		Event = {ringing, 10, "agent"},
		Res = find_initiator_limbo(Event, Unterminated, Limbo),
		?assertEqual({[{inqueue, 5, "testqueue"}, {ringing, 10, "agent"}], []}, Res)
	end},
	{"A limbo to test, but it still doesn't match",
	fun() ->
		Unterminated = [{inqueue, 5, "testqueue"}],
		Limbo = {oncall, 15, "agent"},
		Event = {ringing, 10, "garbage"},
		Res = find_initiator_limbo(Event, Unterminated, Limbo),
		?assertEqual({{inqueue, 5, "testqueue"}, []}, Res)
	end},
	{"A limbo to test, but end up with a second limbo",
	fun() ->
		Unterminated = [{inqueue, 5, "testqueue"}],
		Limbo = {oncall, 15, "agent"},
		Event = {wrapup, 20, "agent"},
		?assertError(split_check_fail, find_initiator_limbo(Event, Unterminated, Limbo))
	end}].
	
handle_event_test_() ->
	{foreach,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		build_tables(),
		Pull = fun() ->
			mnesia:transaction(fun() -> mnesia:read(cdr_raw, "testcall") end)
		end,
		{#call{id = "testcall", source = self()}, Pull}
	end,
	fun(_Whatever) ->
		ok
	end,
	[fun({Call, Pull}) ->
		{"handle_event inqueue",
		fun() ->
			State = #state{id = "testcall"},
			{ok, Newstate} = handle_event({inqueue, Call, 10, "testqueue"}, State),
			?assertEqual([], Newstate#state.transactions),
			?assertEqual([{inqueue, 10, "testqueue"}], Newstate#state.unterminated),
			{atomic, [Trans]} = Pull(),
			?assertEqual({inqueue, 10, "testqueue"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"ringing",
		fun() ->
			Unterminated = [{inqueue, 10, "testqueue"}],
			State = #state{unterminated = Unterminated, id = "testcall"},
			{ok, Newstate} = handle_event({ringing, Call, 15, "agent"}, State),
			?assertEqual([{inqueue, 10, 15, 5, "testqueue"}], Newstate#state.transactions),
			?assertEqual([{ringing, 15, "agent"}], Newstate#state.unterminated),
			{atomic, [Trans]} = Pull(),
			?assertEqual({ringing, 15, "agent"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"oncall",
		fun() ->
			Unterminated = [{ringing, 10, "agent"}],
			State = #state{unterminated = Unterminated, id = "testcall"},
			{ok, Newstate} = handle_event({oncall, Call, 15, "agent"}, State),
			?assertEqual([{ringing, 10, 15, 5, "agent"}], Newstate#state.transactions),
			?assertEqual([{oncall, 15, "agent"}], Newstate#state.unterminated),
			{atomic, [Trans]} = Pull(),
			?assertEqual({oncall, 15, "agent"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"wrapup",
		fun() ->
			Unterminated = [{oncall, 10, "agent"}],
			State = #state{unterminated = Unterminated, id="testcall"},
			{ok, Newstate} = handle_event({wrapup, Call, 15, "agent"}, State),
			?assertEqual([{oncall, 10, 15, 5, "agent"}], Newstate#state.transactions),
			?assertEqual([{wrapup, 15, "agent"}], Newstate#state.unterminated),
			{atomic, [Trans]} = Pull(),
			?assertEqual({wrapup, 15, "agent"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"endwrapup",
		fun() ->
			Unterminated = [{wrapup, 10, "agent"}],
			State = #state{unterminated = Unterminated, id="testcall"},
			{ok, Newstate} = handle_event({endwrapup, Call, 15, "agent"}, State),
			?assertEqual([{wrapup, 10, 15, 5, "agent"}], Newstate#state.transactions),
			?assertEqual([], Newstate#state.unterminated),
			{atomic, [Trans]} = Pull(),
			?assertEqual({endwrapup, 15, "agent"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"hangup from agent",
		fun() ->
			State = #state{id = "testcall"},
			{ok, Newstate} = handle_event({hangup, Call, 10, agent}, State),
			?assertEqual([{hangup, 10, 10, 0, agent}], Newstate#state.transactions),
			?assertEqual([], Newstate#state.unterminated),
			?assert(Newstate#state.hangup),
			{atomic, [Trans]} = Pull(),
			?assertEqual({hangup, 10, agent}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"hangup from caller",
		fun() ->
			State = #state{id = "testcall"},
			{ok, Newstate} = handle_event({hangup, Call, 10, "caller"}, State),
			?assertEqual([{hangup, 10, 10, 0, "caller"}], Newstate#state.transactions),
			?assertEqual([], Newstate#state.unterminated),
			?assert(Newstate#state.hangup),
			{atomic, [Trans]} = Pull(),
			?assertEqual({hangup, 10, "caller"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"hangup from caller when a hangup is already received",
		fun() ->
			Protostate = #state{id = "testcall"},
			{ok, State} = handle_event({hangup, Call, 10, agent}, Protostate),
			{ok, Newstate} = handle_event({hangup, Call, 10, "notagent"}, State),
			?assert(Newstate#state.hangup),
			?assertEqual(State#state.transactions, Newstate#state.transactions),
			{atomic, [H | _Tail] = Trans} = Pull(),
			?assertEqual(1, length(Trans)),
			?assertEqual({hangup, 10, agent}, H#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"hangup from agent when hangup from caller is already recieved",
		fun() ->
			Protostate = #state{id = "testcall"},
			{ok, State} = handle_event({hangup, Call, 10, "notagent"}, Protostate),
			{ok, Newstate} = handle_event({hangup, Call, 10, agent}, State),
			?assert(Newstate#state.hangup),
			?assertEqual(State#state.transactions, Newstate#state.transactions),
			{atomic, [H | _Tail] = Trans} = Pull(),
			?assertEqual(1, length(Trans)),
			?assertEqual({hangup, 10, "notagent"}, H#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"transfer event",
		fun() ->
			State = #state{id = "testcall"},
			{ok, Newstate} = handle_event({transfer, Call, 10, "target"}, State),
			?assertEqual([], Newstate#state.unterminated),
			?assertEqual([{transfer, 10, 10, 0, "target"}], Newstate#state.transactions),
			{atomic, [Trans]} = Pull(),
			?assertEqual({transfer, 10, "target"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"endwrapup when a hangup has already been recieved",
		fun() ->
			State = #state{id = "testcall", hangup = true, unterminated = [{wrapup, 10, "agent"}]},
			?assertEqual(remove_handler, handle_event({endwrapup, Call, 15, "agent"}, State)),
			{atomic, [Trans]} = Pull(),
			?assertEqual({endwrapup, 15, "agent"}, Trans#cdr_raw.transaction)
		end}
	end,
	fun({Call, Pull}) ->
		{"handling an event for a differnt call id (ie, not handling it)",
		fun() ->
			State = #state{id = "differs"},
			{ok, Newstate} = handle_event({inqueue, Call, 15, "queue"}, State),
			?assertEqual(State, Newstate),
			?assertEqual({atomic, []}, Pull())
		end}
	end,
	fun({Call, _Pull}) ->
		{"Recovery!",
		fun() ->
			State = #state{id = "testcall"},
			Transactions = [
				{inqueue, 5, "testqueue"},
				{ringing, 10, "agent1"},
				{ringing, 15, "agent2"},
				{oncall, 20, "agent2"},
				{transfer, 25, "agent3"},
				{oncall, 25, "agent3"},
				{wrapup, 25, "agent2"},
				{endwrapup, 30, "agent2"},
				{hangup, 35, agent},
				{wrapup, 35, "agent3"}
			],
			F = fun() ->
				Foreach = fun(I) ->
					mnesia:write(#cdr_raw{id = Call#call.id, transaction = I})
				end,
				lists:foreach(Foreach, Transactions)
			end,
			mnesia:transaction(F),
			{ok, Newstate} = handle_event({recover, Call}, State),
			Expectedstate = #state{
				id = "testcall",
				hangup = true,
				transactions = [
					{oncall, 25, 35, 10, "agent3"},
					{hangup, 35, 35, 0, agent},
					{wrapup, 25, 30, 5, "agent2"},
					{oncall, 20, 25, 5, "agent2"},
					{transfer, 25, 25, 0, "agent3"},
					{ringing, 15, 20, 5, "agent2"},
					{ringing, 10, 15, 5, "agent1"},
					{inqueue, 5, 10, 5, "testqueue"}
				],
				unterminated = [{wrapup, 35, "agent3"}]
			},
			?DEBUG("Expected:  ~p;  Recieved:  ~p", [Expectedstate, Newstate]),
			?assertEqual(Expectedstate, Newstate)
		end}
	end,
	fun({Call, Pull}) ->
		{"On call comes in before ringing",
		fun() ->
			State = #state{
				id = "testcall",
				unterminated = [{inqueue, 5, "testqueue"}]
			},
			{ok, Newstate} = handle_event({oncall, Call, 15, "agent"}, State),
			{atomic, [Trans]} = Pull(),
			?assertEqual(#cdr_raw{id = "testcall", transaction = {oncall, 15, "agent"}}, Trans),
			?assertEqual({oncall, 15, "agent"}, Newstate#state.limbo),
			?assertEqual([{inqueue, 5, "testqueue"}], Newstate#state.unterminated),
			?assertEqual([], Newstate#state.transactions)
		end}
	end,
	fun({Call, Pull}) ->
		{"Ringing comes in while an oncall is in limbo",
		fun() ->
			State = #state{
				id = "testcall",
				unterminated = [{inqueue, 55, "testqueue"}],
				limbo = {oncall, 90, "agent"}
			},
			{ok, Newstate} = handle_event({ringing, Call, 66, "agent"}, State),
			{atomic, [Trans]} = Pull(),
			?assertEqual(#cdr_raw{id = "testcall", transaction = {ringing, 66, "agent"}}, Trans),
			?assertEqual(undefined, Newstate#state.limbo),
			?assertEqual([{oncall, 90, "agent"}], Newstate#state.unterminated),
			?assertEqual([{ringing, 66, 90, 24, "agent"}, {inqueue, 55, 66, 11, "testqueue"}], Newstate#state.transactions)
		end}
	end,
	fun({Call, Pull}) ->
		{"Ringing for a different agent comes in while oncall is in limbo",
		fun() ->
			State = #state{
				id = "testcall",
				unterminated = [{inqueue, 5, "testqueue"}],
				limbo = {oncall, 20, "agent"}
			},
			{ok, Newstate} = handle_event({ringing, Call, 10, "someagent"}, State),
			{atomic, [Trans]} = Pull(),
			?assertEqual(#cdr_raw{id = "testcall", transaction = {ringing, 10, "someagent"}}, Trans),
			?assertEqual({oncall, 20, "agent"}, Newstate#state.limbo),
			?assertEqual([{ringing, 10, "someagent"}], Newstate#state.unterminated),
			?assertEqual([{inqueue, 5, 10, 5, "testqueue"}], Newstate#state.transactions)  
		end}
	end,
	fun({Call, Pull}) ->
		{"agent_transfer ends successfully",
		fun() ->
			State = #state{
				id = "testcall",
				unterminated = [{oncall, 5, "offerer"}]
			},
			{ok, State2} = handle_event({agent_transfer, Call, 10, {"offerer", "recipient"}}, State),
			{ok, State3} = handle_event({wrapup, Call, 15, "offerer"}, State2),
			{ok, State4} = handle_event({oncall, Call, 15, "recipient"}, State3),
			{atomic, Trans} = Pull(),
			?DEBUG("~p", [Trans]),
			?assert(lists:member(#cdr_raw{id = "testcall", transaction = {transfer, 10, "recipient"}}, Trans)),
			?assert(lists:member(#cdr_raw{id = "testcall", transaction = {agent_transfer, 10, {"offerer", "recipient"}}}, Trans))
		end}
	end]}.
		
summarize_test_() ->
	[{"Simple summary, one call, one agent",
	fun() ->
		Transactions = [
			{inqueue, 5, 10, 5, "testqueue"},
			{ringing, 10, 13, 3, "agent"},
			{oncall, 13, 20, 7, "agent"},
			{wrapup, 20, 24, 4, "agent"}
		],
		Dict = summarize(Transactions),
		?assertEqual({5, 3, 7, 4}, proplists:get_value(total, Dict)),
		?assertEqual({0, 3, 7, 4}, proplists:get_value("agent", Dict))
	end},
	{"summary with a ringout",
	fun() ->
		Transactions = [
			{inqueue, 5, 10, 5, "testqueue"},
			{ringing, 10, 20, 10, "agent1"},
			{ringing, 20, 28, 8, "agent2"},
			{oncall, 28, 22, 5, "agent2"},
			{wrapup, 22, 23, 1, "agent2"}
		],
		Dict = summarize(Transactions),
		?assertEqual({5, 18, 5, 1}, proplists:get_value(total, Dict)),
		?assertEqual({0, 10, 0, 0}, proplists:get_value("agent1", Dict)),
		?assertEqual({0, 8, 5, 1}, proplists:get_value("agent2", Dict))
	end},
	{"summary with a transfer to another agent",
	fun() ->
		Transactions = [
			{inqueue, 5, 10, 5, "testqueue"},
			{ringing, 10, 20, 10, "agent1"},
			{oncall, 20, 30, 10, "agent1"},
			{transfer, 30, 30, 0, "agent2"},
			{oncall, 30, 35, 5, "agent2"},
			{wrapup, 30, 40, 10, "agent1"},
			{wrapup, 35, 40, 5, "agent2"}
		],
		Dict = summarize(Transactions),
		?assertEqual({5, 10, 15, 15}, proplists:get_value(total, Dict)),
		?assertEqual({0, 10, 10, 10}, proplists:get_value("agent1", Dict)),
		?assertEqual({0, 0, 5, 5}, proplists:get_value("agent2", Dict))
	end}].

mnesia_test_() ->
	{foreach,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		build_tables(),
		#call{id = "testcall", source = self()}
	end,
	fun(_Whatever) ->
		ok
	end,
	[fun(Call) ->
		{"Summary gets written to db",
		fun() ->
			StateTrans = lists:reverse([
				{inqueue, 5, 10, 5, "testqueue"},
				{ringing, 10, 13, 3, "agent"},
				{oncall, 13, 20, 7, "agent"},
				{hangup, 20, 20, 0, agent}
			]),
			ExpectedTransactions = [ {wrapup, 20, 24, 4, "agent"} | StateTrans ],
			Unterminated = [{wrapup, 20, "agent"}],
			State = #state{unterminated = Unterminated, id="testcall", transactions = StateTrans, hangup=true},
			remove_handler = handle_event({endwrapup, Call, 24, "agent"}, State),
			F = fun() ->
				mnesia:read(cdr_rec, "testcall")
			end,
			{atomic, [Cdrrec]} = mnesia:transaction(F),
			?assertEqual({5, 3, 7, 4}, proplists:get_value(total, Cdrrec#cdr_rec.summary)),
			?assertEqual({0, 3, 7, 4}, proplists:get_value("agent", Cdrrec#cdr_rec.summary)),
			?assertEqual(ExpectedTransactions, Cdrrec#cdr_rec.transactions)
		end}
	end,
	fun(Call) ->
		{"init creates an 'inprogress' summary",
		fun() ->
			init([Call]),
			F = fun() ->
				mnesia:read(cdr_rec, "testcall")
			end,
			{atomic, [Cdrrec]} = mnesia:transaction(F),
			?assertEqual(inprogress, Cdrrec#cdr_rec.summary),
			?assertEqual(inprogress, Cdrrec#cdr_rec.transactions)
		end}
	end]}.

recover_test_() ->
	[
	{"Recover a call that's only been queued",
	fun() ->
		Transactions = [{inqueue, 5, "testqueue"}],
		?assertEqual({[{inqueue, 5, "testqueue"}], [], false}, recover(Transactions, [], [], false))
	end},
	{"Recover a call that's ringing to an agent",
	fun() ->
		Transactions = [{inqueue, 5, "testqueue"}, {ringing, 10, "agent"}],
		Expected = {[{ringing, 10, "agent"}], [{inqueue, 5, 10, 5, "testqueue"}], false},
		?assertEqual(Expected, recover(Transactions, [], [], false))
	end},
	{"Recover a call that's ringing to two different agents",
	fun() ->
		Transactions = [{inqueue, 5, "testqueue"}, {ringing, 10, "agent1"}, {ringing, 15, "agent2"}],
		Expected = {[{ringing, 15, "agent2"}], [{ringing, 10, 15, 5, "agent1"}, {inqueue, 5, 10, 5, "testqueue"}], false},
		?assertEqual(Expected, recover(Transactions, [], [], false))
	end},
	{"Recover a call that's all but completed",
	fun() ->
		Transactions = [
			{inqueue, 5, "testqueue"},
			{ringing, 10, "agent1"},
			{ringing, 15, "agent2"},
			{oncall, 20, "agent2"},
			{transfer, 25, "agent3"},
			{oncall, 25, "agent3"},
			{wrapup, 25, "agent2"},
			{endwrapup, 30, "agent2"},
			{hangup, 35, agent},
			{wrapup, 35, "agent3"}
		],
		Expecteduntermed = [{wrapup, 35, "agent3"}],
		Expectedtermed = [
			{oncall, 25, 35, 10, "agent3"},
			{hangup, 35, 35, 0, agent},
			{wrapup, 25, 30, 5, "agent2"},
			{oncall, 20, 25, 5, "agent2"},
			{transfer, 25, 25, 0, "agent3"},
			{ringing, 15, 20, 5, "agent2"},
			{ringing, 10, 15, 5, "agent1"},
			{inqueue, 5, 10, 5, "testqueue"}
		],
		Expected = {Expecteduntermed, Expectedtermed, true},
		?DEBUG("recovery:  ~p", [recover(Transactions, [], [], false)]),
		?assertEqual(Expected, recover(Transactions, [], [], false))
	end}
	].

merge_test_() ->
	[{"Get ids",
	fun() ->
		Raws = [{atomic, [
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "e"}]},
			{atomic, [
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "d"},
			#cdr_raw{id = "d"}]},
			{atomic, [
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "e"}]}],
		Sort = fun(A, B) ->
			A < B
		end,
		Expected = ["a", "b", "c", "d", "e"],
		Res = lists:sort(Sort, get_ids(Raws)),
		?assertEqual(Expected, Res)
	end}].

-endif.

