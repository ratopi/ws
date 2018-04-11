%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2018 22:11
%%%-------------------------------------------------------------------
-module(ws_sup).
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Sup, Id) ->
	Pid = get_child_pid(Sup, ws_serv_sup),
	% io:fwrite("PID ~p~n", [Pid]),
	ws_serv_sup:start_a_child(Id).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
		MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
		[ChildSpec :: supervisor:child_spec()]
	}} |
	ignore |
	{error, Reason :: term()}).
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	MyPid = self(),

	{
		ok,
		{
			SupFlags,
			[
				{ws_serv_ctrl, {ws_serv_ctrl, start_link, [MyPid]}, permanent, 2000, worker, [ws_serv_ctrl]},
				{ws_serv_sup, {ws_serv_sup, start_link, []}, permanent, 2000, supervisor, [ws_serv_sup]}
			]
		}
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_child_pid([], _ChildId) ->
	{error, not_found};

get_child_pid([{ChildId, Pid, _Type, _} | T], ChildId) ->
	{ok, Pid};

get_child_pid([_ | T], ChildId) ->
	get_child_pid(T, ChildId);

get_child_pid(Sup, ChildId) ->
	Childs = supervisor:which_children(Sup),
	% io:fwrite("Childs:~n~p~n", [Childs]),
	get_child_pid(Childs, ChildId).
