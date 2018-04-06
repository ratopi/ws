%%%-------------------------------------------------------------------
%% @doc ws top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ws_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_child/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, ListenSocket} = gen_tcp:listen(
		18080,
		[
			% binary,
			{packet, line},
			{active, false}
		]
	),
	ws_log:info("listening..."),
	spawn_link(fun start_child/0),
	X = {
		ok,
		{
			{simple_one_for_one, 60, 3600},
			[
				{
					socket,
					{ws_serv, start_link, [ListenSocket]},
					temporary,
					1000,
					worker,
					[ws_serv]
				}
			]
		}
	},
%	ws_log:info("Sup ~p~n", [X]),
	X.

%%====================================================================
%% Internal functions
%%====================================================================

start_child() ->
	ws_log:info("start_child()"),
	R = supervisor:start_child(?SERVER, []),
	ws_log:info("R = ~p~n", [R]).
