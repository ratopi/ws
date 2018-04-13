%%%-------------------------------------------------------------------
%% @doc ws top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ws_serv_sup).

-include("ws_logger.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_a_child/2]).

% -define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link(?MODULE, []).

start_a_child(Sup, Id) ->
	{ok, Pid} = supervisor:start_child(Sup, [Id]),
	?LOG_DEBUG("New child: ~p ~p", [Id, Pid]),
	Pid.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, ListenSocket} = gen_tcp:listen(
		ws_config:get(port),
		[
			% binary,
			{packet, line},
			{active, false}
		]
	),
	?LOG_INFO("listening..."),
	{
		ok,
		{
			{simple_one_for_one, 60, 3600},
			[
				{
					socket,
					{ws_serv_worker, start_link, [ListenSocket]},
					temporary,
					1000,
					worker,
					[ws_serv_worker]
				}
			]
		}
	}.

%%====================================================================
%% Internal functions
%%====================================================================
