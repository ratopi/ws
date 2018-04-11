%%%-------------------------------------------------------------------
%% @doc ws top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ws_serv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_a_child/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_a_child(Id) ->
	% ws_log:info("start_child()"),
	% ws_log:info("start_child(~p)", [N]),
	R = {ok, Pid} = supervisor:start_child(?SERVER, [Id]),
	% ws_log:info("New child: ~p", [R]),
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
	ws_log:info("listening..."),
	% spawn_link(fun() -> start_childs(ws_config:get(listener_count)) end),
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
