%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2018 22:11
%%%-------------------------------------------------------------------
-module(ws_serv_worker).
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

-include("ws_logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

% -define(SERVER, ?MODULE).

-record(state, {n, listenSocket, port, phase, method, path, headers = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ListenSocket, N) ->
	gen_server:start_link(?MODULE, [ListenSocket, N], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ListenSocket, N]) ->
	?LOG_DEBUG("INIT ~p ~p~n", [ListenSocket, N]),
	?LOG_DEBUG("cast accept"),
	gen_server:cast(self(), accept),
	?LOG_DEBUG("~n~p~n", [#state{}]),
	{ok, #state{n = N, listenSocket = ListenSocket}}.


handle_call(_Request, _From, State) ->
	{noreply, State}.


handle_cast(accept, State = #state{n = N, listenSocket = ListenSocket}) ->
	?LOG_INFO("~p accepting ...", [N]),
	{ok, Port} = gen_tcp:accept(ListenSocket),
	inet:setopts(Port, [{active, once}]),
	{noreply, State#state{port = Port, phase = start}};

handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({tcp, Port, Data}, State = #state{n = N, phase = start, port = Port}) ->
	[MethodText, Path, _Protcol] = Parts = re:split(Data, "[ \n\r]", [trim]),
	?LOG_DEBUG("recv : ~p", [Data]),
	?LOG_DEBUG("recv : ~p", [Parts]),
	Method = binary_to_atom(MethodText, utf8),
	NewState = State#state{method = Method, path = Path, phase = header},
	?LOG_DEBUG("NewState ~p", [NewState]),
	inet:setopts(Port, [{active, once}]),
	{noreply, NewState};

handle_info({tcp, Port, "\r\n"}, State = #state{n = N, phase = header, port = Port, path = Path}) ->
	inet:setopts(Port, [{active, once}]),
	?LOG_DEBUG("end of header"),
	send_data(Port, Path),
	inet:close(Port),
	?LOG_INFO("~p closed", [N]),
	gen_server:cast(self(), accept),
	NewState = State#state{phase = accept, port = undefined},
	?LOG_DEBUG("NewState ~p", [NewState]),
	{noreply, NewState};

handle_info({tcp, Port, Data}, State = #state{n = N, phase = header, port = Port}) ->
	?LOG_DEBUG("header : ~p", [re:replace(Data, "\n\r", "")]),
	?LOG_DEBUG("header : ~p", [string:split(Data, ": ")]),
	inet:setopts(Port, [{active, once}]),
	NewState = State,
	?LOG_DEBUG("NewState ~p", [NewState]),
	{noreply, NewState};

handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_data(Port, Path) ->
	?LOG_INFO("Requested path ~p", [Path]),
	inet:send(Port, "Request path was:\r\n"),
	inet:send(Port, Path),
	inet:close(Port).
