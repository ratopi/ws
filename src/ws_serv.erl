%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2018 22:11
%%%-------------------------------------------------------------------
-module(ws_serv).
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {listenSocket, port, phase, method, path, headers = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ListenSocket) ->
	gen_server:start_link(?MODULE, [ListenSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ListenSocket]) ->
	% ws_log:info("cast accept"),
	gen_server:cast(self(), accept),
	% ws_log:info("~n~p~n", [#state{}]),
	{ok, #state{listenSocket = ListenSocket}}.


handle_call(_Request, _From, State) ->
	{noreply, State}.


handle_cast(accept, State = #state{listenSocket = ListenSocket}) ->
	% ws_log:info("accepting ..."),
	{ok, Port} = gen_tcp:accept(ListenSocket),
	ws_sup:start_child(), % todo: new child should not be started here, but when this child dies
	inet:setopts(Port, [{active, once}]),
	{noreply, State#state{port = Port, phase = start}};

handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({tcp, Port, Data}, State = #state{phase = start, port = Port}) ->
	[MethodText, Path, _Protcol] = Parts = re:split(Data, "[ \n\r]", [trim]),
	% ws_log:info("recv : ~p", [Data]),
	% ws_log:info("recv : ~p", [Parts]),
	Method = binary_to_atom(MethodText, utf8),
	NewState = State#state{method = Method, path = Path, phase = header},
	% ws_log:info("NewState ~p", [NewState]),
	inet:setopts(Port, [{active, once}]),
	{noreply, NewState};

handle_info({tcp, Port, "\r\n"}, State = #state{phase = header, port = Port, path = Path}) ->
	inet:setopts(Port, [{active, once}]),
	NewState = State#state{phase = send},
	% ws_log:info("end of header"),
	send_data(Port, Path),
	% ws_log:info("NewState ~p", [NewState]),
	{stop, normal, NewState};

handle_info({tcp, Port, Data}, State = #state{phase = header, port = Port}) ->
	% ws_log:info("header : ~p", [re:replace(Data, "\n\r", "")]),
	% ws_log:info("header : ~p", [string:split(Data, ": ")]),
	inet:setopts(Port, [{active, once}]),
	NewState = State,
	% ws_log:info("NewState ~p", [NewState]),
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
	ws_log:info("Requested path ~p", [Path]),
	inet:send(Port, "Request path was:\r\n"),
	inet:send(Port, Path),
	inet:close(Port).
