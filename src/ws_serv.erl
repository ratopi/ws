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

-record(state, {socket, phase, method, path, headers = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ListenSocket) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [ListenSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ListenSocket]) ->
	ws_log:info("cast accept"),
	gen_server:cast(self(), accept),
	{ok, #state{socket = ListenSocket}}.


handle_call(_Request, _From, State) ->
	{noreply, State}.


handle_cast(accept, State = #state{socket = ListenSocket}) ->
	ws_log:info("accepting ..."),
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	ws_sup:start_child(),
	inet:setopts(AcceptSocket, [{active, once}]),
	{noreply, State#state{socket = AcceptSocket, phase = start}};

handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({tcp, _Port, Data}, State = #state{phase = start, socket = AcceptSocket}) ->
	[MethodText, Path, _Protcol] = Parts = re:split(Data, "[ \n\r]", [trim]),
	ws_log:info("recv : ~p~n", [Data]),
	ws_log:info("recv : ~p~n", [Parts]),
	Method = binary_to_atom(MethodText, utf8),
	NewState = State#state{method = Method, path = Path, phase = header},
	ws_log:info("NewState ~p~n", [NewState]),
	inet:setopts(AcceptSocket, [{active, once}]),
	{noreply, NewState};

handle_info({tcp, _Port, "\r\n"}, State = #state{phase = header, socket = AcceptSocket}) ->
	inet:setopts(AcceptSocket, [{active, once}]),
	NewState = State#state{phase = send},
	ws_log:info("end of header"),
	ws_log:info("NewState ~p~n", [NewState]),
	{noreply, NewState};

handle_info({tcp, _Port, Data}, State = #state{phase = header, socket = AcceptSocket}) ->
	ws_log:info("header : ~p~n", [re:replace(Data, "\n\r", "")]),
	ws_log:info("header : ~p~n", [string:split(Data, ": ")]),
	inet:setopts(AcceptSocket, [{active, once}]),
	NewState = State,
	ws_log:info("NewState ~p~n", [NewState]),
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
