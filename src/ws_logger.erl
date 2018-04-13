%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Th. Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2018 13:59
%%%-------------------------------------------------------------------
-module(ws_logger).
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([log/5]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {debug = true, info = true, warning = true, error = true}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Level :: term()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Level) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Level, []).


log(Level, Module, Pid, Message, Arguments) ->
	gen_server:cast(?SERVER, {log, Level, Module, Pid, Message, Arguments}).


set_log_level(Level) ->
	gen_server:cast(?SERVER, {set_log_level, Level}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init(Level) ->
	set_log_level(Level),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).


handle_cast(Msg = {log, debug, _Module, _Pid, _Message, _Arguments}, State = #state{debug = true}) ->
	do_log(Msg),
	{noreply, State};

handle_cast(Msg = {log, info, _Module, _Pid, _Message, _Arguments}, State = #state{info = true}) ->
	do_log(Msg),
	{noreply, State};

handle_cast(Msg = {log, warning, _Module, _Pid, _Message, _Arguments}, State = #state{warning = true}) ->
	do_log(Msg),
	{noreply, State};

handle_cast(Msg = {log, debug, _Module, _Pid, _Message, _Arguments}, State = #state{debug = true}) ->
	do_log(Msg),
	{noreply, State};


handle_cast({set_log_level, debug}, State) ->
	gen_server:cast(?SERVER, print_state),
	{noreply, State#state{debug = true, info = true, warning = true, error = true}};

handle_cast({set_log_level, info}, State) ->
	gen_server:cast(?SERVER, print_state),
	{noreply, State#state{debug = false, info = true, warning = true, error = true}};

handle_cast({set_log_level, warning}, State) ->
	gen_server:cast(?SERVER, print_state),
	{noreply, State#state{debug = false, info = false, warning = true, error = true}};

handle_cast({set_log_level, error}, State) ->
	gen_server:cast(?SERVER, print_state),
	{noreply, State#state{debug = false, info = false, warning = false, error = true}};


handle_cast(print_state, State) ->
	io:fwrite("### ~p ~p~n", [?MODULE, State]),
	{noreply, State};

handle_cast(Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_log({log, Level, Module, Pid, Message, Arguments}) ->
	io:fwrite("~p [~p ~p] ", [Level, Module, Pid]),
	io:fwrite(Message, Arguments),
	io:fwrite("~n").
