%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2018 13:40
%%%-------------------------------------------------------------------
-module(ws_config).
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

%% API
-export([get/1]).


get(Key) ->
	getValue(application:get_env(Key), default(Key)).


default(port) -> 8080;
default(listener_count) -> 20;

default(_) -> undefined.



getValue(undefined, Default) ->
	Default;

getValue(Value, _Default) ->
	Value.
