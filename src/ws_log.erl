%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2018 23:11
%%%-------------------------------------------------------------------
-module(ws_log).
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

%% API
-export([info/1, info/2]).

info(Message) ->
	io:fwrite(Message ++ "~n").

info(Message, Args) ->
	io:fwrite(Message ++ "~n", Args).
