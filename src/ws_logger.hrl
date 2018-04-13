%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Th. Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2018 14:03
%%%-------------------------------------------------------------------
-author("Ralf Th. Pietsch <ratopi@abwesend.de>").

-define(LOG_DEBUG(Message), ws_logger:log(debug, ?MODULE, self(), Message, [])).
-define(LOG_DEBUG(Message, Arguments), ws_logger:log(debug, ?MODULE, self(), Message, Arguments)).

-define(LOG_INFO(Message), ws_logger:log(info, ?MODULE, self(), Message, [])).
-define(LOG_INFO(Message, Arguments), ws_logger:log(info, ?MODULE, self(), Message, Arguments)).
