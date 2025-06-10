%%%-------------------------------------------------------------------
%%% @author luoruiyu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 5月 2025 00:54
%%%-------------------------------------------------------------------
-module(chat_app).
-author("luoruiyu").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  chat_sup:start_link().

stop(_State) ->
  ok.
