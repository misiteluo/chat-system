%%%-------------------------------------------------------------------
%%% @author luoruiyu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 5月 2025 21:42
%%%-------------------------------------------------------------------
-module(chat_sup).
-author("luoruiyu").

-behaviour(supervisor).
%% API
-export([start_link/0]).
-export([init/1]).
-define(SERVER,?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Server = {chat_server,{chat_server,start_link,[]},permanent,5000,worker,[chat_server]},
  Children = [Server],
  RestartStrategy = {one_for_one,5,10},
  {ok, {RestartStrategy, Children}}.

