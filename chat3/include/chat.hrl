-ifndef(CHAT_HRL).
-define(CHAT_HRL, true).

-define(TIMEOUT, 60000). % 心跳超时时间（60秒）
-define(TIME_DIFF(End, Start), timer:now_diff(End, Start) / 1000).

-define(DEBUG(Fmt), ?DEBUG(Fmt, [])).
-define(DEBUG(Fmt, Args), io:format("[DEBUG] ~p:~p: " ++ Fmt, [?MODULE, ?LINE | Args])).
-define(DEBUG_MOD(Fmt, Args, Mod), io:format("[DEBUG] ~p:~p: " ++ Fmt, [Mod, ?LINE | Args])).

-define(LOGIN_BUTTON, 100).
-define(SEND_BUTTON, 101).
-define(LOGOUT_BUTTON, 102).

-endif.