%%%-------------------------------------------------------------------
%%% @author luoruiyu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 5月 2025 17:45
%%%-------------------------------------------------------------------
-module(chat_client_handler).
-author("luoruiyu").
-behaviour(gen_server).
%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% 定义状态记录，存储客户端套接字和用户名
-record(state, {socket, username}).
% 定义心跳超时时间（60秒）
-define(TIMEOUT, 60000).
-define(DEBUG_MOD(Fmt, Args, Mod), io:format("[DEBUG] ~p:~p: " ++ Fmt, [Mod, ?LINE | Args])).
-define(TIME_DIFF(End, Start), timer:now_diff(End, Start) / 1000).

% 启动 gen_server 进程，处理单个客户端连接
start_link(Socket) ->
  % 启动 gen_server，传递 Socket 作为初始参数（这里使用匿名注册进程，防止多用户连接时模块名冲突）
  gen_server:start_link(?MODULE, [Socket], []).

% 初始化 gen_server 进程，将Socket放入State中
init([Socket]) ->
  % 设置 60 秒后发送超时消息
  erlang:send_after(?TIMEOUT, self(), timeout),
  % 返回初始状态，用户名为空
  {ok, #state{socket = Socket, username = undefined}}.

% 处理远程调用（当前未使用）
handle_call(_Request, _From, State) ->
  % 返回 ok，保持状态不变
  {reply, ok, State}.

% 处理通知消息（当前未使用）
handle_cast(_Request, State) ->
  % 保持状态不变，继续运行
  {noreply, State}.

% 处理接收到的 TCP 消息
handle_info({tcp, Socket, Data}, State = #state{socket = Socket}) ->
  ?DEBUG_MOD("收到TCP消息~n", [], ?MODULE),
  % 处理客户端消息（如登录、聊天、退出）
  case handle_message(Socket, State#state.username, Data) of
    % 消息处理成功，返回新的用户名
    {ok, NewUsername} ->
      % 消息处理成功，证明用户活跃，继续设置下一次超时检查
      erlang:send_after(?TIMEOUT, self(), timeout),
      % 更新状态中的用户名，继续运行
      {noreply, State#state{username = NewUsername}};
    % 收到停止指令（例如退出）
    stop ->
      % 停止 gen_server 进程
      {stop, normal, State}
  end;

% 处理客户端连接关闭
handle_info({tcp_closed, Socket}, State = #state{username = Username}) ->
  % 如果用户已登录，从 ETS 表中删除
  if Username =/= undefined -> ets:delete(users, Username); true -> ok end,
  % 停止 gen_server 进程
  {stop, normal, State};

% 处理心跳超时（用户如果在TIMEOUT的时间内没执行任何操作，如login、chat，则视为用户不存活）
handle_info(timeout, State = #state{username = Username, socket = Socket}) ->
  case Username of
    % 未登录用户则继续循环
    undefined ->
      erlang:send_after(?TIMEOUT, self(), timeout),
      {noreply, State};
    % 若用户已登录
    Username ->
      % 检查最后心跳时间
      case ets:lookup(users, Username) of
        [{Username, Socket, LastHeartbeat, _}] ->
          % 计算时间差（毫秒）
          Diff = ?TIME_DIFF(os:timestamp(), LastHeartbeat),
          % 检查是否超时
          if Diff > ?TIMEOUT ->
            % 尝试关闭套接字并捕获异常
            CloseResult = try
                            case is_port(Socket) andalso Socket =/= undefined of
                              true -> gen_tcp:close(Socket);
                              false -> {error, invalid_socket}
                            end
                          catch
                            error:Reason:Stacktrace ->
                              ?DEBUG_MOD("关闭客户端套接字异常: ~p, 堆栈: ~p~n", [Reason, Stacktrace], ?MODULE),
                              {error, exception}
                          end,
            case CloseResult of
              ok ->
                ?DEBUG_MOD("客户端套接字成功关闭~n", [], ?MODULE);
              {error, invalid_socket} ->
                ?DEBUG_MOD("关闭客户端套接字失败: 无效套接字~n", [], ?MODULE);
              {error, exception} ->
                ?DEBUG_MOD("关闭客户端套接字失败: 已记录异常~n", [], ?MODULE)
            end,
            % 清理ETS表
            ets:delete(users, Username),
            % 打印超时日志
            ?DEBUG_MOD("用户 ~p 超时~n", [Username], ?MODULE),
            {stop, normal, State};
            true ->
              % 未超时，继续循环
              erlang:send_after(?TIMEOUT, self(), timeout),
              {noreply, State}
          end;
        _ ->
          % ets表中用户不存在，继续循环
          erlang:send_after(?TIMEOUT, self(), timeout),
          {noreply, State}
      end
  end;

% 处理收到的其他意外消息
handle_info(Msg, State) ->
  ?DEBUG_MOD("意外消息: ~p, 此时状态: ~p~n", [Msg, State], ?MODULE),
  {noreply, State}.

% 进程终止时清理资源
terminate(_Reason, #state{socket = Socket, username = Username}) ->
  % 尝试关闭套接字并捕获异常
  CloseResult = try
                  case is_port(Socket) andalso Socket =/= undefined of
                    true -> gen_tcp:close(Socket);
                    false -> {error, invalid_socket}
                  end
                catch
                  error:Reason:Stacktrace ->
                    ?DEBUG_MOD("关闭客户端套接字异常: ~p, 堆栈: ~p~n", [Reason, Stacktrace], ?MODULE),
                    {error, exception}
                end,
  case CloseResult of
    ok ->
      ?DEBUG_MOD("客户端套接字成功关闭~n", [], ?MODULE);
    {error, invalid_socket} ->
      ?DEBUG_MOD("关闭客户端套接字失败: 无效套接字~n", [], ?MODULE);
    {error, exception} ->
      ?DEBUG_MOD("关闭客户端套接字失败: 已记录异常~n", [], ?MODULE)
  end,

  if Username =/= undefined ->
    ets:delete(users, Username),
    ?DEBUG_MOD("用户 ~p ETS记录已删除~n", [Username], ?MODULE);
    true ->
      ok
  end,
  ok.

% 支持代码热升级（当前为默认实现）
code_change(_OldVsn, State, _Extra) ->
  % 返回当前状态
  {ok, State}.




%%=================================================
%% 下面是内部函数
%%=================================================

% 处理用户登录消息
handle_message(Socket, undefined, Data) ->
  try
    % 安全解析消息并直接处理
    case binary_to_term(Data, [safe]) of
      {login, Username} when is_list(Username), Username =/= [] ->
        case ets:info(users) of
          undefined ->
            ?DEBUG_MOD("ETS表users不存在~n", [], ?MODULE),
            gen_tcp:send(Socket, term_to_binary({error, "Server error"})),
            {ok, undefined};
          _ ->
            case ets:lookup(users, Username) of
              [] ->
                ets:insert(users, {Username, Socket, os:timestamp(), {0, 0, 0}}),
                gen_tcp:send(Socket, term_to_binary({ok, "Login successful"})),
                ?DEBUG_MOD("服务器确认用户~p登录，并在ets表中创建新用户~n", [Username], ?MODULE),
                {ok, Username};
              [_] ->
                gen_tcp:send(Socket, term_to_binary({error, "Username taken"})),
                {ok, undefined}
            end
        end;
      _ ->
        ?DEBUG_MOD("无效登录消息~n", [], ?MODULE),
        gen_tcp:send(Socket, term_to_binary({error, "Invalid login message"})),
        {ok, undefined}
    end
  catch
    Error:Reason:Stacktrace ->
      ?DEBUG_MOD("处理登录消息异常: ~p:~p, 堆栈: ~p~n", [Error, Reason, Stacktrace], ?MODULE),
      gen_tcp:send(Socket, term_to_binary({error, "Internal server error"})),
      {ok, undefined}
  end;

% 处理用户聊天消息和退出消息
handle_message(Socket, Username, Data) when Username =/= undefined ->
  try
    % 安全解析消息并直接处理
    case binary_to_term(Data, [safe]) of
      {chat, Message} when is_list(Message), Message =/= [] ->
        case ets:info(users) of
          undefined ->
            ?DEBUG_MOD("ETS表users不存在~n", [], ?MODULE),
            gen_tcp:send(Socket, term_to_binary({error, "Server error"})),
            {ok, undefined};
          _ ->
            case ets:lookup(users, Username) of
              [{Username, Socket, _, LastMessageTime}] ->
                Now = os:timestamp(),
                Diff = ?TIME_DIFF(Now, LastMessageTime),
                if LastMessageTime =:= {0, 0, 0} orelse Diff >= 1000 ->
                  ets:insert(users, {Username, Socket, Now, Now}),
                  broadcast(term_to_binary({chat, Username, Message}), Username),
                  ?DEBUG_MOD("~p广播消息成功~n", [Username], ?MODULE),
                  gen_tcp:send(Socket, term_to_binary({sent, Username, Message})),
                  {ok, Username};
                  true ->
                    gen_tcp:send(Socket, term_to_binary({error, "Message too frequent"})),
                    ets:insert(users, {Username, Socket, Now, LastMessageTime}),
                    ?DEBUG_MOD("用户：~p 消息发送太频繁~n", [Username], ?MODULE),
                    {ok, Username}
                end;
              _ ->
                ?DEBUG_MOD("用户~p不存在~n", [Username], ?MODULE),
                gen_tcp:send(Socket, term_to_binary({error, "User not found"})),
                {ok, undefined}
            end
        end;
      logout ->
        case ets:info(users) of
          undefined ->
            ?DEBUG_MOD("ETS表users不存在~n", [], ?MODULE),
            gen_tcp:send(Socket, term_to_binary({error, "Server error"})),
            {ok, undefined};
          _ ->
            case ets:lookup(users, Username) of
              [_] ->
                ets:delete(users, Username),
                gen_tcp:send(Socket, term_to_binary({logout, Username, "Logged out"})),
                stop;
              _ ->
                ?DEBUG_MOD("用户~p不存在~n", [Username], ?MODULE),
                gen_tcp:send(Socket, term_to_binary({error, "User not found"})),
                {ok, undefined}
            end
        end;
      _ ->
        ?DEBUG_MOD("无效聊天或退出消息~n", [], ?MODULE),
        gen_tcp:send(Socket, term_to_binary({error, "Invalid chat or logout message"})),
        {ok, Username}
    end
  catch
    Error:Reason:Stacktrace ->
      ?DEBUG_MOD("处理聊天消息异常: ~p:~p~n", [Error, Reason], ?MODULE),
      gen_tcp:send(Socket, term_to_binary({error, "Internal server error"})),
      {ok, Username}
  end;



% 处理无效消息
handle_message(Socket, _, Data) ->
  ?DEBUG_MOD("无效消息数据: ~p~n", [Data], ?MODULE),
  gen_tcp:send(Socket, term_to_binary({error, "Invalid message"})),
  {ok, undefined}.

% 广播消息
broadcast(Message, Sender) ->
  UserList = ets:tab2list(users),
  lists:foreach(fun({User, Socket,_,_}) ->
    if User =/= Sender -> gen_tcp:send(Socket, Message); true -> ok end
                end, UserList).





