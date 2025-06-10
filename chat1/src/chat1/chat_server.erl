%%%-------------------------------------------------------------------
%%% @author luoruiyu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 5月 2025 17:36
%%%-------------------------------------------------------------------
-module(chat_server).
-author("luoruiyu").

%% API
-export([start/0,kick_user/2]).

% 启动服务器
start() ->
  {ok, Listen} = gen_tcp:listen(12345, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
  io:format("Server started on port 12345~n"),
  ets:new(users, [set, named_table, public]), % 存储 {Username, Socket,时间戳}
  spawn(fun() -> accept_loop(Listen) end).

% 接受客户端连接
accept_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> accept_loop(Listen) end),
  client_loop(Socket, undefined).

% 处理客户端消息循环
client_loop(Socket, Username) ->
  % 定义心跳超时时间（60秒）
  Timeout = 60000,
  receive
  % 收到客户端的消息
    {tcp, Socket, Data} ->
      case handle_message(Socket, Username, binary_to_term(Data)) of
        {ok, NewUsername} -> client_loop(Socket, NewUsername);
        stop -> gen_tcp:close(Socket)
      end;
  % 收到客户端的断开消息
    {tcp_closed, Socket} ->
      if Username =/= undefined -> ets:delete(users, Username); true -> ok end;
  % 处理心跳超时（用户如果在timeout的时间内没执行任何操作，如login、chat，则视为用户不存活）
    {timeout} ->
      % 检查用户是否超时
      case Username of
        undefined ->
          % 未登录用户继续循环
          erlang:send_after(Timeout, self(), {timeout}),
          client_loop(Socket, Username);
        _ ->
          % 检查最后心跳时间
          case ets:lookup(users, Username) of
            [{Username, Socket, LastHeartbeat, _}] ->
              % 计算时间差（毫秒）
              Diff = timer:now_diff(os:timestamp(), LastHeartbeat) / 1000,
              % 检查是否超时
              if Diff > Timeout ->
                % 关闭Socket
                gen_tcp:close(Socket),
                % 清理ETS表
                ets:delete(users, Username),
                % 打印超时日志
                io:format("用户 ~p 超时~n", [Username]);
                true ->
                  % 未超时，继续循环
                  erlang:send_after(Timeout, self(), {timeout}),
                  client_loop(Socket, Username)
              end;
            _ ->
              % 用户不存在，继续循环
              erlang:send_after(Timeout, self(), {timeout}),
              client_loop(Socket, Username)
          end
      end
  after Timeout ->
    % 启动超时定时器
    erlang:send_after(Timeout, self(), {timeout}),
    % 继续循环
    client_loop(Socket, Username)
  end.


% 处理用户登录消息
handle_message(Socket, undefined, {login, Username}) ->
  case ets:lookup(users, Username) of
    % 如果用户不在ets表中
    [] ->
      % 插入新用户，LastHeartbeat为当前时间，LastMessageTime为{0,0,0}保证用户刚登录即可发送消息
      ets:insert(users, {Username, Socket, os:timestamp(),{0, 0, 0}}),
      gen_tcp:send(Socket, term_to_binary({ok, "Login successful"})),
      io:format("服务器确认用户~p登录，并在ets表中创建新用户：~p~n", [Username, Username]),
      % 返回用户名以便之后处理此用户发送的消息
      {ok, Username};
    % 如果用户在ets表中
    [_] ->
      gen_tcp:send(Socket, term_to_binary({error, "Username taken"})),
      {ok, undefined}
  end;
% 处理用户聊天消息
handle_message(Socket, Username, {chat, Message}) when Username =/= undefined ->
  Now = os:timestamp(),
  case ets:lookup(users, Username) of
    [{Username, Socket, _, LastMessageTime}] ->
      % 计算时间差（毫秒）
      Diff = timer:now_diff(Now, LastMessageTime) / 1000,
      % 如果LastHeartbeat为{0,0,0}，则证明用户刚登录，可直接发消息，否则判断时间差是否大于1秒,大于即可接着发消息
      if LastMessageTime =:= {0, 0, 0} orelse Diff >= 1000 ->
        % 更新LastHeartbeat和LastMessageTime
        ets:insert(users, {Username, Socket, Now, Now}),
        broadcast(term_to_binary({chat, Username, Message}), Username),
        io:format("~p广播消息成功~n", [Username]),
        gen_tcp:send(Socket, term_to_binary({sent, Username, Message})),
        % 返回用户名以便之后继续处理此用户发送的消息
        {ok, Username};
        true ->
          % 间隔时间小于一秒，则发送错误消息（消息发送频率太高）
          gen_tcp:send(Socket, term_to_binary({error, "Message too frequent"})),
          % 更新LastHeartbeat（即使频率过高，发送了消息仍证明用户活跃）
          ets:insert(users, {Username, Socket, Now, LastMessageTime}),
          % 打印频率过高日志
          io:format("用户：~p 消息发送太频繁~n", [Username]),
          % 返回用户名以便之后继续处理此用户发送的消息
          {ok, Username}
      end;
    _ ->
      % 用户不存在，返回未登录状态
      {ok, undefined}
  end;


% 处理用户退出消息
handle_message(Socket, Username, logout) when Username =/= undefined ->
  ets:delete(users, Username),
  gen_tcp:send(Socket, term_to_binary({logout, Username, "Logged out"})),
  stop;

% 处理无效消息
handle_message(Socket, _, _) ->
  gen_tcp:send(Socket, term_to_binary({error, "Invalid message"})),
  {ok, undefined}.


% 广播消息
broadcast(Message, Sender) ->
  UserList = ets:tab2list(users),
  lists:foreach(fun({User, Socket,_,_}) ->
    if User =/= Sender -> gen_tcp:send(Socket, Message); true -> ok end
                end, UserList).

% 踢出用户  如使用chat_server:kick_user("xiaoming","talkdirty").
kick_user(Username, Reason) ->
  % 查找用户
  case ets:lookup(users, Username) of
    % 用户存在
    [{Username, Socket, _,_}] ->
      % 发送踢出消息
      gen_tcp:send(Socket, term_to_binary({kicked, Reason})),
      % 关闭Socket
      gen_tcp:close(Socket),
      % 清理ETS表
      ets:delete(users, Username),
      % 打印踢出日志
      io:format("用户 ~p 被踢出，原因是: ~p~n", [Username, Reason]);
    % 用户不存在
    [] ->
      % 打印用户不存在日志
      io:format("用户 ~p 不存在~n", [Username])
  end.