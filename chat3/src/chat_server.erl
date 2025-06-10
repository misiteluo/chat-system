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
-behaviour(gen_server).
-include("../include/chat.hrl").
%% API
-export([start_link/0, kick_user/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% 定义状态记录，存储监听套接字
-record(state, {listen_socket, socket}).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% 初始化 gen_server 进程,监听连接并创建ets表
init([]) ->
  case gen_tcp:listen(12345, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]) of
    {ok, Listen} ->
      try
        ets:new(users, [set, named_table, public]),
        ?DEBUG("Server started on port 12345, ETS表创建成功~n"),
        erlang:send_after(0, self(), accept),
        {ok, #state{listen_socket = Listen}}
      catch
        error:EtsReason ->
          ?DEBUG("创建ETS表 users 失败: ~p~n", [EtsReason]),
          {stop, {ets_error, EtsReason}}
      end;
    {error, Reason} ->
      ?DEBUG("监听端口 12345 失败: ~p~n", [Reason]),
      {stop, {listen_error, Reason}}
  end.


% 处理远程调用（当前未使用）
handle_call(_Request, _From, State) ->
  % 返回 ok，保持状态不变
  {reply, ok, State}.

% 处理通知消息（当前未使用）
handle_cast(_Request, State) ->
  % 保持状态不变，继续运行
  {noreply, State}.

% 处理接受客户端连接的消息
handle_info(accept, State = #state{listen_socket = Listen}) ->
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
      % 启动一个 chat_client_handler(服务端的子模块) 的新 gen_server 进程来处理该客户端的消息
      {ok, Pid} = start_client_handler(Socket),
      % 把chat_server已建立的 TCP 连接控制权(Socket)，从当前监听进程(chat_server)移交给专门的处理进程（chat_client_handler的gen_server进程）。
      gen_tcp:controlling_process(Socket, Pid),
      ?DEBUG("接受新客户端连接: ~p~n", [Socket]);
    {error, Reason} ->
      ?DEBUG("接受客户端连接失败: ~p~n", [Reason])
  end,

  % 再次发送 accept 消息以继续接受下一个连接
  erlang:send_after(0, self(), accept),
  % 保持状态不变，继续运行
  {noreply, State}.

% 动态启动 chat_client_handler,向监控树动态添加一个子节点
start_client_handler(Socket) ->
  Children = {{chat_client_handler, make_ref()}, {chat_client_handler, start_link, [Socket]}, temporary, 5000, worker, [chat_client_handler]},
  supervisor:start_child(chat_sup, Children).


% 进程终止时清理资源
terminate(_Reason, #state{listen_socket = Listen}) ->
  % 尝试关闭监听套接字并捕获异常
  Result = try
             case is_port(Listen) andalso Listen =/= undefined of
               true ->
                 gen_tcp:close(Listen);
               false ->
                 {error, invalid_socket}
             end
           catch
             error:Reason:Stacktrace ->
               ?DEBUG("关闭客户端套接字异常: ~p, 堆栈: ~p~n", [Reason, Stacktrace]),
               {error, exception}
           end,
  % 记录关闭套接字的结果
  case Result of
    ok ->
      ?DEBUG("客户端套接字成功关闭~n");
    {error, invalid_socket} ->
      ?DEBUG("关闭客户端套接字失败: 无效套接字~n");
    {error, exception} ->
      ?DEBUG("关闭客户端套接字失败: 已记录异常~n")
  end,
  ets:delete(users),
  ok.

% 支持代码热升级（当前为默认实现）
code_change(_OldVsn, State, _Extra) ->
  % 返回当前状态
  {ok, State}.


% 踢出用户  如使用chat_server:kick_user("xiaoming","talkdirty").
kick_user(Username, Reason) ->
  % 查找用户
  case ets:lookup(users, Username) of
    % 用户存在
    [{Username, Socket, _, _}] ->
      % 发送踢出消息
      gen_tcp:send(Socket, term_to_binary({kicked, Reason})),
      % 尝试关闭套接字并捕获异常
      CloseResult = try
                      case is_port(Socket) andalso Socket =/= undefined of
                        true ->
                          gen_tcp:close(Socket);
                        false ->
                          {error, invalid_socket}
                      end
                    catch
                      error:Reason:Stacktrace ->
                        ?DEBUG("关闭客户端套接字异常: ~p, 堆栈: ~p~n", [Reason, Stacktrace]),
                        {error, exception}
                    end,
      % 记录关闭结果
      case CloseResult of
        ok ->
          ?DEBUG("客户端套接字成功关闭~n");
        {error, invalid_socket} ->
          ?DEBUG("关闭客户端套接字失败: 无效套接字~n");
        {error, exception} ->
          ?DEBUG("关闭客户端套接字失败: 已记录异常~n")
      end,
      % 清理ETS表
      ets:delete(users, Username),
      % 打印踢出日志
      ?DEBUG("用户 ~p 被踢出，原因是: ~p~n", [Username, Reason]);
    % 用户不存在
    [] ->
      % 打印用户不存在日志
      ?DEBUG("用户 ~p 不存在~n", [Username])
  end.



