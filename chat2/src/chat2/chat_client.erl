%%%-------------------------------------------------------------------
%%% @author luoruiyu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 5月 2025 17:36
%%%-------------------------------------------------------------------
-module(chat_client).
-author("luoruiyu").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% API
-export([start_link/0]).

-include_lib("wx/include/wx.hrl").

% 定义按钮ID
-define(LOGIN_BUTTON, 100).
-define(SEND_BUTTON, 101).
-define(LOGOUT_BUTTON, 102).
-define(DEBUG(Fmt), ?DEBUG(Fmt, [])).
-define(DEBUG(Fmt, Args), io:format("[DEBUG] ~p:~p: " ++ Fmt, [?MODULE, ?LINE | Args])).

% 定义状态记录
-record(state, {socket, username, frame, username_input, chat_display, message_input}).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% 初始化 gen_server 进程,创建GUI并与服务器建立socket连接
init([]) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Chat Client", [{size, {400, 300}}]),
  Panel = wxPanel:new(Frame),

  % GUI组件
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  UsernameInput = wxTextCtrl:new(Panel, -1),
  LoginButton = wxButton:new(Panel, ?LOGIN_BUTTON, [{label, "Login"}]),
  ChatDisplay = wxTextCtrl:new(Panel, -1, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}, {size, {380, 150}}]),
  MessageInput = wxTextCtrl:new(Panel, -1),
  SendButton = wxButton:new(Panel, ?SEND_BUTTON, [{label, "Send"}]),
  LogoutButton = wxButton:new(Panel, ?LOGOUT_BUTTON, [{label, "Logout"}]),

  % 布局
  wxBoxSizer:add(Sizer, UsernameInput, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxBoxSizer:add(Sizer, LoginButton, [{flag, ?wxALL}, {border, 5}]),
  wxBoxSizer:add(Sizer, ChatDisplay, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  BottomSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(BottomSizer, MessageInput, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxBoxSizer:add(BottomSizer, SendButton, [{flag, ?wxALL}, {border, 5}]),
  wxBoxSizer:add(BottomSizer, LogoutButton, [{flag, ?wxALL}, {border, 5}]),
  wxBoxSizer:add(Sizer, BottomSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxPanel:setSizer(Panel, Sizer),
  wxFrame:show(Frame),

  % 连接服务器
  {ok, Socket} = gen_tcp:connect("localhost", 12345, [binary, {packet, 0}, {active, true}]),

  % 为按钮绑定点击事件，并使用gen_server:cast()来做通知
  wxButton:connect(LoginButton, command_button_clicked, [{callback, fun(Event, _) ->
    gen_server:cast(?MODULE, {login_button, Event}) end}]),
  wxButton:connect(SendButton, command_button_clicked, [{callback, fun(Event, _) ->
    gen_server:cast(?MODULE, {send_button, Event}) end}]),
  wxButton:connect(LogoutButton, command_button_clicked, [{callback, fun(Event, _) ->
    gen_server:cast(?MODULE, {logout_button, Event}) end}]),
  wxFrame:connect(Frame, close_window, [{callback, fun(Event, _) ->
    gen_server:cast(?MODULE, {close_window, Event}) end}]),

  % 初始GUI状态(不显示发送及退出按钮)
  wxButton:disable(SendButton),
  wxButton:disable(LogoutButton),

  % 返回初始状态
  {ok, #state{socket = Socket, username = undefined, frame = Frame, username_input = UsernameInput, chat_display = ChatDisplay, message_input = MessageInput}}.

handle_call(_Request, _From, State) ->
  {reply,ok,State}.

% 用户点击登录按钮后的回调函数
handle_cast({login_button, _Event}, State = #state{socket = Socket, username_input = UsernameInput, chat_display = ChatDisplay}) ->
  % 从用户名输入框中读出用户名
  UsernameStr = wxTextCtrl:getValue(UsernameInput),
  ?DEBUG("~p点击了登录按钮~n", [UsernameStr]),
  if UsernameStr =/= [] ->
    ?DEBUG("~p开始向服务器发送登录请求~n", [UsernameStr]),
    gen_tcp:send(Socket, term_to_binary({login, UsernameStr}));
    true ->
      wxTextCtrl:appendText(ChatDisplay, "请输入用户名...\n")
  end,
  {noreply, State};

% 用户点击发送按钮后的回调函数
handle_cast({send_button, _Event}, State = #state{socket = Socket, message_input = MessageInput, chat_display = ChatDisplay}) ->
  % 从消息输入框中读出用户输入的消息
  Message = wxTextCtrl:getValue(MessageInput),
  if Message =/= [] ->
    gen_tcp:send(Socket, term_to_binary({chat, Message})),
    % 清空消息输入框
    wxTextCtrl:setValue(MessageInput, "");
    true ->
      wxTextCtrl:appendText(ChatDisplay, "请输入消息...\n")
  end,
  {noreply, State};

% 用户点击登出按钮后的回调函数
handle_cast({logout_button, _Event}, State = #state{socket = Socket, username_input = UsernameInput}) ->
  gen_tcp:send(Socket, term_to_binary(logout)),
  wxTextCtrl:enable(UsernameInput),
  wxButton:disable(wxWindow:findWindowById(?SEND_BUTTON)),
  wxButton:disable(wxWindow:findWindowById(?LOGOUT_BUTTON)),
  {noreply, State#state{username = undefined}};

% 用户点击窗口关闭按钮后的回调函数
handle_cast({close_window, _Event}, State = #state{socket = Socket, frame = Frame}) ->
  % 尝试关闭套接字并捕获异常
  Result = try
             case is_port(Socket) andalso Socket =/= undefined of
               true ->
                 % 如果是有效的端口 且 Socket =/= undefined，正常关闭
                 gen_tcp:close(Socket);
               false ->
                 {error, invalid_socket}
             end
           catch
             error:Reason:Stacktrace ->
               ?DEBUG("关闭客户端套接字异常: ~p, 堆栈: ~p~n", [Reason, Stacktrace]),
               {error, exception}
           end,
  % 根据关闭结果记录日志
  case Result of
    ok ->
      ?DEBUG("客户端套接字成功关闭~n");
    {error, invalid_socket} ->
      ?DEBUG("关闭客户端套接字失败: 无效套接字~n");
    {error, exception} ->
      ?DEBUG("关闭客户端套接字失败: 已记录异常~n")
  end,
  wxFrame:destroy(Frame),
  {stop, normal, State}.

% 收到服务器发来的消息
handle_info({tcp, Socket, Data}, State = #state{socket = Socket, username_input = UsernameInput, chat_display = ChatDisplay}) ->
  handle_server_message(binary_to_term(Data), ChatDisplay, UsernameInput),
  {noreply, State};

% 处理服务器连接关闭
handle_info({tcp_closed, Socket}, State = #state{chat_display = ChatDisplay, frame = Frame}) ->
  wxTextCtrl:appendText(ChatDisplay, "已和服务器断开连接...\n"),
  wxFrame:destroy(Frame),
  % 停止 gen_server 进程
  {stop, normal, State}.

% 进程终止时清理资源
terminate(_Reason, #state{socket = Socket, frame = Frame}) ->
  % 尝试关闭套接字并捕获异常
  Result = try
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
  % 根据关闭结果记录日志
  case Result of
    ok ->
      ?DEBUG("客户端套接字成功关闭~n");
    {error, invalid_socket} ->
      ?DEBUG("关闭客户端套接字失败: 无效套接字~n");
    {error, exception} ->
      ?DEBUG("关闭客户端套接字失败: 已记录异常~n")
  end,

  wxFrame:destroy(Frame),
  ?DEBUG("GUI窗口已销毁~n"),
  ok.

% 支持代码热升级（当前为默认实现）
code_change(_OldVsn, State, _Extra) ->
  % 返回当前状态
  {ok, State}.


%%=================================================
%% 下面是内部函数
%%=================================================

% 处理服务器发来的登录处理消息
handle_server_message({ok, "Login successful"}, ChatDisplay, UsernameInput) ->
  ?DEBUG("客户端收到服务器的确认登录请求~n"),
  wxTextCtrl:appendText(ChatDisplay, "登录成功...\n"),
  % 禁用用户名输入框
  wxTextCtrl:disable(UsernameInput),
  % 显示发送和退出按钮
  wxButton:enable(wxWindow:findWindowById(?SEND_BUTTON)),
  wxButton:enable(wxWindow:findWindowById(?LOGOUT_BUTTON));

% 处理服务器发来的聊天广播消息
handle_server_message({chat, Username, Message}, ChatDisplay, _) ->
  wxTextCtrl:appendText(ChatDisplay, io_lib:format("~s: ~s\n", [Username, Message]));

% 处理用户发送消息成功的消息
handle_server_message({sent, Username, Message}, ChatDisplay, _) ->
  wxTextCtrl:appendText(ChatDisplay, io_lib:format("~s\n", [Message])),
  ?DEBUG("~p发送消息成功~n", [Username]);

% 处理消息发送过于频繁
handle_server_message({error, "Message too frequent"}, ChatDisplay, _) ->
  % 显示发送过于频繁提示
  wxTextCtrl:appendText(ChatDisplay, "消息发送过于频繁，请稍后再试...\n");

% 处理用户退出成功的消息
handle_server_message({logout, Username,"Logged out"}, ChatDisplay, _) ->
  wxTextCtrl:appendText(ChatDisplay, "您已成功退出..."),
  ?DEBUG("~p退出登录成功~n", [Username]);

% 处理被踢出消息
handle_server_message({kicked, Reason}, ChatDisplay, _) ->
  % 显示被踢出提示
  wxTextCtrl:appendText(ChatDisplay, io_lib:format("您被踢出，原因是：~s\n", [Reason])),
  % 打印被踢出日志
  ?DEBUG("客户端被踢出，原因是: ~p~n", [Reason]);

% 处理出错的消息
handle_server_message({error, Reason}, ChatDisplay, _) ->
  wxTextCtrl:appendText(ChatDisplay, io_lib:format("Error: ~s\n", [Reason])).