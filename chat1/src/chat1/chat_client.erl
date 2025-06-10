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

%% API
-export([start/0]).
-include_lib("wx/include/wx.hrl").


% 定义按钮ID
-define(LOGIN_BUTTON, 100).
-define(SEND_BUTTON, 101).
-define(LOGOUT_BUTTON, 102).

% 启动客户端和GUI
start() ->
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

  % 绑定事件
  wxButton:connect(LoginButton, command_button_clicked),
  wxButton:connect(SendButton, command_button_clicked),
  wxButton:connect(LogoutButton, command_button_clicked),
  wxFrame:connect(Frame, close_window),

  % 初始状态(不显示发送及退出按钮)
  wxButton:disable(SendButton),
  wxButton:disable(LogoutButton),

  % 主循环
  loop(Socket, undefined, Frame, UsernameInput, ChatDisplay, MessageInput).

% 主循环
loop(Socket, Username, Frame, UsernameInput, ChatDisplay, MessageInput) ->
  receive
  % 收到服务器发来的消息
    {tcp, Socket, Data} ->
      handle_server_message(binary_to_term(Data), ChatDisplay, UsernameInput),
      loop(Socket, Username, Frame, UsernameInput, ChatDisplay, MessageInput);

  % 收到服务器断开消息
    {tcp_closed, Socket} ->
      wxTextCtrl:appendText(ChatDisplay, "已和服务器断开连接...\n"),
      wxFrame:destroy(Frame);

  % 用户点击登录按钮
    {wx, ?LOGIN_BUTTON, _, _, {_, command_button_clicked, _, _, _}} ->
      % 从用户名输入框中读出用户名
      UsernameStr = wxTextCtrl:getValue(UsernameInput),
      io:format("~p点击了登录按钮~n", [UsernameStr]),
      if UsernameStr =/= [] ->
        io:format("~p开始向服务器发送登录请求~n", [UsernameStr]),
        gen_tcp:send(Socket, term_to_binary({login, UsernameStr}));
        true ->
          wxTextCtrl:appendText(ChatDisplay, "请输入用户名...\n")
      end,
      loop(Socket, Username, Frame, UsernameInput, ChatDisplay, MessageInput);

  % 用户点击发送按钮
    {wx, ?SEND_BUTTON, _, _, {_, command_button_clicked, _, _, _}} ->
      % 从消息输入框中读出用户输入的消息
      Message = wxTextCtrl:getValue(MessageInput),
      if Message =/= [] ->
        gen_tcp:send(Socket, term_to_binary({chat, Message})),
        % 清空消息输入框
        wxTextCtrl:setValue(MessageInput, "");
        true ->
          wxTextCtrl:appendText(ChatDisplay, "请输入消息...\n")
      end,
      loop(Socket, Username, Frame, UsernameInput, ChatDisplay, MessageInput);

  % 用户点击登出按钮
    {wx, ?LOGOUT_BUTTON, _, _, {_, command_button_clicked, _, _, _}} ->
      gen_tcp:send(Socket, term_to_binary(logout)),
      wxTextCtrl:enable(UsernameInput),
      wxButton:disable(wxWindow:findWindowById(?SEND_BUTTON)),
      wxButton:disable(wxWindow:findWindowById(?LOGOUT_BUTTON)),
      loop(Socket, undefined, Frame, UsernameInput, ChatDisplay, MessageInput);

  % 用户点击窗口关闭按钮
    {wx, _, _, _, {_, close_window}} ->
      gen_tcp:close(Socket),
      wxFrame:destroy(Frame)
  end.

% 处理服务器发来的登录处理消息
handle_server_message({ok, "Login successful"}, ChatDisplay, UsernameInput) ->
  io:format("客户端收到服务器的确认登录请求~n"),
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
  io:format("~p发送消息成功~n", [Username]);

% 处理消息发送过于频繁
handle_server_message({error, "Message too frequent"}, ChatDisplay, _) ->
  % 显示发送过于频繁提示
  wxTextCtrl:appendText(ChatDisplay, "消息发送过于频繁，请稍后再试...\n");

% 处理用户退出成功的消息
handle_server_message({logout, Username,"Logged out"}, ChatDisplay, _) ->
  wxTextCtrl:appendText(ChatDisplay, "您已成功退出..."),
  io:format("~p退出登录成功~n", [Username]);

% 处理被踢出消息
handle_server_message({kicked, Reason}, ChatDisplay, _) ->
  % 显示被踢出提示
  wxTextCtrl:appendText(ChatDisplay, io_lib:format("您被踢出，原因是：~s\n", [Reason])),
  % 打印被踢出日志
  io:format("客户端被踢出，原因是: ~p~n", [Reason]);

% 处理出错的消息
handle_server_message({error, Reason}, ChatDisplay, _) ->
  wxTextCtrl:appendText(ChatDisplay, io_lib:format("Error: ~s\n", [Reason])).
