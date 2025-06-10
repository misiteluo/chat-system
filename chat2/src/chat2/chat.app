{application, chat,
  [{description, "Chat System Application"},
    {vsn, "1.0"},
    {registered, [chat_sup, chat_server]},
    {applications, [kernel, stdlib]},
    {mod, {chat_app, []}}
  ]}.