{application, redis,
  [{description, "redis proxy"},
   {vsn, "1.0"},
   {modules, [redis, tcp_proxy]},
   {registered, []},
   {applications, [kernel, stdlib]},
   {mod, {redis, []}}
]}.
