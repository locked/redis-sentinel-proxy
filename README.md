Redis proxy in erlang
---------------------

 This is a PROTOTYPE


Configuration
-------------

 Edit lib/redis-1.0/ebin/redis.config and set the sentinels IPs.


Compile
-------

 - copy erts-5.9.1 to ./
 - copy kernel, stdlib, sasl to lib/
 - run release.sh &lt;dest_path&gt;


Autosync usage
--------------

Install https://github.com/rustyio/sync.

In the lib/redis-1.0/src folder, run erlang shell and type:

```
application:start(redis).
sync:go().
```


Links
-----

 - Base code for this proxy: http://www.duomark.com/erlang/tutorials/proxy2.html
 - Awesome auto recompiler: https://github.com/rustyio/sync
 - http://www.erlang.org/doc/system_principles/create_target.html
 - http://www.erlang.org/doc/design_principles/release_handling.html
 - http://www.erlang.org/doc/design_principles/applications.html
