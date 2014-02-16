Redis proxy in erlang
---------------------

 This is a PROTOTYPE


Compile
-------

 - copy erts-5.9.1 to ./
 - copy kernel, stdlib, sasl to lib/
 - run release.sh &lt;dest_path&gt;


autosync usage
--------------

Install https://github.com/rustyio/sync. Run erlang shell and type:

 application:start(redis).
 sync:go().


Links
-----

 - Base code for this proxy: http://www.duomark.com/erlang/tutorials/proxy2.html
 - Awesome auto recompiler: https://github.com/rustyio/sync
 - http://www.erlang.org/doc/system_principles/create_target.html
 - http://www.erlang.org/doc/design_principles/release_handling.html
 - http://www.erlang.org/doc/design_principles/applications.html
