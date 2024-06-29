db
=====

An OTP application

Build
-----

    $ rebar3 compile

Build with tests
-----

    $ rebar3 as test compile
    $ rebar3 as test shell
    $ eunit:test([{application, calculator}]).


Run tests
-----
    $ rebar3 compile
    $ rebar3 eunit


Task
-----

* Написать распределенную БД
```erlang
db:new() -> Pid.
db:destroy(Pid) -> ok.
db:write(Key, Element, Pid) -> ok.
db:delete(Key, Pid) -> ok.
db:read(Key, Pid) -> {ok, Element} | {error, Reason}.
db:match(Element, Pid) -> [Keyl, ..., KeyN].
```

* В качестве хранения можно использовать всё, что угодно
* Имплементация через gen_server/supervisor/application
