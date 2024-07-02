%%%-------------------------------------------------------------------
%%% @doc
%%% Модуль db_tests содержит тесты для сервера базы данных.
%%% Тесты включают в себя проверку операций записи, чтения, удаления и поиска.
%%% @end
%%% Created : 29. Jun 2024 13:20
%%%-------------------------------------------------------------------
-module(db_tests).
-author("cat").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%% Тесты

should_success_destroy_test() ->
  {setup,
    %% given
    fun() ->
      {ok, Pid} = db_server:start_link(),
      Pid
    end,
    %% cleanup
    fun(Pid) ->
      db_server:stop(Pid),
      ok
    end,
    %% when and then
    fun(Pid) ->
      db_server:stop(Pid),
      ?assertEqual(ok, ok)
    end}.

should_success_write_read_test() ->
  {setup,
    %% given
    fun() ->
      {ok, Pid} = db_server:start_link(),
      Pid
    end,
    %% cleanup
    fun(Pid) ->
      db_server:stop(Pid),
      ok
    end,
    %% when and then
    fun(Pid) ->
      db_server:write(test_key, test_value, Pid),
      {ok, Value} = db_server:read(test_key, Pid),
      ?assertEqual(test_value, Value)
    end}.

should_success_delete_test() ->
  {setup,
    %% given
    fun() ->
      {ok, Pid} = db_server:start_link(),
      Pid
    end,
    %% cleanup
    fun(Pid) ->
      db_server:stop(Pid),
      ok
    end,
    %% when and then
    fun(Pid) ->
      db_server:write(test_key, test_value, Pid),
      db_server:delete(test_key, Pid),
      {error, not_found} = db_server:read(test_key, Pid),
      ?assert(true)
    end}.

should_success_match_test() ->
  {setup,
    %% given
    fun() ->
      {ok, Pid} = db_server:start_link(),
      Pid
    end,
    %% cleanup
    fun(Pid) ->
      db_server:stop(Pid),
      ok
    end,
    %% when and then
    fun(Pid) ->
      db_server:write(key1, value, Pid),
      db_server:write(key2, value, Pid),
      db_server:write(key3, different_value, Pid),
      Keys = db_server:match(value, Pid),
      ?assertEqual([key1, key2], lists:sort(Keys))
    end}.
-endif.
