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

should_work_with_multiple_processes_test() ->
  {setup,
    %% given
    fun() ->
      {ok, Pid1} = db_server:start_link(),
      {ok, Pid2} = db_server:start_link(),
      {ok, Pid3} = db_server:start_link(),
      {ok, Pid4} = db_server:start_link(),
      {ok, Pid5} = db_server:start_link(),
      [Pid1, Pid2, Pid3, Pid4, Pid5]
    end,
    %% cleanup
    fun(Pids) ->
      lists:foreach(fun(Pid) -> db_server:stop(Pid) end, Pids),
      ok
    end,
    %% when and then
    fun(Pids) ->
      lists:foreach(
        fun({Pid, N}) ->
          db_server:write(N, N, Pid),
          {ok, Value} = db_server:read(N, Pid),
          ?assertEqual(N, Value)
        end,
        lists:zip(Pids, lists:seq(1, length(Pids)))
      )
    end}.
-endif.
