%%%-------------------------------------------------------------------
%%% @author cat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2024 13:20
%%%-------------------------------------------------------------------
-module(db_tests).
-author("cat").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


should_success_destroy_test() ->
  {setup,
    %%  given
    fun() ->
      {ok, Pid} = db:new(),
      Pid
    end,

    %%  when
    fun(Pid) ->
      db:destroy(Pid)
    end,

    %% then
    fun(Pid) ->
      ?assertEqual(undefined, Pid)
    end}.

should_success_write_read_test() ->
  {setup,
    fun() ->
      %%  given
      {ok, Pid} = db:new(),
      Pid
    end,
    fun(Pid) ->
      db:destroy(Pid)
    end,
    fun(Pid) ->
      %%  when
      db:write(test_key, test_value, Pid),
      {ok, Value} = db:read(test_key, Pid),

      %% then
      ?assertEqual(test_value, Value)
    end}.

% Test for deleting from the DB
should_success_delete_test() ->
  {setup,
    fun() ->
      %%  given
      {ok, Pid} = db:new(),
      Pid
    end,
    fun(Pid) ->
      db:destroy(Pid)
    end,
    fun(Pid) ->
      %%  when
      db:write(test_key, test_value, Pid),
      db:delete(test_key, Pid),

      %% then
      % Ensure the key is deleted
      {error, not_found} = db:read(test_key, Pid),
      ?assert(true)
    end}.

% Test for matching in the DB
should_success_match_test() ->
  {setup,
    fun() ->
      %%  given
      {ok, Pid} = db:new(),
      Pid
    end,
    fun(Pid) ->
      db:destroy(Pid)
    end,
    fun(Pid) ->
      %%  when
      db:write(key1, value, Pid),
      db:write(key2, value, Pid),
      db:write(key3, different_value, Pid),
      Keys = db:match(value, Pid),

      %% then
      ?assertEqual([key1, key2], lists:sort(Keys))
    end}.
-endif.