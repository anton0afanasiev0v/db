%%%-------------------------------------------------------------------
%%% @author cat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2024 13:16
%%%-------------------------------------------------------------------
-module(db).
-author("cat").

%% API
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
  {ok, Pid} = db_server:start_link(),
  {ok, Pid}.

destroy(Pid) ->
  db_server:stop(Pid).

write(Key, Element, Pid) ->
  db_server:write(Key, Element, Pid).

delete(Key, Pid) ->
  db_server:delete(Key, Pid).

read(Key, Pid) ->
  db_server:read(Key, Pid).

match(Element, Pid) ->
  db_server:match(Element, Pid).
