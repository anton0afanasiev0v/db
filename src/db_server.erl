%%%-------------------------------------------------------------------
%%% @doc
%%% Модуль db_server реализует простой сервер базы данных с
%%% использованием ETS (Erlang Term Storage).
%%% Функции:
%%% - start_link/0: Запускает сервер.
%%% - stop/1: Останавливает сервер.
%%% - write/3: Записывает ключ-значение в базу.
%%% - delete/2: Удаляет ключ из базы.
%%% - read/2: Читает значение по ключу из базы.
%%% - match/2: Ищет ключи по значению.
%%% @end
%%% Created : 29. Jun 2024 13:17
%%%-------------------------------------------------------------------
-module(db_server).
-author("cat").
-behaviour(gen_server).

%% API
-export([start_link/0, stop/1, write/3, delete/2, read/2, match/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {ets_table}).

%%% API Functions

start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

write(Key, Element, Pid) ->
  gen_server:call(Pid, {write, Key, Element}).

delete(Key, Pid) ->
  gen_server:call(Pid, {delete, Key}).

read(Key, Pid) ->
  gen_server:call(Pid, {read, Key}).

match(Element, Pid) ->
  gen_server:call(Pid, {match, Element}).

%%% gen_server Callbacks

init([]) ->
  ets_table = ets:new(unique_name(), [set, public]),
  {ok, #state{ets_table = ets_table}}.

unique_name() ->
  list_to_atom("db_table_" ++ integer_to_list(erlang:unique_integer([monotonic, positive]))).

handle_call({write, Key, Element}, _From, State) ->
  ets:insert(State#state.ets_table, {Key, Element}),
  {reply, ok, State};

handle_call({delete, Key}, _From, State) ->
  ets:delete(State#state.ets_table, Key),
  {reply, ok, State};

handle_call({read, Key}, _From, State) ->
  case ets:lookup(State#state.ets_table, Key) of
    [{Key, Element}] -> {reply, {ok, Element}, State};
    [] -> {reply, {error, not_found}, State}
  end;

handle_call({match, Element}, _From, State) ->
  MatchFun = fun({K, E}) when E =:= Element -> K; (_) -> false end,
  Keys = [K || {K, E} <- ets:tab2list(State#state.ets_table), MatchFun({K, E}) =/= false],
  {reply, Keys, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  ets:delete(State#state.ets_table),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
