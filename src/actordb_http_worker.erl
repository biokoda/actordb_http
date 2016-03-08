% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_worker).

-include_lib("actordb_http/include/actordb_http.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([actor_types/1]).
-export([actor_tables/2]).
-export([actor_table_columns/3]).

-export([exec_sql/2, exec_sql/3]).
-export([exec_single/5, exec_single/6]).
-export([exec_single_param/6, exec_single_param/7]).
-export([exec_multi/5, exec_multi/6]).

-record(state, { user, bp }).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([Username, Password] = _Args) ->
  lager:debug("initialized a new worker for: ~p",[_Args]),
  case actordb_backpressure:start_caller(Username, Password, <<>>) of
    State when element(1, State) == caller ->
      {ok, #state{ user = Username, bp = State }};
    _Othr ->
      lager:debug("stopped worker for user=~p, reason=~p",[Username, _Othr]),
      {stop, invalid_login}
  end.

handle_call({actor_types}, _, State) ->
  case actordb:types() of
		schema_not_loaded ->
			{reply, [], State};
		L ->
			{reply, [atom_to_binary(A, utf8) || A <- L], State}
	end;
handle_call({actor_tables, ActorType}, _, State) ->
  ActorTables = case catch actordb:tables(ActorType) of
    {'EXIT', Err} ->
      lager:error("actor_tables couldn't resolve for ~s: ~p",[ActorType, Err]),
      {error, internal};
    schema_not_loaded ->
      lager:error("actor_tables couldn't resolve for ~s: ~p",[ActorType, schema_not_loaded]),
      {error, internal};
    Val ->
      Val
  end,
  {reply, ActorTables, State};
handle_call({actor_table_columns, ActorType, ActorTable}, _, State) ->
  ActorTableColumns = case catch actordb:columns(ActorType, ActorTable) of
    {'EXIT', Err} ->
      lager:error("actor_table_columns couldn't resolve for ~s.~s: ~p",[ActorType, ActorTable, Err]),
      {error, internal};
    schema_not_loaded ->
      lager:error("actor_table_columns couldn't resolve for ~s.~s: ~p",[ActorType, ActorTable, schema_not_loaded]),
      {error, internal};
    Val ->
      Val
  end,
  {reply, ActorTableColumns, State};
handle_call({exec_sql, Sql, Opts}, _, State) ->
  Bp = handle_bp(State),
  Result = (catch actordb:exec_bp(Bp, Sql)),
%  lager:debug("sql :~p result: ~p",[Sql, Result]),
  {reply, db_res(Sql, Result, Opts), State};

handle_call({exec_single, Actor, Type, Flags, Sql, Opts}, _, State) ->
  Bp = handle_bp(State),
  Result = (catch actordb:exec_bp(Bp, Actor, Type, flags(Flags), Sql)),
  {reply, db_res(Sql, Result, Opts), State};

handle_call({exec_single_param, Actor, Type, Flags, Sql, BindingVals, Opts}, _, State) ->
  Bp = handle_bp(State),
  Result = (catch actordb:exec_bp(Bp, Actor, Type, flags(Flags), Sql, bindings(BindingVals))),
  {reply, db_res(Sql, Result, Opts), State};

handle_call({exec_multi, Actors, Type, Flags, Sql, Pool, Opts}, _, State) ->
  Bp = handle_bp(State),
  Result = (catch actordb:exec_bp(Bp, Actors, Type, flags(Flags), Sql)),
  {reply, db_res(Sql, Result, Opts), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{ bp = _State }) ->
  lager:debug("worker terminated"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% statements/query level
bindings(L) ->
  [bindings_q(X) || X <- L].

% query-repeat level
bindings_q(S) ->
  [bindings_int(S1) || S1 <- S].

% query values
bindings_int(Q) ->
  [bindval(I) || I <- Q].

bindval(#{<<"type">> := <<"blob">>, <<"encoding">> := <<"base64">>, <<"value">> := V}) ->
  {blob, base64:decode(V)};
bindval(#{<<"type">> := <<"blob">>, <<"encoding">> := <<"none">>, <<"value">> := V}) ->
  {blob, V};
bindval(V) when is_map(V) ->
  throw({error, {bad_bind_value, V}});
bindval(V) ->
  V.

%%
%%  API
%%

actor_types(Pool) ->
  execute_call({actor_types}, Pool).

actor_tables(ActorType, Pool) ->
  execute_call({actor_tables, ActorType}, Pool).

actor_table_columns(ActorType, ActorTable, Pool) ->
  execute_call({actor_table_columns, ActorType, ActorTable}, Pool).

exec_sql(Sql, Pool) ->
  exec_sql(Sql, Pool, []).
exec_sql(Sql, Pool, Opts) ->
  execute_call({exec_sql, Sql, Opts}, Pool).

exec_single(Actor, Type, Flags, Sql, Pool) ->
  exec_single(Actor, Type, Flags, Sql, Pool, []).
exec_single(Actor, Type, Flags, Sql, Pool, Opts) ->
  execute_call({exec_single, Actor, Type, Flags, Sql, Opts}, Pool).

exec_single_param(Actor, Type, Flags, Sql, BindingVals, Pool) ->
  exec_single_param(Actor, Type, Flags, Sql, BindingVals, Pool, []).
exec_single_param(Actor, Type, Flags, Sql, BindingVals, Pool, Opts) ->
  execute_call({exec_single_param, Actor, Type, Flags, Sql, BindingVals, Opts}, Pool).

exec_multi(Actors, Type, Flags, Sql, Pool) ->
  exec_multi(Actors, Type, Flags, Sql, Pool, []).
exec_multi(Actors, Type, Flags, Sql, Pool, Opts) ->
  execute_call({exec_multi, Actors, Type, Flags, Sql, Pool, Opts}, Pool).

%% @private
execute_call(Message, Pool) ->
  poolboy:transaction(Pool, fun(Worker) ->
      gen_server:call(Worker, Message)
  end).

%% @private
handle_bp(#state{ bp = Bp} = State) ->
  case actordb:check_bp() of
		sleep ->
			actordb:sleep_bp(Bp),
			handle_bp(State);
		ok ->
			Bp
	end.

%% @private
db_res(_Sql, {_WhatNow,{ok,[{columns,[]},{rows,[]}]}}, _) ->
	Cols = [],
	Rows = [#{}],
  #{ has_more => false, columns => Cols, rows => Rows };
db_res(_Sql, {_WhatNow,{ok,[[_,_] = R|_]}}, Opts) ->
	db_res(_Sql, {_WhatNow,{ok,R}}, Opts);
db_res(_Sql, {_WhatNow,{ok,[{columns,Cols1},{rows,Rows1}]}}, Opts) ->
	Cols = tuple_to_list(Cols1),
	Rows = [maps:from_list(lists:zip(Cols,[val(Val,Opts) || Val <- tuple_to_list(R)])) || R <- lists:reverse(Rows1)],
  #{ has_more => false, columns => Cols, rows => Rows };
db_res(_Sql, {_WhatNow,{ok,{changes,LastId,NChanged}}}, _) ->
  #{ last_change_rowid => LastId, rows_changed => NChanged};
db_res(_Sql, {_WhatNow,{ok,[{changes,_,_} = H|_]}}, Opts) ->
	db_res(_Sql, {_WhatNow,{ok,H}}, Opts);
db_res(_Sql,{ok,{sql_error,E}}, Opts) ->
	db_res(_Sql,{sql_error,E}, Opts);
db_res(_Sql,{ok,{error,E}}, Opts) ->
	db_res(_Sql,{error,E}, Opts);
db_res(_Sql, {'EXIT',_Exc}, _) ->
  {error, {internal, exception}};
db_res(_Sql, Err, _) ->
  lager:error("execute exception: ~p~n",[Err]),
  {error, {internal, actordb_err_desc:desc(Err)}}.

%% @private
val({blob,V}, Opts) ->
  case lists:member(blob2base64, Opts) of
    true ->
      #{ <<"type">> => <<"blob">>, <<"encoding">> => <<"base64">>, <<"value">> => base64:encode(V)};
    false ->
      #{ <<"type">> => <<"blob">>, <<"encoding">> => <<"none">>, <<"value">> => base64:encode(V)}
  end;
val(undefined,_) ->
	null;
val(true,_) ->
	true;
val(false,_) ->
	false;
val(V,_) when is_float(V) ->
	V;
val(V,_) when is_binary(V); is_list(V) ->
	V;
val(V,_) when is_integer(V) ->
	V.

flags([H|T]) ->
	actordb_sqlparse:check_flags(H,[])++flags(T);
flags([]) ->
	[].
