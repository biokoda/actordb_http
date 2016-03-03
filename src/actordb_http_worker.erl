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

-export([exec_sql/2]).

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
handle_call({exec_sql, Sql}, _, State) ->
  Bp = handle_bp(State),
  Result = (catch actordb:exec_bp(Bp, Sql)),
  lager:debug("sql :~p result: ~p",[Sql, Result]),
  {reply, db_res(Sql, Result), State};
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
  execute_call({exec_sql, Sql}, Pool).

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
db_res(_Sql, {_WhatNow,{ok,[{columns,[]},{rows,[]}]}}) ->
	Cols = [],
	Rows = [#{}],
  #{ has_more => false, columns => Cols, rows => Rows };
db_res(_Sql, {_WhatNow,{ok,[[_,_] = R|_]}}) ->
	db_res(_Sql, {_WhatNow,{ok,R}});
db_res(_Sql, {_WhatNow,{ok,[{columns,Cols1},{rows,Rows1}]}}) ->
	Cols = tuple_to_list(Cols1),
	Rows = [maps:from_list(lists:zip(Cols,[val(Val) || Val <- tuple_to_list(R)])) || R <- lists:reverse(Rows1)],
  #{ has_more => false, columns => Cols, rows => Rows };
db_res(_Sql, {_WhatNow,{ok,{changes,LastId,NChanged}}}) ->
  #{ last_change_rowid => LastId, rows_changed => NChanged};
db_res(_Sql, {_WhatNow,{ok,[{changes,_,_} = H|_]}}) ->
	db_res(_Sql, {_WhatNow,{ok,H}});
db_res(_Sql,{ok,{sql_error,E}}) ->
	db_res(_Sql,{sql_error,E});
db_res(_Sql,{ok,{error,E}}) ->
	db_res(_Sql,{error,E});
db_res(_Sql, {'EXIT',_Exc}) ->
  {error, {internal, exception}};
db_res(_Sql, Err) ->
  lager:error("execute exception: ~p~n",[Err]),
  {error, {internal, actordb_err_desc:desc(Err)}}.

%% @private
val({blob,V}) ->
	V;
val(undefined) ->
	null;
val(true) ->
	true;
val(false) ->
	false;
val(V) when is_float(V) ->
	V;
val(V) when is_binary(V); is_list(V) ->
	V;
val(V) when is_integer(V) ->
	V.
