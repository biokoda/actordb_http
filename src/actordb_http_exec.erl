% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_exec).

-include_lib("actordb_http/include/actordb_http.hrl").

-export([req/3]).
-export([resp/2]).

%%
%%  API
%%
req(Data, Req, #req_state{ pool = Pool, opts = [exec], data_type = DataType}) ->
  Binding = cowboy_req:binding(exec_type, Req, undefined),
  exec(Binding, Data, DataType, Pool);
req(_, _, _) ->
  {error, unknown}.

resp(Req, #req_state{ pool = Pool, opts = [db], data_type = DataType}) ->
  ActorType = cowboy_req:binding(actor_type, Req, undefined),
  ActorTable = cowboy_req:binding(actor_table, Req, undefined),
  db(ActorType, ActorTable, DataType, Pool);
resp(_, _) ->
  {error, unknown}.

%% @private
db(undefined, undefined, DataType, Pool) ->
  R = actordb_http_worker:actor_types(Pool),
  build_response(R, DataType, fun(ActorTypes) ->
      #{<<"actors">> => ActorTypes }
  end);
db(ActorType, undefined, DataType, Pool) ->
  R = actordb_http_worker:actor_tables(ActorType, Pool),
  build_response(R, DataType, fun(ActorTables) ->
    #{<<"tables">> => ActorTables}
  end);
db(ActorType, ActorTable, DataType, Pool) ->
  R = actordb_http_worker:actor_table_columns(ActorType, ActorTable, Pool),
  build_response(R, DataType, fun(ActorColumns) ->
    #{<<"columns">> => [ #{ K => V} || {K, V}<- ActorColumns]}
  end).

%% @private
exec(<<"exec">>, Data, DataType, Pool) ->
  exec_sql(Data, DataType, Pool);
exec(<<"exec_single">>, Data, DataType, Pool) ->
  exec_single(Data, DataType, Pool);
exec(<<"exec_single_param">>, Data, DataType, Pool) ->
  exec_single_param(Data, DataType, Pool);
exec(<<"exec_multi">>, Data, DataType, Pool) ->
  exec_multi(Data, DataType, Pool);
exec(Typ, _, _, _) ->
  {error, {bad_exec_type,Typ}}.

exec_sql(#{<<"q">> := Sql}, DataType, Pool) ->
  lager:debug("exec_sql, q=~p",[Sql]),
  R = actordb_http_worker:exec_sql(Sql, Pool, datatype_to_opts(DataType)),
  query_response(R, DataType);
exec_sql(_, _, _) ->
  {error, bad_exec_data}.

exec_single(#{<<"id">> := Actor, <<"flags">> := Flags, <<"type">> := Type, <<"q">> := Sql}, DataType, Pool) ->
  lager:debug("exec_single, id=~p flags=~p type=~p sql=~p",[Actor, Flags, Type, Sql]),
  R = actordb_http_worker:exec_single(Actor, Type, Flags, Sql, Pool, datatype_to_opts(DataType)),
  query_response(R, DataType);
exec_single(_, _, _) ->
  {error, bad_exec_single_data}.

exec_single_param(#{<<"id">> := Actor, <<"flags">> := Flags, <<"type">> := Type, <<"q">> := Sql, <<"p">> := BindingVals}, DataType, Pool) ->
  lager:debug("exec_single_param, id=~p flags=~p type=~p bindings=~p sql=~p",[Actor, Flags, Type, BindingVals, Sql]),
  R = actordb_http_worker:exec_single_param(Actor, Type, Flags, Sql, [BindingVals], Pool, datatype_to_opts(DataType)),
  query_response(R, DataType);
exec_single_param(_, _, _) ->
  {error, bad_exec_single_param_data}.

exec_multi(#{<<"ids">> := Actors, <<"flags">> := Flags, <<"type">> := Type, <<"q">> := Sql}, DataType, Pool) ->
  lager:debug("exec_multi, ids=~p flags=~p type=~p bindings=~p sql=~p",[Actors, Flags, Type, Sql]),
  R = actordb_http_worker:exec_multi(Actors, Type, Flags, Sql, Pool, datatype_to_opts(DataType)),
  query_response(R, DataType);
exec_multi(_, _, _) ->
  {error, bad_exec_multi_data}.

%% @private
datatype_to_opts(json) ->
  [blob2base64];
datatype_to_opts(msgpack) ->
  [];
datatype_to_opts(_) ->
  [].

-spec query_response( Result :: map() | [map()], DataType :: json | msgpack) -> map().
%%
query_response(R, DataType) ->
  lager:debug("reply=~p",[R]),
  build_response(R, DataType, fun(SR) ->
    case SR of
      #{ last_change_rowid := LastId, rows_changed := NChanged} ->
        #{ <<"result">> => null,
           <<"meta">> => [
              #{<<"last_change_rowid">> => LastId},
              #{<<"rows_changed">> => NChanged}
            ]
          };
      #{ rows := Rows, has_more := HasMore } ->
        #{ <<"result">> => Rows,
           <<"meta">> => [
              #{<<"has_more">> => HasMore}
            ]
          }
    end
  end).


%% @private
% build_response(R) ->
%   build_response(R, undefined).

build_response({error, _} = Err, _, _) ->
  Err;
build_response(Result, _DataType, Fun) when is_function(Fun) ->
  {reply, Fun(Result)};
build_response(Result, _DataType, _) ->
  {reply, Result}.
