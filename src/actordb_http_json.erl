% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_json).

-include_lib("actordb_http/include/actordb_http.hrl").

-export([handle_req/3]).
-export([handle_resp/2]).

%%
%%  API
%%
handle_req(Json, Req, #req_state{ pool = Pool, opts = [exec]}) ->
  Binding = cowboy_req:binding(exec_type, Req, undefined),
  lager:debug("exec binding: ~p", [Binding]),
  exec(Binding, Json, Pool);
%
%
handle_req(_, _, _) ->
  {error, unknown}.

handle_resp(Req, #req_state{ pool = Pool, opts = [db]}) ->
  ActorType = cowboy_req:binding(actor_type, Req, undefined),
  ActorTable = cowboy_req:binding(actor_table, Req, undefined),
  db(ActorType, ActorTable, Pool);
handle_resp(_, _) ->
  {error, unknown}.

%% @private
db(undefined, undefined, Pool) ->
  R = actordb_http_worker:actor_types(Pool),
  build_response(R, fun(ActorTypes) ->
      #{<<"actors">> => ActorTypes }
  end);
db(ActorType, undefined, Pool) ->
  R = actordb_http_worker:actor_tables(ActorType, Pool),
  build_response(R, fun(ActorTables) ->
    #{<<"tables">> => ActorTables}
  end);
db(ActorType, ActorTable, Pool) ->
  R = actordb_http_worker:actor_table_columns(ActorType, ActorTable, Pool),
  build_response(R, fun(ActorColumns) ->
    #{<<"columns">> => [ #{ K => V} || {K, V}<- ActorColumns]}
  end).

%% @private
exec(<<"exec">>, Json, Pool) ->
  exec_sql(Json, Pool);
exec(<<"exec_single">>, Json, Pool) ->
  exec_single(Json, Pool);
exec(<<"exec_single_param">>, Json, Pool) ->
  exec_single_param(Json, Pool);
exec(<<"exec_multi">>, Json, Pool) ->
  exec_multi(Json, Pool);
exec(Typ, _, _) ->
  {error, {bad_exec_type,Typ}}.

exec_sql(#{<<"q">> := Sql}, Pool) ->
  lager:debug("exec_sql, q=~p",[Sql]),
  R = actordb_http_worker:exec_sql(Sql, Pool,[blob2base64]),
  query_response(R);
exec_sql(_,_) ->
  {error, bad_exec_data}.

exec_single(#{<<"id">> := Actor, <<"flags">> := Flags, <<"type">> := Type, <<"q">> := Sql}, Pool) ->
  lager:debug("exec_single, id=~p flags=~p type=~p sql=~p",[Actor, Flags, Type, Sql]),
  R = actordb_http_worker:exec_single(Actor, Type, Flags, Sql, Pool,[blob2base64]),
  query_response(R);
exec_single(_,_) ->
  {error, bad_exec_single_data}.

exec_single_param(#{<<"id">> := Actor, <<"flags">> := Flags, <<"type">> := Type, <<"q">> := Sql, <<"p">> := BindingVals}, Pool) ->
  lager:debug("exec_single_param, id=~p flags=~p type=~p bindings=~p sql=~p",[Actor, Flags, Type, BindingVals, Sql]),
  R = actordb_http_worker:exec_single_param(Actor, Type, Flags, Sql, [BindingVals], Pool, [blob2base64]),
  query_response(R);
exec_single_param(_,_) ->
  {error, bad_exec_single_param_data}.

exec_multi(#{<<"ids">> := Actors, <<"flags">> := Flags, <<"type">> := Type, <<"q">> := Sql}, Pool) ->
  lager:debug("exec_multi, ids=~p flags=~p type=~p bindings=~p sql=~p",[Actors, Flags, Type, Sql]),
  R = actordb_http_worker:exec_multi(Actors, Type, Flags, Sql, Pool, [blob2base64]),
  query_response(R);
exec_multi(_, _) ->
  {error, bad_exec_multi_data}.

query_response(R) ->
  lager:debug("reply=~p",[R]),
  build_response(R, fun(SR) ->
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

build_response({error, _} = Err, _) ->
  Err;
build_response(Result, Fun) when is_function(Fun) ->
  {reply, Fun(Result)};
build_response(Result, _) ->
  {reply, Result}.
