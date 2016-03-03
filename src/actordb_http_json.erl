% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_json).

-include_lib("actordb_http/include/actordb_http.hrl").

-export([handle_req/3]).
-export([handle_resp/2]).

handle_req(Json, _, #req_state{ opts = [db]}) ->
 jiffy:encode(Json);
handle_req(_, _, _) ->
  <<"">>.

handle_resp(Req, #req_state{ pool = Pool, opts = [db]}) ->
  ActorType = cowboy_req:binding(actor_type, Req, undefined),
  ActorTable = cowboy_req:binding(actor_table, Req, undefined),
  lager:info("handle db resp: ~p ~p",[ActorType, ActorTable]),
  handle_db_resp(ActorType, ActorTable, Pool);
handle_resp(_, _) ->
  [].

handle_db_resp(undefined, undefined, Pool) ->
  case actordb_http_worker:actor_types(Pool) of
    {error, _} = Err ->
      Err;
    ActorTypes ->
      {[{<<"actors">>, ActorTypes}]}
  end;
handle_db_resp(ActorType, undefined, Pool) ->
  case actordb_http_worker:actor_tables(ActorType, Pool) of
    {error, _} = Err ->
      Err;
    ActorTables ->
      {[{<<"tables">>, ActorTables}]}
  end;
handle_db_resp(ActorType, ActorTable, Pool) ->
  case actordb_http_worker:actor_table_columns(ActorType, ActorTable, Pool) of
    {error, _} = Err ->
      Err;
    ActorColumns ->
      {[{<<"columns">>, {ActorColumns}}]}
  end.
