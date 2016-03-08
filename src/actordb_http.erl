% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http).

-include_lib("actordb_http/include/actordb_http.hrl").

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
  case actordb_http_cfg:config(http_interfaces, undefined) of
    HttpIfaces when is_list(HttpIfaces) ->
      [begin
        start_http_listener(Iface)
      end || Iface <- HttpIfaces];
    _ ->
      ok
  end,
  actordb_http_sup:start_link().

stop(_) ->
  case actordb_http_cfg:config(http_interfaces, undefined) of
    HttpIfaces when is_list(HttpIfaces) ->
      [begin
        stop_http_listener(Iface)
      end || Iface <- HttpIfaces];
    _ ->
      ok
  end,
  ok.

-spec start_http_listener({inet:ip_address(),integer()}) -> ok.
start_http_listener({{_,_,_,_},_} = C)  ->
  start_http_listener1(C);
start_http_listener({{_,_,_,_,_,_,_,_},_} = C)  ->
  start_http_listener1(C);
start_http_listener(C) ->
  lager:error("bad http listener configuration: ~p",[C]),
  throw({bad_http_config, C}).

stop_http_listener({_,_} = Id) ->
  cowboy:stop_listener(interface_id(Id)).

interface_id({_,_} = Id) ->
  IdBin = butil:tobin(["http-" , io_lib:format("~p",[Id])]),
  butil:toatom(IdBin).

start_http_listener1({Interface, Port} = Id) ->
  ListenerId = interface_id(Id),
  lager:info("starting API http listener with id= ~p interface= ~p, port= ~p",[ListenerId, Interface, Port]),
  HttpOpts = [
    {port, Port},
    {timeout, 5000},
    {max_connections, 1000}],
  {ok, _} = cowboy:start_http(ListenerId, 10, HttpOpts, [{env, [{dispatch, dispatch_rules()} ]}, {max_keepalive, 1000}]),
  ok.

dispatch_rules() ->
  Rules =
    [{'_',[
      {"/v1/_db/:actor_type[/:actor_table]", actordb_http_req, [db]},
      {"/v1/_db", actordb_http_req, [db]},
      {"/v1/q/:exec_type", actordb_http_req, [exec]},
      {"/v1/r/:actor_type/:actor_id/:actor_table", actordb_http_req, [resource]},
      {"/[...]", actordb_http_req, []}
    ]}],
  cowboy_router:compile(Rules).
