% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_req).

-include_lib("actordb_http/include/actordb_http.hrl").

-export([init/2]).
-export([terminate/3]).

-export([is_authorized/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).

-export([from_json/2]).
-export([to_json/2]).

-export([from_msgpack/2]).
-export([to_msgpack/2]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  Peer = cowboy_req:peer(Req),
  State = #req_state{ path = Path, peer = Peer, opts = Opts },
  lager:debug("~p ~s processing, opts: ~p",[State#req_state.peer, State#req_state.path, State#req_state.opts]),
  {cowboy_rest, Req, State}.

terminate(_Reason, _Req, _State) ->
  lager:debug("~p ~s done.",[_State#req_state.peer, _State#req_state.path]),
	ok.

is_authorized(Req, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
		{basic, User, Pw} ->
      case actordb_http_sup:ensure_pool(?B(User), ?B(Pw)) of
        {error, invalid_login} ->
          {{false, <<"Basic realm=\"actordb\"">>}, Req, State};
        true ->
          {true, Req, State#req_state{ pool = actordb_http_sup:pool_for(User) }}
      end;
		_ ->
			{{false, <<"Basic realm=\"actordb\"">>}, Req, State}
	end.

allowed_methods(Req, State) ->
	{[<<"GET">>,<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  Accepted = [
    {{<<"application">>, <<"json">>, '*'}, from_json},
    {{<<"application">>, <<"x-msgpack">>, '*'}, from_msgpack}
  ],
  {Accepted, Req, State}.

content_types_provided(Req, State) ->
  Provided = [
    {{<<"application">>, <<"json">>, '*'}, to_json},
    {{<<"application">>, <<"x-msgpack">>, '*'}, to_msgpack}
  ],
  {Provided, Req, State}.

from_json(Req, State) ->
	{ok, Body, Req1} = cowboy_req:body(Req),
	Json = jiffy:decode(Body,[return_maps]),
	case actordb_http_exec:handle_req(Json, Req1, State) of
    {error, _} ->
      Req2 = ?REPLY(400, <<>>, Req1),
      {stop, Req2, State};
    {reply, Reply} ->
      JsonReply = jiffy:encode(Reply),
      Req2 = cowboy_req:set_resp_body(JsonReply, Req1),
      {true, Req2, State}
  end.

to_json(Req, State) ->
	case actordb_http_exec:handle_resp(Req, State) of
    {error, _} ->
      Req1 = ?REPLY(400, <<>>, Req),
      {stop, Req1, State};
    {reply, Reply} ->
      JsonReply = jiffy:encode(Reply),
      {JsonReply, Req, State}
  end.

from_msgpack(Req, State) ->
  {ok, BodyPacked, Req1} = cowboy_req:body(Req),
  case msgpack:unpack(BodyPacked) of
    {ok, Unpacked} ->
      case actordb_http_exec:handle_req(Unpacked, Req1, State) of
        {error, _} ->
          Req2 = ?REPLY(400, <<>>, Req1),
          {stop, Req2, State};
        {reply, Reply} ->
          MsgPacked = msgpack:pack(Reply),
          Req2 = cowboy_req:set_resp_body(MsgPacked, Req1),
          {true, Req2, State}
      end;
    {error, Reason} ->
      lager:debug("error unpacking message=~p, reason=~p",[BodyPacked, Reason]),
      Req2 = ?REPLY(400, <<>>, Req1),
      {stop, Req2, State}
  end.



to_msgpack(Req, State) ->
  case actordb_http_exec:handle_resp(Req, State) of
    {error, _} ->
      Req1 = ?REPLY(400, <<>>, Req),
      {stop, Req1, State};
    {reply, Reply} ->
      MsgPacked = msgpack:pack(Reply),
      {MsgPacked, Req, State}
  end.
