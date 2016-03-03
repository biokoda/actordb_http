% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_util).

-include_lib("actordb_http/include/actordb_http.hrl").

-export([reply/3, reply/4]).

reply(Status, Content, Req) ->
  reply(Status, [], Content, Req).

reply(Status, Headers, Content, Req) ->
  cowboy_req:reply(Status, Headers, Content, Req).
