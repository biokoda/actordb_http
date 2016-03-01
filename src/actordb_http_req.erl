% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_req).

-include_lib("include/actordb_http.hrl").

-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
	Path = cowboy_req:path(Req),
	Req2 = handle(Path, Req),
  {ok, Req2, Opts}.

handle(Path, Req) ->
  lager:debug("API unhandled request: ~p",[Path]),
  ?REPLY(404, <<"404">>, Req).

terminate(_Reason, _Req, _State) ->
	ok.
