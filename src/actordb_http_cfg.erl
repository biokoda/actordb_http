% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_cfg).

-include_lib("actordb_http/include/actordb_http.hrl").

-export([config/2]).
-export([config/3]).

config(Itm, Default) ->
  case application:get_env(actordb_core, http) of
    {ok, L} when is_list(L) ->
      butil:ds_val(Itm, L, Default);
    _ ->
      Default
  end.

config(Itm, SubItm, Default) ->
  case config(Itm, undefined) of
    L when is_list(L) ->
      butil:ds_val(SubItm, L, Default);
    _ ->
      Default
  end.
