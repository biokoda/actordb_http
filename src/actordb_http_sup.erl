% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_http_sup).

-include_lib("actordb_http/include/actordb_http.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([ensure_pool/2]).
-export([start_pool/2]).
-export([pool_for/1]).

%% Supervisor callbacks
-export([init/1]).

-define(ADB_HTTPAUTH_ETS,actordb_http_auth).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  ets:new(?ADB_HTTPAUTH_ETS, [{write_concurrency,true},named_table,public,set,{heir,self(),<<>>}]),
  SupFlags = #{ strategy => one_for_one, intensity => 10, period => 1},
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

ensure_pool(Username, Password) ->
%  lager:debug("ensure pool for ~p",[Username]),
  case ets:lookup(?ADB_HTTPAUTH_ETS,{Username, Password}) of
    [{{Username, Password},true}] ->
      true;
    [] ->
      start_pool(Username, Password)
  end.

pool_for(Username) when is_binary(Username) ->
  pool_name(Username);
pool_for(Username) ->
  pool_for(butil:tobin(Username)).

start_pool(Username, Password) ->
%  lager:debug("setting up pool for ~p",[Username]),
  case catch handle_login(Username, Password) of
    true ->
      ets:insert(?ADB_HTTPAUTH_ETS, {{Username, Password}, true}),
      Name = pool_name(Username),% {actordb_http_sup_pool, Username},
      PoolArgs = [
        {name, {local, pool_name(Username)}},
        {size, actordb_http_cfg:config(pool_size, 10)},
        {max_overflow, actordb_http_cfg:config(max_pool_size, 20)},
        {worker_module, actordb_http_worker}
      ],
      WorkerArgs = [
        Username, Password
      ],
      ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
      {ok, _} = supervisor:start_child(whereis(?MODULE), ChildSpec),
      true;
    _X ->
%      io:format("state: ~p",[_X]),
      {error, invalid_login}
  end.

handle_login(Username, Pw) ->
  handle_login1(Username, Pw, actordb_sharedstate:read_global_users()).

handle_login1(_, _, nostate) ->
  error;
handle_login1(_, _, []) ->
  error;
handle_login1(Username, Pw, Users) ->
  case lists:keyfind(butil:tobin(Username), 1, Users) of
    {_, Pw, _} ->
      true;
    _ ->
      false
  end.

pool_name(Username) when is_binary(Username) ->
  butil:toatom(<<"actordb_http_pool_",Username/binary>>);
pool_name(Username) ->
  pool_name(butil:tobin(Username)).
