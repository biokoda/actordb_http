% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-compile([{parse_transform, lager_transform}]).

-define(REPLY(S,C,R), actordb_http_util:reply(S,C,R)).
-define(REPLY(S,H,C,R), actordb_http_util:reply(S,H,C,R)).

-define(B(X), butil:tobin(X)).

-record(req_state,{ peer = undefined, path = undefined, opts = [], pool = undefined, data_type = undefined }).
