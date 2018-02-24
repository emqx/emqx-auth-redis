%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_auth_redis_cli).

-behaviour(ecpool_worker).

-include_lib("emqx/include/emqx.hrl").

-define(ENV(Key, Opts), proplists:get_value(Key, Opts)).

-export([connect/1, q/2]).

%%--------------------------------------------------------------------
%% Redis Connect/Query
%%--------------------------------------------------------------------

connect(Opts) ->
    Sentinel = ?ENV(sentinel, Opts),
    Host = case Sentinel =:= "" of
        true -> ?ENV(host, Opts);
        false ->
            eredis_sentinel:start_link([{?ENV(host, Opts), ?ENV(port, Opts)}]),
            "sentinel:" ++ Sentinel
    end,
    eredis:start_link(Host,
                      ?ENV(port, Opts),
                      ?ENV(database, Opts),
                      ?ENV(password, Opts),
                      no_reconnect).

%% Redis Query.
-spec(q(string(), mqtt_client()) -> {ok, undefined | binary() | list()} | {error, atom() | binary()}).
q(CmdStr, Client) ->
    Cmd = string:tokens(replvar(CmdStr, Client), " "),
    ecpool:with_client(emqx_auth_redis, fun(C) -> eredis:q(C, Cmd) end).

replvar(Cmd, #mqtt_client{client_id = ClientId, username = Username}) ->
    replvar(replvar(Cmd, "%u", Username), "%c", ClientId).

replvar(S, _Var, undefined) ->
    S;
replvar(S, Var, Val) ->
    re:replace(S, Var, Val, [{return, list}]).

