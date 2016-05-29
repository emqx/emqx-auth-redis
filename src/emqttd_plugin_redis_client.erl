%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
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

%% @doc emqttd redis client
-module(emqttd_plugin_redis_client).

-behaviour(ecpool_worker).

-include("../../../include/emqttd.hrl").

-import(proplists, [get_value/2]).

-export([is_superuser/2, connect/1, query/1, query/2]).

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | list(), mqtt_client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser(SuperCmd, Client) ->
    case query(SuperCmd, Client) of
        {ok, undefined} -> false;
        {ok, <<"1">>}   -> true;
        {ok, _Other}    -> false;
        {error, _Error} -> false
    end.

%%--------------------------------------------------------------------
%% Redis Connect/Query
%%--------------------------------------------------------------------

connect(Opts) ->
    eredis:start_link(get_value(host, Opts),
                      get_value(port, Opts),
                      get_value(database, Opts),
                      get_value(password, Opts),
                      no_reconnect).

%% Redis Query.
-spec(query(list()) -> {ok, undefined | binary() | list()} | {error, atom() | binary()}).
query(Cmd) ->
    ecpool:with_client(emqttd_plugin_redis, fun(C) -> eredis:q(C, Cmd) end).

-spec(query(list(), mqtt_client()) -> {ok, undefined | binary() | list()} | {error, atom() | binary()}).
query(Cmd, Client) ->
    ecpool:with_client(emqttd_plugin_redis, fun(C) -> eredis:q(C, replvar(Cmd, Client)) end).

replvar(Cmd, #mqtt_client{client_id = ClientId, username = Username}) ->
    [replvar(replvar(S, "%u", Username), "%c", ClientId) || S <- Cmd].

replvar(S, _Var, undefined) ->
    S;
replvar(S, Var, Val) ->
    re:replace(S, Var, Val, [{return, binary}]).

