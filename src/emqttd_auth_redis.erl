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

%% @doc Authentication with Redis.
-module(emqttd_auth_redis).

-behaviour(emqttd_auth_mod).

-include("emqttd_auth_redis.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-import(emqttd_auth_redis_client, [q/2, is_superuser/2]).

-export([init/1, check/3, description/0]).

-record(state, {auth_cmd, super_cmd, hash_type}).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

init({AuthCmd, SuperCmd, HashType}) ->
    {ok, #state{auth_cmd = AuthCmd, super_cmd = SuperCmd, hash_type = HashType}}.

check(#mqtt_client{username = Username}, Password, _State)
    when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Client, Password, #state{auth_cmd  = AuthCmd,
                               super_cmd = SuperCmd,
                               hash_type = HashType}) ->
    Result = case q(AuthCmd, Client) of
                {ok, undefined} ->
                    {error, not_found};
                {ok, HashPass} ->
                    check_pass(HashPass, Password, HashType);
                {error, Reason} ->
                    {error, Reason}
             end,
    case Result of ok -> {ok, is_superuser(SuperCmd, Client)}; Error -> Error end.

check_pass(PassHash, Password, HashType) ->
    case PassHash =:= hash(HashType, Password) of
        true  -> ok;
        false -> {error, password_error}
    end.

hash(Type, Password) ->
    emqttd_auth_mod:passwd_hash(Type, Password).

description() -> "Authentication with Redis".

