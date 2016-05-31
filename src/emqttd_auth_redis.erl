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

-include("../../../include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-record(state, {super_cmd, auth_cmd, hash_type}).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

init({SuperCmd, AuthCmd, HashType}) -> 
    {ok, #state{super_cmd = SuperCmd, auth_cmd = AuthCmd, hash_type = HashType}}.

check(#mqtt_client{username = Username}, _Password, _State) when ?UNDEFINED(Username) ->
    {error, username_undefined};

check(Client, Password, #state{super_cmd = SuperCmd}) when ?UNDEFINED(Password) ->
    case emqttd_plugin_redis_client:is_superuser(SuperCmd, Client) of
        true  -> ok;
        false -> {error, password_undefined}
    end;

check(Client, Password, #state{super_cmd = SuperCmd,
                               auth_cmd  = AuthCmd,
                               hash_type = HashType}) ->
    case emqttd_plugin_redis_client:is_superuser(SuperCmd, Client) of
        false -> case emqttd_plugin_redis_client:query(AuthCmd, Client) of
                     {ok, undefined} ->
                         {error, not_found};
                     {ok, HashPass} ->
                         check_pass(HashPass, Password, HashType);
                     {error, Error} ->
                         {error, Error}
                 end;
        true  -> ok
    end.

check_pass(PassHash, Password, HashType) ->
    case PassHash =:= hash(HashType, Password) of
        true  -> ok;
        false -> {error, password_error}
    end.

hash(Type, Password) ->
    emqttd_auth_mod:passwd_hash(Type, Password).

description() -> "Authentication with Redis".

