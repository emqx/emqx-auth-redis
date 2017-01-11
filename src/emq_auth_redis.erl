%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 Feng Lee <feng@emqtt.io>.
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

-module(emq_auth_redis).

-behaviour(emqttd_auth_mod).

-include("emq_auth_redis.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-define(UNDEFINED(S), (S =:= undefined)).

-record(state, {auth_cmd, super_cmd, hash_type}).

init({AuthCmd, SuperCmd, HashType}) ->
    {ok, #state{auth_cmd = AuthCmd, super_cmd = SuperCmd, hash_type = HashType}}.

check(#mqtt_client{username = Username}, Password, _State)
    when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Client, Password, #state{auth_cmd  = AuthCmd,
                               super_cmd = SuperCmd,
                               hash_type = HashType}) ->
    Result = case emq_auth_redis_cli:q(AuthCmd, Client) of
                {ok, undefined} ->
                    {error, not_found};
                {ok, HashPass} ->
                    check_pass(HashPass, Password, HashType);
                {error, Reason} ->
                    {error, Reason}
             end,
    case Result of ok -> {ok, is_superuser(SuperCmd, Client)}; Error -> Error end.

check_pass(PassHash, Password, HashType) ->
    case emqttd_auth_mod:passwd_hash(HashType, Password) of
        PassHash -> ok;
        _ErrHash -> {error, password_error}
    end.

description() -> "Authentication with Redis".

-spec(is_superuser(undefined | list(), mqtt_client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser(SuperCmd, Client) ->
    case emq_auth_redis_cli:q(SuperCmd, Client) of
        {ok, undefined} -> false;
        {ok, <<"1">>}   -> true;
        {ok, _Other}    -> false;
        {error, _Error} -> false
    end.

