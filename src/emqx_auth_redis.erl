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

-module(emqx_auth_redis).

-behaviour(emqx_auth_mod).

-include_lib("emqx/include/emqx.hrl").

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
    Result = case emqx_auth_redis_cli:q(AuthCmd, Client) of
                {ok, PassHash} when is_binary(PassHash) ->
                    check_pass(PassHash, Password, HashType);  
                {ok, [undefined|_]} ->
                    ignore;
                {ok, [PassHash]} ->
                    check_pass(PassHash, Password, HashType);
                {ok, [PassHash, Salt|_]} ->
                    check_pass(PassHash, Salt, Password, HashType);
                {error, Reason} ->
                    {error, Reason}
             end,
    case Result of ok -> {ok, is_superuser(SuperCmd, Client)}; Error -> Error end.

check_pass(PassHash, Password, HashType) ->
    check_pass(PassHash, hash(HashType, Password)).
check_pass(PassHash, Salt, Password, {pbkdf2, Macfun, Iterations, Dklen}) ->
  check_pass(PassHash, hash(pbkdf2, {Salt, Password, Macfun, Iterations, Dklen}));
check_pass(PassHash, Salt, Password, {salt, bcrypt}) ->
    check_pass(PassHash, hash(bcrypt, {Salt, Password}));
check_pass(PassHash, Salt, Password, {salt, HashType}) ->
    check_pass(PassHash, hash(HashType, <<Salt/binary, Password/binary>>));
check_pass(PassHash, Salt, Password, {HashType, salt}) ->
    check_pass(PassHash, hash(HashType, <<Password/binary, Salt/binary>>)).

check_pass(PassHash, PassHash) -> ok;
check_pass(_, _)               -> {error, password_error}.

description() -> "Authentication with Redis".

hash(Type, Password) -> emqx_auth_mod:passwd_hash(Type, Password).

-spec(is_superuser(undefined | list(), mqtt_client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser(SuperCmd, Client) ->
    case emqx_auth_redis_cli:q(SuperCmd, Client) of
        {ok, undefined} -> false;
        {ok, <<"1">>}   -> true;
        {ok, _Other}    -> false;
        {error, _Error} -> false
    end.

