%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_redis).

-behaviour(emqx_auth_mod).

-include_lib("emqx/include/emqx.hrl").

-export([init/1, check/3, description/0]).

-define(UNDEFINED(S), (S =:= undefined)).

init({AuthCmd, SuperCmd, HashType}) ->
    {ok, #{auth_cmd => AuthCmd, super_cmd => SuperCmd, hash_type => HashType}}.

check(#{username := Username}, Password, _State)
    when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Credetials, Password, #{auth_cmd  := AuthCmd,
                              super_cmd := SuperCmd,
                              hash_type := HashType}) ->
    Result = case emqx_auth_redis_cli:q(AuthCmd, Credetials) of
                {ok, PassHash} when is_binary(PassHash) ->
                    emqx_passwd:check_pass({PassHash, Password}, HashType);
                {ok, [undefined|_]} ->
                    ignore;
                {ok, [PassHash]} ->
                    emqx_passwd:check_pass({PassHash, Password}, HashType);
                {ok, [PassHash, Salt|_]} ->
                    emqx_passwd:check_pass({PassHash, Salt, Password}, HashType);
                {error, Reason} ->
                    {error, Reason}
             end,
    case Result of ok -> {ok, is_superuser(SuperCmd, Credetials)}; Error -> Error end.

description() -> "Authentication with Redis".

-spec(is_superuser(undefined | list(), emqx_types:credentials()) -> boolean()).
is_superuser(undefined, _Credentials) ->
    false;
is_superuser(SuperCmd, Credentials) ->
    case emqx_auth_redis_cli:q(SuperCmd, Credentials) of
        {ok, undefined} -> false;
        {ok, <<"1">>}   -> true;
        {ok, _Other}    -> false;
        {error, _Error} -> false
    end.

