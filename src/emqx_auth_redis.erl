%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-export([ register_metrics/0
        , check/3
        , description/0
        ]).

-define(AUTH_METRICS,
        ['auth.redis.success',
         'auth.redis.failure',
         'auth.redis.ignore'
        ]).

register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).

check(Client= #{password := Password}, AuthResult,
      #{auth_cmd  := AuthCmd,
        super_cmd := SuperCmd,
        hash_type := HashType,
        timeout   := Timeout}) ->
    CheckPass = case emqx_auth_redis_cli:q(AuthCmd, Client, Timeout) of
                    {ok, PassHash} when is_binary(PassHash) ->
                        check_pass({PassHash, Password}, HashType);
                    {ok, [undefined|_]} ->
                        {error, not_found};
                    {ok, [PassHash]} ->
                        check_pass({PassHash, Password}, HashType);
                    {ok, [PassHash, Salt|_]} ->
                        check_pass({PassHash, Salt, Password}, HashType);
                    {error, Reason} ->
                        ?LOG(error, "[Redis] Command: ~p failed: ~p", [AuthCmd, Reason]),
                        {error, not_found}
                end,
    case CheckPass of
        ok ->
            emqx_metrics:inc('auth.redis.success'),
            {stop, AuthResult#{is_superuser => is_superuser(SuperCmd, Client, Timeout),
                               anonymous => false,
                               auth_result => success}};
        {error, not_found} ->
            emqx_metrics:inc('auth.redis.ignore'), ok;
        {error, ResultCode} ->
            ?LOG(error, "[Redis] Auth from redis failed: ~p", [ResultCode]),
            emqx_metrics:inc('auth.redis.failure'),
            {stop, AuthResult#{auth_result => ResultCode, anonymous => false}}
    end.

description() -> "Authentication with Redis".

-spec(is_superuser(undefined | list(), emqx_types:client(), timeout()) -> boolean()).
is_superuser(undefined, _Client, _Timeout) -> false;
is_superuser(SuperCmd, Client, Timeout) ->
    case emqx_auth_redis_cli:q(SuperCmd, Client, Timeout) of
        {ok, undefined} -> false;
        {ok, <<"1">>}   -> true;
        {ok, _Other}    -> false;
        {error, _Error} -> false
    end.

check_pass(Password, HashType) ->
    case emqx_passwd:check_pass(Password, HashType) of
        ok -> ok;
        {error, _Reason} -> {error, not_authorized}
    end.

