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

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-export([ check/2
        , description/0
        ]).

check(Credentials = #{password := Password}, #{auth_cmd  := AuthCmd,
                                               super_cmd := SuperCmd,
                                               hash_type := HashType}) ->
    CheckPass = case emqx_auth_redis_cli:q(AuthCmd, Credentials) of
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
        ok -> {stop, Credentials#{is_superuser => is_superuser(SuperCmd, Credentials),
                                  anonymous => false,
                                  auth_result => success}};
        {error, not_found} -> ok;
        {error, ResultCode} ->
            ?LOG(error, "[Redis] Auth from redis failed: ~p", [ResultCode]),
            {stop, Credentials#{auth_result => ResultCode, anonymous => false}}
    end.

description() -> "Authentication with Redis".

-spec(is_superuser(undefined | list(), emqx_types:credentials()) -> true | false).
is_superuser(undefined, _Credentials) -> false;
is_superuser(SuperCmd, Credentials) ->
    case emqx_auth_redis_cli:q(SuperCmd, Credentials) of
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

