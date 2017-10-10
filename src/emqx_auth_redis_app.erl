%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emqx_auth_redis_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(APP, emqx_auth_redis).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_auth_redis_sup:start_link(),
    if_cmd_enabled(auth_cmd, fun reg_authmod/1),
    if_cmd_enabled(acl_cmd,  fun reg_aclmod/1),
    emqx_auth_redis_cfg:register(),
    {ok, Sup}.

stop(_State) ->
    emqx_access_control:unregister_mod(auth, emqx_auth_redis),
    emqx_access_control:unregister_mod(acl, emqx_acl_redis),
    emqx_auth_redis_cfg:unregister().

reg_authmod(AuthCmd) ->
    SuperCmd = application:get_env(?APP, super_cmd, undefined),
    {ok, PasswdHash} = application:get_env(?APP, password_hash),
    emqx_access_control:register_mod(auth, emqx_auth_redis, {AuthCmd, SuperCmd, PasswdHash}).

reg_aclmod(AclCmd) ->
    emqx_access_control:register_mod(acl, emqx_acl_redis, AclCmd).

if_cmd_enabled(Par, Fun) ->
    case application:get_env(?APP, Par) of
        {ok, Cmd} -> Fun(Cmd);
        undefined -> ok
    end.

