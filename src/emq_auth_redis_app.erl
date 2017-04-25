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

-module(emq_auth_redis_app).

-behaviour(application).

-include("emq_auth_redis.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_auth_redis_sup:start_link(),
    if_cmd_enabled(auth_cmd, fun reg_authmod/1),
    if_cmd_enabled(acl_cmd,  fun reg_aclmod/1),
    {ok, Sup}.

stop(_State) ->
    emqttd_access_control:unregister_mod(auth, emq_auth_redis),
    emqttd_access_control:unregister_mod(acl, emq_acl_redis),
    emqttd_plugin_redis:unload().

reg_authmod(AuthCmd) ->
    SuperCmd = application:get_env(?APP, super_cmd, undefined),
    {ok, PasswdHash} = application:get_env(?APP, password_hash),
    emqttd_access_control:register_mod(auth, emq_auth_redis, {AuthCmd, SuperCmd, PasswdHash}).
reg_aclmod(AclCmd) ->
    emqttd_access_control:register_mod(acl, emq_acl_redis, AclCmd).

if_cmd_enabled(Par, Fun) ->
    case application:get_env(?APP, Par) of
        {ok, Cmd} -> Fun(Cmd);
        undefined -> ok
    end.

