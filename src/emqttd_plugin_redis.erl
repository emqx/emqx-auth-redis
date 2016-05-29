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

%% @doc emqttd redis plugin.
-module(emqttd_plugin_redis).

-include("../../../include/emqttd.hrl").

-include("../../../include/emqttd_protocol.hrl").

-export([load/0, unload/0]).

-export([on_client_connected/3]).

-define(APP, ?MODULE).

%% Called when the plugin loaded
load() ->
    SuperCmd = application:get_env(?APP, supercmd, undefined),
    ok = emqttd_access_control:register_mod(
            auth, emqttd_auth_redis, {SuperCmd, env(authcmd), env(password_hash)}),
    ok = with_cmd_enabled(aclcmd, fun(AclCmd) ->
            emqttd_access_control:register_mod(acl, emqttd_acl_redis, {SuperCmd, AclCmd, env(acl_nomatch)})
        end),
    ok = with_cmd_enabled(subcmd, fun(SubCmd) ->
            emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [SubCmd])
        end).

env(Key) -> {ok, Val} = application:get_env(?APP, Key), Val.

on_client_connected(?CONNACK_ACCEPT, Client = #mqtt_client{client_pid = ClientPid}, SubCmd) ->
    case emqttd_plugin_redis_client:query(SubCmd, Client) of
        {ok, Values}   -> emqttd_client:subscribe(ClientPid, topics(Values));
        {error, Error} -> lager:error("Redis Error: ~p, Cmd: ~p", [Error, SubCmd])
    end,
    {ok, Client};

on_client_connected(_ConnAck, _Client, _LoadCmd) ->
    ok.

unload() ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqttd_access_control:unregister_mod(auth, emqttd_auth_redis),
    with_cmd_enabled(aclcmd, fun(_AclCmd) ->
            emqttd_access_control:unregister_mod(acl, emqttd_acl_redis)
        end).

with_cmd_enabled(Name, Fun) ->
    case application:get_env(?APP, Name) of
        {ok, Cmd} -> Fun(Cmd);
        undefined -> ok
    end.

topics(Values) ->
    topics(Values, []).
topics([], Acc) ->
    Acc;
topics([Topic, Qos | Vals], Acc) ->
    topics(Vals, [{Topic, i(Qos)}|Acc]).

i(S) -> list_to_integer(binary_to_list(S)).

