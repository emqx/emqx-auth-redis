%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2016 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc emqttd redis plugin.
%%%
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------

-module(emqttd_plugin_redis).

-export([load/0, unload/0]).

-define(APP, ?MODULE).

load() ->
    {ok, AuthCmd}  = application:get_env(?APP, authcmd),
    {ok, HashType} = application:get_env(?APP, password_hash),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_redis, {AuthCmd, HashType}),
    with_acl_enabled(fun(AclCmd) ->
        {ok, AclNomatch} = application:get_env(?APP, acl_nomatch),
        ok = emqttd_access_control:register_mod(acl, emqttd_acl_redis, {AclCmd, AclNomatch})
    end).

unload() ->
    ok.

with_acl_enabled(Fun) ->
    case application:get_env(?MODULE, aclcmd) of
        {ok, AclCmd} -> Fun(AclCmd);
        undefined    -> ok
    end.

