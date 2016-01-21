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
%%% @doc emqttd redis backend.
%%%
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------

-module(emqttd_backend_redis).

-include("../../../include/emqttd.hrl").

-include("../../../include/emqttd_protocol.hrl").

-export([load/0, unload/0]).

-export([on_client_subscribe_after/3,
         on_client_connected/3,
         on_client_unsubscribe/3]).

%% Called when the plugin application start
load() ->
    with_cmd_enabled(subcmd, fun(SubCmd) ->
                emqttd_broker:hook('client.subscribe.after', {?MODULE, on_client_subscribe_after},
                                   {?MODULE, on_client_subscribe_after, [SubCmd]})
        end),
    with_cmd_enabled(loadsub, fun(LoadCmd) ->
                emqttd_broker:hook('client.connected', {?MODULE, on_client_connected},
                                   {?MODULE, on_client_connected, [LoadCmd]})
        end),
    with_cmd_enabled(unsubcmd, fun(UnsubCmd) ->
                emqttd_broker:hook('client.unsubscribe', {?MODULE, on_client_unsubscribe},
                                   {?MODULE, on_client_unsubscribe, [UnsubCmd]})
        end).

on_client_subscribe_after(ClientId, TopicTable, SubCmd) ->
    with_username(ClientId, fun(Username) ->
                io:format("client ~s subscribe ~p~n", [ClientId, TopicTable]),
                Res = emqttd_redis_client:query(repl_var(SubCmd, Username) ++ TopicTable),
                io:format("Result: ~p~n", [Res])
        end).

on_client_connected(?CONNACK_ACCEPT, #mqtt_client{username = undefined}, _LoadCmd) ->
    ok;

on_client_connected(?CONNACK_ACCEPT, #mqtt_client{username = Username}, LoadCmd) ->
    case emqttd_redis_client:query(repl_var(LoadCmd, Username)) of
        {ok, Values} -> io:format("Values: ~p~n", [Values]);
        {error, Error} -> io:format("Error: ~p~n", [Error])
    end;

on_client_connected(_ConnAck, _Client) -> ok.

on_client_unsubscribe(ClientId, Topics, UnsubCmd) ->
    with_username(ClientId, fun(Username) ->
                io:format("client ~s unsubscribe ~p~n", [ClientId, Topics]),
                Res = emqttd_redis_client:query(repl_var(UnsubCmd, Username) ++ Topics),
                io:format("Result: ~p~n", [Res])
        end).

%% Called when the plugin application stop
unload() ->
    emqttd_broker:unhook('client.subscribe.after', {?MODULE, on_client_subscribe_after}),
    emqttd_broker:unhook('client.connected',       {?MODULE, on_client_connected}),
    emqttd_broker:unhook('client.unsubscribe',     {?MODULE, on_client_unsubscribe}).

with_cmd_enabled(Name, Fun) ->
    case application:get_env(emqttd_plugin_redis, Name) of
        {ok, Cmd}  -> Fun(Cmd);
        undefined  -> ok
    end.

with_username(ClientId, Fun) ->
    case emqttd_cm:lookup(ClientId) of
        undefined ->
            ok;
        #mqtt_client{username = undefined} ->
            ok;
        #mqtt_client{username = Username} ->
            Fun(Username)
    end.

repl_var(Cmd, Username) ->
    [re:replace(S, "%u", Username, [{return, binary}]) || S <- Cmd].

