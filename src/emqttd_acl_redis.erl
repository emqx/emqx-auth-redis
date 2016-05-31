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

%% @doc ACL with Redis.
-module(emqttd_acl_redis).

-behaviour(emqttd_acl_mod).

-include("../../../include/emqttd.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {super_cmd, acl_cmd, acl_nomatch}).

-define(IS_PUB(PubSub), PubSub =:= publish orelse PubSub =:= pubsub).

-define(IS_SUB(PubSub), PubSub =:= subscribe orelse PubSub =:= pubsub).

init({SuperCmd, AclCmd, AclNomatch}) ->
    {ok, #state{super_cmd = SuperCmd, acl_cmd = AclCmd, acl_nomatch = AclNomatch}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client, PubSub, Topic}, #state{super_cmd   = SuperCmd,
                                          acl_cmd     = AclCmd,
                                          acl_nomatch = Default}) ->

    case emqttd_plugin_redis_client:is_superuser(SuperCmd, Client) of
        false -> case emqttd_plugin_redis_client:query(AclCmd, Client) of
                     {ok, []} ->
                         Default;
                     {ok, Rules} ->
                         case match(Client, Topic, filter(PubSub, Rules)) of
                             allow   -> allow;
                             nomatch -> Default
                         end;
                     {error, Error} ->
                         {error, Error}
                 end;
        true  -> allow
    end.

filter(PubSub, Rules) ->
    filter(PubSub, Rules, []).

filter(_, [], Acc) ->
    lists:reverse(Acc);

filter(publish, [<<"publish ", Topic/binary>> | Rules], Acc) ->
    filter(publish, Rules, [Topic | Acc]);

filter(subscribe, [<<"subscribe ", Topic/binary>> | Rules], Acc) ->
    filter(subscribe, Rules, [Topic | Acc]);

filter(PubSub, [<<"pubsub ", Topic/binary>> | Rules], Acc) ->
    filter(PubSub, Rules, [Topic | Acc]);

filter(PubSub, [_ | Rules], Acc) ->
    filter(PubSub, Rules, Acc).

match(_Client, _Topic, []) ->
    nomatch;

match(Client, Topic, [Rule|Rules]) ->
    case emqttd_topic:match(Topic, feed_var(Client, Rule)) of
        false -> match(Client, Topic, Rules);
        true  -> allow
    end.

feed_var(#mqtt_client{client_id = ClientId, username = Username}, Str) ->
    lists:foldl(fun({Var, Val}, StrAcc) ->
                feed_var(StrAcc, Var, Val)
        end, Str, [{"%u", Username}, {"%c", ClientId}]).

feed_var(Str, _Var, undefined) ->
    Str;
feed_var(Str, Var, Val) ->
    re:replace(Str, Var, Val, [global, {return, binary}]).

reload_acl(_State) -> ok.

description() -> "Redis ACL Module".

