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
%%% @doc ACL with Redis.
%%%
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------
-module(emqttd_acl_redis).

-behaviour(emqttd_acl_mod).

-include("../../../include/emqttd.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {acl_cmd, acl_nomatch}).

-define(IS_PUB(PubSub), PubSub =:= publish orelse PubSub =:= pubsub).

-define(IS_SUB(PubSub), PubSub =:= subscribe orelse PubSub =:= pubsub).

init({AclCmd, AclNomatch}) ->
    {ok, #state{acl_cmd = AclCmd, acl_nomatch = AclNomatch}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client, PubSub, Topic}, #state{acl_cmd     = AclCmd,
                                          acl_nomatch = Default}) ->
    case emqttd_redis_client:query(repl_var(Client, AclCmd)) of
        {ok, []} ->
            Default;
        {ok, Rules} ->
            case match(Client, Topic, filter(PubSub, Rules)) of
                allow   -> allow;
                nomatch -> Default
            end;
        {error, Error} ->
            {error, Error}
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

repl_var(Client, AclCmd) ->
    [feed_var(Client, Token) || Token <- AclCmd].

feed_var(#mqtt_client{client_id = ClientId, username = Username}, Str) ->
    lists:foldl(fun({Var, Val}, StrAcc) ->
                feed_var(StrAcc, Var, Val)
        end, Str, [{"%u", Username}, {"%c", ClientId}]).

feed_var(Str, Var, Val) ->
    re:replace(Str, Var, Val, [global, {return, binary}]).

reload_acl(_State) -> ok.

description() -> "Redis ACL Module".
