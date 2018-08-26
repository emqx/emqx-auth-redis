%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_acl_redis).

-behaviour(emqx_acl_mod).

-include_lib("emqx/include/emqx.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

init(AclCmd) ->
    {ok, #{acl_cmd => AclCmd}}.

check_acl({#{username := <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    ignore;
check_acl({Credetials, PubSub, Topic}, #{acl_cmd := AclCmd}) ->
    case emqx_auth_redis_cli:q(AclCmd, Credetials) of
        {ok, []} -> ignore;
        {ok, Rules} ->
            case match(Credetials, PubSub, Topic, Rules) of
                allow   -> allow;
                nomatch -> deny
            end;
        {error, Reason} ->
            emqx_logger:error("Redis check_acl error: ~p", [Reason]),
            ignore
    end.

match(_Credetials, _PubSub, _Topic, []) ->
    nomatch;
match(Credetials, PubSub, Topic, [Filter, Access | Rules]) ->
    case {match_topic(Topic, feed_var(Credetials, Filter)), match_access(PubSub, b2i(Access))} of
        {true, true} -> allow;
        {_, _} -> match(Credetials, PubSub, Topic, Rules)
    end.

match_topic(Topic, Filter) ->
    emqx_topic:match(Topic, Filter).

match_access(subscribe, Access) ->
    (1 band Access) > 0;
match_access(publish, Access) ->
    (2 band Access) > 0.

feed_var(#{client_id := ClientId, username := Username}, Str) ->
    lists:foldl(fun({Var, Val}, Acc) ->
                feed_var(Acc, Var, Val)
        end, Str, [{"%u", Username}, {"%c", ClientId}]).

feed_var(Str, _Var, undefined) ->
    Str;
feed_var(Str, Var, Val) ->
    re:replace(Str, Var, Val, [global, {return, binary}]).

b2i(Bin) -> list_to_integer(binary_to_list(Bin)).

reload_acl(_State) -> ok.

description() -> "Redis ACL Module".

