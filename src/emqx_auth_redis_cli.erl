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

-module(emqx_auth_redis_cli).

-behaviour(ecpool_worker).

-include("emqx_auth_redis.hrl").

-include_lib("emqx/include/emqx.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([ connect/1
        , q/3
        ]).

%%--------------------------------------------------------------------
%% Redis Connect/Query
%%--------------------------------------------------------------------

connect(Opts) ->
    Sentinel = get_value(sentinel, Opts),
    Host = case Sentinel =:= "" of
        true -> get_value(host, Opts);
        false ->
            eredis_sentinel:start_link([{get_value(host, Opts),
                                         get_value(port, Opts)}]),
            "sentinel:" ++ Sentinel
    end,
    eredis:start_link(Host,
                      get_value(port, Opts, 6379),
                      get_value(database, Opts),
                      get_value(password, Opts),
                      no_reconnect).

%% Redis Query.
-spec(q(string(), emqx_types:credentials(), timeout())
      -> {ok, undefined | binary() | list()} | {error, atom() | binary()}).
q(CmdStr, Credentials, Timeout) ->
    Cmd = string:tokens(replvar(CmdStr, Credentials), " "),
    case get_value(type, application:get_env(?APP, server, [])) of
        cluster -> eredis_cluster:q(?APP, Cmd);
        _ -> ecpool:with_client(?APP, fun(C) -> eredis:q(C, Cmd, Timeout) end)
    end.

replvar(Cmd, Credentials = #{cn := CN}) ->
    replvar(repl(Cmd, "%C", CN), maps:remove(cn, Credentials));
replvar(Cmd, Credentials = #{dn := DN}) ->
    replvar(repl(Cmd, "%d", DN), maps:remove(dn, Credentials));
replvar(Cmd, Credentials = #{client_id := ClientId}) ->
    replvar(repl(Cmd, "%c", ClientId), maps:remove(client_id, Credentials));
replvar(Cmd, Credentials = #{username := Username}) ->
    replvar(repl(Cmd, "%u", Username), maps:remove(username, Credentials));
replvar(Cmd, _) ->
    Cmd.

repl(S, _Var, undefined) ->
    S;
repl(S, Var, Val) ->
    re:replace(S, Var, Val, [{return, list}]).

