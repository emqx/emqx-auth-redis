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

%% @doc emqttd redis pool client
%% @author Feng Lee <feng@emqtt.io>
-module(emqttd_redis_client).

-behaviour(ecpool_worker).

-import(proplists, [get_value/2]).

-export([connect/1, query/1]).

connect(Opts) ->
    eredis:start_link(get_value(host, Opts),
                      get_value(port, Opts),
                      get_value(database, Opts),
                      get_value(password, Opts),
                      no_reconnect).

%% Redis Query.
-spec query(list()) -> {ok, undefined | binary() | list()} | {error, atom() | binary()}.
query(Cmd) -> ecpool:with_client(eredis_pool, fun(C) -> eredis:q(C, Cmd) end).

