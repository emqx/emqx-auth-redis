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

-module(emqttd_auth_redis_sup).

-behaviour(supervisor).

-include("emqttd_auth_redis.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools]).

init([Pools]) ->
    {ok, {{one_for_one, 10, 100}, [pool_spec(Pool, Env) || {redis, Pool, Env} <- Pools]}}.

pool_spec(Pool, Env) ->
    ecpool:pool_spec({?APP, Pool}, ?APP:pool_name(Pool), ?APP, Env).

