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

-module(emqx_auth_redis_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(APP, emqx_auth_redis).

-define(POOL(App), ecpool_worker:client(gproc_pool:pick_worker({ecpool, App}))).

-define(INIT_ACL, [{"mqtt_acl:test1", "topic1", "2"},
                   {"mqtt_acl:test2", "topic2", "1"},
                   {"mqtt_acl:test3", "topic3", "3"}]).

-define(INIT_AUTH, [{"mqtt_user:plain", ["password", "plain", "salt", "salt", "is_superuser", "1"]},
                    {"mqtt_user:md5", ["password", "1bc29b36f623ba82aaf6724fd3b16718", "salt", "salt", "is_superuser", "0"]},
                    {"mqtt_user:sha", ["password", "d8f4590320e1343a915b6394170650a8f35d6926", "salt", "salt", "is_superuser", "0"]},
                    {"mqtt_user:sha256", ["password", "5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e", "salt", "salt", "is_superuser", "0"]},
                    {"mqtt_user:pbkdf2_password", ["password", "cdedb5281bb2f801565a1122b2563515", "salt", "ATHENA.MIT.EDUraeburn", "is_superuser", "0"]},
                    {"mqtt_user:bcrypt_foo", ["password", "$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6", "salt", "$2a$12$sSS8Eg.ovVzaHzi1nUHYK.", "is_superuser", "0"]},
                    {"mqtt_user:bcrypt", ["password", "$2y$16$rEVsDarhgHYB0TGnDFJzyu5f.T.Ha9iXMTk9J36NCMWWM7O16qyaK", "salt", "salt", "is_superuser", "0"]}]).

all() ->
    [{group, emqx_auth_redis_auth},
     {group, emqx_auth_redis_acl},
     {group, emqx_auth_redis}
     %{group, auth_redis_config}
    ].

groups() ->
    [{emqx_auth_redis_auth, [sequence], [check_auth, list_auth, check_auth_hget]},
     {emqx_auth_redis_acl, [sequence], [check_acl, acl_super]},
     {emqx_auth_redis, [sequence], [comment_config]},
     {auth_redis_config, [sequence], [server_config]}
     ].

init_per_suite(Config) ->
    [start_apps(App, {SchemaFile, ConfigFile}) ||
        {App, SchemaFile, ConfigFile}
            <- [{emqx, local_path("deps/emqx/priv/emqx.schema"),
                       local_path("deps/emqx/etc/emqx.conf")},
                {emqx_auth_redis, local_path("priv/emqx_auth_redis.schema"),
                                  local_path("etc/emqx_auth_redis.conf")}]],
    Config.

end_per_suite(_Config) ->
    {ok, Connection} = ?POOL(?APP),
    AuthKeys = [Key || {Key, _Filed, _Value} <- ?INIT_AUTH],
    AclKeys = [Key || {Key, _Value} <- ?INIT_ACL],
    eredis:q(Connection, ["DEL" | AuthKeys]),
    eredis:q(Connection, ["DEL" | AclKeys]),
    application:stop(emqx_auth_redis),
    application:stop(ecpool).

get_base_dir() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).

local_path(RelativePath) ->
    filename:join([get_base_dir(), RelativePath]).

start_apps(App, {SchemaFile, ConfigFile}) ->
    read_schema_configs(App, {SchemaFile, ConfigFile}),
    set_special_configs(App),
    application:ensure_all_started(App).

read_schema_configs(App, {SchemaFile, ConfigFile}) ->
    ct:pal("Read configs - SchemaFile: ~p, ConfigFile: ~p", [SchemaFile, ConfigFile]),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = conf_parse:file(ConfigFile),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals].

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, plugins_loaded_file,
                        local_path("deps/emqx/test/emqx_SUITE_data/loaded_plugins"));
set_special_configs(_App) ->
    ok.

check_auth(_Config) ->
    {ok, Connection} = ?POOL(?APP),
    [eredis:q(Connection, ["HMSET", Key|FiledValue]) || {Key, FiledValue} <- ?INIT_AUTH],
    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    Md5 = #{client_id => <<"md5">>, username => <<"md5">>},
    Sha = #{client_id => <<"sha">>, username => <<"sha">>},
    Sha256 = #{client_id => <<"sha256">>, username => <<"sha256">>},
    Pbkdf2 = #{client_id => <<"pbkdf2_password">>, username => <<"pbkdf2_password">>},
    BcryptFoo = #{client_id => <<"bcrypt_foo">>, username => <<"bcrypt_foo">>},
    User1 = #{client_id => <<"bcrypt_foo">>, username => <<"user">>},
    User3 = #{client_id => <<"client3">>},
    Bcrypt = #{client_id => <<"bcrypt">>, username => <<"bcrypt">>},
    {error, _} = emqx_access_control:authenticate(User3, <<>>),
    reload([{password_hash, plain}]),
    {ok, #{is_superuser := true}} = emqx_access_control:authenticate(Plain, <<"plain">>),
    reload([{password_hash, md5}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Md5, <<"md5">>),
    reload([{password_hash, sha}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Sha, <<"sha">>),
    reload([{password_hash, sha256}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Sha256, <<"sha256">>),
    reload([{password_hash, bcrypt}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Bcrypt, <<"password">>),
    %%pbkdf2 sha
    reload([{password_hash, {pbkdf2, sha, 1, 16}}, {auth_cmd, "HMGET mqtt_user:%u password salt"}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Pbkdf2, <<"password">>),
    reload([{password_hash, {salt, bcrypt}}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(BcryptFoo, <<"foo">>),
    {error,_} = emqx_access_control:authenticate(User1, <<"foo">>),
    {error, password_error} = emqx_access_control:authenticate(Bcrypt, <<"password">>).

list_auth(_Config) ->
    application:start(emqx_auth_username),
    emqx_auth_username:add_user(<<"user1">>, <<"password1">>),
    User1 = #{client_id => <<"client1">>, username => <<"user1">>},
    ok = emqx_access_control:authenticate(User1, <<"password1">>),
    reload([{password_hash, plain}, {auth_cmd, "HMGET mqtt_user:%u password"}]),
    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    {ok, #{is_superuser := true}} = emqx_access_control:authenticate(Plain, <<"plain">>),
    Stop = application:stop(emqx_auth_username),
    ct:log("Stop:~p~n", [Stop]).

check_auth_hget(_Config) ->
    {ok, Connection} = ?POOL(?APP),
    eredis:q(Connection, ["HSET", "mqtt_user:hset", "password", "hset"]),
    eredis:q(Connection, ["HSET", "mqtt_user:hset", "is_superuser", "1"]),
    reload([{password_hash, plain}, {auth_cmd, "HGET mqtt_user:%u password"}]),
    Hset = #{client_id => <<"hset">>, username => <<"hset">>},
    {ok, #{is_superuser := true}} = emqx_access_control:authenticate(Hset, <<"hset">>).

check_acl(_Config) ->
    {ok, Connection} = ?POOL(?APP),
    Result = [eredis:q(Connection, ["HSET", Key, Filed, Value]) || {Key, Filed, Value} <- ?INIT_ACL],
    ct:pal("redis init result: ~p~n", [Result]),
    User1 = #{zone => external, client_id => <<"client1">>, username => <<"test1">>},
    User2 = #{zone => external, client_id => <<"client2">>, username => <<"test2">>},
    User3 = #{zone => external, client_id => <<"client3">>, username => <<"test3">>},
    User4 = #{zone => external, client_id => <<"client4">>, username => <<"$$user4">>},
    deny = emqx_access_control:check_acl(User1, subscribe, <<"topic1">>),
    allow = emqx_access_control:check_acl(User1, publish, <<"topic1">>),

    deny = emqx_access_control:check_acl(User2, publish, <<"topic2">>),
    allow = emqx_access_control:check_acl(User2, subscribe, <<"topic2">>),
    allow = emqx_access_control:check_acl(User3, publish, <<"topic3">>),
    allow = emqx_access_control:check_acl(User3, subscribe, <<"topic3">>),
    allow = emqx_access_control:check_acl(User4, publish, <<"a/b/c">>).

acl_super(_Config) ->
    reload([{password_hash, plain}]),
    {ok, C} = emqx_client:start_link([{host,      "localhost"},
                                      {client_id, <<"simpleClient">>},
                                      {username,  <<"plain">>},
                                      {password,  <<"plain">>}]),
    {ok, _} = emqx_client:connect(C),
    timer:sleep(10),
    emqx_client:subscribe(C, <<"TopicA">>, qos2),
    timer:sleep(1000),
    emqx_client:publish(C, <<"TopicA">>, <<"Payload">>, qos2),
    timer:sleep(1000),
    receive
        %% TODO: 3.0 ???
        {publish, _Topic, Payload} ->
        ?assertEqual(<<"Payload">>, Payload)
    after
        1000 ->
        io:format("Error: receive timeout!~n"),
        ok
    end,
    emqx_client:disconnect(C).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_cmd, auth_cmd]],
    application:start(?APP),
    ?assertEqual([], emqx_access_control:lookup_mods(auth)),
    ?assertEqual([], emqx_access_control:lookup_mods(acl)).

server_config(_) ->
    I = [{host, "localhost"},
         {pool_size, 1},
         {port, 6377},
         {auto_reconnect, 1},
         {password, "public"},
         {database, 1}
       ],
    SetConfigKeys = ["server=localhost:6377",
                     "pool=1",
                     "password=public",
                     "database=1",
                     "password_hash=salt,sha256"],
    lists:foreach(fun set_cmd/1, SetConfigKeys),
    {ok, E} =  application:get_env(emqx_auth_redis, server),
    {ok, Hash} =  application:get_env(emqx_auth_redis, password_hash),
    ?assertEqual(lists:sort(I), lists:sort(E)),
    ?assertEqual({salt, sha256}, Hash).

set_cmd(Key) ->
    emqx_cli_config:run(["config", "set", string:join(["auth.redis", Key], "."), "--app=emqx_auth_redis"]).

reload(Config) when is_list(Config) ->
    application:stop(?APP),
    [application:set_env(?APP, K, V) || {K, V} <- Config],
    application:start(?APP).
