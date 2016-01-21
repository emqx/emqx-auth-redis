# emqttd_plugin_redis

emqttd Redis Plugin

## Build Plugin

Add the plugin as submodule of emqttd project.

If the submodules exist:

```
git submodule update --remote plugins/emqttd_plugin_redis
```

Orelse:

```
git submodule add https://github.com/emqtt/emqttd_plugin_redis.git plugins/emqttd_plugin_redis
```

And then build emqttd project.

## Configure Plugin

File: etc/plugin.config

```erlang
[
  {emqttd_plugin_redis, [

    {eredis_pool, [
      %% ecpool options
      {pool_size, 8},
      {pool_type, round_robin},
      {auto_reconnect, 2},

      %% eredis options
      {host, "127.0.0.1"},
      {port, 6379},
      {database, 0},
      {password, ""}
    ]},

    %% HMGET mqtt_user:%u password
    {authcmd, ["HGET", "mqtt_user:%u", "password"]},

    %% Password hash algorithm: plain, md5, sha, sha256, pbkdf2?
    {password_hash, sha256},

    %% SMEMBERS mqtt_acl:%u
    {aclcmd, ["SMEMBERS", "mqtt_acl:%u"]},

    %% If no rules matched, return...
    {acl_nomatch, deny},

    %% Store subscriptions to redis when SUBSCRIBE packets received.
    {subcmd, ["HMSET", "mqtt_subs:%u"]},

    %% Load Subscriptions form Redis when client connected.
    {loadsub, ["HGETALL", "mqtt_subs:%u"]},

    %% Remove subscriptions from redis when UNSUBSCRIBE packets received.
    {unsubcmd, ["HDEL", "mqtt_subs:%u"]}

  ]}
].
```

## Load Plugin

```
./bin/emqttd_ctl plugins load emqttd_plugin_redis
```

## Author

Feng Lee <feng@emqtt.io>

