
emqttd_plugin_redis
===================

emqttd Redis Plugin

Build Plugin
------------

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

Configure Plugin
----------------

File: etc/plugin.config

```erlang
[
  {emqttd_plugin_redis, [

    {eredis_pool, [
      %% ecpool options
      {pool_size, 8},
      {auto_reconnect, 2},

      %% eredis options
      {host, "127.0.0.1"},
      {port, 6379},
      {database, 0},
      {password, ""}
    ]},

    %% Variables: %u = username, %c = clientid

    %% HMGET mqtt_user:%u is_superuser
    {supercmd, ["HGET", "mqtt_user:%u", "is_superuser"]},

    %% HMGET mqtt_user:%u password
    {authcmd, ["HGET", "mqtt_user:%u", "password"]},

    %% Password hash algorithm: plain, md5, sha, sha256, pbkdf2?
    {password_hash, sha256},

    %% SMEMBERS mqtt_acl:%u
    {aclcmd, ["SMEMBERS", "mqtt_acl:%u"]},

    %% If no rules matched, return...
    {acl_nomatch, deny},

    %% Load Subscriptions form Redis when client connected.
    {subcmd, ["HGETALL", "mqtt_subs:%u"]}

  ]}
].
```

Super User
----------

```
HSET mqtt_user:<username> is_superuser 1
```

User Hash with Password
-----------------------

Set a 'user' hash with 'password' field, for example:

```
HSET mqtt_user:<username> password "passwd"
```

ACL Rule Set
------------

The plugin uses a redis set to store ACL rules:

```
SADD mqtt_acl:<username> "publish topic1"
SADD mqtt_acl:<username> "subscribe topic2"
SADD mqtt_acl:<username> "pubsub topic3"
```

Subscription Hash
-----------------

The plugin could store the static subscriptions into a redis Hash:

```
HSET mqtt_subs:<username> topic1 0
HSET mqtt_subs:<username> topic2 1
HSET mqtt_subs:<username> topic3 2
```

Load Plugin
-----------

```
./bin/emqttd_ctl plugins load emqttd_plugin_redis
```

Author
------

Feng Lee <feng@emqtt.io>

