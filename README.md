
emq_auth_redis
==============

Redis Authentication/ACL Plugin

Build Plugin
------------

```
make && make tests
```

Configure Plugin
----------------

File: etc/emq_auth_redis.conf

```
## Redis Server
auth.redis.server = 127.0.0.1:6379

## Redis Pool Size
auth.redis.pool = 8

## Redis Database
auth.redis.database = 0

## Redis Password
## auth.redis.password =

## Variables: %u = username, %c = clientid

## Authentication Query Command
auth.redis.auth_cmd = HGET mqtt_user:%u password

## Password hash: plain, md5, sha, sha256, pbkdf2
auth.redis.passwd.hash = sha256

## Superuser Query Command
auth.redis.super_cmd = HGET mqtt_user:%u is_superuser

## ACL Query Command
auth.redis.acl_cmd = HGETALL mqtt_acl:%u

## ACL nomatch
auth.redis.acl_nomatch = deny

```

SuperUser
---------

```
HSET mqtt_user:<username> is_superuser 1
```

User Hash with Password
-----------------------

Set a 'user' hash with 'password' field, for example:

```
HSET mqtt_user:<username> password "passwd"
```

ACL Rule Hash
-------------

The plugin uses a redis hash to store ACL rules:

```
HSET mqtt_acl:<username> topic1 1
HSET mqtt_acl:<username> topic2 2
HSET mqtt_acl:<username> topic3 3
```

NOTE: 1: subscribe, 2: publish, 3: pubsub

Subscription Hash
-----------------

NOTICE: Move to emq_backend_redis...

The plugin could store the static subscriptions into a redis Hash:

```
HSET mqtt_sub:<username> topic1 0
HSET mqtt_sub:<username> topic2 1
HSET mqtt_sub:<username> topic3 2
```

Load Plugin
-----------

```
./bin/emqttd_ctl plugins load emq_auth_redis
```

Author
------

Feng Lee <feng@emqtt.io>

