PROJECT = emq_auth_redis
PROJECT_DESCRIPTION = Authentication/ACL with Redis
PROJECT_VERSION = 3.0

DEPS = eredis ecpool

dep_eredis   = git https://github.com/wooga/eredis master
dep_ecpool   = git https://github.com/emqtt/ecpool master

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd emq30

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/basho/cuttlefish master

COVER = true

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app:: rebar.config

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_auth_redis.conf -i priv/emq_auth_redis.schema -d data
