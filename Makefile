PROJECT = emqttd_auth_redis
PROJECT_DESCRIPTION = Authentication/ACL with Redis
PROJECT_VERSION = 2.0

DEPS = eredis ecpool gen_conf

dep_eredis   = git https://github.com/wooga/eredis master
dep_ecpool   = git https://github.com/emqtt/ecpool master
dep_gen_conf = git https://github.com/emqtt/gen_conf master

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd emq20

COVER = true

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app:: rebar.config
