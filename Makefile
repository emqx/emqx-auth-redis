PROJECT = emqttd_auth_redis
PROJECT_DESCRIPTION = emqttd Authentication/ACL against Redis
PROJECT_VERSION = 2.0

DEPS = eredis ecpool emqttd gen_conf

dep_eredis   = git https://github.com/wooga/eredis master
dep_ecpool   = git https://github.com/emqtt/ecpool master
dep_emqttd   = git https://github.com/emqtt/emqttd plus
dep_gen_conf = git https://github.com/emqtt/gen_conf master

COVER = true

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app:: rebar.config
