.PHONY: app tests

PROJECT = emqx_auth_redis
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with Redis
PROJECT_VERSION = 3.0

DEPS = eredis ecpool clique emqx_passwd

##TODO: version or tag?
dep_eredis = git-emqx https://github.com/emqx/eredis 1.1.0
dep_ecpool = git-emqx https://github.com/emqx/ecpool master
dep_clique = git-emqx https://github.com/emqx/clique develop
dep_emqx_passwd = git-emqx https://github.com/emqx/emqx-passwd emqx30

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish emqx30

NO_AUTOPATCH = cuttlefish

TEST_DEPS = emqx_auth_username
dep_emqx_auth_username = git https://github.com/emqx/emqx-auth-username emqx30

TEST_ERLC_OPTS += +debug_info

COVER = true

ERLC_OPTS += +debug_info

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_redis.conf -i priv/emqx_auth_redis.schema -d data
