.PHONY: app tests

PROJECT = emqx_auth_redis
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with Redis

DEPS = eredis_cluster ecpool clique emqx_passwd

##TODO: version or tag?
dep_eredis_cluster = git-emqx https://github.com/emqx/eredis_cluster 0.5.12
dep_ecpool = git-emqx https://github.com/emqx/ecpool v0.3.0
dep_clique = git-emqx https://github.com/emqx/clique v0.3.11
dep_emqx_passwd = git-emqx https://github.com/emqx/emqx-passwd v1.0

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1

NO_AUTOPATCH = cuttlefish

TEST_DEPS = emqx_auth_username
dep_emqx_auth_username = git https://github.com/emqx/emqx-auth-username $(BRANCH)

TEST_ERLC_OPTS += +debug_info

COVER = true

ERLC_OPTS += +debug_info

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_redis.conf -i priv/emqx_auth_redis.schema -d data
