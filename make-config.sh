#!/bin/bash -e

${REBAR_BUILD_DIR}/lib/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_redis.conf -i priv/emqx_auth_redis.schema -d data

