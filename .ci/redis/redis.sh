#!/bin/bash

rm -f \
    /data/conf/r7000i.log \
    /data/conf/r7001i.log \
    /data/conf/r7002i.log \
    /data/conf/nodes.7000.conf \
    /data/conf/nodes.7001.conf \
    /data/conf/nodes.7002.conf ;

redis-server /data/conf/redis.conf --port 7000 --cluster-config-file /data/conf/nodes.7000.conf --daemonize yes ;
redis-server /data/conf/redis.conf --port 7001 --cluster-config-file /data/conf/nodes.7001.conf --daemonize yes ;
redis-server /data/conf/redis.conf --port 7002 --cluster-config-file /data/conf/nodes.7002.conf --daemonize yes ;

REDIS_LOAD_FLG=true;

while $REDIS_LOAD_FLG;
do
    sleep 1;
    redis-cli -p 7000 info 1> /data/conf/r7000i.log 2> /dev/null;
    if [ -s /data/conf/r7000i.log ]; then
        :
    else
        continue;
    fi
    redis-cli -p 7001 info 1> /data/conf/r7001i.log 2> /dev/null;
    if [ -s /data/conf/r7001i.log ]; then
        :
    else
        continue;
    fi
    redis-cli -p 7002 info 1> /data/conf/r7002i.log 2> /dev/null;
    if [ -s /data/conf/r7002i.log ]; then
        :
    else
        continue;
    fi
    yes "yes" | redis-cli --cluster create 172.16.239.10:7000 172.16.239.10:7001 172.16.239.10:7002;
    REDIS_LOAD_FLG=false;
done

exit 0;
