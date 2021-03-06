#!/bin/bash

tls=false;
while getopts t OPT
do
    case $OPT in
        t)  tls=true
            ;;
        \?) exit
            ;;
    esac
done

rm -f \
    /data/conf/r7000i.log \
    /data/conf/r7001i.log \
    /data/conf/r7002i.log \
    /data/conf/nodes.7000.conf \
    /data/conf/nodes.7001.conf \
    /data/conf/nodes.7002.conf ;

if  $tls ; then
  redis-server /data/conf/redis.conf --port 7000 --cluster-config-file /data/conf/nodes.7000.conf --daemonize yes \
                                     --tls-port 8000 \
                                     --tls-cert-file /tls/redis.crt \
                                     --tls-key-file /tls/redis.key \
                                     --tls-ca-cert-file /tls/ca.crt
  redis-server /data/conf/redis.conf --port 7001 --cluster-config-file /data/conf/nodes.7001.conf --daemonize yes \
                                     --tls-port 8001 \
                                     --tls-cert-file /tls/redis.crt \
                                     --tls-key-file /tls/redis.key \
                                     --tls-ca-cert-file /tls/ca.crt
  redis-server /data/conf/redis.conf --port 7002 --cluster-config-file /data/conf/nodes.7002.conf --daemonize yes \
                                     --tls-port 8002 \
                                     --tls-cert-file /tls/redis.crt \
                                     --tls-key-file /tls/redis.key \
                                     --tls-ca-cert-file /tls/ca.crt
else
  redis-server /data/conf/redis.conf --port 7000 --cluster-config-file /data/conf/nodes.7000.conf --daemonize yes ;
  redis-server /data/conf/redis.conf --port 7001 --cluster-config-file /data/conf/nodes.7001.conf --daemonize yes ;
  redis-server /data/conf/redis.conf --port 7002 --cluster-config-file /data/conf/nodes.7002.conf --daemonize yes ;
fi

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
