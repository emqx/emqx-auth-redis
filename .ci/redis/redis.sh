#!/bin/bash

tls=false;
while getopts n:t OPT
do
    case $OPT in
        n)  nodes=$OPTARG
            ;;
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

if [ ${nodes} = "cluster" ] ; then
  if $tls ; then
    redis-server --cluster-enabled yes --port 7000 --cluster-config-file /data/conf/nodes.7000.conf --daemonize yes \
                                       --tls-port 8000 \
                                       --tls-cert-file /tls/redis.crt \
                                       --tls-key-file /tls/redis.key \
                                       --tls-ca-cert-file /tls/ca.crt \
                                       --bind 0.0.0.0 ::
    redis-server --cluster-enabled yes --port 7001 --cluster-config-file /data/conf/nodes.7001.conf --daemonize yes \
                                       --tls-port 8001 \
                                       --tls-cert-file /tls/redis.crt \
                                       --tls-key-file /tls/redis.key \
                                       --tls-ca-cert-file /tls/ca.crt \
                                       --bind 0.0.0.0 ::
    redis-server --cluster-enabled yes --port 7002 --cluster-config-file /data/conf/nodes.7002.conf --daemonize yes \
                                       --tls-port 8002 \
                                       --tls-cert-file /tls/redis.crt \
                                       --tls-key-file /tls/redis.key \
                                       --tls-ca-cert-file /tls/ca.crt \
                                       --bind 0.0.0.0 ::
  else
    redis-server --cluster-enabled yes --port 7000 --cluster-config-file /data/conf/nodes.7000.conf --daemonize yes --bind 0.0.0.0 ::
    redis-server --cluster-enabled yes --port 7001 --cluster-config-file /data/conf/nodes.7001.conf --daemonize yes --bind 0.0.0.0 ::
    redis-server --cluster-enabled yes --port 7002 --cluster-config-file /data/conf/nodes.7002.conf --daemonize yes --bind 0.0.0.0 ::
  fi
elif [ ${nodes} = "sentinel" ] ; then
  if $tls ; then
    redis-server --cluster-enabled no  --port 7000 --cluster-config-file /data/conf/nodes.7000.conf --daemonize yes \
                                       --tls-port 8000 \
                                       --tls-cert-file /tls/redis.crt \
                                       --tls-key-file /tls/redis.key \
                                       --tls-ca-cert-file /tls/ca.crt \
                                       --bind 0.0.0.0 ::
    redis-server --cluster-enabled no  --port 7001 --cluster-config-file /data/conf/nodes.7001.conf --daemonize yes \
                                       --tls-port 8001 \
                                       --tls-cert-file /tls/redis.crt \
                                       --tls-key-file /tls/redis.key \
                                       --tls-ca-cert-file /tls/ca.crt \
                                       --bind 0.0.0.0 :: \
                                       --slaveof 172.16.239.10 7000
    redis-server --cluster-enabled no  --port 7002 --cluster-config-file /data/conf/nodes.7002.conf --daemonize yes \
                                       --tls-port 8002 \
                                       --tls-cert-file /tls/redis.crt \
                                       --tls-key-file /tls/redis.key \
                                       --tls-ca-cert-file /tls/ca.crt \
                                       --bind 0.0.0.0 :: \
                                       --slaveof 172.16.239.10 7000
  else
    redis-server --cluster-enabled no --port 7000 --cluster-config-file /data/conf/nodes.7000.conf --daemonize yes --bind 0.0.0.0 ::
    redis-server --cluster-enabled no --port 7001 --cluster-config-file /data/conf/nodes.7001.conf --daemonize yes --bind 0.0.0.0 :: --slaveof 172.16.239.10 7000
    redis-server --cluster-enabled no --port 7002 --cluster-config-file /data/conf/nodes.7002.conf --daemonize yes --bind 0.0.0.0 :: --slaveof 172.16.239.10 7000
  fi
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
    if [ ${nodes} = "cluster" ] ; then
      yes "yes" | redis-cli --cluster create 172.16.239.10:7000 172.16.239.10:7001 172.16.239.10:7002;
    elif [ ${nodes} = "sentinel" ] ; then
      cp /data/conf/sentinel.conf /_sentinel.conf
      redis-server /_sentinel.conf --sentinel;
    fi
    REDIS_LOAD_FLG=false;
done

exit 0;
