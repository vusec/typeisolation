#!/bin/bash
set -euo pipefail

servers="nginx"
#instances="baseline-lto typedallocator-typeisolation-stores typesafestack-typedmalloc-inline typesafestack-typedmalloc-inline-typeisolation-stores"
instances="typedallocator-typeisolation-stores"
#instances="baseline-lto typesafestack-typedmalloc-inline-typeisolation-stores"

#connections="`seq 16 16 256` `seq 256 128 1024` `seq 1024 256 1536`"
#connections="`seq 16 16 240` `seq 256 128 896` `seq 1024 256 1536`"
connections="`seq 16 16 240` `seq 256 128 896` `seq 1024 256 2048`"
#echo $connections
#connections="96"

# SSH names
sshclient=son
sshserver=father
# Local hosts
hostclient=192.168.0.10
hostserver=192.168.0.20
# Benchmark host (100G NIC)
serverip=10.0.0.20
serverport=20000

#iterations=5
iterations=3
filesize=64
duration=30 # Seconds
wait_time=5 # Seconds


#client_threads=`nproc`
client_threads=16
#client_threads=1

#server_workers=`nproc`
server_workers=16
server_worker_connections=1024

stats="cpu rss"
stats_interval=1 # Seconds

cp nginx.crt nginx.key /tmp

for server in $servers; do
    ./runinfra.py -v debug run $server $instances \
        -t bench \
	--use-ssl \
        --parallel=ssh \
        --ssh-nodes $sshclient $sshserver \
        --remote-client-host $hostclient \
        --remote-server-host $hostserver \
        --server-ip $serverip \
        --port $serverport \
        --duration $duration \
        --threads $client_threads \
        --iterations $iterations \
        --workers $server_workers \
        --worker-connections $server_worker_connections \
        --filesize $filesize \
        --collect-stats $stats \
        --collect-stats-interval $stats_interval \
        --connections $connections \
        --restart-server-between-runs
done
