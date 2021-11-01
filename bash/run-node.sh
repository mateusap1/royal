#!/usr/bin/bash

INFO_PATH=${1}

cardano-node run \
    --topology ${INFO_PATH}/testnet-topology.json \
    --database-path ${INFO_PATH}/db \
    --socket-path ${INFO_PATH}/node.socket \
    --host-addr 0.0.0.0 \
    --port 3001 \
    --config ${INFO_PATH}/testnet-config.json \
