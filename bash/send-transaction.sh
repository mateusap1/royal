#!/bin/bash

UTXO=${1}

SND_ADDR=${2}
RCV_ADDR=${3}

SIG_FILE=${4}
OUT_TX_FILE=tx/tx.raw
OUT_SIG_FILE=tx/tx.sign

cardano-cli transaction build \
    --alonzo-era \
    --tx-in ${UTXO} \
    --tx-out ${RCV_ADDR}+10000000 \
    --change-address ${SND_ADDR} \
    ${MAGIC} \
    --out-file ${OUT_FILE} 

echo "transaction built"

cardano-cli transaction sign \
    ${MAGIC} \
    --signing-key-file ${SIG_FILE} \
    --tx-body-file ${OUT_TX_FILE} \
    --out-file ${OUT_SIG_FILE}

echo "transaction signed"

cardano-cli transaction submit ${MAGIC} --tx-file ${OUT_SIG_FILE}
