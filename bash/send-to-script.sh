#!/bin/bash

UTXO=${1} # The UTxO which is being consumed
SND=${2} # The file with the keys from the sender
SCRIPT_ADDR=${3} # The address of the script that will receive the ADA
AMT=${4}
CHG_ADDR=$(cat ${SND}/payment.addr)
PRIV_KEY=${SND}/payment.skey

DATA_PATH=data
TX_PATH=tx

DATUM_HASH=$(cat ${DATA_PATH}/datum-hash.txt)
    
cardano-cli transaction build \
    --alonzo-era \
    --tx-in ${UTXO} \
    --tx-out ${SCRIPT_ADDR}+${AMT} \
    --tx-out-datum-hash ${DATUM_HASH} \
    --change-address ${CHG_ADDR} \
    ${MAGIC} \
    --protocol-params-file ${DATA_PATH}/pparams.json \
    --out-file ${TX_PATH}/tx.raw

echo "transaction build successful"

cardano-cli transaction sign \
    --tx-body-file ${TX_PATH}/tx.raw \
    --signing-key-file ${PRIV_KEY} \
    ${MAGIC} \
    --out-file ${TX_PATH}/tx.sign

echo "transaction signing successful"

cardano-cli transaction submit ${MAGIC} --tx-file ${TX_PATH}/tx.sign

echo "transaction submission successful"
