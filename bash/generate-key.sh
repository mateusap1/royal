#!/bin/bash

KEY_NAME=${1}
STAKE_NAME=${2}

cardano-cli address key-gen \
    --verification-key-file ${KEY_NAME}.vkey \
    --signing-key-file ${KEY_NAME}.skey

cardano-cli stake-address key-gen \
    --verification-key-file ${STAKE_NAME}.vkey \
    --signing-key-file ${STAKE_NAME}.skey

cardano-cli address build \
    --payment-verification-key-file ${KEY_NAME}.vkey \
    --stake-verification-key-file ${STAKE_NAME}.vkey \
    --out-file ${KEY_NAME}.addr \
    ${MAGIC}
