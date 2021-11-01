#!/bin/bash

SCRIPT=${1}
ADDR_FILE_NAME=${2}

cardano-cli address build --payment-script-file ${SCRIPT} ${MAGIC} --out-file ${ADDR_FILE_NAME} 

