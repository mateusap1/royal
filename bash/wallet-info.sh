#!/bin/bash

ADDR=${1} # The address we want to inspect

cardano-cli query utxo ${MAGIC} --address ${ADDR}
