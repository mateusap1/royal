# Distributor Guide

In order to facilitate the testing of the distribution script, I decided to make a
python wrapper around cardano-cli. After that, I made six functions, each one
testing a different scenario I thought could show script vulnerabilities.

Before moving into the scenarios and how to try them in the testnet, make sure that
cardano-node and cardano-cli are installed

```bash
$ cardano-node --version
cardano-node 1.30.1 - linux-x86_64 - ghc-8.10
git rev 0fb43f4e3da8b225f4f86557aed90a183981a64f
```

```bash
$ cardano-cli --version
cardano-cli 1.30.1 - linux-x86_64 - ghc-8.10
git rev 0fb43f4e3da8b225f4f86557aed90a183981a64f
```

If they are not, you may follow the intructions
[here](https://docs.cardano.org/getting-started/installing-the-cardano-node).

After you made sure both executables are installed, run cardano node in the
testnet mode. The following is the command I run, but depending on how you
installed cardano-node, your command may look different

```bash
cardano-node run \
    --topology testnet-topology.json \
    --database-path db \
    --socket-path node.socket \
    --host-addr 0.0.0.0 \
    --port 3001 \
    --config testnet-config.json \
```

After this, take a look at "data/info.json" and insert the information you will
use. For the scenarios to work you need three "users": "alice", "bob" and
"charlie". The "path" argument from each user is a folder where the keys and
address from the specific user are located ("payment.addr", "payment.vkey",
"payment.skey"). They should all have the name "payment" and be located at
individual folders.

Finally, we can start to run our scenarios

## First scenario

First, make sure the node is running and it's synchronized with the testnet.

```bash
$ cardano-cli query tip --tesnet-magic 1097911063
{
    "epoch": 165,
    "hash": "fdc6793a20200de88c70436925eeca6713e509bc73553fcf959db3024e666231",
    "slot": 41225803,
    "block": 3032237,
    "era": "Alonzo",
    "syncProgress": "100.00"
}
```

If "syncProgress" is 100%, it means the node is synchornised, otherwise, you
need to wait some time.
