# Distributor Guide

In order to facilitate the testing of the distribution script, I decided to make a
python wrapper around cardano-cli. After that, I made six functions, each one
testing a different scenario I thought could show script vulnerabilities.

Before moving into the scenarios and how to try them in the testnet, make sure that
cardano-node and cardano-cli are installed

```bash
cardano-node --version
```

```bash
cardano-node 1.30.1 - linux-x86_64 - ghc-8.10
git rev 0fb43f4e3da8b225f4f86557aed90a183981a64f
```

```bash
cardano-cli --version
```

```bash
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
use. For the scenarios to work you need three users: "alice", "bob" and
"charlie". The "path" argument from each user is a folder where the keys and
address from the specific user are located ("payment.addr", "payment.vkey",
"payment.skey"). They should all have the name "payment" and be located at
individual folders.

Finally, we can start to run our scenarios

## First scenario

In the first scenario, Alice sends 9 ADA to the script with the distribution
`{Alice: 1, Bob: 1, Charlie: 1}` (meaing each one will receive a third) and a
minimum UTxO value of 1 ADA. Then Alice consumes the script, giving 3 ADA to
herself, 3 ADA to Bob and 3 ADA to Charlie. Because the distribution is correct,
the transaction should validate.

Before running it, make sure the node is running and is synchronized with the
testnet.

```bash
cardano-cli query tip --tesnet-magic 1097911063
```

```bash
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

To start, run the following command

```bash
python3 scenarios.py 1
```

After executing the command, you should either see a message saying that the
script was not empty

```
Script not empty, calling function to empty addr_test1wzlrk5ra8pts700jczd44jg328zpz7mjjc7wqfwt3n48pngfdawwn
Transaction successfully submitted.
Please restart the node to update the script balance
```

or that 9 ADA were sent from Alice to the script

```
Transaction successfully submitted.
9 ADA sent from Alice to addr_test1wzlrk5ra8pts700jczd44jg328zpz7mjjc7wqfwt3n48pngfdawwn
Please restart the node to update the script balance
```

In both cases it will ask you to restart the node (if the transaction was
successful) so the node can update it's information. To make sure the
transaction was submitted, you should wait 20-30 seconds and search for the
script address in the [testnet explorer](https://explorer.cardano-testnet.iohkdev.io)

If you see that the balance of the script is 0 ADA in the first case or 9 ADA in
the second, it worked and you should restart the node.

After the node is restarted, there could be a 20 seconds dealay, but you should
receive the following message

```
Transaction successfully submitted.
Alice tried to consume the script, giving 3 ADA to herself, 3 ADA to Bob and 3 ADA to Charlie
Transaction should succeed
```

Wait 20-30 seconds again and look in the testnet for the last transaction related
to our script, it should show something like this

![Example 1 - Expected result](./images/example-1.png)

You should, again, restart the node.
