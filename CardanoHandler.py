import json
import os
import shutil
from utils import tx_in_wrap, sig_wrap, tx_out_wrap, tx_whole_script_wrap, ada
from time import sleep


INFO_PATH = "./data/info.json"
TEMP_PATH = "./data/tmp"


class User (object):
    def __init__(self, name, path):
        self.name = name
        self.path = path

        self.load_info()

    def load_info(self):
        self.addr = os.popen(f"cat {self.path}/payment.addr").read()
        self.skey = f"{self.path}/payment.skey"


class Script (object):
    def __init__(self, name, script, datum, redeemer):
        self.name = name
        self.script = script
        self.datum = datum
        self.redeemer = redeemer


class UTxO (object):
    def __init__(self, address, uid, tokens):
        self.address = address
        self.uid = uid
        self.tokens = tokens

    def parsed(self):
        return f"{self.address}#{self.uid}"

    def display(self):
        print(f"Address: {self.address}#{self.uid}")
        print(f"Tokens Amount: {self.tokens}")


class TxUserIn(object):
    def __init__(self, handler, name):
        self.handler = handler
        self.name = name

        self.command = self.commandify()

    def commandify(self):
        if self.name in self.handler.users:
            addr = self.handler.users[self.name].addr
        elif self.name in self.handler.scripts:
            addr = self.handler.script_address(self.name)

        utxos = self.handler.address_utxos(addr)

        command = "\n".join(
            map(lambda utxo: tx_in_wrap(utxo.parsed()), utxos)
        )

        return command


class TxScriptIn(object):
    def __init__(self, handler, name):
        self.handler = handler
        self.name = name

        self.command = self.commandify()

    def commandify(self):
        if self.name in self.handler.users:
            addr = self.handler.users[self.name].addr
        elif self.name in self.handler.scripts:
            addr = self.handler.script_address(self.name)

        utxos = self.handler.address_utxos(addr)
        script = self.handler.scripts[self.name].script
        datum = self.handler.scripts[self.name].datum
        redeemer = self.handler.scripts[self.name].redeemer

        command = "\n".join(
            map(
                lambda utxo: tx_whole_script_wrap(
                    utxo.parsed(), script, datum, redeemer
                ),
                utxos
            )
        )

        return command


class TxUserOut(object):
    def __init__(self, address, amount):
        self.address = address
        self.amount = amount

    def commandify(self):
        return tx_out_wrap(self.address, self.amount)


class TxScriptOut(object):
    def __init__(self, address, amount, datum_hash):
        self.address = address
        self.amount = amount
        self.datum_hash = datum_hash

    def commandify(self):
        return \
            tx_out_wrap(self.address, self.amount) + \
            f"\n--tx-out-datum-hash {self.datum_hash} \\"


class Tx(object):
    def __init__(self, handler):
        self.handler = handler

        self.user_inputs = []
        self.script_inputs = []

        self.user_outputs = []
        self.script_outputs = []

        self.change_addr = None
        self.collateral = None

    def build_ready(self):
        return \
            (len(self.user_inputs) + len(self.script_inputs) > 0) \
            and (len(self.user_outputs) + len(self.script_outputs) > 0) \
            and self.change_addr is not None

    def add_change_address(self, addr):
        self.change_addr = addr

    def add_collateral(self, user_name):
        if not (user_name in self.handler.users):
            raise Exception("User not found")

        addr = self.handler.users[user_name].addr
        utxos = self.handler.address_utxos(addr)

        collateral_utxo = None
        for utxo in utxos:
            if (utxo.tokens > ada(2)) and collateral_utxo is None:
                collateral_utxo = utxo
            elif (utxo.tokens > ada(2)) and (utxo.tokens < collateral_utxo.tokens):
                collateral_utxo = utxo

        if collateral_utxo is None:
            raise Exception("No collateral found within user UTxOs")

        self.collateral = collateral_utxo

    def add_user_input(self, name):
        txIn = TxUserIn(self.handler, name)
        self.user_inputs.append(txIn)

    def add_script_input(self, name):
        txIn = TxScriptIn(self.handler, name)
        self.script_inputs.append(txIn)

    def add_user_output(self, address, amount):
        txUserOut = TxUserOut(address, amount)
        self.user_outputs.append(txUserOut)

    def add_script_output(self, address, amount, datum_hash):
        txScriptOut = TxScriptOut(address, amount, datum_hash)
        self.script_outputs.append(txScriptOut)

    def submit_tx(self):
        if not self.build_ready():
            raise Exception(
                "Tx not ready yet, make sure you added inputs, outputs and the change address"
            )

        parsed_inputs = "\n".join(
            map(lambda x: x.commandify(), self.user_inputs + self.script_inputs)
        )
        parsed_outputs = "\n".join(
            map(lambda x: x.commandify(), self.user_outputs + self.script_outputs))

        # Make a temporary directory
        os.mkdir(TEMP_PATH)

        if self.collateral:
            parsed_collateral = \
                f"--tx-in-collateral {self.collateral.parsed()} \\"

            build_command = f"""
                cardano-cli transaction build \\
                    --alonzo-era \\
                    {parsed_inputs}
                    {parsed_collateral}
                    {parsed_outputs}
                    --change-address {self.change_addr} \\
                    --testnet-magic {str(self.handler.magic)} \\
                    --protocol-params-file {str(self.handler.protocol_parameters)} \\
                    --out-file {TEMP_PATH}/tx.raw
            """
        else:
            build_command = f"""
                cardano-cli transaction build \\
                    --alonzo-era \\
                    {parsed_inputs}
                    {parsed_outputs}
                    --change-address {self.change_addr} \\
                    --testnet-magic {str(self.handler.magic)} \\
                    --protocol-params-file {str(self.handler.protocol_parameters)} \\
                    --out-file {TEMP_PATH}/tx.raw
            """

        exit_code = os.system(build_command)

        if exit_code != 0:
            shutil.rmtree(TEMP_PATH)
            raise Exception("Transaction build failed")

        required_sigs = "\n".join(
            map(
                lambda x: sig_wrap(x.handler.users[x.name].skey),
                self.user_inputs
            )
        )

        sign_command = f"""
            cardano-cli transaction sign \\
                --testnet-magic {str(self.handler.magic)} \\
                --tx-body-file {TEMP_PATH}/tx.raw \\
                {required_sigs}
                --out-file {TEMP_PATH}/tx.sign
        """

        exit_code = os.system(sign_command)

        if exit_code != 0:
            shutil.rmtree(TEMP_PATH)
            raise Exception("Transaction signing failed")

        submit_command = f"""
            cardano-cli transaction submit \\
                --testnet-magic {str(self.handler.magic)} \\
                --tx-file {TEMP_PATH}/tx.sign
        """

        exit_code = os.system(submit_command)

        if exit_code != 0:
            shutil.rmtree(TEMP_PATH)
            raise Exception("Transaction submission failed")

        shutil.rmtree(TEMP_PATH)


class CardanoHandler (object):
    def __init__(self):
        self.users = {}
        self.scripts = {}

        self.parse_json(INFO_PATH)

    def parse_json(self, info_path):
        with open(info_path, "r") as f:
            parsed = json.loads(f.read())

            self.magic = parsed["magic"]
            self.protocol_parameters = parsed["protocol_params_file"]
            self.node = parsed["node"]

            for user in parsed["users"]:
                self.users[user["name"]] = User(user["name"], user["path"])

            for script in parsed["scripts"]:
                self.scripts[script["name"]] = Script(
                    script["name"],
                    script["script"],
                    script["datum"],
                    script["redeemer"]
                )

    def users_info(self):
        for i, user in enumerate(self.users.values()):
            print(user.name)
            print(user.path)
            print(user.addr)
            print(user.skey)

            if i < (len(self.users) - 1):
                print("------/------")

    def scripts_info(self):
        for i, script in enumerate(self.scripts.values()):
            print(script.name)
            print(script.script)
            print(script.datum)
            print(script.redeemer)

            if i < (len(self.scripts) - 1):
                print("------/------")

    def address_utxos(self, addr):
        command = f"""
            cardano-cli query utxo \\
                --testnet-magic {str(self.magic)} \\
                --address {addr}
        """

        raw_result = os.popen(command).read()
        splitted_result = raw_result.split("\n")[2:-1]

        utxos = []
        for raw in splitted_result:
            addr, uid, value = tuple(raw.split()[:3])
            utxos.append(UTxO(addr, uid, int(value)))

        return utxos

    def wallet_balance(self, addr):
        utxos = self.address_utxos(addr)

        return sum(map(lambda x: x.tokens, utxos))

    def send_to_address(self, name, address, amount):
        tx = Tx(self)
        tx.add_user_input(name)
        tx.add_user_output(address, amount)
        tx.change_addr = self.users[name].addr

        tx.submit_tx()

    def script_address(self, script_name):
        command = f"""
            cardano-cli address build \\
                --payment-script-file {self.scripts[script_name].script} \\
                --testnet-magic {self.magic}
        """

        return os.popen(command).read()

    def datum_hash(self, script_name):
        command = f"""
            cardano-cli transaction hash-script-data \\
                --script-data-file {self.scripts[script_name].datum}
        """

        return os.popen(command).read().replace("\n", "")

    def send_to_script(self, user_name, script_name, amount):
        script_address = self.script_address(script_name)
        datum_hash = self.datum_hash(script_name)

        tx = Tx(self)
        tx.add_user_input(user_name)
        tx.add_script_output(script_address, amount, datum_hash)
        tx.change_addr = self.users[user_name].addr

        tx.submit_tx()

    def distribute_from_script(self, signer, script_name, receivers):
        tx = Tx(self)
        tx.add_user_input(signer)
        tx.add_script_input(script_name)

        for receiver in receivers:
            if ("user_address" in receiver) and ("amount" in receiver):
                tx.add_user_output(
                    receiver["user_address"], receiver["amount"]
                )
            elif ("script_address" in receiver) and \
                 ("datum_hash" in receiver) and ("amount" in receiver):
                tx.add_script_output(
                    receiver["script_address"],
                    receiver["amount"],
                    receiver["datum_hash"]
                )
            else:
                raise Exception(
                    "Wrong `receivers` argument"
                )

        tx.add_change_address(self.users[signer].addr)
        tx.add_collateral(signer)

        tx.submit_tx()


if __name__ == "__main__":
    ch = CardanoHandler()
    print(ch.script_address("first_case"))
