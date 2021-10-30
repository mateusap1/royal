from CardanoHandler import CardanoHandler
from utils import ada, wait_for_balance
import sys

MAX_TIME = 300
INTERVALS = 20


# Empty the "first_case" script ADA
def empty_first_case_script():
    ch = CardanoHandler()
    script_name = "first_case"

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)
    blc = ch.wallet_balance(addr)

    # Make sure script is not empty
    if blc == 0:
        raise Exception(
            f"Script {addr} already holds tokens. Make sure it's not empty"
        )
    
    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]
    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    ch.distribute_from_script("alice", script_name, [
        {"user_address": alice.addr, "amount": int(blc/3)+1},
        {"user_address": bob.addr, "amount": int(blc/3)+1},
        {"user_address": charlie.addr, "amount": int(blc/3)+1}
    ])


def empty_third_case_script():
    ch = CardanoHandler()
    script_name = "third_case"

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)
    blc = ch.wallet_balance(addr)

    # Make sure script is not empty
    if blc == 0:
        raise Exception(
            f"Script {addr} already holds tokens. Make sure it's not empty"
        )
    
    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]
    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    if blc < ada(12):
        ch.send_to_script("alice", script_name, (ada(12) - blc))
        wait_for_balance(ch, addr, ada(12), INTERVALS, MAX_TIME)

        blc = 12

    ch.distribute_from_script("alice", script_name, [
        {"user_address": alice.addr, "amount": int(blc/3)+100},
        {"user_address": bob.addr, "amount": int(blc/3)+100},
        {"user_address": charlie.addr, "amount": int(blc/3)+100}
    ])

def empty_fifth_case_script():
    ch = CardanoHandler()
    script_name = "fifth_case"

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)
    blc = ch.wallet_balance(addr)

    # Make sure script is not empty
    if blc == 0:
        raise Exception(
            f"Script {addr} already holds tokens. Make sure it's not empty"
        )
    
    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    ch.distribute_from_script("alice", script_name, [
        {"user_address": bob.addr, "amount": int(blc/2)+1},
        {"user_address": charlie.addr, "amount": int(blc/2)+1}
    ])


def first_scenario():
    ch = CardanoHandler()
    script_name = "first_case"

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)

    print(
        f"Using script {addr} with the distribution 1/3 to Alice, " +
        "1/3 to Bob and 1/3 to Charlie and minimum utxo value of 1 ADA"
    )

    # Make sure script is empty
    if ch.wallet_balance(addr) > 0:
        print(f"Script not empty, calling function to empty {addr}")
        empty_first_case_script()
        wait_for_balance(ch, addr, 0, INTERVALS, MAX_TIME)

    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]
    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    # Alice sends 9 ADA to the scipt
    ch.send_to_script("alice", script_name, ada(9))

    print(f"9 ADA sent from Alice to {addr}")

    wait_for_balance(ch, addr, ada(9), INTERVALS, MAX_TIME)

    ch.distribute_from_script("alice", script_name, [
        {"user_address": alice.addr, "amount": ada(3)},
        {"user_address": bob.addr, "amount": ada(3)},
        {"user_address": charlie.addr, "amount": ada(3)}
    ])

    print(
        "Alice tried to consume the script, giving 3 ADA to herself, " +
        "3 ADA to Bob and 3 ADA to Charlie"
    )
    print("Transaction should succeed, please restart the node for the next scenario...")


def second_scenario():
    ch = CardanoHandler()
    script_name = "first_case"  # Same script as first case

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)

    print(
        f"Using script {addr} with the distribution 1/3 to Alice, " +
        "1/3 to Bob and 1/3 to Charlie and minimum utxo value of 1 ADA"
    )

    # Make sure script is empty
    if ch.wallet_balance(addr) > 0:
        print(f"Script not empty, calling function to empty {addr}")
        empty_first_case_script()
        wait_for_balance(ch, addr, 0, INTERVALS, MAX_TIME)

    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]

    # Alice sends 9 ADA to the scipt
    ch.send_to_script("alice", script_name, ada(9))

    print(f"9 ADA sent from Alice to {addr}")

    wait_for_balance(ch, addr, ada(9), INTERVALS, MAX_TIME)

    print(
        "Alice will try to consume the script, giving 9 ADA to herself. " +
        "Since this is the wrong distribution, you should receive an exception."
    )

    ch.distribute_from_script("alice", script_name, [
        {"user_address": alice.addr, "amount": ada(9)},
    ])

    print("Something went wrong, transaction should fail")


def third_scenario():
    ch = CardanoHandler()
    script_name = "third_case"  # Minimum UTxO now 4 ADA

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)

    print(
        f"Using script {addr} with the distribution 1/3 to Alice, " +
        "1/3 to Bob and 1/3 to Charlie and minimum utxo value of **4 ADA**"
    )

    # Make sure script is empty
    if ch.wallet_balance(addr) > 0:
        print(f"Script not empty, calling function to empty {addr}")
        empty_third_case_script()
        wait_for_balance(ch, addr, 0, INTERVALS, MAX_TIME)

    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]
    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    # Alice sends 9 ADA to the scipt
    ch.send_to_script("alice", script_name, ada(9))

    print(f"9 ADA sent from Alice to {addr}")

    wait_for_balance(ch, addr, ada(9), INTERVALS, MAX_TIME)

    print(
        "Alice will try to consume the script, giving 3 ADA to herself, " +
        "3 ADA to Bob and 3 ADA to Charlie. Since this is below the minimum " +
        "UTxO value, the transaction should fail, throwing an exception"
    )

    ch.distribute_from_script("alice", script_name, [
        {"user_address": alice.addr, "amount": ada(3)},
        {"user_address": bob.addr, "amount": ada(3)},
        {"user_address": charlie.addr, "amount": ada(3)}
    ])

    print("Something went wrong, transaction should fail")


def fourth_scenario():
    ch = CardanoHandler()
    script_name = "first_case"  # Same script as first case

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)

    print(
        f"Using script {addr} with the distribution 1/3 to Alice, " +
        "1/3 to Bob and 1/3 to Charlie and minimum utxo value of 1 ADA"
    )

    # Make sure script is empty
    if ch.wallet_balance(addr) > 0:
        print(f"Script not empty, calling function to empty {addr}")
        empty_first_case_script()
        wait_for_balance(ch, addr, 0, INTERVALS, MAX_TIME)

    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]
    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    # Alice sends 3 ADA
    ch.send_to_script("alice", script_name, ada(3))

    print(f"3 ADA sent from Alice to {addr}")

    wait_for_balance(ch, addr, ada(3), INTERVALS, MAX_TIME)

    # Bob sends 3 ADA
    ch.send_to_script("bob", script_name, ada(3))

    print(f"3 ADA sent from Bob to {addr}")

    wait_for_balance(ch, addr, ada(6), INTERVALS, MAX_TIME)

    # Charlie sends 3 ADA
    ch.send_to_script("charlie", script_name, ada(3))

    print(f"3 ADA sent from Charlie to {addr}")

    wait_for_balance(ch, addr, ada(9), INTERVALS, MAX_TIME)

    ch.distribute_from_script("alice", script_name, [
        {"user_address": alice.addr, "amount": ada(3)},
        {"user_address": bob.addr, "amount": ada(3)},
        {"user_address": charlie.addr, "amount": ada(3)}
    ])

    print("Alice tried to consume the script, giving 3 ADA to herself, 3 ADA to Bob and 3 ADA to Charlie")
    print("Transaction should work")


def fifth_scenario():
    ch = CardanoHandler()
    script_name = "fifth_case"

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)

    print(
        f"Using script {addr} with the distribution 1/2 to Bob, " +
        "1/2 to Charlie and minimum utxo value of 1 ADA"
    )

    # Make sure script is empty
    if ch.wallet_balance(addr) > 0:
        print(f"Script not empty, calling function to empty {addr}")
        empty_fifth_case_script()
        wait_for_balance(ch, addr, 0, INTERVALS, MAX_TIME)

    # Now we also need David
    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]
    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    # Alice (not one of the receivers) sends 8 ADA to the scipt
    ch.send_to_script("alice", script_name, ada(8))

    print(f"9 ADA sent from Alice to {addr}")

    wait_for_balance(ch, addr, ada(8), INTERVALS, MAX_TIME)

    ch.distribute_from_script("alice", script_name, [
        {"user_address": bob.addr, "amount": ada(4)},
        {"user_address": charlie.addr, "amount": ada(4)}
    ])

    print(
        "Alice tried to consume the script, giving 4 ADA to Bob and 4 ADA to Charlie"
    )
    print("Transaction should succeed, please restart the node for the next scenario...")


def sixth_scenario():
    ch = CardanoHandler()
    script_name = "first_case"  # Same script as first case

    if not (script_name in ch.scripts):
        raise Exception(f"Script {script_name} not found")

    addr = ch.script_address(script_name)

    print(
        f"Using script {addr} with the distribution 1/3 to Alice, " +
        "1/3 to Bob and 1/3 to Charlie and minimum utxo value of 1 ADA"
    )

    # Make sure script is empty
    if ch.wallet_balance(addr) > 0:
        print(f"Script not empty, calling function to empty {addr}")
        empty_first_case_script()
        wait_for_balance(ch, addr, 0, INTERVALS, MAX_TIME)

    if not (all([x in ch.users for x in ["alice", "bob", "charlie"]])):
        raise Exception("Couldn't find Alice, Bob and/or Charlie")

    alice = ch.users["alice"]
    bob = ch.users["bob"]
    charlie = ch.users["charlie"]

    # Alice sends 9 ADA to the scipt
    ch.send_to_script("alice", script_name, ada(9))

    print(f"9 ADA sent from Alice to {addr}")

    # wait_for_balance(ch, addr, ada(9), INTERVALS, MAX_TIME)

    # Bob sends 9 ADA to the scipt
    ch.send_to_script("bob", script_name, ada(9))

    print(f"9 ADA sent from Bob to {addr}")

    wait_for_balance(ch, addr, ada(18), INTERVALS, MAX_TIME)

    ch.distribute_from_script("alice", script_name, [
        {"user_address": alice.addr, "amount": ada(3)},
        {"user_address": bob.addr, "amount": ada(3)},
        {"user_address": charlie.addr, "amount": ada(3)},
        {
            "script_address": addr,
            "amount": ada(9),
            "datum_hash": ch.datum_hash(script_name)
        }
    ])

    print(
        "Alice tried to consume the script, giving 3 ADA to herself, " +
        "3 ADA to Bob, 3 ADA to Charlie and keeping 9 ADA in the script"
    )
    print("Transaction should succeed, please restart the node for the next scenario...")


def main():
    if len(sys.argv) == 1:
        print("Command syntax:\npython3 scenarios.py <scenario_number>")
        sys.exit(1)

    try:
        num = int(sys.argv[1])

        if (num < 0) or (num > 6):
            print("Command argument must be number between 1 and 6")
            sys.exit(1)
        elif num == 1:
            first_scenario()
        elif num == 2:
            second_scenario()
        elif num == 3:
            third_scenario()
        elif num == 4:
            fourth_scenario()
        elif num == 5:
            fifth_scenario()
        elif num == 6:
            sixth_scenario()

    except ValueError:
        print("Command argument must be number between 1 and 6")
        sys.exit(1)


if __name__ == "__main__":
    main()
