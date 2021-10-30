from time import sleep

def ada(lvc):
    return lvc * 1_000_000


def sig_wrap(path):
    return f"--signing-key-file {path} \\"


def tx_in_wrap(utxo):
    return f"--tx-in {utxo} \\"


def tx_in_script_file_wrap(script):
    return f"--tx-in-script-file {script} \\"


def tx_in_datum_file_wrap(datum):
    return f"--tx-in-datum-file {datum} \\"


def tx_in_redeemer_file_wrap(redeemer):
    return f"--tx-in-redeemer-file {redeemer} \\"


def tx_whole_script_wrap(utxo, script, datum, redeemer):
    return "\n".join([
        tx_in_wrap(utxo),
        tx_in_script_file_wrap(script),
        tx_in_datum_file_wrap(datum),
        tx_in_redeemer_file_wrap(redeemer)
    ])


def tx_out_wrap(address, amount):
    return f"--tx-out {address}+{amount} \\"


def wait_for_balance(handler, addr, blc, intervals):
    i = 0
    while True:
        if (handler.wallet_balance(addr) == blc):
            break

        print("Please restart the node to update the script balance")

        if i > 0:
            print(f"Script balance still not {blc}...")
            print("Verifying again...")
        
        sleep(intervals)

        i += intervals
