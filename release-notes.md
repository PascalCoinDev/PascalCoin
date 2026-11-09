# PascalCoin 5.9.5

The macOS wallets now run natively on Apple silicon (M1 and later) and on Intel
Macs. Download `PascalCoin-5.9.5-macOS.dmg`, open it and drag the apps to
Applications:

- PascalCoin Wallet, the classic wallet.
- PascalCoin Wallet Experimental, the newer interface.

The apps are not notarized by Apple, so macOS asks for confirmation on the first
start. On macOS 15 and later, click Done on the warning, then open System
Settings, Privacy & Security and click Open Anyway. On macOS 14 and earlier,
Control-click the app and choose Open.

Wallet, node and storage behavior is unchanged from 5.9.4. The READMEs have been
rewritten to be clear and concise.

## Applications

| Application | Executable | macOS app |
|---|---|---|
| Classic wallet | `PascalCoinWalletLazarus` | PascalCoin Wallet |
| Experimental wallet | `PascalCoinWalletExperimental` | PascalCoin Wallet Experimental |
| Daemon | `pascalcoin_daemon` | |
| Reference miner | `PascalCoinMiner` | |

Windows executables have the `.exe` suffix. Windows and Linux archives include
the OpenSSL 1.1 runtime, which must stay beside the executables. The macOS build
uses CryptoLib4Pascal and does not require OpenSSL. The macOS `.tar.gz` archive
keeps all four programs for command-line use. Its daemon and miner are Intel
builds, which Apple silicon runs through Rosetta. Linux wallets require the native GTK2
runtime libraries. On Linux, launch the executables from the extracted folder so
the bundled runtime can be found.

See [Build 5.9.5](https://github.com/PascalCoin/PascalCoin/blob/5.9.5/CHANGELOG.md#build-595)
for the complete change list.
