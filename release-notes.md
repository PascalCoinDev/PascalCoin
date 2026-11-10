# PascalCoin 5.9.6

This patch fixes account selection and operation Details in the Experimental wallet.

## Changes

- Clicking the selected My Accounts row again keeps that account and its history selected.
- History is labelled All Account Operations or Selected Account Operations to show its scope.
- The whole Details button hides without a usable operation selection and returns when the selected operation is available.
- Key-filter clearing, the 250 ms operations refresh and synchronization behavior are preserved.

Native Delphi and Lazarus regressions cover repeated clicks, both sides of an
owned transfer and filter clearing. Details visibility is checked at 100%, 125%
and 150% scaling.

## macOS

Download `PascalCoin-5.9.6-macOS.dmg` and drag either wallet app to Applications.
Both wallets run natively on Apple silicon and Intel Macs.

The apps are not notarized by Apple. On macOS 15 and later, click Done on the
first-start warning, then open System Settings, Privacy & Security and click
Open Anyway. On macOS 14 and earlier, Control-click the app and choose Open.

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

See [Build 5.9.6](https://github.com/PascalCoin/PascalCoin/blob/5.9.6/CHANGELOG.md#build-596)
for the complete change list.
