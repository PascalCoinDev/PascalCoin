# PascalCoin 5.9.4

This patch adds a standard macOS download. The disk image
`PascalCoin-5.9.4-x86_64-darwin.dmg` contains the two wallets as Mac apps:

- PascalCoin Wallet, the classic wallet.
- PascalCoin Wallet Experimental, the newer interface.

Open the disk image and drag the apps to Applications. The apps are not
notarised by Apple, so macOS asks for confirmation on the first start. On macOS
15 and later, click Done on the warning, then open System Settings, Privacy &
Security and click Open Anyway. On macOS 14 and earlier, Control-click the app
and choose Open. The apps are built for Intel Macs and run on Apple silicon
through Rosetta.

Wallet, node and storage behavior is unchanged from 5.9.3. Classic and
Experimental remain separate applications in each platform package.

## Applications

| Application | Executable | macOS app |
|---|---|---|
| Classic wallet | `PascalCoinWalletLazarus` | PascalCoin Wallet |
| Experimental wallet | `PascalCoinWalletExperimental` | PascalCoin Wallet Experimental |
| Daemon | `pascalcoin_daemon` | |
| Reference miner | `PascalCoinMiner` | |

Windows executables have the `.exe` suffix. Windows and Linux archives include
the OpenSSL 1.1 runtime; keep it beside the executables. The macOS build uses
CryptoLib4Pascal and does not require OpenSSL. The macOS `.tar.gz` archive keeps
all four programs for command-line use. Linux wallets require the native GTK2
runtime libraries.
On Linux, launch the executables from the extracted folder so the bundled runtime
can be found.

See [Build 5.9.4](https://github.com/PascalCoin/PascalCoin/blob/5.9.4/CHANGELOG.md#build-594)
for the complete change list.
