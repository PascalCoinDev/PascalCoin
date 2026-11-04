# PascalCoin 5.9.2

This patch restores Experimental wallet account history when older operations
remain in legacy blockchain files outside the newer index or within its gaps.

- Resolve legacy blocks through the same storage lookup used by Classic before reading their operations.
- Keep Recent Operations controlled by the selected 7 Days, 30 Days or Maximum option.
- Return only available history when a block is absent from both stores.
- Add three shared Delphi/FPC regressions and register them in the wallet unit runner.
- Update runtime and executable versions to 5.9.2.

This patch retains existing validation, persistence and synchronization behavior.
Classic and Experimental remain separate applications in each platform package.

## Applications

| Application | Executable |
|---|---|
| Classic wallet | `PascalCoinWalletLazarus` |
| Experimental wallet | `PascalCoinWalletExperimental` |
| Daemon | `pascalcoin_daemon` |
| Reference miner | `PascalCoinMiner` |

Windows executables have the `.exe` suffix. Windows and Linux archives include
the OpenSSL 1.1 runtime; keep it beside the executables. The macOS build uses
CryptoLib4Pascal and does not require OpenSSL. Linux wallets require the native
GTK2 runtime libraries.
On Linux, launch the executables from the extracted folder so the bundled runtime
can be found.

See [Build 5.9.2](https://github.com/PascalCoin/PascalCoin/blob/5.9.2/CHANGELOG.md#build-592)
for the complete change list.
