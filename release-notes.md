# PascalCoin 5.9.3

This patch applies maintenance corrections from a review of the 5.9.1 and 5.9.2
changes across the wallets, core storage and the embedded Sphere10 library.

- Keep the Delphi Classic and Experimental log views following the newest batch.
- Translate the grid paging-mode menu and hint through resourcestrings while keeping fixed component names.
- Raise a constructed exception for unsupported dispose policies in the embedded library.
- Remove a redundant thread lookup from abstract-memory check detection and size header read-ahead buffers from the block header constant.
- Create the Experimental startup progress lock without recursion.
- Key the RandomHash adoption regression on the shipping protocol define and wait for queued workers in the scope regressions.
- Update runtime and executable versions to 5.9.3.

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

See [Build 5.9.3](https://github.com/PascalCoin/PascalCoin/blob/5.9.3/CHANGELOG.md#build-593)
for the complete change list.
