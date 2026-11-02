# PascalCoin 5.9

Classic and experimental wallets are separate applications. Both are included in
all Windows, Linux and Intel macOS release archives and can be kept side by side.

- Add the experimental wallet with native Delphi/VCL and Lazarus/FPC support.
- Import [Sphere 10 Pascal Framework](https://github.com/HermanSchoenfeld/PascalFramework) for caching, concurrency, scopes, grids and wizards.
- Add bulk-fetch caching, read/write locks, cleanup scopes and Single/Auto/Manual grid paging.
- Complete operation wizards, advanced operations and RAW/multioperation signing, including batch-change approval and optional fee payers.
- Run account/history queries, filters, totals and type statistics on cancellable workers.
- Update synchronization and status from node events; refresh account selections immediately outside synchronization.
- Fix peer reconnection, safebox recovery, pending restore/saving and cooperative shutdown.
- Reuse mining keys and improve native layout, icons, loading feedback and operation dialogs.
- Keep paging controls visible in Release builds, reflow large operation fields and open details through explicit actions.
- Build and package both wallets on Windows, Linux and macOS; run the Windows wallet regressions and portable core/framework lock tests in CI.
- Add Delphi/FPC screenshots and credit Herman Schoenfeld for the experimental GUI while preserving Albert Molina's original attribution.
- Align application and executable versions at 5.9.

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

See [Build 5.9](https://github.com/PascalCoin/PascalCoin/blob/5.9/CHANGELOG.md#build-59---2026-10-28)
for the complete change list.
