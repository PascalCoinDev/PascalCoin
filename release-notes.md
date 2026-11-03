# PascalCoin 5.9.1

This patch updates application locking in the classic and experimental wallets,
node, daemon and reference miner, and corrects legacy checkpoint loading and recovery.

- Use `TReadWriteLock` for node, network, storage, mining and wallet guards.
- Allow concurrent readers on proven read-only paths and retain recursive writes for mutable storage, streams, caches and callbacks.
- Preserve timed lock acquisition, connection ownership and cooperative shutdown.
- Balance operation-component ownership before freeing its lock.
- Preserve the original storage error during failed safebox replacement and constructor cleanup.
- Report progress and honor cancellation during legacy checkpoint inspection.
- Restore skip-and-replay for checkpoints ahead of local history while retaining unreadable-storage protection.
- Buffer legacy checkpoint reads with the framework's bounded stream decorator and optional ownership.
- Configure read buffering and stream-length caching independently; both default on, with explicit unread-data flushing before disabling read buffering.
- Buffer pending-operation input, serialized block headers and optional RandomHash input with their existing sharing rules.
- Keep log producers independent of GUI rendering and batch native log updates in both wallets.
- Align experimental startup with Classic's configured indexed reader, legacy-file fallback and deferred index flushing.
- Fit compact grid navigation beside large totals before wrapping the footer.
- Correct Delphi banner alpha edges and improve Network Details label spacing in Delphi and Lazarus.
- Add regression tests for actual recursive owners, exclusion, wakeups and timeout conversion.
- Update runtime and executable versions to 5.9.1.

Operation validation retains its existing rules. The experimental wallet adopts
Classic's existing blockchain index and cache policy. Both wallets remain
separate applications in each platform package.

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

See [Build 5.9.1](https://github.com/PascalCoin/PascalCoin/blob/5.9.1/CHANGELOG.md#build-591)
for the complete change list.
