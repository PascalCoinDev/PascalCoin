# Native experimental wallet startup checks

These checks run the experimental wallet's startup and close paths offline on a
private Windows desktop. [Other native checks](#other-native-checks) run
separately.

## Run the checks

Build the [shipping wallet](../../../gui-experimental/README.md),
`src/PascalCoinWalletExperimental.exe`, then run from the repository root:

```powershell
& src/tests/experimental/native/RunWalletStartupNativeTests.ps1
```

The script builds `PrivateDesktopBroker.exe` and `WalletStartupProbe.exe` into
the output folder with FPC and the LCL only. Exit code 0 means every process
passed.

- `-FpcPath` and `-LazarusDirectory` locate a Windows x64 FPC and Lazarus.
- `-OutputDirectory <new-directory>` replaces the default
  `src/tests/experimental/lib/native-<guid>` folder.
- `-WalletExecutable <path>` selects another shipping wallet.

The output folder holds build and broker logs, `<process>.log` files and, in
`temporary-data`, the data directories.

## Private-desktop broker

`PrivateDesktopBroker.exe <executable> <log file>` runs a probe on a new private
desktop, passes it only the log path and returns its exit code. It never
switches the input desktop or targets other processes. It stops such a probe
after 30 minutes, startup probes after 30 seconds and shipping runs after 10 seconds.

## Processes

Every process gets a new temporary `--data-dir` and `--offline`. None loads the
normal wallet or connects to peers.

- `loaded` completes startup on empty data and raises `Loaded` once. It checks
  window layout, screen switches, maximize and restore, and footer buttons at
  normal and enlarged DPI. It also checks Wallet Keys artwork, modeless focus,
  offline enforcement, key changes, classic tools and callbacks while the
  mempool is held. It quits through Close and Yes.
- `close-loading` shuts down just after the loader starts, and
  `close-before-load` shuts down from `AppStarted`, before the worker starts.
  Both require zero `Loaded` callbacks and a fully released application.
- `slow-close` closes the window while a startup task is running. The window
  must stay enabled with wallet commands disabled, and cancellation must finish
  without a `Loaded` callback.
- `actual-first-run` and `actual-no-dialogs` start the shipping wallet with and
  without startup dialogs. Each needs a visible, enabled main window and no
  automatic About dialog, then closes through the exit confirmation.

## Large-dataset and performance checks

The script uses empty-chain data. For large datasets:

1. Restore a separate copy of the public chain and saved pending buffer. Leave
   the source unchanged.
2. Select a public key that owns at least 326,387 accounts in that copy and has
   history rows among the roughly 185,000 pending operations.
3. Check account and history fetches, UI heartbeats, repeated view switches,
   native icon handles and the white status label below the logo.
4. Compare the copied pending buffer's hash before and after the run.

Do not import the normal wallet's private keys.

For update performance, measure cold loading, warm paging and a backend
notification burst separately. Count `FetchAll` calls and grid completions.
Record working and private memory at the same checkpoints before and after a
change. The [regression test guide](../README.md) and
[grid refresh demo](grid-refresh/README.md) cover paging, totals, refresh and
loading cards.

## Other native checks

`RunWalletRegressionTests.ps1 -Probe <name> -OutputDirectory <new-directory>` runs
the `classic`, `connection-restart`, `pending-protocol`, `buffered-frames`,
`node-sync` or `mempool-lock` probe with FPC.

- [Wallet grid refresh demo](grid-refresh/README.md) covers synchronization
  bursts and fetch counters.
- [Send PASC presentation](grid-refresh/README.md#send-pasc-presentation)
  captures every reachable wizard step at 100% and 150%.
- [Account selector](account-selector/README.md) covers key filtering, paged
  confirmed accounts and type statistics.
- [Classic compatibility probe](classic/README.md) covers local restore, five
  validated blocks, grids, accounts and native Close.
- [AbstractMem storage](abstractmem-storage/README.md)
- [Block application and pending replay](block-throughput/README.md)
- [Buffered adoption](buffered-adoption/README.md)
- [Checkpoint storage](storage-recovery/README.md)
- [Connection restart](connection-restart/README.md)
- [Log memo batches](log-memo-batch/README.md)
- [Logger GUI handoff](log-gui-handoff/README.md)
- [Node synchronization](node-sync/README.md)
- [Pascal Framework components](common/README.md)
- [Pending protocol](pending-protocol/README.md)
- [Received block backlog](network-backlog/README.md)
- [Safebox download shutdown](download-stop/README.md)
- [Safebox recovery](safebox-recovery/README.md)

These helpers run in Delphi and FPC:

- [Core and lifetime](network-lifetime/README.md)
- [Buffered frames](network-backlog/BufferedBlockFrameProbe.md)
- [Cardinal ordering](ordered-cardinal/README.md)
- [Stored history](stored-history/README.md)

## License

The broker is adapted from PascalFramework's native wizard test infrastructure.
Its MIT license is included in this directory.
