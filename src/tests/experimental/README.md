# Experimental wallet regression tests

This folder holds FPCUnit fixtures and native Delphi and FPC probes for the
experimental wallet, the core and the shared framework units.

## Run all tests on Windows

Build the experimental wallet, then run from the repository root:

```powershell
& src/tests/experimental/RunAllWalletTests.ps1
```

- `-FpcPath` and `-LazarusDirectory` select FPC and Lazarus. By default the
  runner finds them on `PATH`.
- `-OutputDirectory` sets a new folder for compiler outputs and reports. The
  default is `lib/all-<run-id>` in this folder.
- `-WalletExecutable` selects the built shipping wallet for the startup checks.
  The default is the Lazarus build, `src/PascalCoinWalletExperimental.exe`.

The runner needs:

- a Windows x64 FPC and Lazarus with the Win32 widgetset
- Python 3 on `PATH`, to recompute the proof-of-work vectors
- a Full HD desktop for the Advanced Operation checks at 150% scale

Native checks run on private Windows desktops through the private-desktop
broker. The runner executes these sixteen phases in order, each in its own
process:

| Phase | Covers |
|---|---|
| `unit` | Every registered [unit fixture](#fpcunit-fixtures) except `UWalletGridIntegrationTests` |
| `log-memo-batch` | Experimental [log-memo checks](native/log-memo-batch/README.md) |
| `classic-log-memo-batch` | Classic log-memo checks |
| `grid-integration` | The main-grid integration fixture, `UWalletGridIntegrationTests` |
| `grid-native` | The thirteen [grid refresh demo](native/grid-refresh/README.md) groups |
| `network-toggle` | The connectivity toggle and deferred refresh |
| `buffered-backlog` | [Received block backlog](native/network-backlog/README.md) scenarios |
| `startup` | [Startup and shutdown](native/README.md) |
| `download-stop` | [Safebox download shutdown](native/download-stop/README.md) |
| `safebox-recovery` | [Safebox recovery](native/safebox-recovery/README.md) |
| `classic` | [Classic](native/classic/README.md) key selection, filtering and shutdown |
| `connection-restart` | [Connection restart](native/connection-restart/README.md) |
| `pending-protocol` | All nine [pending-protocol](native/pending-protocol/README.md) groups |
| `buffered-frames` | [Buffered block frames](native/network-backlog/BufferedBlockFrameProbe.md) |
| `node-sync` | Main-form synchronization, storage failures and grid cancellation on a real node |
| `mempool-lock` | Mempool lock upgrades |

Each phase writes to the `<phase>` subfolder and `<phase>-runner.log` in the
output folder. `all-tests.json` records each phase's exit code. The runner runs
every phase and exits 1 if any failed. Every process must exit naturally, and
each heap-traced process must leave a clean heap report.

The runner stops before the first phase if a registered fixture unit is missing
from both unit runners, `ExperimentalWalletTests.lpr` and
`ui/WalletGridRefreshTests.lpr`. `test-coverage.json` lists the native phases
and the shared helpers that the unit fixtures already cover. Delphi-only probes
and the opt-in [large-chain checks](#large-chain-checks) have separate inputs
and commands.

### CI

CI builds both wallets on Windows, Linux and macOS with the stable Lazarus
release and runs the [portable lock tests](#portable-lock-tests) on each. On
Windows it also runs the `operation-messages` demo group at the default
resolution, switches the display to Full HD with `native/SetCiDisplayMode.ps1`
and runs this runner. That script changes the display only on GitHub-hosted
runners. Locally, run it with `-InspectOnly` for diagnostics. CI uploads the
reports as the `Wallet-tests-Windows`, `Native-display-Windows` and
`Threading-<OS>` artifacts.

## Portable lock tests

[ThreadingTests.lpr](ThreadingTests.lpr) runs the core and shared read-write
lock cases without a GUI, Lazarus units or OpenSSL. It covers concurrent
readers, exclusive writers, upgrades, recursion, timed waits, ownership and
disposal. From the repository root on Linux or macOS:

```sh
mkdir -p src/tests/experimental/lib/threading/units
fpc -B -Mdelphi -Sh -gl -Cr -Co \
  -FUsrc/tests/experimental/lib/threading/units \
  -FEsrc/tests/experimental/lib/threading \
  -Fusrc/core -Fusrc/libraries/sphere10 -Fusrc/libraries/pascalcoin \
  -Fusrc/libraries/generics.collections -Fisrc/libraries/generics.collections/inc \
  -Fusrc/tests/experimental/core -Fusrc/tests/experimental/framework \
  src/tests/experimental/ThreadingTests.lpr
src/tests/experimental/lib/threading/ThreadingTests src/tests/experimental/lib/threading/tests.log
```

The same compiler arguments work on Windows, where you run
`ThreadingTests.exe`. The program writes the test report to the log path you
pass and the heap report to `<log>.heap.log`. It exits 0 only when every test
passes, and a clean heap report contains `0 unfreed memory blocks`. In
`ThreadingTests.lpr`, keep `heaptrc` before `cthreads`.

## Run individual suites

Run these from the repository root on Windows:

```powershell
& src\tests\experimental\RunExperimentalWalletTests.ps1
& src\tests\experimental\native\RunWalletStartupNativeTests.ps1
& src\tests\experimental\RunWalletGridRefreshTests.ps1
& src\tests\experimental\RunWalletDownloadStopTests.ps1
& src\tests\experimental\RunWalletSafeboxRecoveryTests.ps1
```

- Pass `-FpcPath` as needed, and `-LazarusDirectory` for scripts that use the
  LCL. These scripts do not search `PATH`.
- Each script takes the OpenSSL 1.1 runtime from `src` or `windows-dll`. It
  writes to a new folder under `lib` in this folder, or to a new
  `-OutputDirectory`, and prints that folder when it finishes.
- `RunExperimentalWalletTests.ps1` builds and runs `ExperimentalWalletTests.lpr`.
  Each run writes its build log, test report, heap report and broker log to
  `lib/<target>/<run-id>`.
- `RunWalletGridRefreshTests.ps1` runs `grid-integration` without switches,
  `grid-native` with `-Demo -AllDemoCases`, `network-toggle` with `-NetworkOff`
  and `buffered-backlog` with `-Backlog`. For one demo group, use
  `RunWalletGridRefreshTests.ps1 -Demo -DemoCase operation-messages`.
  `-DemoCase` and `-AllDemoCases` are mutually exclusive.
- `native/RunWalletRegressionTests.ps1 -Probe <phase> -OutputDirectory <new-directory>`
  runs the `classic`, `connection-restart`, `pending-protocol`,
  `buffered-frames`, `node-sync` or `mempool-lock` phase.
- [`Run-CheckpointStorageChecks.ps1`](native/storage-recovery/Run-CheckpointStorageChecks.ps1)
  runs the [checkpoint storage checks](native/storage-recovery/README.md) alone.
  They use generated data, never a user's wallet. The unit suite also runs
  them.

## Delphi

- The scripts compile with Free Pascal in its `-MDelphi` mode, never with
  Embarcadero Delphi.
- Delphi 13 Community Edition's command-line compilers are edition-restricted.
  Build Delphi projects in its IDE.
- The classic DUnit project,
  [`../PascalCoinUnitTests.dproj`](../PascalCoinUnitTests.dproj), and the
  native VCL probes are separate from the FPCUnit fixtures. The native Delphi
  checks do not cover the full wallet on Win32.
- With `-WalletExecutable src/pascalcoin_wallet_experimental.exe`,
  `native/RunWalletStartupNativeTests.ps1` also starts the Delphi Win64 Debug
  build offline and closes it normally.

## FPCUnit fixtures

| Suite | Covers |
|---|---|
| `startup/UWalletStartupTests.pas` | Option parsing, startup outcomes, main-thread completion and progress records |
| `startup/UPendingRestoreTests.pas` | Restoring saved pending operations, with cancellation, retry and truncated streams |
| `startup/UPendingCleanupTests.pas` | Bulk pending cleanup, byte-identical buffered saving and stage-based cancellation |
| `startup/UPendingSaveLifecycleTests.pas` | Pending saves at shutdown, on failure and by atomic replacement |
| `startup/UBufferedAdoptionTests.pas` | Core use of buffered streams for pending packages, history files and RandomHash tables |
| `startup/UCheckpointStorageTests.pas` | [Checkpoint storage](native/storage-recovery/README.md) failures and recovery |
| `core/UThreadConcurrencyTests.pas` | Core read-write lock: readers, writers, promotion, recursion, timeouts and ownership |
| `core/UCoreLockMigrationTests.pas` | Application lock owners: recursive leases, exclusion, timeouts and destruction |
| `core/UNodeSynchronizationTests.pas` | Node synchronization targets, request generations, notification order and storage failures |
| `core/UPendingCatchupTests.pas` | [Deferred pending replay](native/block-throughput/README.md#pending-catch-up) |
| `core/UChangeAccountInfoReplayTests.pas` | Signed-operation equivalence when signer and target accounts are reused |
| `core/UBlockSanitizeTests.pas` | Block replay and sanitizing: rejected and duplicate operations, fee limits and signature caches |
| `core/UAccountCachePolicyTests.pas` | Account cache capacity and reloads, and safebox cleanup failures |
| `core/UAbstractMemStorageFailureTests.pas` | [AbstractMem block-storage](native/abstractmem-storage/README.md) failures |
| `core/UDownloadWorkerLifetimeTests.pas` | Download worker, socket lease and discovery ownership through shutdown |
| `core/ULegacyConnectionLifetimeTests.pas` | Connection retirement after legacy connection-lock releases |
| `core/UOrderedCardinalTests.pas` | AbstractMem `Cardinal` comparator and ordered list |
| `core/ULogGuiHandoffTests.pas` | [Logger handoff](native/log-gui-handoff/README.md) to GUI receivers |
| `operations/UOperationPreparationTests.pas` | Wizard operation preparation, fees, payloads, signers and swaps, accepted by core `DoOperation` |
| `operations/UChangeInfoBatchTests.pas` | Change-info batches, fees, signers and signatures (`UChangeInfoBatchChecks.pas`) |
| `operations/UOperationWizardTests.pas` | Wizard unlock ownership, cancellation, retry and signer eligibility |
| `operations/UWalletParityTests.pas` | Key formats, wizard inputs, sale and swap filtering, RAW workbench, import and partial signatures |
| `ui/UWalletProgressDisplayTests.pas` | Progress text, including legacy strings |
| `ui/UWalletRestoreTests.pas` | Key backup and restore, wrong passwords and watch-only upgrade |
| `ui/UWalletViewTests.pas` | Core-work accounting: silent activity, nested scopes and exceptional cleanup |
| `ui/UWalletGridRefreshTests.pas` | Grid refresh on node notifications |
| `ui/UWalletGridIntegrationTests.pas` | Main-form grid scenarios |
| `ui/UWalletExplorerTests.pas` | Accounts Explorer snapshots, worker reads, history, totals and cancellation |
| `ui/UWalletExplorerSchedulingTests.pas` | Explorer refresh scheduling |
| `ui/UWalletAccountEnumerationTests.pas` | Persisted account snapshots and wallet account traversal |
| `ui/UWalletHistoryTests.pas` | Pending and stored history: order, balances, paging and archive fallback |
| `ui/UWalletSnapshotTests.pas` | Wallet membership resolution and responsive settings during work |
| `ui/UWalletMempoolReadSafetyTests.pas` | Mempool reads during active writes |
| `ui/UWalletPagingTests.pas` | Account paging, sorting, global totals and query cancellation |
| `ui/UWalletMiningTests.pas` | Mining reward keys across wallet states, and key rotation |
| `ui/UWalletRenderTests.pas` | Rendering from captured pending state, with PASC amounts |
| `ui/UWalletInfoTests.pas` | Account data, sale and swap details, multioperations and JSON |
| `ui/UWalletLanguageTests.pas` | Language catalogs, preferences, fallback and form translation |
| `framework/UCache.Tests.pas` | Cache expiration, capacity, pins, notifications and concurrency |
| `framework/UCommon.Data.Tests.pas` | Data snapshots: paging, search, sort, invalidation and schemas |
| `framework/UCommon.UI.Tests.pas` | Refresh throttle timers, Waiting state, callbacks and cancellation |
| `framework/UVisualGrid.Tests.pas` | Grid selection deduplication and clearing |
| [`framework/UVisualGrid.Refresh.Tests.pas`](framework/UVisualGrid.Refresh.Tests.pas) | Per-grid request admission, loading, replay, cancellation and `RefreshSuspended` |
| `framework/UFramework.UI.Integration.Tests.pas` | Docking, worker completion, source replacement and error recovery |
| `framework/UWizard.Tests.pas` | Wizard lifecycle, validation, routes, retries and model binding |
| `framework/UStreams.Tests.pas` | Stream decorator contracts shared with PascalFramework: read-ahead, seeking, writes, resizing and failure recovery |
| [`framework/UScope.Tests.pas`](framework/UScope.Tests.pas) | Managed cleanup scopes |
| `framework/UConcurrency.Tests.pas` | Shared framework read-write lock |
| [`framework/UReadWriteScope.Tests.pas`](framework/UReadWriteScope.Tests.pas) | Scoped read, write and upgradeable lock acquisition |
| `../UCommon.Tests.pas` | Shared `UCommon` helpers |
| `../UCommon.Collections.Tests.pas` | Collection comparers, filters and transforms |
| [`../UMemory.Tests.pas`](../UMemory.Tests.pas) | Disposal scopes, capacity growth, ownership transfer and exception cleanup |

The imported framework tests are kept here so this checkout has no dependency on
a separate PascalFramework repository. Their original MIT notices are retained.
PascalFramework mirrors the shared component test sources and the
[native Delphi timer probe](native/common/README.md).

The fixtures never load a user wallet, connect to peers, mine or broadcast.

## Native probes

These probes have their own projects and commands:

- [ScopeProbe](native/common/ScopeProbe.dproj) runs the shared scope contract
  checks in Delphi.
- The [native common probe](native/common/README.md#per-grid-request-admission)
  runs the VCL grid request-admission scenarios.
- The [logger handoff regression](native/log-gui-handoff/README.md) shares its
  `ULogGuiHandoffTests` assertions with native Delphi and FPC probes.
- The [core synchronization probe](native/node-sync/README.md#core-lifecycle-probe)
  checks node synchronization across batch gaps, network-off, disconnects and
  storage failures, in Delphi and Free Pascal.
- Shared Delphi and FPC helpers cover
  [core and lifetime checks](native/network-lifetime/README.md),
  [buffered frames and transport](native/network-backlog/BufferedBlockFrameProbe.md),
  [full-range Cardinal ordering](native/ordered-cardinal/README.md) and
  [stored history](native/stored-history/README.md).
- [Buffered adoption](native/buffered-adoption/README.md) probes run the
  `UBufferedAdoptionTests` checks in Delphi and FPC.
- The [block application probes](native/block-throughput/README.md) test
  pending catch-up, sanitizing and pending saves in Delphi and FPC.
- The [account selector probe](native/account-selector/README.md) tests the
  account-selection form in Delphi and FPC.

## Large-chain checks

These opt-in checks use a copy of the public chain. They cover received block
backlogs, dense paging, startup and busy close, pending cleanup and saving, and
Accounts Explorer key changes. See
[received block backlog](native/network-backlog/README.md),
[block throughput](native/block-throughput/README.md) and the
[startup checks](native/README.md) for setup. Never use a live wallet or its
private keys.

## Limits

- All checks run offline. Live synchronization, peer throughput and
  transaction delivery need separate testing.
- The five-second shutdown assertion in the `buffered-backlog` phase once
  failed at 6,062 ms although the process exited cleanly. The cause is unknown.

## Recorded results

An earlier full FPCUnit run passed 525 tests with zero leaks under FPC 3.2.0
and Lazarus 2.1.0.
