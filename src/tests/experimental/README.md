# Experimental wallet regression tests

## Complete Windows runner

Build the experimental wallet, then run:

```powershell
& src/tests/experimental/RunAllWalletTests.ps1
```

The runner finds FPC and Lazarus on `PATH`, or accepts `-FpcPath` and
`-LazarusDirectory`. Use `-OutputDirectory` for compiler outputs and reports;
`-WalletExecutable` selects the already-built shipping wallet used by startup
checks. Python 3 recomputes the independent proof-of-work vectors. CI builds
both wallet applications before invoking this runner.
Startup checks stage an identical executable beside bundled OpenSSL in the test
output directory.

It runs every registered unit fixture, the isolated main-grid integration
fixture, native grid scenarios, network toggling, buffered backlog, startup and
shutdown, downloader cancellation and safebox recovery. Native checks also cover
Classic key selection/closure, connection restart, all nine pending-protocol
groups, buffered frames, real-node grid cancellation and mempool lock upgrades.
The demo is compiled once, then runs thirteen fresh processes covering its
default refresh checks and distinct selection, logging, painting, Tools,
history limits, operation input, block filters, approval, Send PASC and dialog
presentation groups. Subset selectors already covered by the default branch are
not repeated.
Each phase uses its own
process and fresh data. The controller fixture remains separate because its
application state lasts for the process. A registration manifest fails if a
fixture unit is absent from both unit runners. `test-coverage.json` records the
native phases and helpers already covered by the unit fixtures.

The general suite passed **751 tests** on 2 November 2026 with zero errors,
failures or unfreed blocks. This includes the common/collection fixtures and
the previously unregistered box and comparer cases. The dedicated controller
fixture supplies one additional FPCUnit case in its own process.
The thirteen native demo groups passed, along with all nine pending-protocol
groups and both independent Python vectors. Startup passed four controlled
modes and both normal-executable modes against the optimized 5.9 Release wallet.

The native Windows checks require the real Win32 widgetset. Linux and macOS
build both wallets and run the portable lock suite below. Delphi-only probes
and the opt-in large-chain fixture have separate inputs and commands.
The pending-protocol checks use the existing visibility-only source-copy script:
three declarations become protected, every method body stays identical, and
source hashes/equivalence are recorded. That copy is never shipped. Classic uses
its application's arithmetic settings; the other native core probes enable range
and overflow checks. All processes must exit naturally with clean heap reports.

## Portable lock tests

[ThreadingTests.lpr](ThreadingTests.lpr) runs the 62 core and shared read/write
lock cases without a GUI, Lazarus units or OpenSSL. It covers concurrent readers,
exclusive writers, upgrades, recursion, timed waits, ownership and disposal.
From the repository root on Linux or macOS:

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

The same compiler arguments work on Windows; run `ThreadingTests.exe` there.
The runner returns a nonzero code on failure and writes the test and heap reports
beside the supplied log path. Unix initializes the RTL thread manager before the
tests; Darwin's pointer-shaped thread IDs exercise the same ownership checks.
Keep `heaptrc` before `cthreads` so heap tracing is installed before POSIX
thread-local storage is relocated.

## Wallet and native tests

The shared scope fixtures are [UScope.Tests](framework/UScope.Tests.pas),
[UMemory.Tests](../UMemory.Tests.pas) and
[UReadWriteScope.Tests](framework/UReadWriteScope.Tests.pas). Together with
the underlying lock fixture, these provide 99 focused cases for managed cleanup,
resource bags and scoped synchronization. The same 63 shared contract checks also
run in the native Delphi [ScopeProbe](native/common/ScopeProbe.dproj).

Run on Windows with FPC/Lazarus and the existing OpenSSL 1.1 runtime:

```powershell
& src\tests\experimental\RunExperimentalWalletTests.ps1
& src\tests\experimental\native\RunWalletStartupNativeTests.ps1
& src\tests\experimental\RunWalletGridRefreshTests.ps1
& src\tests\experimental\RunWalletDownloadStopTests.ps1
& src\tests\experimental\RunWalletSafeboxRecoveryTests.ps1
```

Override `-FpcPath` and, for scripts using LCL, `-LazarusDirectory` as needed. The first script performs a
clean compile with range/overflow checks and heap tracing, then launches FPCUnit
through the private desktop broker. Its per-run `lib/<target>/<run-id>` directory
contains the build log, test report, heap report and broker log.

The [safebox recovery checks](native/safebox-recovery/README.md) exercise real
loopback peer loss, automatic reconnect, obsolete responses, request leases and
worker handoff. Invalid transfer types and chunk headers must still be rejected.
Its parallel Delphi and FPC projects share the same checks; interrupted downloads
must join cooperatively, preserve saved files and publish no partial archive.

The 2026-10-31 recovery run passed **8/8 in each compiler**, including automatic
reconnection after the sole loopback peer drops. The pre-fix core failed all six
positive recovery cases while retaining both invalid-input rejection controls.
The FPC run reported zero unfreed blocks. Pending protocol (8 cases), network
lifetime (7 cases) and cooperative downloader shutdown also passed in both
compilers. Both experimental application builds passed fresh offline startup and
normal Close/Yes exit. These are controlled transport/lifecycle checks, not a
complete valid-checkpoint download or public-peer throughput measurement.

Earlier full experimental-suite evidence records **525 passing tests** with FPC 3.2.0 /
Lazarus 2.1.0: zero errors/failures/ignored tests, natural exit and **7,474,266** allocations
and frees with zero unfreed blocks. This is FPCUnit evidence, not a Delphi run.
Earlier native acceptance recorded **56 distinct cases in each actual compiler**:
[core/lifetime checks](native/network-lifetime/README.md) (34),
[buffered frames/transport](native/network-backlog/BufferedBlockFrameProbe.md) (15),
[full-range Cardinal ordering](native/ordered-cardinal/README.md) (3), and
[shared stored-history checks](native/stored-history/README.md) (4).
Separate real-main-form sparse/dense checks pass **four actual child processes**,
one of each data case in Delphi and FPC, with real providers and five buffered
historic commits. Fourteen formatter tests and 36 actual Delphi literal checks
verify reconstructed legacy progress text. These controlled fixtures do not
measure live-peer throughput; the earlier seven-case wallet cohort remains
separate dated evidence.

Both actual shipping projects also build. Fresh offline checks pass the FPC
Default executable and both the normal and fixed-alias Delphi Win64 Debug
executables through native Close/Yes and natural exit. These shipping smoke
checks use empty fresh data and are separate from the heap-traced test processes.
One earlier buffered-shutdown check exceeded its five-second assertion at
6,062 ms despite a clean natural exit. Eleven instrumented repeats and the final
native scenarios passed; the unexplained outlier does not establish a universal
five-second shutdown guarantee.

The shared [grid fixture](framework/UVisualGrid.Refresh.Tests.pas) now passes
**47 cases against each source copy** with checked FPC 3.2.0 builds, natural
process exit and zero leaks. Eight cases cover generic `RefreshSuspended`:
retained page/filter/sort/source intent, visual-only resize, quiet completion
while paused, cooperative worker cancellation, ignored cancelled results/errors,
immediate resume after cancelling the old throttle interval, and
suspend/resume/suspend races. See the
[native common probe](native/common/README.md#per-grid-request-admission) for the
matching VCL scenarios and focused environment selector. The component contains
no node policy; the wallet owns its synchronization-state binding. The
queued-loading group additionally passes four actual VCL scenarios against each
copy: waiting feedback, cancellation, manual-artwork precedence and the transition
from a deferred single request to its held provider. It starts no query early and
keeps the existing loading interval through admission.
The separate VCL suspension group also passes actual Delphi Win64 checked builds and
private-desktop runs against both library copies, with natural exit zero; this
does not claim a new run of the other six native groups.

The actual main wallet now uses immediate refresh admission (`temNone`) for both
accounts and operations. Node synchronization alone suspends queries and owns
**Please Wait / Synchronizing**. While a page worker is busy, newer requests
coalesce into one latest query; accepted data clears ordinary loading without a
quiet-period wait. The [grid refresh test and interactive demo](native/grid-refresh/README.md)
retain the real main form, typed sources, caches, paging and selection callbacks
while replacing stored records. Their Delphi and Lazarus projects exercise this
product policy separately from the generic throttle component.

The FPCUnit `TWalletGridIntegrationTests` fixture keeps eight scenarios in one
controller-lifetime test: production wiring and cached pages, completion-driven
overview totals, ordinary loading, continuous inputs, physical backlog across a
silent gap, busy-worker coalescing, independent client inputs and hidden-grid
reparenting. The separate native network-off probe checks the actual connectivity
handler with unavailable loopback peers. Turning networking off preserves a
received backlog's synchronization card; ending that work resumes one latest page
immediately. The current focused run passes twenty consecutive clicks and the
completion/lifecycle checks in both compilers, both network-off groups in each,
and the FPC controller-lifetime test covering all eight scenarios.

Earlier main-form/mock runs required five quiet seconds after every refresh and
used a Waiting-only synchronization card. Those runs, including the former
seven-case demo and network-off result, are historical evidence for the withdrawn
wallet policy. They do not verify the current immediate-admission behavior.
The generic component's independent timer, all-input reset and quiet trailing
contracts remain covered by the mirrored common UI and visual-grid fixtures.

An earlier component-only cohort passed the mirrored common UI suite's **53 cases
per copy** with FPC 3.2.0 and zero leaks. Its focused grid runs passed **30 cases**
against standalone FPC 3.2.2 and embedded
FPC 3.2.0, with zero leaks; the final VCL-only correction leaves the standalone
run's FPC statements unchanged. Actual Delphi request probes pass five groups per
copy. The standalone pinned Lazarus 4.8/FPC 3.2.2 package and demo build, and its
public console command passed **226 tests**, zero errors/failures. These are
historical counts predating the later grid additions, not new full-suite results.
That complete console run was not heap-traced; hosted CI remains unrun while
uncommitted.

The separate [download-stop regression](native/download-stop/README.md) exercises
two real chunk workers contending for the downloader's list during shutdown. It
checks cancel-all before joining, lock release during joins, stream cleanup and
normal process exit. Both FPC and Delphi entry points use the same checks; this
core ownership test is independent of the grid throttle.

The event-driven experimental UI has no `UIRefreshTimer` fallback. Targeted
regressions cover aggregate completion without repeated `TryGet` calls, retained
totals while a newer aggregate is unfinished, Explorer readiness on
completion/error, and status
publication after independent mempool/safebox contention clears. Shared grid
completion cases disable all timers and require `CheckSynchronize` alone;
the earlier standalone focused fixture contained **35 cases**, before the eight
suspension regressions above. Earlier full-suite counts predate both changes.
The focused Explorer fixture passes **17 cases**, including a real cancelled
filter that retires after an independent snapshot has already finished. It must
announce idle once on the UI thread without starting another query. This covers
the readiness transition that periodic UI polling previously concealed.

The following full-suite results are a historical baseline. Their earlier
producer-aware timing assertions and later main-wallet quiet-time assertions
are superseded by the immediate-admission/node-lifecycle policy above. Generic
throttle tests retain their independent component contract.

The preceding full regression run with FPC 3.2.0 / Lazarus 2.1.0 passed **320 tests**,
zero errors/failures and zero leaks: **7,089,597** allocations and frees.
This includes the account paging, chunked global totals, renderer, mining and
five-second producer-aware debounce regressions, bounded explorer snapshots,
asynchronous account history, selected-account totals beyond the row-cache capacity,
retired filter-worker ownership, captured-key filtering under a locked key index,
pending cleanup/save, checked empty-genesis and worker-query cancellation tests.
The backend timing cases include a six-second interval of active block validation
without accepted-block notifications, followed by five quiet seconds after work
finishes. Nested/concurrent work and exceptional cleanup are covered separately;
work accounting alone must not fabricate a block-change event.
The separate native Delphi check uses the inherited wallet and actual grids with
counted offline providers. A 6.531-second processing gap causes no additional grid
query, then one final refresh starts exactly five seconds after processing ends.
This checks the rendered fetch path independently of the scheduler-only fixtures;
it does not measure live peer-network throughput.
Final executable checks pass both actual launch modes for Default and Debug.
The extended native probe passes all four harness modes.
The harness checks resize/restore during loading, wallet and sync views, modeless
focus through loading/error cards, and native Close/Yes with busy callbacks.
The separate dense UI/data baseline passes with 457,637 owned accounts and
186,098 history rows: an explicit 25-row page reads
25 account records, and the maximum observed UI gap is 125 ms. Cold history
continues as background work. The final dense startup/busy-close
case also exits cleanly: Loaded follows the last 99.12% application sample by
688 ms, and Close joins active grid/aggregate work in 8,734 ms. The core
microbenchmark preserves the exact 21,183,301-byte saved package while reducing
cleanup from 3,484 to 203 ms and saving from 14,203 to 125 ms. This is offline
regression evidence; live synchronization and
transaction delivery are separate checks.

The native Accounts Explorer regression also uses the copied 4,207,645-account
chain and 185,831 pending operations. Explorer opens in 32 ms and publishes
457,637 owned accounts. Switching to a four-account key while that broad scan is
active returns immediately and publishes the replacement in 313 ms; the measured
loaded-view heartbeat gap is at most 78 ms. The process exits normally, and the
pending buffer and public-only wallet keys retain their hashes. A separate fresh
native fixture verifies that replacing already-cached rows rebinds history even
when no further snapshot fetch is needed.

The scripts invoke Free Pascal, including its `-MDelphi` language mode. They do
not run Embarcadero Delphi. The existing classic DUnit project is separate from
these FPCUnit fixtures. Delphi 13 Community Edition is installed, but its
command-line compilers are edition-restricted. Its IDE compiler successfully
builds the classic wallet for Win64. The current shared framework package includes
`UConcurrency` and builds for Win64 Debug; earlier nine-unit package checks cover
Win32 and Win64 configurations. Native VCL grid/wizard probes are separate from
the FPCUnit fixtures. The actual experimental project also builds for Win64 Debug;
separate offline checks launch its executable, require a responsive enabled main
window and complete normal Close/confirmation with exit status zero. Four CLI
checks cover help and invalid arguments with and without redirected output.
These native checks do not execute the FPCUnit fixtures or establish Win32
full-wallet runtime coverage.

| Suite | Coverage |
|---|---|
| `startup/UWalletStartupTests.pas` | Pure option parsing; success/failure/cancellation; main-thread completion; coherent typed progress records including managed metadata; queued-callback removal |
| `startup/UPendingRestoreTests.pas` | Eight cases: typed saved-operation decoding stages/counts, caller-thread delivery, cooperative cancellation, retry, preserved source bytes and notification handlers, empty/truncated streams and legacy storage virtual dispatch |
| `startup/UPendingCleanupTests.pas` | Bulk cleanup, coherent pending persistence, byte-identical buffered saving, checked core digest vectors, typed operation-stage sequencing and stage-based cancellation |
| `core/UThreadConcurrencyTests.pas` | 31 behavioral cases for the independently adapted core lock: concurrent reads, writer exclusion, promotion, recursion, timeouts, ownership and cleanup |
| `core/UNodeSynchronizationTests.pas` | Eight cases: selected targets, real work across disconnects, retired request generations, cached committed summaries, notification order and recovery after a subscriber exception |
| `core/UChangeAccountInfoReplayTests.pas` | Four signed-operation equivalence cases for the existing signer/target account reuse path, including distinct accounts and rejection |
| `core/UBlockSanitizeTests.pas` | Nine replay/index cases: unchanged retention, rejected operations, duplicate references/hashes, high unsigned references, zero-fee limits, exceptions, registry ownership and block/operation protocol signature-cache boundaries |
| `core/UAccountCachePolicyTests.pas` | Core default capacity, configured capacity across safebox recreation, and cleared/reloaded account records without stale cache entries |
| `core/UDownloadWorkerLifetimeTests.pas` | Seven ownership cases: selected/unselected socket leases, factory failure, incoming retirement, shutdown joining, owned node finalization callbacks, pending/running discovery, and late-client draining after an empty registry scan |
| `core/ULegacyConnectionLifetimeTests.pas` | Actual connection retirement must wait for balanced legacy connection-lock releases, including recursion and timed/nonblocking admission failures |
| `startup/UPendingSaveLifecycleTests.pas` | Five actual saver/storage scenarios: final dirty shutdown, failure/termination race, sanitization removals, atomic replacement and final I/O failure |
| `ui/UWalletProgressDisplayTests.pas` | Fourteen presentation-only cases: independent percentage oracles, exact legacy raw/decorated strings, raw/display count separation, unknown totals, large counters, file/peer metadata and stage coverage |
| `operations/UOperationPreparationTests.pas` | 35 tests: six wizard operation types, fees/payloads, signer batches, copied pending-state preflight, atomic swaps/secret claims and account data, with actual core `DoOperation` acceptance against a synthetic memory safebox |
| `operations/UChangeInfoBatchTests.pas` | Ten portable groups in `UChangeInfoBatchChecks.pas`: mixed/all-unchanged batches, skipped account fees, common/self/fallback signers, original snapshots, approved target identity, real signatures and core application |
| `operations/UOperationWizardTests.pas` | Per-model unlock ownership, cancellation/retry/disposal and signer candidate eligibility |
| `operations/UWalletParityTests.pas` | 19 tests: classic key formats/all curves, retained wizard inputs, advanced form resources, sale/swap filtering, offline RAW workbench guards, transactional import and two-party partial-signature exchange/core acceptance |
| `ui/UWalletRestoreTests.pas` | Generated-key backup/restore, duplicates, wrong passwords, watch-only upgrade, locked-target protection and file release |
| `ui/UWalletViewTests.pas` | Three independent core-work accounting cases: silent activity, nested/concurrent scopes and exceptional cleanup; actual grid admission is covered by the shared 43-case fixture |
| `ui/UWalletGridRefreshTests.pas` | Real node notifications with the shared grid fixture; generic component cases live in the mirrored framework units |
| `ui/UWalletGridIntegrationTests.pas` | One actual-main/controller lifetime test with eight scenarios for immediate admission, cached pages and selection, aggregate completion, loading, busy coalescing, node-held suspension, independent inputs and hidden-grid return |
| `ui/UWalletExplorerTests.pas` | Bounded cached account snapshots, worker-only reads, stale-generation/error rejection, asynchronous history selection/cancellation, selected totals beyond cache capacity, retired filter-worker ownership, locked-index captured-key filtering and cooperative storage traversal cancellation |
| `ui/UWalletExplorerSchedulingTests.pas` | Actual Explorer refresh-input scheduler, deferred leading delivery, queued timer order and quiet trailing delivery without producer/core-state vetoes |
| `ui/UWalletAccountEnumerationTests.pas` | Six cases: bounded/empty persisted account snapshots, unsigned account numbers, invalid values, sorted unique wallet accounts and an 8,192-account traversal with instrumented node-read bounds |
| `ui/UWalletHistoryTests.pas` | Pending-history order/index/balance/paging/cancellation checks and twelve shared stored-history groups: wide balances, bounded linked traversal, finite/Maximum paging, pending-index isolation and hashes encoded with the actual block height; the shared native probe runs under Delphi and FPC |
| `core/UOrderedCardinalTests.pas` | Three shared AbstractMem comparator/list cases: all 25 boundary pairs, all 120 insertion/find/removal permutations, and duplicate/missing-value policy; checked Delphi and FPC both reproduce the old subtraction failure and pass the correction |
| `ui/UWalletSnapshotTests.pas` | Three cases: worker-only wallet membership resolution with same-count replacements, stale resolution rejection, and responsive membership/account-filter/pending settings during gated work |
| `ui/UWalletPagingTests.pas` | Counted default-page reads, small-key isolation and custom sorting, pending ownership corrections, prefix growth, captured pending state, global totals, aggregate cancellation/publication and cancellation/retry of custom queries |
| `ui/UWalletMiningTests.pas` | Existing eligible reward keys, locked/watch-only/empty/read-only wallets, selection/deletion, unchanged ordinary refresh/reload files and explicit post-block rotation/encryption |
| `ui/UWalletRenderTests.pas` | Captured pending-state rendering without live core reads, including unsigned balances rendered as four-decimal PASC amounts |
| `ui/UWalletInfoTests.pas` | Account data/seal, private-sale/swap details, multioperation participants and canonical JSON |
| `ui/UWalletLanguageTests.pas` | Usable catalogs, saved preferences, fallback, and native translation of existing and later forms |
| `framework/UCache.Tests.pas` | Cache expiration, capacity, pins, notifications, failure/reentrancy, ownership and concurrency |
| `framework/UCommon.Data.Tests.pas` | Fourteen cases: snapshot paging, search/sort reuse, copied criteria, invalidation/retry, empty rows/page bounds, rejected schemas, independent key/column access, positional dispatch and native String/Unicode values |
| `framework/UCommon.UI.Tests.pas` | Fifty-three cases: actual timer resets, leading/trailing and Waiting lifecycle, 4.9-second input cadence, stale timer delivery, slow/reentrant callbacks, exceptions, cancellation/disposal and queued-listener ownership |
| `framework/UVisualGrid.Tests.pas` | Disjoint selection deduplication and safe selection clearing without a callback |
| `framework/UVisualGrid.Refresh.Tests.pas` | Forty-three cases: layout/loading/default behavior, owned per-grid admission, all query inputs, visual-only resize, latest snapshots, busy/hidden replay, cancellation, empty/source replacement, queued completion with every timer disabled, cooperative suspension and immediate latest-request resume |
| `framework/UFramework.UI.Integration.Tests.pas` | Eight common duration, docking, worker completion/disposal, source replacement/error recovery and concurrent schema cases |
| `framework/UWizard.Tests.pas` | Wizard lifecycle, validation, dynamic routes, retries, cross-unit model binding and hidden inactive screens |
| `../UMemory.Tests.pas` | Disposal scopes, capacity growth, ownership transfer and exception cleanup |

The imported framework tests are kept here so this checkout has no dependency on
a separate PascalFramework repository. Their original MIT notices are retained.
Shared component test sources are mirrored in PascalFramework. The separate
[native Delphi timer probe](native/common/README.md) is mirrored there too.

The [native startup probe](native/README.md) exercises the actual application
composition and forms with a new disposable data directory and offline mode. It
checks completed empty-chain startup, enabled UI, stationary embedded screens,
offline enforcement after settings/connectivity actions, generated key changes
and actual tool/menu composition. The loaded mode also holds the mempool while
visible synchronization block/state/network callbacks run: they must return with
the previous statistics intact, then publish by worker completion after release
without another refresh request or polling tick. Its four harness
modes are `loaded`,
`close-loading`, `close-before-load` and `slow-close`; the last holds core locks
while checking responsive native Close and cancellation. `actual-first-run` and
`actual-no-dialogs` then launch the built wallet with fresh temporary data, verify
an enabled main window and close through its native exit handlers. Each process
has a watchdog.

RAW tests exchange independently signed copies of the same multioperation, merge
missing signatures and verify the combined operation against core account state.
They also cover repeat imports, wrong-owner signatures, conflicting bodies, a late
conflict after staged signature merging, and presentation callbacks that fail
after a successful import. A rejected import preserves the original package;
signature encoding alone never establishes cryptographic validity.

Large-wallet regressions exercise more than an empty blockchain. The account
fixture persists and reopens its index; the history fixture generates signed
operations, checks actual core acceptance and counts affected-account visits.
Its concurrency case blocks a real `FetchPage` call and requires both account and
history-depth getters/setters to finish before opening the fetch gate. Cleanup
opens the gate and joins both workers even when an assertion fails.

Snapshot fixtures also require that a 50,000-item sorted dataset is fetched once:
the next 25-row page performs only 25 field conversions and no additional sort.
Changing search/sort rebuilds the view from the existing raw snapshot. A thousand
logical invalidations coalesce into one subsequent rebuild, and invalidation
returns while the fetch gate remains closed. Wallet-specific gates cover account
filter/pending settings and membership/depth changes without constructing a node.

Account paging regressions must count both ID traversal and full account-record
reads: a default page should read only its visible records rather than first
materializing every owned account. Cover multiple key prefixes, page clamping,
same-count pending ownership changes and the current-key-only fallback for other
sorts/searches. Aggregate tests must keep Overview totals global across key-filter
changes, exercise cancellation/state changes between short read chunks and reject
partial or stale publication. Renderer tests must paint while core state is busy
using only the row's captured pending flag.

Generic component admission checks distinguish a leading request from a final
request after five quiet seconds. Every backend, page, filter, sort, source or
resize input resets that component's timer. Resizing while Waiting is visual
only; busy/hidden requests retain the latest parameters. Internal completion and
layout replay do not notify again. No node/network predicate, UI poll or separate
cooldown controls the generic component's Waiting. These contracts belong to the
mirrored framework fixtures and native common probe.

The actual main wallet enables no refresh throttle. Its node-sync binding holds
`RefreshSuspended` and the manual synchronization card while physical work remains;
completion resumes one latest retained request immediately. Ordinary page loading
ends when the latest accepted rows arrive, independently of that manual card.
Wallet checks record actual admissions, fetch/record-read counts and rendered
values; they cover cancellation, busy/hidden replay, network-off with received
work and selected-row-to-empty transitions. Cancelled results/errors must neither
publish nor clear a newer node-owned card.

These fixtures do not load a user wallet, connect to peers, mine or broadcast.
Core acceptance uses a test-only safebox subclass to seed state; production
consensus checks and `config.inc` remain unchanged.
## Native core synchronization lifecycle

The [core synchronization probe](native/node-sync/README.md#core-lifecycle-probe)
uses the same plain Pascal helper in Delphi and Free Pascal. Eight cases cover a
6.2-second inter-batch gap, received work continuing across network-off and
disconnect, fresh request generations, main-thread notifier ordering, committed
summary publication and a subscriber exception. Physical work remains
synchronizing until it finishes, independently of network availability. These
core checks complement the actual-wallet/data-source and buffered transport
checks; they do not replace them.
