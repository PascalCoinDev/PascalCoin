# Block application and pending replay

These console probes separate core correctness from UI and network activity.
They do not connect to peers or use private wallet keys.

`PendingCatchupProbe.dproj` (Delphi) and `PendingCatchupProbe.lpr` (FPC) exercise
the real node's deferred pending maintenance in ten generated-data scenarios.
They count inherited signed-operation validation against the node's actual
pending transaction, advance the confirmed bank through its normal subscriber,
and wait for the typed completion event. The cases cover buffered commits,
selected targets between batches, mixed confirmed/pending conflicts, unavailable
read/admission contracts, new work arriving during final replay, retired and
reconnected owners, complete pending-file publication on Quit, empty scopes,
automatic retry after a replay exception, and Quit after a failed replay.
Generated commits exercise transaction execution and accounting; they do not
claim proof-of-work acceptance.

Pass one new log path. By default all ten cases run. To isolate one case per
process, set `PASCAL_PENDING_CATCHUP_CASE` to an ordinal from `0` through `9`.
Both native compilers and the FPCUnit wrapper share
[`UPendingCatchupChecks.pas`](UPendingCatchupChecks.pas).

```powershell
$env:TEMP = '<local-folder>'
$env:TMP = $env:TEMP
New-Item -ItemType Directory -Path $env:TEMP -Force
.\PendingCatchupProbe.exe <local-folder>
```

`SanitizeOperationsProbe.dproj` (Delphi) and `SanitizeOperationsProbe.lpr` (FPC)
run nine groups with generated accounts and signed operations. They cover
unchanged-tree retention, invalid nonces, duplicate hashes/references, an actual
delete-created index gap, protocol zero-fee signer limits, complete-input
preservation after a replay exception, shared operation-registry ownership,
rejected recovery metadata and an empty pool. The normal operation methods and
sanitizer execute unchanged. Only the exception case supplies a throwing
transaction subclass. A separate lookup-boundary case supplies controlled
reference keys and a SHA collision while retaining inherited signed transaction
execution; it checks zero, the high-bit boundary, `High(UInt64)` and first
duplicate representatives under checked arithmetic.

The signature-cache regression verifies a real version-3 signature, loads its
same wire bytes as version 4, and demonstrates that the operation hash is equal
while the signing digest differs and actual signature verification fails. Cache
copying must match both the node block protocol and individual operation
protocol. Same-protocol positive cases retain the optimization.

The same nine scenarios are registered individually by
[`UBlockSanitizeTests.pas`](../../core/UBlockSanitizeTests.pas); the FPCUnit wrapper
calls the shared helper rather than duplicating sanitizer logic.

Pass one new logfile path to the executable. It returns zero on success. The
FPC runner also writes `<logfile>.heap.log`; enable compiler range and overflow
checks when building this correctness probe.

```powershell
.\SanitizeOperationsProbe.exe <local-folder>
```

`BlockThroughputProbe.lpr` / [`.dproj`](BlockThroughputProbe.dproj) measure actual pending replay after restoring
an explicitly prepared public-data clone. Set `PASCAL_BLOCK_BENCH_DATA` to its
root, containing a `.block-throughput-owned` marker and these three files inside
`Data`: `BlockChainStream.blocks`, `safebox.am_safebox`, and `pendingbuffer.ops`.
The fixture expects 185,831 pending operations and refuses an existing logfile.
Never point it at a working wallet directory. Restore and sanitization use the
normal storage, operation and transaction implementations.

```powershell
$env:PASCAL_BLOCK_BENCH_DATA = '<local-folder>'
.\BlockThroughputProbe.exe <local-folder>
```

Three sanitizations report write-lock admission, body wall time and process CPU
time separately. Outside the timed body, the probe checks ordered operation
identity, both lookup indexes, aggregate amounts/fees and exact pending-storage
serialization bytes. `PASCAL_BLOCK_BENCH_ASSERT_RETENTION=1` additionally requires
the all-valid fast path to retain the original tree owner. Optional
`PASCAL_BLOCK_BENCH_PHASES=1` runs one sanitization followed by independent actual
replay, tree-rebuilding, copy, serialization and pending-save experiments. These
phase experiments have different allocation/cache contexts and are not additive
profiler percentages. Build performance probes with consistent optimization and
without heap tracing; correctness probes serve the separate memory checks.

The frozen dataset contains 841,529 blocks and a 21,183,301-byte pending
file. An earlier optimization cohort took 10,281 / 10,000 / 10,063 ms without a UI or active
network. Independent phase measurements found 6,093 ms in actual operation replay
and 3,078 ms rebuilding eligibility/index state. Those results identify core work;
they do not establish network throughput or a completed performance fix.

With the first unchanged-operation index fast path enabled, the same frozen core
and input measured 7,407 / 7,375 / 7,422 ms. Every ordered operation, both lookup
indexes and the exact pending-file bytes remained unchanged. This is a partial
improvement of about 27%: replay still executes every operation. The subsequent
fallback builder uses RTL sorting with the original position as a tie-break,
then deduplicates in one pass; it preserves accepted operation order and first
representatives. Reference search uses explicit unsigned comparisons, fixing
the previous signed-subtraction overflow across `UInt64` halves. These are
FPC timings with no UI or active network, not Delphi or end-to-end block timings.

`PASCAL_BLOCK_BENCH_CONFIRM_EVERY=1000` enables the separate mixed-rejection
fixture. It commits a deterministic subset of the actual pending operations
through normal `DoOperation` and `TPCSafeBoxTransaction.Commit`, using a clearly
simulated block header without PoW validation. It then restores the original
in-memory input before each of three measured sanitizations and compares the
accepted output with normal public tree insertion, including every SHA/reference
representative, signer counts, fees, registry ownership, order and serialized
bytes. Use an owned clone: this mode deliberately changes its safebox. Its
results measure pending replay, not historical block-download throughput.

`PASCAL_BLOCK_BENCH_REAL_BLOCKS` selects a directory containing
`block-841529.bin` through `block-841533.bin`, in the normal network block format.
The probe starts from the owned 841,529-block clone, validates and applies all
five through `TPCBank.AddNewBlockChainBlock` with actual node write ownership
inside a real catch-up scope. Each block must advance cached confirmed facts
while leaving pending balances unavailable and the old pending header untouched.
A single existing signed change-info operation uses an inherited validator
counter: zero entries per block and one final entry. Its wire and storage bytes
must remain identical; this is a sentinel observation, not a global operation
counter. The maintenance worker signals completion after the physical scope
ends. An independent ordinary replay then compares all 185,831 operations,
their affected accounts and seals, both indexes, fees and serialized bytes.
Block application and final reconciliation are timed separately from this oracle.
The probe closes the writer, reopens the chain
read-only and requires every stored network byte to equal the original fixture.
This mode changes the owned clone; preserve input hashes and restore it after
the process has closed. Use actual publicly extracted block fixtures, not the
simulated-confirmation mode above.

Before deferred catch-up replay, the measured combined core with a 250,000-record account
cache took 4,000 / 3,578 / 3,515 / 3,453 / 3,469 ms for those five complete bank
calls, plus a 266 ms final safebox flush. All 185,831 pending operations stayed
valid, every output index matched normal insertion, and reopened block bytes
matched the real fixtures. The separate mixed-rejection fixture removed 186
confirmed operations and took 3,187 / 3,203 / 3,234 ms, preserving exact output
bytes and metadata. These are controlled FPC core measurements with no UI or
active network; oracle preparation is excluded from the measured calls. They
do not establish end-to-end network throughput. Set
`PASCAL_BLOCK_BENCH_ACCOUNT_CACHE=250000` explicitly to reproduce this composition;
the probe checks that restore preserved the requested cache budget.

`PASCAL_BLOCK_BENCH_ANNOUNCED_LAST=1` applies the fifth real block through
`TNode.AddNewBlockChain`, while the first four retain the normal bulk-bank path.
Controlled zero resend metadata must remain untouched through the batch. After
the single final replay, the announcement must collect the original first
50,000 eligible operations exactly once; the bulk-only mode must collect none.
Both modes retain the same PoW/storage and complete pending-account oracle.
Broadcasting stays disabled, so this checks selection and lifecycle rather than
live peer gossip.

With deferred catch-up replay, checked native probes applied these same five
public blocks with all 185,831 pending operations retained:

| Compiler and route | Complete block calls, milliseconds | Final pending reconciliation |
|---|---|---|
| FPC, five bulk-bank calls | 578 / 250 / 265 / 250 / 219 | 4,016 ms |
| Delphi Win64, four bulk calls and one node announcement | 375 / 63 / 31 / 62 / 500 | 3,047 ms |

Both probes observed zero pending-validator entries during every block and one
entry after the catch-up scope ended. The independent final oracle checked all
185,831 affected accounts and seals, operation order, both indexes, fees and the
exact 21,183,301-byte pending package. Reopened stored blocks matched every
original network byte. The bulk route selected no resend operations; the
announcement route selected exactly the original bounded 50,000 after replay.
These are single controlled core runs, not live-peer throughput or a comparison
between compiler speeds. The FPC measurement preceded the subsequent optional
empty-payload write guards; the Delphi measurement and both 44-check regression
runs included those final guards. Final empty and nonempty proof-of-work vectors
were verified separately with both compilers.

`PendingSaveProbe.dproj` / `.lpr` run five saver and persistence checks through
the actual node, signed `AddOperation` calls, bank subscriber, saver thread and
file storage. Pass one new logfile path. Each scenario creates its own generated
bank and data directory; the probe writes individual `<logfile>.<number>.log`
files. Several scenarios wait for the real ten-second saver interval, so allow
up to two minutes with Windows scheduling granularity.
[`UPendingSaveLifecycleTests.pas`](../../startup/UPendingSaveLifecycleTests.pas)
registers these same checks with FPCUnit.

The checks cover dirty shutdown during the quiet wait, a running save failure
racing termination, sanitizer removals scheduling persistence, serialization and
post-write publication failures, cached-handle reopening, and a failed final
publication that reports an error and exits with the last complete package.
The test storage observes normal writes; only the injected transient failure
occurs at its override boundary. Windows sharing-denial checks fail the actual
file replacement after the temporary package has been written.

Pending saves serialize to memory, write and flush a unique temporary file in
the destination directory, and publish with Windows `MoveFileExW` replacement
and write-through flags. The FPC Unix path uses `FileFlush` and `RenameFile`.
The previous package is never truncated before publication. File-format bytes
remain unchanged. These projects target Windows with Delphi and FPC. Their
sharing-denial cases do not establish Unix execution or filesystem power-loss
guarantees.
