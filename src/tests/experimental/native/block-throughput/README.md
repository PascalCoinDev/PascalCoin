# Block application and pending replay

These Windows console probes test the core's block application, pending replay
and pending saves. They never connect to peers or use private wallet keys. Put
OpenSSL 1.1 beside each executable and pass it the path of a new log file. Each
exits with 0 on success. All but the throughput probe create data folders under
`TEMP`, and their FPC builds write `<logfile>.heap.log`.

## Pending catch-up

`PendingCatchupProbe.dproj` (Delphi) and `PendingCatchupProbe.lpr` (FPC) test
the node's deferred pending maintenance in ten generated-data cases. Set
`PASCAL_PENDING_CATCHUP_CASE` to a case number to run only that case:

0. Buffered commits
1. Remote target across batch gaps
2. Confirmed and pending conflicts
3. Unavailable reads and admission
4. New work during final replay
5. Retired and reconnected owners
6. Pending-file publication on Quit
7. Empty scopes
8. Automatic retry after a failed replay
9. Quit after a failed replay

Both projects and the FPCUnit wrapper share
[`UPendingCatchupChecks.pas`](UPendingCatchupChecks.pas). Point `TEMP` and `TMP`
at a test folder:

```powershell
$env:TEMP = '<new-folder>'
$env:TMP = $env:TEMP
New-Item -ItemType Directory -Path $env:TEMP -Force
.\PendingCatchupProbe.exe <new-folder>\checks.log
```

## Operation sanitizer

`SanitizeOperationsProbe.dproj` (Delphi) and `SanitizeOperationsProbe.lpr` (FPC)
run nine check groups with generated accounts and signed operations:

- All-valid tree retention
- Invalid nonces and duplicates
- Delete-created index gaps
- Zero-fee signer limits
- Input preservation after replay exceptions
- Shared operation-registry ownership
- Rejected recovery metadata and empty pools
- Reference-lookup boundaries and first duplicates
- Signature caches across protocols

[`UBlockSanitizeTests.pas`](../../core/UBlockSanitizeTests.pas) registers each
group as an FPCUnit test. Build the probe with range and overflow checks.

```powershell
.\SanitizeOperationsProbe.exe <new-folder>\checks.log
```

## Pending saves

`PendingSaveProbe.dproj` and `PendingSaveProbe.lpr` run five saver and
persistence checks.
[`UPendingSaveLifecycleTests.pas`](../../startup/UPendingSaveLifecycleTests.pas)
registers them with FPCUnit. The checks cover dirty shutdown, a save failure
racing termination, sanitizer removals, publication failures and a failed final
publication.

Each check writes `<logfile>.<number>.log`. Allow up to two minutes.

## Block throughput

`BlockThroughputProbe.lpr` and [`BlockThroughputProbe.dproj`](BlockThroughputProbe.dproj)
time pending replay on a prepared public-chain clone. Build them with consistent
optimization and without heap tracing.

Set `PASCAL_BLOCK_BENCH_DATA` to the clone's root, never a working wallet
directory. The root needs a `.block-throughput-owned` marker. Its `Data` folder
needs `BlockChainStream.blocks`, `safebox.am_safebox` and a `pendingbuffer.ops`
with 185,831 pending operations.

```powershell
$env:PASCAL_BLOCK_BENCH_DATA = '<chain-copy-folder>'
.\BlockThroughputProbe.exe <chain-copy-folder>\throughput.log
```

By default the probe times three sanitizations and checks that the output is
unchanged. It reports write-lock admission, body wall time and process CPU time.
Optional variables:

- `PASCAL_BLOCK_BENCH_ASSERT_RETENTION=1` fails the run if the all-valid fast
  path replaces the original tree.
- `PASCAL_BLOCK_BENCH_PHASES=1` runs one sanitization, then replay, tree-rebuild,
  copy, serialization and pending-save experiments whose times are not additive.
- `PASCAL_BLOCK_BENCH_ACCOUNT_CACHE` sets the account cache to `50000` or
  `250000` records.

### Mixed-rejection mode

`PASCAL_BLOCK_BENCH_CONFIRM_EVERY=1000` commits every 1,000th pending operation
in a simulated block, then times three sanitizations. This changes the clone's
safebox.

### Real-block mode

Set `PASCAL_BLOCK_BENCH_REAL_BLOCKS` to a folder holding public-chain blocks
`block-841529.bin` through `block-841533.bin` in network format. The probe
applies them to the 841,529-block clone, timing each block call and the final
pending reconciliation. Restore the changed clone after the process exits.

`PASCAL_BLOCK_BENCH_ANNOUNCED_LAST=1` applies the fifth block as a node
announcement.

## Results

Results are core runs without a UI or active network, on the frozen
841,529-block dataset with a 21,183,301-byte pending file. Timings exclude the
probe's checks.

### Sanitization

Three FPC sanitizations took 7,407 / 7,375 / 7,422 ms. Before the
unchanged-operation index fast path, they took 10,281 / 10,000 / 10,063 ms, and
phase experiments measured 6,093 ms of replay and 3,078 ms of tree rebuilding.

### Before deferred catch-up replay

With the 250,000-record account cache, the five block calls took
4,000 / 3,578 / 3,515 / 3,453 / 3,469 ms, plus a 266 ms final safebox flush.
The mixed-rejection mode removed 186 confirmed operations and took
3,187 / 3,203 / 3,234 ms. These are FPC runs.

### With deferred catch-up replay

Both runs, built with range and overflow checks, kept all 185,831 pending
operations:

| Compiler and route | Block calls, ms | Final pending reconciliation |
|---|---|---|
| FPC, five bulk calls | 578 / 250 / 265 / 250 / 219 | 4,016 ms |
| Delphi Win64, four bulk calls and one node announcement | 375 / 63 / 31 / 62 / 500 | 3,047 ms |

Each row is one run, not a compiler-speed comparison. The FPC run predates the
proof-of-work empty-payload write guards.
