# Node synchronization checks

These probes use generated state in a new directory beneath `TEMP`. They keep
existing wallet data untouched and use owned peer identities without opening a
network listener or connecting to external peers.

## Core lifecycle probe

Open [NodeSynchronizationCoreProbe.dproj](NodeSynchronizationCoreProbe.dproj)
in Delphi and build Win64 Debug. The [Delphi entry point](NodeSynchronizationCoreProbe.dpr)
and [Free Pascal entry point](NodeSynchronizationCoreProbe.lpr) call the same
plain Pascal [checks](UNodeSynchronizationChecks.pas); neither requires FPCUnit or
wallet controls. Keep the existing OpenSSL 1.1 runtime beside the executable.

The seventeen cases reuse the acceptance checks in
[UNodeSynchronizationTests](../../core/UNodeSynchronizationTests.pas):

| Case | Required behavior |
|---|---|
| Selected target and 6.2-second batch gap | Synchronization persists without active transfers or periodic input |
| Network off during buffered work | Downloads stop and remote targets clear; synchronization continues until physical work drains |
| Last disconnect and reconnect | No peers clears remote targets; buffered processing finishes before local availability resumes |
| Synchronizing notifier batch | Pause precedes block, operation, message and key inputs |
| Offline notifier batch | Resume follows all those inputs |
| Actual write release | The cached local-tip summary completes a reached target |
| Buffered and new processing actors | Reconnect retains actual work; stale download targets stay cleared; every nested and parallel scope exits before idle |
| Failing backend subscriber | An exception in a block listener cannot lose the final synchronization status; no additional producer signal is required |
| Persistent storage failure | First stage/path/exception remain visible until restart; toggles, progress and drained work cannot clear it; operations and mining readiness reject new work |
| Failure notification delivery | A reporting worker completes without pumping the UI; listeners receive copied error data on the main thread |
| Storage failure during buffered work | Physical scopes finish cooperatively and stay balanced; no next download is admitted |
| Failure notifier batch | Error state reaches listeners before backend refresh inputs |
| Non-storage exceptions | Cancellation, resource exhaustion and programmer faults do not become storage-error state |
| Actual checkpoint publication failure | A real SaveBank call fails publication to an occupied directory; the canonical node latches the path/cause and preserves the previous checkpoint |
| Actual block-storage rejection | A false DoSaveBlockChain result becomes a write error, latches the canonical node, preserves supplied operations and prevents further mutation |
| Safebox flush boundary | Cancellation propagates without a latch; a write failure latches before UI delivery, preserves previous bytes and rejects admission even after a later successful flush |
| Block-cache flush boundary | A false final flush becomes a write error; the canonical node remains paused through later flushes and network toggles |

Every case creates and destroys a real node and its actual notifier. Listener
thread identity is checked. The target tests use the actual node state setters;
they do not substitute a UI clock or invoke notification handlers by hand.
The local-tip case seeds metadata through a test-only safebox subclass; it checks
summary publication rather than block consensus validation.

The executable requires one log-file argument. Use an owned D: directory for
`TEMP`, `TMP` and logs. The existing private-desktop broker can launch it with
its ordinary two arguments, `EXE` and `LOG`; it never switches the input desktop.
A successful process exits zero and logs fifteen case passes; a check failure exits
one. Outputs from the public Delphi project go to
`src/tests/experimental/lib/node-sync-core/Delphi/<Platform>/<Config>`.

Earlier seven-case results used an offline policy that retired the UI's work
count before buffered processing finished. Those results are historical and do
not establish the current contract. Network availability controls new downloads;
the actual processing count controls whether synchronization has finished.

These checks establish core lifecycle behavior. They do not by themselves
establish wallet grid presentation, large-dataset throughput or live peer
synchronization.

On 30 October 2026, clean checked Delphi Win64 and FPC 3.2.0 native builds
passed all eight then-current cases and exited naturally. The FPC process reported
6,840 allocations and frees, with zero unfreed blocks. The last case deliberately
raises once inside a backend subscriber and confirms that the real notifier
still delivers its final status without another producer event.
