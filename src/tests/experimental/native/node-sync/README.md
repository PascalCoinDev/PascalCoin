# Node synchronization checks

These probes generate state in a new folder under `TEMP`. They never touch
existing wallet data, open a network listener or connect to external peers.

## Core lifecycle probe

This probe mirrors the seventeen synchronization lifecycle cases of
[UNodeSynchronizationTests](../../core/UNodeSynchronizationTests.pas). The
[Delphi](NodeSynchronizationCoreProbe.dpr) and
[Free Pascal entry points](NodeSynchronizationCoreProbe.lpr) run the same
[checks](UNodeSynchronizationChecks.pas) without FPCUnit or wallet controls.

Open [NodeSynchronizationCoreProbe.dproj](NodeSynchronizationCoreProbe.dproj)
in Delphi and build Win64 Debug. Output goes to
`src/tests/experimental/lib/node-sync-core/Delphi/<Platform>/<Config>`. Put
OpenSSL 1.1 beside the executable.

Use a test folder for `TEMP`, `TMP` and the log. Pass one log-file
argument, or run the probe through the
[private-desktop broker](../README.md#private-desktop-broker) with its `EXE` and `LOG`
arguments.

A successful run exits 0 and logs a pass per case. A check failure exits 1.

The cases cover:

- **Synchronization lifetime.** A selected target survives gaps between
  batches. With the network off or no peers left, downloads stop and remote
  targets clear. Synchronization still lasts until buffered work drains, even
  across a reconnect.
- **Completion.** Reaching the target completes synchronization, and a failing
  block listener cannot lose the final status.
- **Notifier order.** The synchronizing pause and storage errors precede block,
  operation, message and key inputs. The offline resume follows them.
- **Storage failures.** After a failed checkpoint, block save or flush, the
  node latches the first error until restart. It keeps earlier data and rejects
  new work, but buffered work finishes. Listeners get the error on the main
  thread without blocking the worker. Only storage errors latch.

## Wallet probe

[WalletNodeSynchronizationProbe](WalletNodeSynchronizationProbe.lpr) checks the
main form on a real node: synchronization cards, account-cache order, cancelled
grid reads and the storage-failure banner.
`../RunWalletRegressionTests.ps1 -Probe node-sync` builds and runs it through
the broker.

## Recorded results

On 30 October 2026, Delphi Win64 and FPC 3.2.0 builds passed the eight cases
then present, and FPC reported zero unfreed blocks.
