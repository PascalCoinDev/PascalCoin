# Received block backlog and wallet status

These regressions feed the embedded
[small public-chain fixture](../../fixtures/README.md) to the experimental wallet
over a loopback socket. They check its status, grids and storage while the
backlog drains. No check loads existing wallet data or private keys.

Run the FPC target from the repository root:

```powershell
.\src\tests\experimental\RunWalletGridRefreshTests.ps1 -Backlog `
  -OutputDirectory <new-folder>
```

The output directory must be new. Each scenario runs on a private desktop and
must exit naturally with a clean FPC heap report.

For Delphi, build a project below in the IDE and run it through the
[private-desktop broker](../README.md#private-desktop-broker) with a new absolute log-file path. Each
project also has an FPC `.lpr` entry point.

## Scenarios

| Case | Delphi project | Scenario |
|---|---|---|
| `direct` | `BufferedBlockBacklogProbe.dproj` | Calls the response handler directly and checks nested requests online and offline |
| `synchronization` | `BufferedBlockBacklogProbe.dproj` | Both synchronization cards stay visible through a quiet gap longer than six seconds, then clear |
| `outgoing` | `OutgoingBufferedBlockProbe.dproj` | OFF must not truncate the client's buffered batch |
| `outgoing-shutdown` | `OutgoingShutdownProbe.dproj` | Shutdown finishes within 15 seconds and stops between complete blocks |
| `incoming` | `IncomingBufferedBlockProbe.dproj` | OFF returns promptly while all received blocks drain |
| `incoming-settings` | `IncomingSettingsProbe.dproj` | Latest settings win after rapid OFF, ON, OFF, port and name changes |
| `incoming-shutdown` | `IncomingShutdownProbe.dproj` | As `outgoing-shutdown`, while OFF waits for an incoming batch |
| `connected-miner` | `ConnectedMinerProbe.dproj` | With a loopback miner, notifications reach the UI promptly while the bank writer is held |

`BufferedBlockBacklogProbe` runs the case named in `PASCALCOIN_BACKLOG_CASE`, or
`direct` when it is unset. Other projects ignore the variable.

## Optional large public-chain fixture

`LargeBufferedBlockBacklogProbe.dproj` and its FPC `.lpr` entry point run the
main form against a chain copy at block count **841529** with **185831** pending
operations. The embedded
[five public blocks](../../fixtures/mainnet-blocks-841529-841533.json) advance
it to **841534**.

Prepare a new app root that holds a `LargeBacklog.owned` marker and a `Data`
junction to that chain copy, but no `WalletKeys.dat` or `AppParams.prm`. Confirm
that `Data` resolves to the copy. The run changes the copy, so reserve it for
this run alone. The test never uses a live wallet directory.

```powershell
.\src\tests\experimental\RunWalletGridRefreshTests.ps1 -LargeBacklog `
  -LargeDataRoot <large-wallet-copy> `
  -OutputDirectory <another-new-folder>
```

Without the script, set `PASCALCOIN_LARGE_BACKLOG_ROOT` to the app root and run
the executable through the broker with a new absolute log-file path. A direct
FPC compilation also needs `-dLARGE_BACKLOG_PROBE`.

The run requires that:

- the initial pages show at least 326,387 owned accounts and 1,000
  pending-history rows
- both grids stay covered without new queries while the batch drains after OFF
- main-message dispatch stays below 400 ms
- the pending operations stay unchanged
- all five blocks and 185,831 pending operations reopen after shutdown

Keep before-and-after hashes of the chain files, and restore the copy before
the next run.
