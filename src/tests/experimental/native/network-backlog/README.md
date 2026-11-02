# Received block backlog and wallet status

These regressions feed the five original mainnet blocks through the actual
network response processor, normal proof-of-work/bank validation and storage.
They include the real outgoing `TNetClient` receive thread and incoming
`TNetServerClient` listener/socket ownership. They run the real experimental main
form and inherited data sources,
with an owned loopback socket and fresh data. No existing wallet data or private
keys are loaded. The settings case generates one disposable key and configures
the mining listener on an OS-selected port, without external miners. The
[small public-chain fixture](../../fixtures/README.md) is embedded
in the executable.

Run the Free Pascal target from the checkout root:

```powershell
.\src\tests\experimental\RunWalletGridRefreshTests.ps1 -Backlog `
  -OutputDirectory <local-folder>
```

The output directory must be new. The script compiles once and runs all seven
scenarios in separate fresh processes. The existing broker creates a private
desktop and passes the probe one new absolute log-file path. Each scenario must
exit naturally with a clean FPC heap report. The probe refuses the input desktop.

For Delphi, build a project below with the installed IDE and run its executable
through the same broker/log contract. These are automatic tests, not interactive
wallets. Each project also has a corresponding Free Pascal `.lpr` entry point.

| Delphi project | Scenario |
|---|---|
| `BufferedBlockBacklogProbe.dproj` | Direct protected response entry, real online/offline nested request admission and per-commit UI assertions. |
| `OutgoingBufferedBlockProbe.dproj` | Actual client-owned receive thread; OFF must not let automatic client destruction truncate its buffered batch. |
| `OutgoingShutdownProbe.dproj` | Actual application shutdown while that outgoing batch is held; stop between complete blocks and join naturally. |
| `IncomingBufferedBlockProbe.dproj` | Actual listener/socket worker; the real toolbar OFF action must return promptly while all received blocks drain. |
| `IncomingSettingsProbe.dproj` | Rapid OFF/ON/OFF and port/name settings while the incoming batch is held, with mining configured on using a disposable generated key. Latest settings, unchanged ordinary reward-key selection and subsequent listener/footer events must win. |
| `IncomingShutdownProbe.dproj` | Actual application shutdown while the serialized listener-stop task is waiting for an incoming buffer. |
| `ConnectedMinerProbe.dproj` | One actual loopback miner consumes a production job. Real block/operation notifications must reach the UI promptly while the bank writer is held; settings retain the same reward key. |

The primary executable also accepts a test-only process environment variable,
`PASCALCOIN_BACKLOG_CASE`, with values `direct`, `outgoing`, `outgoing-shutdown`,
`incoming`, `incoming-settings`, `incoming-shutdown` or `connected-miner`. An absent value selects
`direct`. The PowerShell runner sets and restores this variable around each
child; no environment setting is required for the individually named projects.

Outgoing/direct cases switch networking off in the first real committed-block
notification. Incoming cases hold that notification and invoke the actual
toolbar handler on the main thread. A separate owned gate actor bounds even a
blocking old listener shutdown without forcibly terminating any worker. Normal
OFF must finish all five already-received blocks; explicit application shutdown
must stop between complete blocks and reopen the complete first block.
The checks cover:

- Node status remains synchronizing, and both main grids stay suspended and
  covered until the batch drains.
- The footer and basic sync labels display the same committed block count and
  timestamp from the cached node record, while a modeless dialog keeps focus.
- Network-off rejects new connections, block requests and nested request waits,
  including native main-thread/external-thread callers.
- Normal OFF reopens all five blocks through the production disk-restore
  entrypoint with the correct final timestamp. Explicit shutdown reopens exactly
  one complete block.

These five empty historical blocks verify processing, UI consistency and
persistence. They do not measure dense-mempool or live-network throughput.
The owned peer is a real loopback socket. Owner scenarios reflect an exact
production-serialized response frame into the real socket, so the actual owned
worker performs decoding and application. Only the `direct` scenario invokes the
protected received-response entry point itself; it independently observes the
real outgoing request frame and public nested request/wait admission. The raw
peer does not implement full HELLO negotiation or peer discovery.

## Optional large public-chain fixture

`LargeBufferedBlockBacklogProbe.dproj` and its FPC `.lpr` entry point use the
same real main form and outgoing transport owner with a matching owned chain
copy at block count **841529** and **185831** pending operations. The embedded
[five public blocks](../../fixtures/mainnet-blocks-841529-841533.json) advance
the chain to **841534** through normal proof-of-work, operation, bank and storage
validation. This is separate from the seven fresh, small-chain scenarios.

Prepare a new app root with no `WalletKeys.dat` or `AppParams.prm`. Its `Data`
directory must be a junction to the explicitly owned public-chain copy, with a
`LargeBacklog.owned` marker in the new app root. The operator must validate the
resolved junction target and exclusively reserve that mutable copy for the run;
the test never creates a second chain copy or uses a live wallet directory.

```powershell
.\src\tests\experimental\RunWalletGridRefreshTests.ps1 -LargeBacklog `
  -LargeDataRoot <local-folder> `
  -OutputDirectory <local-folder>
```

The script supplies the global `LARGE_BACKLOG_PROBE` compiler define and the
`PASCALCOIN_LARGE_BACKLOG_ROOT` process variable. The Delphi project supplies the
same define; set that variable when invoking its executable through the existing
private-desktop broker with one new log-file argument. A direct FPC compilation
also needs `-dLARGE_BACKLOG_PROBE`. Neither project is an interactive wallet.

The fixture samples distinct public owners of the first, middle and last
pending signers, then affected accounts and a bounded public-chain fallback.
It loads no private keys. Real typed
provider subclasses count calls and invoke their inherited implementations;
they do not replace data reads, invalidation, refresh or lock behavior. Initial
pages must expose at least 326,387 owned accounts and 1,000 actual pending-history
rows. While the received batch remains active after OFF, both grids must stay
covered without new provider queries, the footer and sync labels must follow
each real commit, and main-message dispatch must stay below 400 ms. Page requests
made while paused must resume as one latest successful query per grid, with
their actual rows published.

The complete pending-operation count, ordered hash and serialized SHA-256 must
remain unchanged. After ordinary joined shutdown, all five stored blocks must
reopen with the exact original public network bytes and the pending package
must reopen with the same 185,831 operations. Retain before/after hashes of the
owned chain files, and restore that fixture before another compiler run.
These checks verify the combined UI/data/transport lifecycle; they are not a
live-network download-rate benchmark.
The dense FPC entry point keeps range and overflow checks but does not enable
heap tracing, so its allocation behavior matches the shipping program. Its
receipt does not assert zero unfreed blocks. The seven small scenarios and the
full wallet suite retain their separate heap-traced checks.
