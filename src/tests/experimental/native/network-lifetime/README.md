# Download and node lifetime checks

[DownloadLifetimeProbe.dproj](DownloadLifetimeProbe.dproj) and the
[FPC entry point](DownloadLifetimeProbe.lpr) run seven scenarios through the
same [acceptance helper](UDownloadLifetimeChecks.pas). They establish real
loopback sockets and generate fresh data beneath the supplied log directory.
They do not load an existing wallet, connect to public peers or submit operations.

| Scenario | Required result |
|---|---|
| Selected outgoing connection and factory failure | Failed construction does not poison worker admission; the selected transport stays alive while its download is gated; an unselected peer retires independently |
| Incoming retirement | An accepted socket cannot be replaced or freed before its selected download lease ends |
| Owner shutdown | Shutdown refuses new workers and joins the existing worker before releasing its connection and shared owner state |
| Owned node callbacks | A real accepted-client finalization subscriber can still acquire a node read scope while the node drains its owned server |
| Pending discovery | A discovery worker is owned before it starts; shutdown refuses another worker and joins the pending worker without creating a late client |
| Running discovery | Owner resources remain alive while a discovery worker is gated; the resumed production body observes shutdown before creating a client |
| Client registered after an empty scan | The one owned destroyer is paused after examining an empty registry; its drain must wait for a subsequently registered real client to retire |

Candidate selection and the following download boundary are gated for
determinism. The production worker's leased snapshot, unselected release,
retirement, listener and destructor paths execute unchanged. These checks do
not establish consensus validation, live-peer throughput or atomic snapshots of
all mutable peer metadata. Factory failure is injected at the existing virtual
factory; operating-system thread creation failure is not simulated.

The [combined Delphi project](CoreSynchronizationProbe.dproj) and
[FPC program](CoreSynchronizationProbe.lpr) reuse those seven checks alongside
nine sanitizer/signature, eight node-status, four signed change-info and three
cache-policy checks. Three further
[legacy connection checks](ULegacyConnectionLifetimeChecks.pas) cover retirement
while a connection lock is held, balanced recursive acquisition and timed or
nonblocking acquisition failure. Ten further
[pending catch-up checks](../block-throughput/UPendingCatchupChecks.pas) cover
deferred validation, unavailable reads/admission, final completion, conflicting
operations, reconnect races, failure-atomic retries and safe Quit packages.
The combined probe runs 44 scenarios in one native core compilation graph.

Build Win64 Debug in Delphi, put the existing OpenSSL 1.1 runtime beside the
resulting executable, and pass one new log-file path:

```powershell
.\CoreSynchronizationProbe.exe <local-folder>
```

Create the parent directory first. A successful run exits zero and writes
individual scenario logs as well as its summary. FPC writes an additional heap
report. The normal
[`RunExperimentalWalletTests.ps1`](../../RunExperimentalWalletTests.ps1) also
registers all seven lifetime scenarios individually with FPCUnit. Use an owned
`TEMP`/`TMP` directory for native checks.

The legacy retirement gate runs before inherited destruction, leaving the real
connection alive while a failing assertion releases its lock. It neither reads
a freed object nor replaces the production retirement or lock implementation.
Those three cases are registered separately in the full FPCUnit suite.

The node owns its server callbacks and must drain them before freeing node
resources. Shared network services still have their own owner: application
shutdown publishes `BeginShutdown`, stops and joins those services, then frees
the node. A retained connection is a lifetime guarantee, not a replacement for
synchronizing access to its mutable fields.
