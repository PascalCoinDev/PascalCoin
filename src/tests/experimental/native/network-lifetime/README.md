# Download and node lifetime checks

[DownloadLifetimeProbe.dproj](DownloadLifetimeProbe.dproj) and its
[FPC entry point](DownloadLifetimeProbe.lpr) run seven lifetime
[scenarios](UDownloadLifetimeChecks.pas):

- selected outgoing connection and factory failure
- incoming retirement
- owner shutdown
- node-owned callbacks
- pending discovery
- running discovery
- client registered after an empty scan

They open loopback sockets and write data under the log folder. They never
load a user's wallet, contact public peers or submit operations.

The [combined Delphi project](CoreSynchronizationProbe.dproj) and
[FPC program](CoreSynchronizationProbe.lpr) run 57 checks:

- the lifetime scenarios
- sanitizer, signature, cache-policy and signed change-info checks
- 17 [node synchronization checks](../node-sync/README.md#core-lifecycle-probe)
- three [legacy connection checks](ULegacyConnectionLifetimeChecks.pas)
- ten [pending catch-up checks](../block-throughput/UPendingCatchupChecks.pas)

## Run the probes

Build Win64 Debug in Delphi, put OpenSSL 1.1 beside the executable and point
`TEMP` and `TMP` at a test folder. Create the parent folder, then pass one
new log-file path:

```powershell
.\CoreSynchronizationProbe.exe <local-folder>
```

A successful run exits 0 and writes a summary and per-scenario logs. FPC also
writes a heap report.

[`RunExperimentalWalletTests.ps1`](../../RunExperimentalWalletTests.ps1) also
runs each lifetime scenario and legacy check as an FPCUnit case.

## Ownership rules

The node owns its server callbacks and drains them before freeing its
resources. Application shutdown publishes `BeginShutdown`, stops and joins the
shared network services, then frees the node. A retained connection stays
alive, but its mutable fields still need synchronized access.
