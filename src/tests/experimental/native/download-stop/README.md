# Safebox download shutdown regression

The downloader requests cooperative cancellation from every chunk worker,
releases its chunk-list lock, waits for their normal exits, then releases their
streams and shared state. `TThread.Terminate` sets a flag; it does not kill a
thread or close its in-progress stream. A worker retries
under the same lock; waiting for it while holding that lock creates a deadlock.

Run the checked Lazarus/FPC probe from the repository root:

```powershell
& src\tests\experimental\RunWalletDownloadStopTests.ps1
```

The script builds into a fresh directory and uses the existing private-desktop
broker. Its log and heap report must show a natural exit and no leaks. Use
`-OutputDirectory <new-directory>` to retain evidence in a chosen location.

The matching [Delphi project](DownloadSafeboxStopProbe.dproj) uses the same
[checks](UDownloadSafeboxStopChecks.pas). Build it with the normal Delphi compiler
and launch its EXE through the private-desktop broker with one absolute log-file
argument. Keep it beside the existing matching OpenSSL runtime; there are no build
events or generated source-resource dependencies.

The probe uses a new explicit data directory and loopback-only discovery. The
public downloader creates two actual chunk workers. A gate releases both into
their inherited retry loop at the first real join, making contention for the
owner's list deterministic. It checks that both workers were cancelled before
that join, both retries return, both joins complete, temporary streams are removed
and the enclosing core-work scope ends. Empty downloader destruction is covered
too. The fixture does not replace the production lock acquisition or worker retry.

Each worker also completes, flushes and rereads a real temporary-file write after
the cancellation flag is set. Streams must remain alive through worker exit and
all joins; partial files are deleted afterward. Existing saved-file bytes must
remain unchanged and a cancelled download must publish no archive. This checks
the downloader's cooperative stream lifetime; it is not a general protocol
durability or power-loss test.

This shutdown test is independent of grid synchronization presentation. The
wallet's synchronization card follows the node's physical synchronization work;
the generic refresh throttle retains its independent timer/reset contract.
