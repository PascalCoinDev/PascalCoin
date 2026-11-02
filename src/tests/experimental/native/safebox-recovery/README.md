# Interrupted safebox recovery

`SafeboxRecoveryProbe` uses production node/network owners and raw loopback peers.
Run the checked FPC suite from the repository root:

```powershell
& src\tests\experimental\RunWalletSafeboxRecoveryTests.ps1 -OutputDirectory <new-directory>
```

The script builds the probe and its private-desktop broker, then checks the
result log, natural exit and heap report. It creates fresh data directories
beside that log, and never opens the user's wallet or public peers. The desktop
is never switched. Use `-FpcPath` to select another Windows x64 compiler.

The [Lazarus project](SafeboxRecoveryProbe.lpi) and
[Delphi project](SafeboxRecoveryProbe.dproj) use the same checks. For a separate
Delphi build, place the matching OpenSSL runtime beside the EXE and launch it
through the generated `PrivateDesktopBroker.exe`, passing the EXE and an absolute
new log path. The probe requires that isolated desktop.

The eight cases check:

- A late `GET_SAFEBOX` response after timeout is discarded while the next request
  still consumes its own response, without disconnecting or blacklisting the peer.
- Socket EOF detaches a discovery endpoint even while a download lease keeps its
  old connection object alive. The real candidate filter admits the endpoint;
  eventual old-owner destruction preserves a new connection's binding.
- The actual public safebox downloader issues a new chunk request on a replacement
  socket after its sole peer disconnects. The existing background discovery and
  connection worker create the replacement; the fixture only accepts its socket.
  The inherited discovery, selection and chunk retry code run unchanged.
- A real chunk response reaches decompression while the worker still owns its
  processing lock. EOF and cooperative cancellation let validation finish before
  the worker joins and its temporary stream is removed.
- A blockchain start request during a busy worker is retained. Exactly one
  successor starts after normal completion, without a second HELLO.
- An actual peer-ahead HELLO on an empty local chain defers pending-operation
  exchange, preventing repeated protocol-5 operations being applied at protocol 1.
- An invalid `GET_SAFEBOX` autosend still logs its exact invalid-client rejection
  and disconnects the peer.
- A matching chunk response with an invalid safebox header still logs its exact
  invalid-chunk rejection, disconnects and never publishes an archive.

Positive waits are bounded at five seconds, except automatic discovery, which
allows 20 seconds for its existing randomized start. Gated boundaries have a
15-second emergency release deadline; cleanup always opens them before joining.
No thread is forcibly terminated. Cancellation checks verify zero published
chunks, zero remaining physical synchronization scopes, no final safebox archive,
no partial temporary files and byte-for-byte preservation of a completed-file
sentinel. The sentinel is not a consensus-valid safebox.

Synthetic operation metadata partitions one 100-block chunk. The compression
envelope used by the ownership case deliberately contains an invalid safebox
header, so the case asserts request/validation lifetime, not checkpoint consensus
acceptance or successful multi-chunk assembly. Existing automatic discovery
backoff remains in force; only the owned endpoint's timestamp is aged for the
candidate-filter assertion.

Setup reserves its owned initial endpoint before network activation, then runs
the normal `ConnectTo` path. This prevents an unrelated discovery completion from
supplying the handoff case's successor. After connection, the address retains its
normal retry backoff until the automatic-recovery case deliberately ages it.

Production deliberately exempts loopback/private addresses from blacklisting.
The negative cases preserve that exemption while observing the actual rejection
reason through `TLog.OnInThreadNewLog` and confirming socket disconnection.
They do not change the blacklist policy or claim to exercise a public peer's ban.

The exit code is the number of failed cases (`99` for a fatal fixture error).
The result log ends with `RESULT cases=8 failures=N`; FPC also writes a heap
trace beside it. These are native integration checks, separate from consensus
unit tests and the application's interactive projects.
