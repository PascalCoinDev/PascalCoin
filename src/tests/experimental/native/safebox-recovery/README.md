# Interrupted safebox recovery

`SafeboxRecoveryProbe` checks recovery from an interrupted safebox download. It
never opens the user's wallet, contacts public peers or switches the desktop.

Run the FPC suite from the repository root:

```powershell
& src\tests\experimental\RunWalletSafeboxRecoveryTests.ps1 -OutputDirectory <new-directory>
```

The script builds the probe and its [private-desktop broker](../README.md#private-desktop-broker), then
checks the result log, natural exit and heap report. The heap report and data
directories go beside the log. `-FpcPath` selects another Windows x64 compiler.

The [Lazarus project](SafeboxRecoveryProbe.lpi) and
[Delphi project](SafeboxRecoveryProbe.dproj) run the same checks. For Delphi,
place the matching OpenSSL runtime beside the EXE. Run it only through the
script's `PrivateDesktopBroker.exe`, passing the EXE and a new absolute log
path.

The probe's exit code is the number of failed cases, or `99` for a fatal fixture
error. The result log ends with `RESULT cases=8 failures=N`.

## Cases

- A late `GET_SAFEBOX` response is discarded without disconnecting or
  blacklisting the peer.
- Socket EOF detaches a discovery endpoint during a download lease.
- After its only peer disconnects, the downloader retries its chunk request on a
  replacement socket.
- EOF during chunk decompression lets validation finish before the worker joins.
- A start request during a download queues one follow-up download, without a
  second HELLO.
- A peer-ahead HELLO on an empty chain defers pending-operation exchange.
- An invalid `GET_SAFEBOX` autosend logs its rejection and disconnects the peer.
- An invalid safebox chunk logs its rejection, disconnects the peer and
  publishes no archive.
