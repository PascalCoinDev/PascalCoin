# Safebox download shutdown regression

Checks that stopping a safebox download causes no deadlock, lets in-flight
writes finish, deletes partial files, keeps saved files unchanged and publishes
no archive. It also covers destroying an empty downloader.

Run the Lazarus/FPC probe from the repository root:

```powershell
& src\tests\experimental\RunWalletDownloadStopTests.ps1
```

The script builds into a new folder and runs the probe through the
[private-desktop broker](../README.md#private-desktop-broker). Pass
`-OutputDirectory <new-directory>` to choose the output folder. `-FpcPath` and
`-LazarusDirectory` select a Windows x64 FPC and Lazarus. The log and heap
report must show a natural exit and no leaks.

The [Delphi project](DownloadSafeboxStopProbe.dproj) uses the same
[checks](UDownloadSafeboxStopChecks.pas). Build it with Delphi and put the
matching OpenSSL runtime beside the EXE. Run it through the broker with one
absolute log filename.

The probe uses a new data folder and loopback-only discovery.
