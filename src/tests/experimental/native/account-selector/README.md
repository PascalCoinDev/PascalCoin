# Experimental account selector

[AccountSelectorNativeProbe.dpr](AccountSelectorNativeProbe.dpr) tests the
experimental account-selection form, visual grid and confirmed-account source.
It creates no node, wallet file or peer connection.

Build it with Delphi (VCL) and Lazarus/FPC, using the experimental wallet's
source paths and compiler settings plus this directory. Enable range and
overflow checks (`-Cr -Co` for FPC). FPC also needs the matching LCL, LazUtils
and embedded Generics.Collections paths. Keep each compiler's output separate,
with the matching OpenSSL runtime beside each EXE.

Run each EXE through the [private-desktop broker](../README.md#private-desktop-broker),
passing one absolute log filename in a new folder:

```powershell
& $Broker $DelphiSelectorExe '<local-folder>'
& $Broker $FpcSelectorExe '<local-folder>'
```

Success writes `PASS complete experimental account selector checks` and exits
with code 0. Failure writes `FAIL` and exits nonzero.

The [checks](UAccountSelector.Checks.pas) cover:

- sale-price ordering
- snapshot caching
- key selection, refreshed only by reassigning `WalletKeys`
- filters and totals
- rapid key changes
- retries
- background type statistics
- closing mid-read
