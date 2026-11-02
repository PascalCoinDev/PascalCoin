# Experimental account selector

[AccountSelectorNativeProbe.dpr](AccountSelectorNativeProbe.dpr) exercises the
real experimental account-selection form, visual grid and confirmed-account
source. Its generated public keys and 20-account safebox live only in memory;
it creates no singleton node, wallet file or peer connection.

Build the source with Delphi/VCL and Lazarus/FPC using the experimental wallet's
source paths and compiler settings, plus this directory. FPC also needs the
matching LCL/LazUtils and embedded Generics.Collections paths. Keep compiler
outputs separate and place the matching OpenSSL runtime beside each EXE.
This is a focused test entry point, not another wallet project.

Run each built EXE through the existing
[private-desktop broker](../PrivateDesktopBroker.lpr), passing one absolute log
filename in a fresh QA directory:

```powershell
& $Broker $DelphiSelectorExe '<local-folder>'
& $Broker $FpcSelectorExe '<local-folder>'
```

The shared [checks](UAccountSelector.Checks.pas) cover ten groups:

- Full-range unsigned sale-price ordering, account-number ties and replacement
  searches through the actual core comparer, with arithmetic checks enabled.
- Inherited generic snapshot-cache invalidation across owner, empty-key and All
  configurations, including sorting and a warm paged request.
- Sorted key identities, indexed membership, missing/removed keys and explicit
  All selection. Reassigning the same `WalletKeys` object refreshes its snapshot;
  the selector adds no live wallet-event subscription.
- Name, balance and sale-filter combinations, complete totals and selection of
  a default account on a one-row page. A private offer from a nonwallet owner
  checks the established wallet-All/private-sale scope.
- Rapid key changes while a provider is held: the UI returns promptly and only
  the latest selected owner's rows may publish.
- Bounded snapshot retries, a nonspinning error state and successful retry.
- Background global type statistics with the real loading card and UI
  heartbeat. The actual modal report is shown and closed normally, then every
  line is compared with an independent implementation of the original report.
- Bounded type-statistics retries while the chain keeps changing.
- Closing during held statistics, retiring its worker and queued completion.
- Closing during a held grid read without leaving a worker or wallet handler.

Account reads are checked against the 128-record chunk bound. Held storage is
released by cooperative cancellation during close; the checks do not terminate
workers. Success writes `PASS complete experimental account selector checks`
and exits zero. An assertion failure writes `FAIL` and exits nonzero. Inspect
each compiler's own log before reporting execution coverage.
