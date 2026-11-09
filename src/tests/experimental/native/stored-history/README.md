# Generated stored-history checks

These checks test history queries against generated stored blocks and live
pending operations. The shared [`UStoredHistoryChecks`](UStoredHistoryChecks.pas)
fixture runs under [FPCUnit](../../README.md#fpcunit-fixtures) and in `StoredHistoryProbe`. Its
fifteen groups cover signed 64-bit balances, reverse traversal, finite and
Maximum history, paging, tip-only and empty queries, pending rows, operation
hashes and layered storage.
[`ULivePendingHistoryChecks`](ULivePendingHistoryChecks.pas) checks the live
pending account index.

Build [`StoredHistoryProbe.dproj`](StoredHistoryProbe.dproj) in Delphi, or
`StoredHistoryProbe.lpr` with FPC range and overflow checks, from the current
product, core and library sources. Pass the probe one new absolute log path, and
point `TEMP` at a test folder. A passing log ends with `RESULT 0`. The fixtures
use new node directories and generated keys, and open no peers or wallet.
