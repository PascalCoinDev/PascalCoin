# Logger GUI handoff checks

Six checks verify that the logger releases its producer queue before calling a
GUI receiver. They also cover FIFO and reentrant next-batch delivery, callback
and batch-hook exceptions, empty batches, unsubscribe cleanup, and legacy
callbacks and file output.

Build `LogGuiHandoffProbe.lpr` (FPC) or `LogGuiHandoffProbe.dpr` (Delphi) and
pass a new report path. Exit code zero and `PASS 6` mean success. The probe
starts no node, wallet or network connection.

`ULogGuiHandoffTests` registers the same checks in `ExperimentalWalletTests` and
the [complete Windows test runner](../../README.md), logging details to
`LogGuiHandoff-<GUID>.log` in `TEMP`.
