# Logger GUI handoff checks

The real logger must release its producer queue before calling a GUI receiver.
These six checks cover a producer finishing while the receiver is still active,
FIFO and reentrant next-batch delivery, callback and batch-hook exceptions,
empty batches, unsubscribe cleanup, and legacy callbacks/file output.

`LogGuiHandoffProbe.lpr` and `.dpr` run the same assertions in FPC and Delphi.
Pass a new writable report path; exit code zero and `PASS 6` indicate success.
The probe starts no node, wallet or network connection. The bounded producer
check also exits normally when the old lock-held implementation fails.

`ULogGuiHandoffTests` registers these contracts in `ExperimentalWalletTests` and
the complete Windows test runner. Its detailed report uses an owned temporary
path. The shared assertions exercise `ULog` directly; they do not simulate
blockchain throughput or prove full-wallet dragging performance.
