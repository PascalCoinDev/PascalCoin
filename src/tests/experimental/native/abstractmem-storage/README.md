# AbstractMem block-storage failure checks

Six offline checks test failure handling in the optional AbstractMem block and
operation index. They cover false completion flushes, partial writes racing a
flush, shutdown during a worker failure, failed-queue retirement, suppression of
unsafe close retries and a successful queue drain. They use a new data directory
and generated keys.

The [FPCUnit fixtures](../../README.md#fpcunit-fixtures) register them through
`TAbstractMemStorageFailureTests`. For a standalone Delphi or FPC probe, call
`RunAbstractMemStorageFailureChecks(LogPath)` and return its exit code. Logs and
fixtures go beside `LogPath`. A passing log ends with `PASS 6`.
