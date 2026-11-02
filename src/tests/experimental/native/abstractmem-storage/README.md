# AbstractMem block-storage failure checks

These six offline checks exercise the optional block/operation index used by the
classic wallet. They use a new data directory, generated keys and actual indexing
workers. The experimental wallet continues to select `TFileStorage`.

Coverage includes a false completion flush, a worker's partial write racing a
previously admitted foreground flush, failed-queue retirement, suppression of
unsafe close retries, successful queue drain and actual node shutdown while an
in-flight worker reports its failure. Existing raw-block backup bytes must remain
unchanged after a failed index write; the incomplete index must retain its
unstable marker. Threads unwind and join cooperatively.

The general experimental test runner registers these checks through
`TAbstractMemStorageFailureTests`. The helper functions also support standalone
Delphi/FPC console probes: call `RunAbstractMemStorageFailureChecks(LogPath)` and
return its exit code. All logs and synthetic fixture directories are created next
to that supplied log path. Success ends with `PASS 6`.

The tests do not validate an existing user dataset or model arbitrary media damage
or power loss. The optional backend's existing startup policy rebuilds an invalid
derived index from separate raw storage; these checks stop before that recovery
and verify that failed cleanup has not certified or retried the partial index.
