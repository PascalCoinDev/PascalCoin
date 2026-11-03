# Checkpoint storage checks

These checks exercise real safebox serialization and file publication using
small generated safeboxes. They inject temporary-file creation, short-write,
flush, validation and replacement failures. The previous checkpoint and live
source must remain unchanged, unpublished temporary files must be removed, and
a retry must reopen correctly. A separate Windows check uses an actual handle
that denies atomic replacement.

Recovery cases cover malformed and truncated candidates, a healthy main safebox
beside a bad checkpoint, a bad main safebox with a valid checkpoint, all-invalid
files, and replay of the committed five-block mainnet fixture after recovery.
Two cases retain a newer legacy file's valid header/footer while removing its
block bodies: recovery must retain a healthy live safebox or use an older valid
legacy file. Rejected candidates and intact recovery sources remain unchanged.
Cached and uncached live-file cases require a failed partial write to retain an
unstable physical header and a successful retry to persist all dirty bytes.
Exceptional cleanup must release normally after the write was reported. A first
failure discovered by automatic cached close must reach the existing log hook.
An initial unstable-header write failure must be retried before a different
payload reaches storage, both with caching enabled and disabled; normal close
must then leave a valid file containing both repaired payloads.

Fourteen admission cases distinguish a valid checkpoint beyond the available
local history or explicit restore limit from unreadable storage. Ahead legacy
and AM checkpoints are logged and left intact while replay restores the exact
eligible block count, tip and safebox hash. An eligible legacy fallback still
receives secure inspection and loading. Unreadable headers/bodies, corrupt main
files beside ahead checkpoints, and an ahead-only live AM file remain protected
failures. An unused ahead legacy body is deliberately not scanned.

The checks are registered in `ExperimentalWalletTests.lpr`. The same check body
also runs in `CheckpointStorageProbe.lpr` and `CheckpointStorageProbe.dproj` for
focused FPC and Delphi verification. The runner executes all 43 storage checks,
including progress and cancellation during legacy checkpoint inspection.
Use a new output directory:

```powershell
.\src\tests\experimental\native\storage-recovery\Run-CheckpointStorageChecks.ps1 -OutputDirectory <local-folder>Qa\checkpoint-storage
```

For Delphi, place the existing OpenSSL runtime and committed
`fixtures/mainnet-blocks-0-4.bin` beside the probe executable. Supply an owned
`TEMP` directory and a result-log filename as its first argument. No check opens
an existing wallet, starts networking or removes files outside its own fresh
fixture directory. This tests failed-I/O recovery; it does not claim to simulate
arbitrary physical media damage or power-loss durability.

The probe accepts an optional second argument with a check's zero-based ordinal
for diagnosis. Normal runners execute every check. The probe also assigns a
fresh process-wide node directory before any fixture starts; an unexpected
singleton is reported and retired before that owned directory is restored.
