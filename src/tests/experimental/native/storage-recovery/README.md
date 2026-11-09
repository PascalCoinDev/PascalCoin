# Checkpoint storage checks

These 43 checks test saving, recovering and admitting safebox checkpoints,
using small generated safeboxes.

`ExperimentalWalletTests.lpr` registers the checks. `CheckpointStorageProbe.lpr`
(FPC) and `CheckpointStorageProbe.dproj` (Delphi) also run them. Run all 43 with
a new output directory:

```powershell
.\src\tests\experimental\native\storage-recovery\Run-CheckpointStorageChecks.ps1 -OutputDirectory <local-folder>Qa\checkpoint-storage
```

`-FpcPath` selects another Windows x64 compiler. A passing log ends with
`PASS 43`.

For Delphi, place the OpenSSL runtime and
[`fixtures/mainnet-blocks-0-4.bin`](../../fixtures/README.md) beside the probe.
Set `TEMP` to a test folder and pass a result-log filename as the first
argument. An optional second argument selects one check by zero-based ordinal.

No check opens a wallet, starts networking or removes files outside its fixture
directory.

## Coverage

- injected temporary-file creation, short-write, flush, validation and
  replacement failures, which must leave the previous checkpoint and source
  safebox unchanged
- a Windows handle denying atomic replacement
- recovery from malformed and truncated candidates, legacy files without block
  bodies and bad main or checkpoint files
- failed live-file writes, with and without caching
- admission of checkpoints beyond local history or the restore limit
- progress and cancellation during legacy checkpoint inspection
