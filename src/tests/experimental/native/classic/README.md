# Classic GUI compatibility probe

This Windows probe constructs the production Classic `TFRMWallet` and its actual
DFM/LFM, local-load worker, node observers, account source and block-grid worker.
It selects a fresh data directory beside its new log before form construction,
seeds owned settings, two historic public watch keys and an empty public watch
key, then applies five historic blocks through `TNode.AddNewBlockChain` on an owned
worker. Each decoded block is copied into a container bound to the actual bank,
as required by the normal validation transaction. No validation flag is bypassed.
The empty watch key is derived from a temporary generated key which is freed
before startup. Only public keys are persisted; no user wallet/configuration is read.

The Classic shipping entry point does not implement the experimental
application's `--offline` or `--data-dir` options. The probe sets the folder before
creating the actual form and disables outbound connections. Classic activation
still starts its own listener on an ephemeral port, so the tested composition is
outbound-disabled rather than socket-free. Fixed discovery is restricted to
loopback, and RPC/mining are disabled in the owned settings.

Build `ClassicWalletProbe.lpi` with Lazarus/LCL or `ClassicWalletProbe.dproj` with
Delphi/VCL. The projects use repository-relative source paths and match the
Classic application's arithmetic settings. The independently checked core probes
cover the changed lock, transport and numeric contracts.

Run the executable on an owned non-input Windows desktop with one new absolute
log filename. Set `PASCAL_CLASSIC_BLOCK_FIXTURE` to
`src/tests/experimental/fixtures/mainnet-blocks-0-4.bin` and make the matching
OpenSSL DLL available beside the executable. Bind the fixture hash to the test
receipt. The probe rejects the input desktop and existing log/data directories.

The checks cover actual activation/local restore, five fully validated commits,
committed-tip labels, real block-grid rows and actual key-dropdown changes.
Selections cover two disjoint account sets, an empty key, all wallet keys and
rapid changes, both while idle and while catch-up keeps pending data unavailable.
Every phase checks account identities, actual cell records, balances and totals;
reconciliation must retain the latest selection. No extra mining key or
unexpected dialog/application exception is permitted. After loading
finishes at two blocks, only the test disables Classic's normal status timer so
the final three label checks exercise its existing node notifications. This does
not change or require an event-only Classic status policy. The activation worker
retains its production `FreeOnTerminate` policy; the probe observes completion
through `OnTerminate` and a duplicated Windows thread handle. Native Close runs
the actual form's shutdown flow with an account filter still busy. It has no
confirmation dialog.
Delphi runs also check that a translated form hooks its real `BeforeDestruction`
VMT slot, preventing freed controls from surviving in gettext's cleanup registry.

Build and run each required compiler/architecture separately; a passing Lazarus
or Win64 run does not establish Win32 behavior. The probe exits zero only after
its native checks and normal closure finish. Keep its source/executable hashes,
log, captures and heap/leak report with each run. This probe does not establish
live-peer synchronization or large-wallet throughput.
