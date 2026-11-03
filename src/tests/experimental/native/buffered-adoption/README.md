# Buffered input adoption checks

These eleven checks exercise the real pending-package and block-history storage
APIs with small owned files. They cover signed-operation round trips,
cancellation and retry, atomic pending replacement, out-of-order history reads,
save/delete/reopen, the 1,000-header boundary and read-only damaged files.
No peers, existing wallet data or whole-chain replay are used.

Pending cases cover both mainnet protocol 5 and supported protocol 6, using the
same test-only safebox upgrade methods used by other core fixtures. Public
genesis data seeds its bank; this does not change production protocol admission.

RandomHash cases compare canonical, malformed, truncated and mismatched table
inputs against the existing raw decoder. When
`ASSUME_VALID_POW_OLD_PROTOCOLS` is enabled, the public table entry point returns
before opening files; the test log identifies that shipping early-return path.
Exercising the table-entry branch requires a separate explicitly configured
test build, not a change to shipping configuration.

Run the Windows x64 FPC probe with a fresh output directory:

```powershell
./Run-BufferedAdoptionChecks.ps1 -FpcPath '<fpc>\bin\x86_64-win64\fpc.exe' -OutputDirectory '<local-folder>'
```

For native Delphi, open `BufferedAdoptionProbe.dproj`, supply an owned `TEMP`
directory and pass an output-log filename. Put `libcrypto-1_1-x64.dll` and
`tests/experimental/fixtures/mainnet-blocks-0-4.bin` beside the executable.
`UBufferedAdoptionTests` registers the same checks in FPCUnit.

The optional `perf` argument performs three bounded actual-history and
pending-load measurements. It requires `BUFFERED_ADOPTION_PENDING_SAMPLE` to
name one shared owned sample: it is generated once, then reused byte-for-byte
across compared builds. Input hashes and post-run byte/digest checks accompany
the timings. FPC timing builds define `BUFFERED_ADOPTION_PERFORMANCE` to exclude heap-tracing overhead; correctness builds retain leak checks. The five public early blocks and 128 pending operations are small
I/O fixtures, not representative full-node synchronization benchmarks.
