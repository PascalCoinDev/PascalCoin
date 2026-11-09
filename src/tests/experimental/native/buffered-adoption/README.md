# Buffered input adoption checks

Eleven offline checks cover pending-package round trips, cancellation and atomic
replacement, block-history reads, the 1,000-header boundary, damaged inputs and
RandomHash table decoding.

Shipping builds define `ASSUME_VALID_POW_OLD_PROTOCOLS`, so
`TPascalCoinProtocol.AllowUseHardcodedRandomHashTable` returns early and the log
shows `RANDOMHASH_ENTRY=shipping-early-return`. Testing its table branch needs a
separate build.

Run on Windows x64 with a new output directory:

```powershell
./Run-BufferedAdoptionChecks.ps1 -OutputDirectory '<new-folder>'
```

For Delphi, build `BufferedAdoptionProbe.dproj`, set `TEMP` to a test folder and
pass an output-log filename. Put `libcrypto-1_1-x64.dll` and
`src/tests/experimental/fixtures/mainnet-blocks-0-4.bin` beside the executable.

For timing, define `BUFFERED_ADOPTION_PERFORMANCE` in FPC builds, set
`BUFFERED_ADOPTION_PENDING_SAMPLE` to one sample path for all compared builds
and run `BufferedAdoptionProbe <log> perf`.
