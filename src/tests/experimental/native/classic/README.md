# Classic GUI compatibility probe

This Windows probe runs the classic wallet form, `TFRMWallet`, on five mainnet
blocks. It checks:

- activation and local restore
- validated commits, committed-tip labels and block-grid rows
- key selection while idle and during catch-up
- native Close while an account filter is busy
- gettext cleanup of translated forms (Delphi only)

Run the Lazarus/FPC probe from the repository root:

```powershell
& src\tests\experimental\native\RunWalletRegressionTests.ps1 -Probe classic -OutputDirectory <new-directory>
```

`-FpcPath` and `-LazarusDirectory` select a Windows x64 FPC and Lazarus.

Alternatively, build `ClassicWalletProbe.lpi` with Lazarus (LCL) or
`ClassicWalletProbe.dproj` with Delphi (VCL). Build and run each compiler and
architecture separately.

1. Place the matching OpenSSL DLL beside the executable.
2. Set `PASCAL_CLASSIC_BLOCK_FIXTURE` to the full path of
   `src/tests/experimental/fixtures/mainnet-blocks-0-4.bin`, the
   [five-block fixture](../../fixtures/README.md).
3. Run the executable through the
   [private-desktop broker](../README.md#private-desktop-broker), passing one
   absolute log filename in a new folder.

Exit code 0 means all checks passed and the form closed normally. Captures and
the Lazarus heap report go beside the log.

The probe keeps its data in a new folder beside the log and never reads a user's
wallet or configuration. It disables outbound connections, RPC and mining, but
the classic wallet still listens on an ephemeral port.
