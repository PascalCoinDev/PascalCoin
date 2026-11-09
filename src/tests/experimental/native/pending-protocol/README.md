# Pending protocol availability regression

Checks the peer protocol and mining server while pending operations are
unavailable. It never contacts public peers or opens a user's wallet or data.

Build `PendingProtocolProbe.lpr` (FPC) or `PendingProtocolProbe.dpr` (Delphi)
with the usual core and library search paths. Enable range and overflow
checking (`-Cr -Co` for FPC). Put OpenSSL 1.1 and
[mainnet-blocks-0-4.bin](../../fixtures/mainnet-blocks-0-4.bin) beside the
executable, and point `TEMP` and `TMP` at a test folder. Run it through the
[private-desktop broker](../README.md#private-desktop-broker) with a new absolute
result-log path. FPC's heap report must show zero unfreed blocks.

The default build runs six cases:

- three miner availability cases
- concurrent job retirement
- notification-lease shutdown
- proof-of-work vectors, reproducible with `python ProofOfWorkVectors.py`

Three more cover legacy decoding of zero available operations with the
free-account refusal reply, and error replies to pending count and page
requests. To run all nine:

1. Run `NewPendingProtocolAccessCopy.ps1 -OutputDirectory <new-directory>`.
2. Put its returned core directory before the normal core search path.
3. Define `PASCAL_PROTOCOL_INTERNAL_CHECKS`.
4. Use a new compiler output directory.

`../RunWalletRegressionTests.ps1 -Probe pending-protocol` does all of this,
requires all nine cases and recomputes the vectors with Python.
