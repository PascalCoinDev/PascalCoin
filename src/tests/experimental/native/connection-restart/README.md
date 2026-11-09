# Connection restart regression

This probe checks that switching networking off and on retries a recently tried
peer at once, while automatic retries keep their backoff.

Build `ConnectionRestartProbe.lpr` (FPC) or `ConnectionRestartProbe.dpr`
(Delphi) with the usual core and library search paths. Put OpenSSL 1.1 beside
the executable and point `TEMP` and `TMP` at a test folder. Run the probe
through the [private-desktop broker](../README.md#private-desktop-broker) with a new
absolute result-log path.

The probe uses its own data folder and loopback ports. It never opens a user's
wallet or contacts public peers.
