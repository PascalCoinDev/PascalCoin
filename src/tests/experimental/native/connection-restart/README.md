# Connection restart regression

Run `ConnectionRestartProbe.lpr` (FPC) or `ConnectionRestartProbe.dpr` (Delphi)
with one new absolute result-log filename and the usual core/library search
paths. Put OpenSSL 1.1 beside the executable and point `TEMP`/`TMP` at an owned
test directory. Use the existing native private-desktop broker.

The two checks preserve automatic retry backoff, then prove that an explicit
OFF → ON transition retries a recently
attempted peer through the actual discovery worker, loopback socket and HELLO.
Only the worker's initial random jitter is seeded; its connection and retirement
logic runs unchanged. Each check owns fresh node/network singletons, data path
and loopback ports. It never loads a user's wallet or connects to public peers.

The first-pass assertion checks ordering: retry admission is reset before the
explicit enable calls discovery. The original server activation also resets
retry state later, so a failure of this assertion does not establish permanent
reconnection failure or measure a live public peer's latency.
