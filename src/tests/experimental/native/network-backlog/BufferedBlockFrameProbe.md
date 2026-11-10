# Buffered block frame regression

`BufferedBlockFrameProbe.dproj` (Delphi) and `BufferedBlockFrameProbe.lpr` (FPC)
run the same 15 scenarios in `UBufferedBlockFrameChecks.pas`. Use the existing
native private-desktop broker, passing the probe executable and a new absolute
result-log path. Keep the existing OpenSSL 1.1 DLL beside the executable and set
`TEMP`/`TMP` to an owned empty test directory. The fixtures create fresh data and
use loopback sockets, generated state and the public historic blocks 0–4; they
do not open a user's wallet or connect to external peers.

The actual incoming and outgoing processing owners receive a GetBlocks response
for blocks 0–3 followed by an unsolicited full NewBlock for block 4. Tests prove
the second complete frame is locally received before switching networking off,
in both the transport buffer and the parser suffix. They check all five real
validated commits, exact reopened bytes and terminal storage height, physical
synchronization scopes, delivered status events, zero live-peer counters after
retirement, and no new request or reserved control callback during the drain.
They also require exactly one `blocks.log` update per accepted block, preserving
the original source, peer, payload, timestamps, age and target columns for both
catch-up batches and unsolicited announcements. Failed or unprocessed suffixes
must not produce accepted-block entries.
One informational record identifies the received batch's count, height range,
payload byte count and peer before validation or acceptance. This distinguishes
already received work from later per-block acceptance without logging each read.

Additional cases cover orderly EOF; Quit between atomic blocks; locally complete
zero-reference fast propagation; a missing-reference announcement without a new
fetch; malformed/full higher-protocol frames followed by a valid block; a first
buffered frame disconnected before parsing; and a genuine matching response
under a recursive legacy connection lock. The matching case exercises the actual
request/parser/processor/drain mechanism, not full orphan-chain selection.

A transport subclass gates the return from the real inherited `ReceiveBuf`
before publication. Receive-idle must wait for those returned bytes, then the
same transport object must receive again after a sequential reconnect. No
concurrent reconnect guarantee is inferred from that case.

The EOF fixture first proves exact received commits, persisted bytes and retired
physical scopes, then asks the network owner to switch off and waits for normal
owned retirement. It does not establish a passive EOF retirement deadline
or change the GUI's separate five-second shutdown assertion. Status evidence combines per-commit
physical counts, coalesced callbacks and the enclosing drain scope; it does not
claim instruction-level absence of every transient state under unrelated
simultaneous inputs. The incomplete suffix and missing fast references cannot be
completed offline and do not authorize another network read or request.
