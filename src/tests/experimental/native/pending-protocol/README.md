# Pending protocol availability regression

Run the FPC `.lpr` or Delphi `.dpr` with one new absolute result-log filename,
the usual core/library search paths, OpenSSL 1.1 beside the executable and an
owned `TEMP`/`TMP` directory. Use the existing private-desktop broker.
Copy `fixtures/mainnet-blocks-0-4.bin` beside the executable. The confirmed-bank
miner case validates its real genesis block. Two further cases retain a genuine
empty bank and a valid generated miner key, using nonempty and empty payloads.

The default project runs six groups using shipping core visibility: the three
miner availability cases, concurrent job retirement, notification-lease shutdown
and independent proof-of-work vectors. For all nine groups, first run
`NewPendingProtocolAccessCopy.ps1 -OutputDirectory <new-empty-directory>`, put its
returned core directory before the normal core search path, define
`PASCAL_PROTOCOL_INTERNAL_CHECKS` and use a fresh compiler output directory.
The script moves only three declarations from private to protected in a copy;
it verifies that all remaining source text is identical. It never changes the
shipping unit or its runtime method bodies.

The extended checks use real loopback transport, framing, production pending handlers,
matching-response logic and a real mining server. They cover legacy decoding of
zero available operations while the overlay is unavailable, the existing free
account refusal reply, errors matching both pending count and page requests,
and miners that stay connected through a busy request and receive a job when
the same node becomes available. The job-retirement check pauses an actual miner
request after it captures its work, then lets the real pool worker clear and
replace that job with a different payload. The paused response must retain its
original payload and digest parts without holding the pool lock through sending.
Another check transfers the existing connection
lifetime lease to the delayed notifier and closes that connection through normal
shutdown. No public peers, user wallet or user data are
opened. The pending wire test seeds a signed raw package entry to distinguish
the retained raw count from the number currently available; account admission
and consensus validation are exercised by the separate core fixtures.

The fixed SHA256d vectors cover empty and nonempty payloads with a fixed valid
secp256k1 public key, timestamp and nonce. Their expected digests were computed
independently using Python `struct` and `hashlib` from the documented protocol
fields; both cases also check the full protocol serializer against those hashes
and the cached serializer.
Run `python ProofOfWorkVectors.py` to reproduce the exact input bytes and hashes.
Use checked builds (`-Cr -Co` for FPC, range/overflow checking for Delphi) so
indexing an empty array cannot silently pass. FPC's heap report must show zero
unfreed blocks after normal process exit.
