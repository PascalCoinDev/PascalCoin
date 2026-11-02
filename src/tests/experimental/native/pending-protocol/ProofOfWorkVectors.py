"""Independent protocol-1 SHA256d vectors for the checked native regression.

Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
Run with Python 3; this script does not import or execute PascalCoin code.
"""

import hashlib
import json
import struct


def sha256d(data):
    return hashlib.sha256(hashlib.sha256(data).digest()).hexdigest().upper()


# Standard secp256k1 generator coordinates: a fixed, valid public key.
x = bytes.fromhex("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798")
y = bytes.fromhex("483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8")
key = struct.pack("<HH", 714, len(x)) + x + struct.pack("<H", len(y)) + y
part1 = struct.pack("<I", 42) + key + struct.pack("<QHHI", 500000, 1, 6, 0x19000000)
# The operations tree is empty; protocol 1 hashes only the fee's low four bytes.
part3 = bytes(range(32)) + hashlib.sha256(b"").digest() + struct.pack("<I", 7)
tail = struct.pack("<II", 1710000000, 123456)
expected = (
    "73E7793B06F15AC2B31520C315B3DC8C638BB34B2F2010E50976AE68F042C7B7",
    "3B274E21701CDD8721E209C76FCD079985D601F9630729F53970D75762E1CA53",
)
vectors = []
for payload, digest in zip((b"", b"pending-protocol-qa"), expected):
    serialized = part1 + payload + part3 + tail
    assert sha256d(serialized) == digest
    vectors.append({"payload_hex": payload.hex(), "input_hex": serialized.hex(), "sha256d": digest})
print(json.dumps(vectors, indent=2))
