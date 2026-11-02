# Historical block fixture

`mainnet-blocks-0-4.bin` contains the five original PascalCoin mainnet blocks at
heights 0–4. It is public blockchain data, with no wallet or private-key data.
The regression feeds this already-received batch through the actual network
response processor and normal proof-of-work, transaction, bank and storage code.
It does not set `HasValidOperationBlockInfo` or replace validation.

The file is 1,630 bytes, SHA-256
`6A47E07B8D0B4696C9D223A6A48B6C449A32FEDE3A95A5E7A9F1B9D0885A8306`.
Its format is a little-endian `Cardinal` count (5), followed by five blocks saved
with `TPCOperationsComp.SaveBlockToStream(False, Stream)`.

| Height | Unix timestamp | Proof of work |
| --- | --- | --- |
| 0 | 1470934813 | `00000003A29C32E84A539ADE24397D41D30116A6FAFEC17B7D9CED68A4238C92` |
| 1 | 1470935312 | `00000009C81199431ACCC786CE5A1DDAA7381DC55261E56E096AA2B8389502F4` |
| 2 | 1470935390 | `0000000C2EAC295B4A182490E1C92C1F450ED99B2C64983ECD20850A0CE1C9FF` |
| 3 | 1470935433 | `0000000333BF4E90CEF1FD2472781DB946837F1F824CA86515E94C6DF6F90FE7` |
| 4 | 1470935652 | `00000001C3B233BA3E49B5BF6272B3A89C29A716A2790A5BF30778E3EF093CB2` |

All five blocks have zero operations. Their operation hash is the SHA-256 of an
empty operation stream,
`E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855`.

The headers were read through `TPCAbstractMem.GetBlockInfo` from a read-only
mainnet safebox. The source file was 629,440,512 bytes, SHA-256
`50F808D9BC382EF5127C9F8480388A43182AD43FF8929F84C26AA5C98FD7663F`;
its length and hash matched before and after extraction. Each retained header's
operation hash was checked against a real empty `TOperationsHashTree` before
serialization. This permits a small reproducible fixture without carrying a
pruned blockchain file or a full safebox in the test tree.
