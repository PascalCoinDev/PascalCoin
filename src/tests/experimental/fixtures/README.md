# Historical block fixtures

Each `.bin` file holds a little-endian `Cardinal` count (5), then five public
PascalCoin mainnet blocks saved with
`TPCOperationsComp.SaveBlockToStream(False, Stream)`.

`mainnet-blocks-0-4.bin` holds blocks 0–4, which have no operations. Regression
tests load it. The 1,630-byte file has SHA-256
`6A47E07B8D0B4696C9D223A6A48B6C449A32FEDE3A95A5E7A9F1B9D0885A8306`. Its headers
come from a mainnet safebox via `TPCAbstractMem.GetBlockInfo`.

| Height | Unix timestamp | Proof of work |
|---|---|---|
| 0 | 1470934813 | `00000003A29C32E84A539ADE24397D41D30116A6FAFEC17B7D9CED68A4238C92` |
| 1 | 1470935312 | `00000009C81199431ACCC786CE5A1DDAA7381DC55261E56E096AA2B8389502F4` |
| 2 | 1470935390 | `0000000C2EAC295B4A182490E1C92C1F450ED99B2C64983ECD20850A0CE1C9FF` |
| 3 | 1470935433 | `0000000333BF4E90CEF1FD2472781DB946837F1F824CA86515E94C6DF6F90FE7` |
| 4 | 1470935652 | `00000001C3B233BA3E49B5BF6272B3A89C29A716A2790A5BF30778E3EF093CB2` |

The [large backlog probe](../native/network-backlog/README.md) embeds
`mainnet-blocks-841529-841533.bin`, described in
`mainnet-blocks-841529-841533.json`.
