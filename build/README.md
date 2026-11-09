This folder holds platform-specific packaging files.

## macOS

[package-macos.sh](macOS/package-macos.sh) packages the Cocoa wallet builds as
`PascalCoin Wallet.app` and `PascalCoin Wallet Experimental.app`. It signs them
ad hoc and puts them in a disk image with an Applications link and
[Read Me.txt](macOS/Read%20Me.txt).

```sh
lazbuild -B --ws=cocoa src/pascalcoin_wallet_classic.lpi
lazbuild -B --ws=cocoa --build-mode=Release src/pascalcoin_wallet_experimental.lpi
bash build/macOS/package-macos.sh src dist
```

Bundles take their version from `CT_ClientAppVersion` in `src/core/UConst.pas`
and metadata from [Info.plist.in](macOS/Info.plist.in). The image is
`dist/PascalCoin-<version>-macOS.dmg`. If `codesign` or `hdiutil` is
missing, the script only assembles the bundles in `dist/stage`.

CI also cross-compiles both wallets for Apple silicon with
`lazbuild --compiler=<fpc driver> --cpu=aarch64`, merges them with `lipo` into
universal binaries and attaches the image to tagged releases as
`PascalCoin-<tag>-macOS.dmg`.

The bundles lack Developer ID signing and notarization, so macOS asks users to
confirm the first launch.
