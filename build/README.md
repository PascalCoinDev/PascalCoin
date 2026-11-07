This folder contains platform specific build configurations.

## macOS

[package-macos.sh](macOS/package-macos.sh) turns the Cocoa wallet builds into
`PascalCoin Wallet.app` and `PascalCoin Wallet Experimental.app`, signs them
ad hoc and places them, an Applications link and [Read Me.txt](macOS/Read%20Me.txt)
in a compressed disk image. CI runs it on the macOS job and attaches the image
to tagged releases as `PascalCoin-<tag>-x86_64-darwin.dmg`.

```sh
lazbuild -B --ws=cocoa src/pascalcoin_wallet_classic.lpi
lazbuild -B --ws=cocoa --build-mode=Release src/pascalcoin_wallet_experimental.lpi
bash build/macOS/package-macos.sh src dist
```

Each bundle takes its version from `CT_ClientAppVersion` in `src/core/UConst.pas`
and its metadata from [Info.plist.in](macOS/Info.plist.in). Translations and
license notices are in `Contents/Resources`; `Contents/MacOS/locale` links to the
translations because both wallets look for them beside the executable.

The bundles are not signed with a Developer ID or notarised, so macOS asks the
user to confirm the first start. Notarisation needs an Apple Developer account
and signing secrets in CI.
