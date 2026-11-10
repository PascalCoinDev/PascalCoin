#!/usr/bin/env bash
# Packages the Cocoa wallets as signed macOS application bundles inside a DMG.
#
# Usage: build/macOS/package-macos.sh <binary-folder> <output-folder> [dmg-name]
#
# <binary-folder> holds PascalCoinWalletLazarus and PascalCoinWalletExperimental
# built with lazbuild --ws=cocoa; CI merges Intel and Apple silicon builds into
# universal binaries with lipo first. The DMG is named <dmg-name>.dmg, by default
# PascalCoin-<version>-macOS, where <version> is CT_ClientAppVersion.
# On a system without codesign or hdiutil the bundles are only staged, so the
# layout can be inspected anywhere.
set -euo pipefail

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "Usage: $0 <binary-folder> <output-folder> [dmg-name]" >&2
  exit 2
fi

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
bin="$(cd "$1" && pwd)"
mkdir -p "$2"
out="$(cd "$2" && pwd)"

version="$(sed -n "s/.*CT_ClientAppVersion.*PRODUCTION}'\([0-9][0-9.]*\)'.*/\1/p" "$repo/src/core/UConst.pas")"
if [[ -z "$version" ]]; then
  echo "Cannot read CT_ClientAppVersion from src/core/UConst.pas" >&2
  exit 1
fi
name="${3:-PascalCoin-$version-macOS}"
stage="$out/stage"
rm -rf "$stage"
mkdir -p "$stage"

# Builds one application bundle. Translations and licences live in Resources;
# the wallets look for locale beside their executable, so Contents/MacOS links
# to it rather than holding data that code signing would treat as code.
make_app() {
  local executable="$1" title="$2" identifier="$3"
  local contents="$stage/$title.app/Contents"
  if [[ ! -f "$bin/$executable" ]]; then
    echo "Missing $bin/$executable" >&2
    exit 1
  fi
  mkdir -p "$contents/MacOS" "$contents/Resources"
  cp "$bin/$executable" "$contents/MacOS/$executable"
  chmod 755 "$contents/MacOS/$executable"
  sed -e "s|@EXECUTABLE@|$executable|g" -e "s|@NAME@|$title|g" \
    -e "s|@IDENTIFIER@|$identifier|g" -e "s|@VERSION@|$version|g" \
    "$here/Info.plist.in" > "$contents/Info.plist"
  printf 'APPLPasc' > "$contents/PkgInfo"
  cp "$here/Logo.icns" "$contents/Resources/Logo.icns"
  cp -R "$repo/src/locale" "$contents/Resources/locale"
  cp -R "$repo/licenses" "$contents/Resources/licenses"
  cp "$repo/LICENSE" "$contents/Resources/LICENSE"
  ln -s ../Resources/locale "$contents/MacOS/locale"
}

make_app PascalCoinWalletLazarus "PascalCoin Wallet" org.pascalcoin.PascalCoinWallet
make_app PascalCoinWalletExperimental "PascalCoin Wallet Experimental" org.pascalcoin.PascalCoinWalletExperimental
cp "$here/Read Me.txt" "$stage/Read Me.txt"

if ! command -v codesign > /dev/null || ! command -v hdiutil > /dev/null; then
  echo "codesign or hdiutil is unavailable; bundles are staged in $stage without a DMG."
  exit 0
fi

for app in "$stage"/*.app; do
  executable="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleExecutable' "$app/Contents/Info.plist")"
  echo "== $(basename "$app")"
  lipo -archs "$app/Contents/MacOS/$executable"
  vtool -show-build "$app/Contents/MacOS/$executable" 2> /dev/null | grep -E 'minos|version' || true
  plutil -lint "$app/Contents/Info.plist"
  # Extended attributes make codesign reject a bundle. Ad-hoc signing seals the
  # bundle; it does not replace Developer ID signing and notarisation.
  xattr -cr "$app"
  codesign --force --sign - "$app"
  codesign --verify --strict --verbose=2 "$app"
done

ln -s /Applications "$stage/Applications"
dmg="$out/$name.dmg"
rm -f "$dmg"
# hdiutil occasionally reports a busy resource on hosted runners; retry it.
for attempt in 1 2 3; do
  if hdiutil create -volname "PascalCoin $version" -srcfolder "$stage" \
      -fs HFS+ -format UDZO -ov "$dmg"; then
    break
  fi
  if [[ $attempt -eq 3 ]]; then
    exit 1
  fi
  sleep 5
done
hdiutil verify "$dmg"

# Check the image as a user receives it.
mount="$(mktemp -d)"
hdiutil attach -nobrowse -readonly -mountpoint "$mount" "$dmg" > /dev/null
trap 'hdiutil detach "$mount" > /dev/null || true' EXIT
ls -la "$mount"
for app in "$mount"/*.app; do
  codesign --verify --strict --verbose=2 "$app"
done
hdiutil detach "$mount" > /dev/null
trap - EXIT
rm -rf "$stage"
echo "Created $dmg"
