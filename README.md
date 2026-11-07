[![Build](https://github.com/PascalCoin/PascalCoin/actions/workflows/workflow.yml/badge.svg?branch=master)](https://github.com/PascalCoin/PascalCoin/actions)
[![Version](https://img.shields.io/github/v/tag/PascalCoin/PascalCoin?sort=semver&style=for-the-badge)](https://github.com/PascalCoin/PascalCoin/releases)
[![License](https://img.shields.io/github/license/PascalCoin/PascalCoin.svg?style=for-the-badge)](LICENSE)
[![Discord](https://img.shields.io/discord/383064643482025984.svg?label=DISCORD%20COMMUNITY&style=for-the-badge)](https://discord.gg/9YYZyw)
[![Reddit](https://img.shields.io/reddit/subreddit-subscribers/pascalcoin.svg?style=for-the-badge)](https://www.reddit.com/r/pascalcoin)

<p align="center"><img width="480" src="resources/logo.svg" alt="PascalCoin" /></p>

Copyright © 2016–2026 PascalCoin developers, based on original Albert Molina source code.

Please use at your own risk.

# PascalCoin - A One of a Kind Cryptocurrency

**PascalCoin is an instant, zero-fee, infinitely scalable, decentralized cryptocurrency with advanced privacy and smart contract capabilities. It is the world's first blockchain independent of historical operations.**

**PascalCoin possesses unlimited potential.**

PascalCoin is a peer-to-peer cryptocurrency written in Object Pascal. Its
**SafeBox** records the current state of every account, allowing nodes to verify
the ledger without retaining the complete history of operations. **PASC** is the
currency; **PASA** is an account with a number, owner key, balance and optional name,
type and data. The node supports payments, transferable accounts, encrypted
payloads, multioperations, atomic swaps, mining and a JSON-RPC API.

This repository contains the full node, two separate desktop wallets, a daemon
and a reference miner. PascalCoin's original implementation is by **Albert Molina**,
with significant contributions by **Herman Schoenfeld** (such as the Experimental
Wallet and core changes), alongside the work of the PascalCoin contributors.

![Experimental wallet 5.9 in Delphi: wallet totals, owned accounts and recent operations](docs/screenshots/5.9/experimental-delphi-wallet.webp)

*Experimental wallet 5.9, Delphi/VCL on Windows. Account and operation grids have
independent search and paging controls; the supplied image includes its original
balance redactions.*

## Downloads

[PascalCoin 5.9.3](https://github.com/PascalCoin/PascalCoin/releases/tag/5.9.3)
packages contain all four applications:

| Platform | Package |
|---|---|
| Windows x64 | [PascalCoin-5.9.3-x86_64-win64.zip](https://github.com/PascalCoin/PascalCoin/releases/download/5.9.3/PascalCoin-5.9.3-x86_64-win64.zip) |
| Linux x64 | [PascalCoin-5.9.3-x86_64-linux.tar.gz](https://github.com/PascalCoin/PascalCoin/releases/download/5.9.3/PascalCoin-5.9.3-x86_64-linux.tar.gz) |
| macOS Intel | [PascalCoin-5.9.3-x86_64-darwin.tar.gz](https://github.com/PascalCoin/PascalCoin/releases/download/5.9.3/PascalCoin-5.9.3-x86_64-darwin.tar.gz) |

Verify packages against the release's
[SHA256SUMS](https://github.com/PascalCoin/PascalCoin/releases/download/5.9.3/SHA256SUMS).
[All releases](https://github.com/PascalCoin/PascalCoin/releases) include earlier
mainnet and testnet builds; choose the intended network.

| Application | Executable in the package | Purpose |
|---|---|---|
| Classic wallet | `PascalCoinWalletLazarus` | Established wallet, miner and explorer interface |
| Experimental wallet | `PascalCoinWalletExperimental` | Separate interface with focused operation wizards and background, paged account/history views |
| Daemon | `pascalcoin_daemon` | Node and JSON-RPC service without a desktop interface |
| Reference miner | `PascalCoinMiner` | Standalone mining application |

Windows executables have the `.exe` suffix. Both GUI wallets are included and can
be kept side by side. Extract the complete package and retain `locale`, miner
resources, runtime libraries and license notices alongside the executables.
Windows and Linux packages bundle OpenSSL 1.1; macOS uses CryptoLib4Pascal.
Linux GUI builds require GTK2 and should be launched from the extracted folder.

## Read/write locking

Application guards use the core `TReadWriteLock` to allow multiple concurrent
readers or one exclusive writer. Read-only status and configuration access can
share ownership. File cursors, lazy caches, mutable lists and re-entrant callbacks
retain recursive write ownership. Lock names and bounded acquisition support
core diagnostics; low-level admission uses a separate primitive.

See [Build 5.9.3](CHANGELOG.md#build-593) for the patch changes. The screenshots
above show the 5.9 interface; the patch retains the same wallet applications.

## Wallet features

Both wallets use the PascalCoin node and operation validation. The experimental
interface brings the following features together; its
[feature map](src/gui-experimental/FEATURES.md) identifies the corresponding
classic capabilities and menu entry points.

| Area | Features |
|---|---|
| Accounts | Filter owned accounts by key, account number/name and balance; select one or many accounts and inspect account details/JSON. Independent **Single**, **Auto** and **Manual** page modes show all rows, fit the viewport or use a chosen page size. Overview totals always cover the whole wallet. **Get An Account** explains receiving a PASA; creating a key alone does not create an account. |
| History | View selected-account or whole-wallet operations, pending operations, operation details/hashes and decoded payloads. Open operation information with the **Details** button or right-click **Details**; selecting a row keeps the grid open. Recent Operations scans **2,016 blocks for 7 Days**, **8,640 for 30 Days**, or all available stored history for **Maximum**. Search, sort and page without repeating completed history queries. |
| Keys | Generate, import, rename and delete private or watch-only keys; copy/export public keys; import/export classic serialized, protected and legacy formats; back up and restore the wallet. Password protection and lock/unlock control signing. Private-key creation/import requires an unlocked wallet; watch-only keys cannot sign. |
| Payments | Send PASC with recipient selection, custom fees and optional public, key-encrypted or password-encrypted message payloads. Wizards retain values when navigating Back, validate before signing and require confirmation before submission. |
| Account management | Buy, list for public/private sale, delist, change ownership keys, and change account names/types. Batch signing validates cumulative fees and operation counters. Batch type changes ask approval for skipped accounts, charge only actual changes and offer an optional single fee payer. Buying can use the target account's existing balance toward the price. |
| Advanced operations | The classic advanced form remains available for account data, EPASA/named/pay-to-key addressing, recipient-signed purchase, account/coin swaps and hash locks. Payloads accept text, hexadecimal and Base58 input. |
| RAW workbench | Import/export RAW packages, assemble/edit/combine multioperations, sign locally or offline, and execute the selected operation. Returned partial copies of an unchanged multioperation merge missing signatures; conflicting bodies/counters are rejected. The core checks all required signatures before execution. |
| Explorers | Browse accounts with persistent multi-selection, balance filters and next/previous higher-balance actions; inspect account history and type statistics. Explore operation block ranges, pending operations and operation hashes. The block explorer shows timing, targets and hash-rate statistics with saved averaging windows and h/s–Eh/s units. |
| Network and tools | Inspect connections, known servers and blacklists; configure seed nodes, network/miner/RPC ports, allowed IPs, fees and checkpoint downloads. Use the RPC request client, encrypted peer messages with unread counts, and logs with debug filtering. Messages remain available while their window is closed. |
| Mining | Configure mining and the reward key. Eligible existing private-key entries are reused, including locked encrypted entries; no eligible key leaves mining inactive. The optional new-key policy rotates after a mined block while the wallet is unlocked and writable. |
| Languages | Choose installed language catalogs from the banner or Options and apply changes on restart. English and Dutch contain usable translations; untranslated labels retain their source text. |

Startup, account queries, history, totals and type statistics run on cancellable
background workers. Account painting uses cached visible rows. Data and status
refresh through node **events**, without data/status polling. During synchronization,
grids retain their rows and show a loading card; the latest selection/page request
resumes after received blocks and pending reconciliation finish. Footer and node
status share committed block counts and timestamps, while secondary statistics
retain their last complete results. Native Close cancels and joins owned workers;
pending operations are saved through an atomic file replacement.

Both wallets select the same configured blockchain reader. The default indexed
reader also accepts legacy `BlockChainStream.blocks` files; startup defers its
automatic index flushing during replay, as Classic does. Builds without
`USE_ABSTRACTMEM_BLOCKCHAIN_STORAGE` retain the raw-file reader.

See the [wallet guide](src/gui-experimental/README.md) and
[Build 5.9 changes](CHANGELOG.md#build-59---2026-10-28) for implementation details,
configuration and measured validation.

## Screenshots

The following are real Windows captures of the experimental wallet's Delphi/VCL
and Free Pascal/Lazarus interfaces. Click an image for its full resolution.

<details>
<summary>Startup, node status and a batch account-type change</summary>

| Startup and account setup | Wallet and operation review |
|---|---|
| [![Cancellable blockchain loading in the Delphi experimental wallet](docs/screenshots/5.9/experimental-delphi-startup.webp)](docs/screenshots/5.9/experimental-delphi-startup.webp)<br>Delphi: cancellable local blockchain loading. | [![FPC experimental wallet with pending operations](docs/screenshots/5.9/experimental-fpc-wallet.webp)](docs/screenshots/5.9/experimental-fpc-wallet.webp)<br>FPC: wallet accounts and pending operations. |
| [![Delphi experimental wallet active node status](docs/screenshots/5.9/experimental-delphi-node-status.webp)](docs/screenshots/5.9/experimental-delphi-node-status.webp)<br>Delphi: node status, chain statistics and mining information. | [![FPC batch operation confirmation](docs/screenshots/5.9/experimental-fpc-batch-confirmation.webp)](docs/screenshots/5.9/experimental-fpc-batch-confirmation.webp)<br>FPC: review account changes, signer and fees before execution. |
| [![Delphi batch Change Account Info account selection](docs/screenshots/5.9/experimental-delphi-batch-selection.webp)](docs/screenshots/5.9/experimental-delphi-batch-selection.webp)<br>Delphi: review 267 selected accounts in the account-information wizard. | [![FPC batch result for 267 account-information operations](docs/screenshots/5.9/experimental-fpc-batch-result.webp)](docs/screenshots/5.9/experimental-fpc-batch-result.webp)<br>FPC: scrollable results for 267 account-information operations. |
| [![Delphi account name and type change options](docs/screenshots/5.9/experimental-delphi-account-info.webp)](docs/screenshots/5.9/experimental-delphi-account-info.webp)<br>Delphi: enter name/type changes and choose fee/payload options. | [![Delphi batch custom fee and one fee payer](docs/screenshots/5.9/experimental-delphi-batch-fees.webp)](docs/screenshots/5.9/experimental-delphi-batch-fees.webp)<br>Delphi: custom per-operation fee, total fee and a single fee payer. |

</details>

[Screenshot provenance and checksums](docs/screenshots/README.md).

## Build

The source includes its Pascal library dependencies. Lazarus/FPC builds all four
applications on Windows, Linux and Intel macOS; the desktop wallets use LCL.
From the repository root, with `lazbuild` and FPC available:

```sh
lazbuild -B src/pascalcoin_daemon.lpi
lazbuild -B src/pascalcoin_miner.lpi
lazbuild -B src/pascalcoin_wallet_classic.lpi
lazbuild -B src/pascalcoin_wallet_experimental.lpi
```

Each command uses its project's default configuration. For the experimental
wallet, optionally add `--build-mode=Release` for an optimized build without debug
symbols, or `--build-mode=Debug` for debugging. Its `Default` mode is unoptimized.

On macOS, add `--ws=cocoa` to both wallet commands. Linux uses GTK2. Outputs are
under `src` with the executable names listed above. For a source build, place the
matching OpenSSL 1.1 runtime from `windows-dll` or `linux-libs` beside the programs;
retain `src/locale` and `src/pascalsha.cl` when distributing them. macOS selects
CryptoLib4Pascal and does not require OpenSSL. On macOS,
`bash build/macOS/package-macos.sh src dist` packages both wallets as
application bundles in a disk image; see [build/README.md](build/README.md).

For native Windows Delphi/VCL builds, open
[pascalcoin_wallet_classic.dproj](src/pascalcoin_wallet_classic.dproj) or
[pascalcoin_wallet_experimental.dproj](src/pascalcoin_wallet_experimental.dproj)
in the Delphi IDE. Both Windows projects compiled with Delphi 13 Community
Edition (classic Win64 Release and experimental Win64 Debug). The experimental
application passed isolated startup/close checks; the classic form has separate
controlled native checks. Win32 configurations exist but have not had the same
runtime verification. Delphi executables are
`src/pascalcoin_wallet_classic.exe` and `src/pascalcoin_wallet_experimental.exe`.
CI builds use FPC/Lazarus. See the
[detailed build guide](src/gui-experimental/README.md#build) for compiler options,
Debug/Release configurations and runtime layout.

For Delphi editions with command-line compilation, open the **RAD Studio Command
Prompt** and run from the repository root:

```bat
msbuild src\pascalcoin_wallet_classic.dproj /t:Build /p:Config=Release /p:Platform=Win64
msbuild src\pascalcoin_wallet_experimental.dproj /t:Build /p:Config=Release /p:Platform=Win64
```

Community Edition builds through the IDE. Its AutoBuild commands open the project
and build its selected configuration:

```bat
bds.exe -b src\pascalcoin_wallet_classic.dproj
bds.exe -b src\pascalcoin_wallet_experimental.dproj
```

Select the intended configuration and platform in the IDE; the verified targets
are listed above. See Embarcadero's [MSBuild guide](https://docwiki.embarcadero.com/RADStudio/Sydney/en/MSBuild)
and [IDE command options](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/IDE_Command_Line_Switches_and_Options).

The wallet incorporates
[Sphere 10 Pascal Framework](https://github.com/HermanSchoenfeld/PascalFramework)
for caching, concurrency, scoped resources, buffered streams, collections, paged grids and wizards.
Its imported sources are in `src/libraries/sphere10`.
Legacy checkpoint reads use `UStreams.TBufferedStream` with a fixed 64 KiB
read-ahead buffer and stream-length caching for files opened with writes denied.
The shared decorator's `EnableReadBuffering` and `EnableStreamLengthCaching`
flags default to `True`; `EnableBuffering`/`DisableBuffering` switch both. Disabling
read buffering frees the byte buffer and forwards reads/writes directly; length
caching works independently, including when read buffering is off. Unread bytes
must first be consumed or explicitly rewound with `FlushUnreadData` before read
buffering can be disabled. Checkpoint reads retain
both options because they read small fields from fixed files. Validation,
recovery selection and atomic publication remain unchanged.

Pending-operation loading uses 64 KiB read-ahead with length caching disabled.
The file-based blockchain backend combines the three serialized header fields
with a header-sized read buffer and keeps bulk block copies direct. The optional
external RandomHash table loader also uses 64 KiB read-ahead without length
caching, preserving its existing shared-file access. That loader remains skipped
when `ASSUME_VALID_POW_OLD_PROTOCOLS` is defined.

## Test

On Windows, build the experimental wallet and run the aggregate regression runner
from the repository root with FPC/Lazarus installed:

```powershell
& .\src\tests\experimental\RunAllWalletTests.ps1
```

Use `-FpcPath`, `-LazarusDirectory`, `-WalletExecutable` and a new
`-OutputDirectory` when needed. The runner executes the unit suite, isolated
controller integration, native grid/network/backlog checks, startup/shutdown,
download cancellation and SafeBox recovery. Tests use temporary keys/data and
private Windows desktops. Separate Delphi probes and optional large-chain
fixtures are described in the [test guide](src/tests/experimental/README.md).
Portable core/framework read-write-lock tests also run on Windows, Linux and
macOS in CI. Offline tests verify controlled behavior; live-peer throughput and
transaction delivery require separate network testing.

For an isolated experimental-wallet session, choose a new data folder:

```powershell
& .\src\PascalCoinWalletExperimental.exe --data-dir="<local-folder>" --offline --no-startup-dialogs
```

`--offline` disables peers, listening, RPC and mining for that process. Saved
network preferences are retained for the next normal run.

## Develop

See [Pascal Improvement Proposals](PIP/README.md), the
[changelog](CHANGELOG.md) and [project website](https://www.pascalcoin.org).
Community libraries include:

| Language | Library | Version support |
|---|---|---|
| JavaScript | [SBX](https://github.com/techworker/sbx) | 5 |
| Dart | [PascalDart](https://github.com/appditto/pascaldart) | 4 |
| Java | [JPascalCoin](https://github.com/davidbolet/JPascalCoin) | 4 |

## Community

- [Twitter](https://twitter.com/PascalCoin)
- [Discord](https://discord.gg/9YYZyw)
- [Reddit](https://www.reddit.com/r/pascalcoin)

## License

```
Copyright (c) 2016-2024 Pascal developers based on original Albert Molina source code
```

The source is distributed under the [MIT license](LICENSE). Please use at your
own risk.

This product includes software developed by the OpenSSL Project and Denis
Grinyuk ([OpenSSL-Delphi](https://github.com/Arvur/OpenSSL-Delphi)), and
cryptographic functions inspired by code written by Ladar Levison and Marco
Ferrante. [Included notices](licenses/README.md) preserve the licenses and
attributions for bundled libraries and runtimes.

## Donations

Support development with PASC to account **`0-10`**, or Bitcoin to
**`16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk`**.
