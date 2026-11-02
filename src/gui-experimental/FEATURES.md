# Classic and experimental wallet feature map

The experimental interface groups the production features of the classic wallet
into separate windows and focused wizards. This map identifies equivalent entry
points; it does not require the two interfaces to have identical layouts.

| Classic capability | Experimental entry point / behavior |
|---|---|
| Accounts, key selection, balance filters, refresh | Wallet account grid uses indexed account-ID prefixes and visible-record reads for default pages; arbitrary sort/search queries only the current key selection; **Tools → Accounts** provides the full explorer |
| Grid page modes | Shared Single / Auto / Manual selector: all matching records with scrolling, complete viewport rows, or a remembered explicit page size; switching mode starts at the first page |
| Whole-wallet balance and account totals | Overview totals use a separate cancellable worker with short lock-held chunks; selecting a grid key does not change the total's scope |
| Synchronization-driven grid updates | Node status suspends grid reads across download/application work and final pending reconciliation, then resumes the latest request once; ordinary selections and updates start immediately and keep loading feedback until the current page completes |
| Pending work during blockchain catch-up | Full confirmed-block validation continues while pending balances and new-operation admission wait; the existing background owner replays pending operations once after catch-up, including offline received-block backlog completion |
| Initial wallet totals | Total PASC and PASA show a dash until the first completed balance summary; later refreshes retain the last known totals |
| Overview layout | Both caption/value pairs are centered vertically inside the native group-box content area in Delphi and Lazarus, including scaling |
| Find an account by number/name and inspect details | Account explorer Find and quick search; account information menu |
| Recipient account selection | Paged confirmed accounts with indexed public-key filtering, explicit empty-key results and cancellable background searches |
| Account type statistics | Account selector Type Stats computes the global confirmed report in a cancellable worker, retaining the original report text and responsive controls |
| Next/previous higher balance | Account explorer Tools actions and balance sorting/filtering |
| Per-account operation history | Wallet operations grid and account explorer history; one pending-tree traversal for the selected accounts, worker-resolved wallet membership and responsive selection/depth settings during background fetching |
| Add/remove/all selected accounts and batch operations | Account explorer selected-account list; wallet multiple selection |
| Send PASC | Focused wizard respects native caption boundaries and provides custom fees and optional public/key/password payload steps; **Wallet → Advanced Operation** for advanced addressing/payloads |
| Change key/account name/type, sale/delist and purchase | Focused operation wizards; batch type changes require approval of skipped accounts and optional single-fee-payer selection on the custom-fee step; confirmation and fees cover only changes; advanced form |
| Multi-account key change | Wallet account popup opens the shared wizard; selected-account and confirmation grids are parented before loading in Delphi and Lazarus |
| Atomic account/coin swap setup and hash-lock helper | Advanced form and existing hash-lock dialog |
| Recipient-signed purchase, EPASA/named/pay-to-key addressing | Advanced form using the current classic operation implementation |
| Account-data editing; string/hex/Base58 payload input | Advanced form |
| RAW import/export, multioperation assembly/editing/signing/execution | **Tools → RAW / Multioperation Workbench**; returned partial copies of the same operation retain each party's missing signatures |
| Local/offline package signing | Same workbench; offline mode blocks RPC and submission handlers |
| Operation history and block range | **Tools → Operations Explorer** |
| Pending operations | **Tools → Operations** |
| Operation-hash search and payload decoding | History/pending-operation menus and payload decoder |
| Account/operation information and JSON | Account information memo, operation details and payload decoder |
| Block range, timing, target and hash-rate statistics | **Tools → Blocks**; Refresh/F5 and reset-range actions, persisted averaging and h/s–Eh/s units |
| Connections, known servers and blacklist | **Tools → Nodes** |
| Fixed peers / node IPs | **Wallet → Seed Nodes** |
| Peer messages, encryption, recipients and unread count | **Tools → Messages**, retained by the running application |
| Logs and debug-log filtering | **Tools → Logs**, controlled by Options |
| RPC request client | **Tools → RPC Calls** |
| Network/miner/RPC ports, allowed IPs, fee and checkpoint policy | **Wallet → Options** |
| Mining reward key selection | Existing eligible private-key entries are reused without startup/lookup generation or setting rewrites; no eligible key leaves mining inactive; configured NewEachTime rotation is limited to a mined block while unlocked |
| Generate/import/rename/delete private and watch-only keys | **Wallet → Private Keys** |
| Wallet lock/unlock, password, public/private export, backup/restore | Private Keys dialog and lock toolbar button |
| Language choice | Banner and Options; existing installed catalogs, applied on restart |
| Protocol, block, peer, mining and synchronization status | The banner shows exceptional node status in white, bold 16-point text aligned left after the logo/version, leaving normal online status blank; the Delphi synchronization details use buffered solid backgrounds to avoid blank erase frames, and keep detailed progress through traffic notifications; synchronization view and footer share cached committed counts/timestamps, and secondary statistics retain their last complete result while core work is busy |
| Existing-chain startup and saved pending operations | Cancellable background restore with separate decoding, validation, application and finalizing phases; bold white progress below the logo |
| Application and tray identity | PascalCoin icon assigned to the application, main window and system tray |
| Grid loading feedback | Shared LCL/VCL bordered overlay with Sphere10's rounded-spoke loading circle; retained rows and a disabled grid during node synchronization |
| Balance-cell rendering | Pending state is captured in the row snapshot; painting does not query safebox/mempool or acquire core locks |
| About and exit | About menu; Wallet Close and native window close |

The classic advanced operation form, RAW workbench, RPC client, grid helpers,
protocol operations and JSON serializers are reused. The experimental UI retains
its wizard flows, disposal/event/collection idioms and explicit component ownership.

The workbench executes the selected operation. A multioperation's full contents
and signatures are checked together by the core. Unselected drafts can remain
unsigned. Returned partial copies must describe the same unchanged operation;
conflicting bodies/counters are rejected without changing the current package.

Classic TESTNET-only diagnostics, including private-sale/swap search tools and
internal operation/memory tests, are conditional development tools and are not
promoted into the production menus. Unreferenced old experimental forms do not
represent live application features.

See [regression tests](../tests/experimental/README.md) for executed coverage.
Offline tests establish preparation, signing, package handling, presentation and
startup/shutdown behavior. They do not establish live network delivery.

The experimental sources retain Lazarus LCL and now provide conditional VCL APIs,
paired Delphi form resources and GNUgettext translation. Separate native checks
load all 48 form companions and exercise a real translated catalog. The shared
framework package and checked VCL grid/wizard probes pass; the classic wallet
compiles for Release Win64 with Delphi 13 Community Edition's IDE compiler.
The actual experimental VCL application also compiles for Win64 Debug and passes
separate fresh-data offline startup and native Close/Yes checks. Win32 runtime
coverage remains unverified.

Current Windows checks pass 525 FPCUnit tests, 56 native core/transport/history
cases in each actual compiler, and four actual sparse/dense main-form processes
(both Delphi and FPC). Fourteen formatter tests and 36 Delphi literal checks
cover legacy progress wording. These are controlled fixture and fresh-startup
checks, with no universal shutdown deadline or live-peer throughput claim.
