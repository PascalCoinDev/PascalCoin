# Native experimental wallet startup checks

For controlled synchronization bursts and real grid fetch counters, use the
[wallet grid refresh demo and dedicated fixture](grid-refresh/README.md). The
demo has Lazarus and Delphi projects and keeps the actual main-wallet wiring.
These startup checks exercise the application's separate startup/close paths.

The [account-selector probe](account-selector/README.md) covers key filtering,
paged confirmed accounts and background type statistics without creating a node.
The [Send PASC presentation case](grid-refresh/README.md#send-pasc-presentation)
uses the real wizard routes and captures every reachable step at 100% and 150%.
These focused checks have separate invocation and evidence from startup tests.

Run from the repository root on Windows with the locally installed FPC/Lazarus:

```powershell
& src/tests/experimental/native/RunWalletStartupNativeTests.ps1
```

Build the shipping wallet first. The script builds the real experimental wallet forms and startup controller into
`WalletStartupProbe.exe`, then launches it through `PrivateDesktopBroker.exe`.
All application windows belong to a new private Windows desktop. The broker never
switches the input desktop and has a 30-second watchdog for each child process.
The probe independently refuses the input desktop, online mode, an existing data
directory, or a data directory outside the OS temporary folder.

This is an FPC/LCL native probe. It does not compile the VCL/Delphi target, and
`-MDelphi` is not an Embarcadero compiler check. The installed Delphi 13 Community
Edition builds the classic wallet for Release Win64 through its IDE compiler.
Separate checked VCL grid/wizard probes, form-resource and GNUgettext checks pass.
The actual experimental Delphi project also builds for Win64 Debug and passes
independent offline fresh-data startup, responsiveness and normal close/exit
checks. Those use its VCL executable; this script continues to build and exercise
the FPC/LCL harness. The Delphi executable is
`src/pascalcoin_wallet_experimental.exe`, beside the existing OpenSSL DLLs and
locale catalogs. Its project has no pre/post-build commands or build-event trust
approval; compiler units/resources remain in isolated platform/configuration
folders.

Each harness process receives `--data-dir=<new temporary directory> --offline
--no-startup-dialogs`. It uses disposable settings, an empty blockchain, and its
own wallet file. It never loads the normal wallet or connects to peers. A generated
test key is added and removed only in the disposable wallet; it has no funds and
is never used for a transaction.

The `loaded` process checks:

- Empty local data completes startup and raises `Loaded` once.
- The main window is enabled, the splash clears, and wallet/synchronization
  screens remain native child windows in one main window. Its HWND stays stable;
  ordinary screen changes preserve its top-left position.
- Switching screens hides the inactive screen and preserves native bounds.
- Wallet and synchronization views each maximize and restore. The restored
  rectangle must match its saved normal bounds. At each size the native banner
  fills the header, its image covers the client area, the language selector keeps
  its right inset, and footer icons remain inside the bottom status bar.
- Footer buttons have matching native and control rectangles in lock/network/sync
  order, with enough height for the original icons. Clicks at each visible center
  dispatch to that button's original handler; synchronization switches the view
  without changing the offline network policy.
  Containment checks use the native status panel's themed content rectangle;
  the panel border and resize grip stay outside the controls at normal and
  enlarged DPI, during loading and after maximization/restoration.
- Wallet Keys retains visible padlock artwork for no password, unlocked and locked
  states, then reopens with the same image-list binding. The checks use only a
  disposable generated key and password in the test wallet. Actual button painting
  is checked through `WM_PAINT`/`LM_PAINT`, and a deterministic high-bit account-key
  hash checks signed-to-unsigned conversion under range checking.
- A disposable modeless dialog keeps activation and keyboard focus through real
  UI/backend callbacks and the wallet grid's delayed loading and error overlays.
- Enabled RPC/mining preferences and cached peers stay saved, while discovery,
  connections, peer listening, RPC and mining remain disabled in offline mode,
  including after the connectivity handler and settings changes.
- Adding/removing a generated key while wallet controls exist survives repeated
  wallet/synchronization transitions.
- Shutdown releases the forms, servers and node after joining the loader.
- The final shutdown uses the real Close command and clicks the actual Yes
  confirmation button. UI/backend callbacks must return during confirmation
  while an owned fixture holds the mempool. That borrowed-node fixture is joined
  before Yes permits node disposal; the separate slow-close check covers a
  canceled loader that still owns core locks during shutdown.
- Classic tool menus, checkpoint download settings, log visibility/capture,
  offline RPC restrictions and message history/unread/recipient updates are wired.
- With synchronization visible and another thread holding the mempool,
  event callbacks return with cached committed facts and the last complete
  secondary statistics. Secondary Bank reads do not retain a mempool read lock;
  an independent completion event publishes their results without a polling tick.
  The reverse-independence fixture gates the actual secondary snapshot worker
  and requires a real node writer to finish before that gate is released.
  A bounded fixture watchdog rejects blocking and joins the worker before cleanup. View switches
  display the cached snapshot immediately, including the values primed at load
  completion.

The `close-loading` process shuts down immediately after starting the local loader,
before its queued completion can run. The `close-before-load` process shuts down
from `AppStarted`, before the worker starts. Both require zero `Loaded` callbacks
and a fully released application graph.

The output directory contains build logs, the broker log and one assertion log per
process. Pass `-OutputDirectory <path>` to keep evidence at a specific location.
Data directories are left under the OS temporary directory for inspection.

The `slow-close` process supplies a controlled `TWalletStartupTask` which waits
for cancellation. It checks that the native main window stays enabled while
wallet commands are disabled. The task holds the existing safebox and mempool
locks while UI refresh/message notifications and settings saves run; these must
avoid querying partially loaded core state. It posts that process's native Close system
command, and requires cancellation/joined cleanup without a `Loaded` callback.
Before closing, the loading host permits width changes but holds its height fixed
and disables native maximize while its loader remains blocked. Native track-size
constraints, header/background, language and footer containment are checked,
together with the large white transparent status text. Successful initialization
restores the original height constraints and maximize icon before the loaded-view
maximize/restore checks.
This covers input responsiveness during a long restore; directly calling the
controller's shutdown method alone does not establish that a user can close it.

The script also launches the actual shipping executable twice, using fresh
temporary data and a private desktop: `actual-first-run` keeps startup dialogs
enabled, and `actual-no-dialogs` supplies the optional suppression flag. These
checks require a visible enabled main window, reject an automatic blocking About
dialog, and close through the real native window and exit-confirmation handlers.
Use `-WalletExecutable <path>` to select another built target. The native broker
never targets a process other than the child it creates.

The regular script uses empty-chain data. Large-dataset checks must also restore
a separate copy of the public chain and saved pending buffer, with a public key
that owns many accounts in that copy. Require the measured selection to cover the
reported scale (at least 326,387 owned accounts for this regression) and produce
nonzero history rows from the roughly 185,000 pending operations. A key with no
matching accounts or no relevant pending operations misses the expensive path.
Check completed account/history fetches, UI
heartbeats, repeated wallet/synchronization transitions, native icon handles and
the white status label below the logo. Preserve the source dataset and compare
the copied pending buffer's hash before and after the run. Do not import the
normal wallet's private keys.

For update performance, measure cold loading, warm paging and an actual backend
notification burst separately. Count underlying `FetchAll` calls as well as grid
completions: a fast cached page does not prove that backend invalidation is cheap.
For the native account page path, count full account-record reads as well as
per-key ID prefixes; only visible records should be loaded for default pages.
Exercise a selected-key arbitrary search/sort separately from Show All.

Verify an immediate eligible leading update and a trailing update only after five
quiet seconds. New backend events restart that quiet period. Busy/hidden grids
retain one pending update; there is no additional post-fetch cooldown. Node
synchronization separately suspends admission until real work drains. Network-off
must leave received buffers processing and the card active, then resume once.
Polling must not create refresh events. Check global Overview totals while changing the grid key filter,
and close while the cancellable aggregate worker is between chunks. Record
working/private memory before and after at the same checkpoints.

Native presentation checks must keep rows visible beneath the compact bordered
card and observe the rounded-spoke animation. The automatic fetch card has a
200 ms display delay and clears when that fetch ends; quick cached pages must
avoid flashing it. The manual node-synchronization card is immediate and stays
held until physical work drains, independently of the throttle or page-worker
completion. Require card/animation cleanup after its own hold ends or the control
is destroyed.

The broker is adapted from PascalFramework's native wizard test infrastructure;
its MIT license is included in this directory.

Current dual-compiler acceptance totals 56 distinct cases across the
[core/lifetime](network-lifetime/README.md),
[buffered-frame](network-backlog/BufferedBlockFrameProbe.md),
[Cardinal ordering](ordered-cardinal/README.md) and
[stored-history](stored-history/README.md) helpers. Actual sparse/dense main-form
checks also pass four child processes, one of each dataset in each compiler;
these keep the inherited providers, received-block processing and committed
footer/status sources intact. Fourteen formatter cases and 36 Delphi literal
checks cover progress text. The full FPCUnit suite separately passes 525 cases;
none of these figures implies Win32 runtime or live-peer throughput coverage.

The [Classic compatibility probe](classic/README.md) separately verifies the
actual Classic form's local loader, five validated blocks, grids/accounts and
native Close on FPC/LCL and Delphi/VCL. Both Classic shipping projects also
compile. Active-downloader caption/error-path runtime coverage is excluded;
exact formatter/literal checks and caller review remain separate evidence.
