# Wallet grid refresh test

This runnable mock wallet uses the real main form, wallet control, typed source
factories and visual grids. Only account/history records are generated. Paging,
snapshot caches, the balance worker, native selection and background completion
remain production code. Accounts use `RefreshThrottle.Mode = temNone`. Operations
use the existing `temNotifyOnEventBurstFinished` mode with a 250 ms interval:
each request resets the quiet deadline, and only the latest request is fetched
after silence. The reusable throttle also has mirrored common grid tests.

The node synchronization lifecycle owns **Please Wait / Synchronizing** and
`RefreshSuspended`. Requests retain their latest parameters while paused, but
start no provider query. Completion releases the cards and resumes current data.
Already-running work retires cooperatively; pending inputs coalesce without
starting parallel workers or imposing a five-second quiet interval.

![Native Delphi demo holding both grids during mock synchronization](grid-refresh-waiting.png)

This is generated data, not a live wallet. Automatic runs disable the test buttons;
run without arguments to use them interactively. Transaction/detail popups remain
disabled in the mock. The isolated Tools route enables only its cancel-only wizard check.

## Open and run

- Lazarus: [WalletGridRefreshDemo.lpi](WalletGridRefreshDemo.lpi).
- Delphi: [WalletGridRefreshDemo.dproj](WalletGridRefreshDemo.dproj).

Normal runs create a fresh temporary wallet and force offline mode. Executables
sit beside the existing OpenSSL runtime in `src`: `WalletGridRefreshDemoFPC.exe`
and `WalletGridRefreshDemo.exe`. Compiler units stay in ignored test `lib` folders.
Projects have no build events.

## Interactive behavior

1. **Start sync (100 ms)** opens an actual node synchronization scope and emits
   controlled backend events. Both grids keep their rows/cards and perform no queries.
2. **Stop** ends that scope and immediately resumes the latest local data.
3. **Busy block batch (6.5 s)** demonstrates the same pause during silent received
   work, then resumes without waiting for an unrelated timer.
4. **Slow provider (7 seconds)**, **Gate next fetch** and **Release fetch** expose
   real loading and worker retirement. Inputs arriving during a fetch retain one
   latest replay; completion cannot start parallel providers.
5. **Cycle row counts** exercises empty, one-row, two-row and larger results.
   **Reset** stops the producer, releases work/gates and restores 200 records.

Counters distinguish actual page queries, complete history reads, visible account
reads and independent balance reads. Sorting, searching, paging and native account
clicks retain the production callbacks and source caches.

## Automated checks

Run the existing script from the repository root:

```powershell
& .\src\tests\experimental\RunWalletGridRefreshTests.ps1 -Demo
```

The automatic demo runs background wallet-key notifications, selection/loading with 250 ms operations admission, 20 consecutive native clicks,
idle backend refresh, held-worker coalescing, zero queries during node synchronization
and current revision/page publication after resume. It closes its owned controller
normally. The existing private-desktop broker keeps the user's input desktop intact;
`-OutputDirectory <new-directory>` selects a fresh evidence location.

Focused environment selectors use the same executable and actual wallet composition:

| Selector | Coverage |
| --- | --- |
| `PASCAL_WALLET_KEY_NOTIFICATIONS_ONLY=1` | A real worker invokes wallet-key notifications without UI pumping. Both native key combos remain untouched until their posted messages run; destroying receivers before delivery is safe. No keys or transactions are changed. |
| `PASCAL_WALLET_CONSECUTIVE_SELECTION_ONLY=1` | Twenty alternating account clicks, mostly 25 ms after publication, plus two spaced clicks and a six-click burst. Each independent click publishes once; the burst fetches only its latest selection after 250 ms silence. Loading clears on publication without stale callbacks. |
| `PASCAL_WALLET_SELECTION_COMPLETION_ONLY=1` | Two consecutive accounts with 268 generated operations each. No provider runs before 250 ms silence; held providers show loading, then clear it when current rows publish. |
| `PASCAL_WALLET_REFRESH_LIFECYCLE_ONLY=1` | Accounts admit immediately and operations after 250 ms silence; held providers retain one latest replay. Node synchronization permits zero queries despite backend/page inputs and resumes the latest revision/page. |
| `PASCAL_WALLET_SELECTION_ONLY=1` | Selection/loading plus initial overview dashes, retained completed totals, genuine zero, and native banner/event presentation. |

The consecutive observer counts every callback, including incorrect/stale
publications. It remains armed through inter-click spacing and a final grace period.
Logs separate input, admission, provider completion, publication and card hiding.
The one-second response bound applies to these fast generated providers, not to
arbitrary real history/storage workloads.

The separate [FPCUnit fixture](../../ui/UWalletGridIntegrationTests.pas),
[network-off probe](WalletNetworkOffProbe.dproj) and
[received-block backlog checks](../network-backlog/README.md) retain their own
entrypoints. The latter use actual core connectivity/received-work processing and
validate committed status against persisted public blocks. Generic five-second
throttle contracts live in the common/PascalFramework component fixtures; this
wallet demo no longer requires them for refresh admission.

Set `PASCAL_WALLET_SAFEBOX_LOG_ONLY=1` for actual structured safebox progress,
Info logging and node-status checks. Set `PASCAL_WALLET_SYNC_PAINT_ONLY=1` for
synchronization presentation. Existing Tools, Send, Change Info and operation-dialog
selectors below remain independent of these refresh scenarios.

## Tools, key-change wizard and Overview layout

Set `PASCAL_WALLET_TOOLS_ONLY=1` for
[UWalletToolsDialogChecks](UWalletToolsDialogChecks.pas) in the automatic demo.
It opens all ten actual Tools menu entries, including the modal RPC client and
RAW workbench and the embedded Synchronization view. RPC requires online mode,
so this case uses fresh settings restricted to the loopback endpoint
`127.0.0.1:1`; it never connects to public peers.

The case loads the public genesis fixture, selects two actual account rows and
opens Change All Key through the native popup. It checks the first wizard screen,
re-presentation and confirmation, then cancels before execution. Its destination
key is generated only in the isolated process; no transaction is signed or sent.
Overview checks compare the two label/value pairs with the native content area
at normal and 150% scale, saving captures for inspection. The same source runs
under Delphi/VCL and Lazarus/LCL.

Wizard checks also require visible native windows, populated columns and mode
selector, a header and both account rows, and painted text inside their cells.
Use the `*-native-grid.bmp` captures for those cells: recursive form/composite
printing on a private desktop can omit windowed descendants even when the actual
native controls are visible and paint correctly.

## Operation Details input and layout

Set `PASCAL_WALLET_TOOLS_ONLY=1` and `PASCAL_WALLET_OPERATION_INPUT_ONLY=1`
to exercise the actual Operations Explorer form and details dialog with
native window messages. Wheel, keyboard and scrollbar navigation must not open
details. A left press/release on a populated cell opens exactly that operation,
including when the row was already selected. Right clicks, header/blank clicks,
unpaired releases and drag gestures must not open a dialog. The fixture uses
read-only generated operation rows and closes its own observed modal dialogs.

The same group checks the main wallet separately: selecting an operation never
opens a dialog. The information button and right-click **Details** action each
request the selected hash once and retain selection. Invalid, missing and busy
rows disable the action. Native checks cover both wallet footers at 100%, 125%
and 150% scaling, including their intermediate parent panels.

[Operation-information layout checks](UWalletOperationInfoLayoutChecks.pas)
exercise the actual decoder with large block/operation counters, localized dates
and long monetary values. Captions must remain readable without overlap on first
show, after updates and across DPI changes; enlarging the form adds payload space.
The demo uses Release optimization (`-O2`) to cover the footer failure that less
optimized builds did not expose.

## Main-wallet history range

Set `PASCAL_WALLET_TOOLS_ONLY=1` and `PASCAL_WALLET_HISTORY_DEPTH_ONLY=1`
to exercise the actual Recent Operations picker and native account-row clicks.
The query fixture records the depth received by `FetchAll`: 2,016 for 7 Days,
8,640 for 30 Days and `High(Cardinal)` for Maximum. Every selection must retain
that depth and query the clicked account. These cases retain the production
selection/provider wiring, including the operations grid's 250 ms quiet interval.

The separate [stored-history checks](../stored-history/README.md) verify that
these query settings bound actual storage reads and retain pending rows.

## Block Explorer Tools

Set both `PASCAL_WALLET_TOOLS_ONLY=1` and
`PASCAL_WALLET_BLOCK_TOOLS_ONLY=1` to run only the Block Explorer menu case.
It opens the real Blocks form from the main menu, checks **Refresh (F5)** and
**Reset block filters**, and verifies the existing grid refreshes. Reset clears
both range edits and applied bounds, including when they are already empty;
hash-rate settings remain unchanged. The fixture uses disposable genesis data
and loopback-only settings. Its detailed log ends with
`PASS actual Block Explorer Tools refresh/reset checks`.

## Change Account Info presentation

Set `PASCAL_WALLET_CHANGE_INFO_UI_ONLY=1`, leaving the other focused selectors
unset, to run the Change Account Info case through the same automatic demo.
Use the fixture and runtime files described below for Send PASC. Single-account
and batch routes visit Selected Accounts and Details at 100% and 150% scale.
Assertions check native group-box content bounds, edit/font fit, overlap and
the populated selected-accounts grid. Batch name editing remains disabled.
The case saves eight native BMPs beside its detailed log and cancels without
Finish, signing or broadcast. Inspect each compiler's images and require
`PASS native Change Account Info presentation` with exit zero.

## Batch type-change approval and fees

Set `PASCAL_WALLET_CHANGE_INFO_APPROVAL_ONLY=1` in the automatic demo to
exercise the actual Change Account Info wizard. Six cases cover skipped-account
consent, all-unchanged rejection, Back/type changes, filtered confirmation, and
optional single-payer selection within the custom-fee page. Native checks run
at 100% and 150% font scale and save the displayed screens for inspection.
A generated, mature in-memory bank provides eligible fee payers; the fixture
never invokes Finish, signs or broadcasts.

The portable [batch preparation checks](../../operations/UChangeInfoBatchChecks.pas)
independently verify signatures, real core application, fees and counters for
changed targets, preserved names/data, and stale approval rejection. They run
through [FPCUnit](../../operations/UChangeInfoBatchTests.pas), or the
`RunChangeInfoBatchChecks(LogFile)` entry point under actual Delphi.

## Operation confirmation and results

Set `PASCAL_WALLET_OPERATION_MESSAGE_ONLY=1` to run the focused native dialog
checks. The case opens the production confirmation and result helpers with a
267-operation list and long lines, then verifies complete read-only text,
scrolling, visible summaries/buttons and bounded layouts at 100% and 150% scale.
The 13 cases include a 1024x720 allowed rectangle for the owned dialog at both
scales, without changing the desktop. Native outer bounds include the window
frame on both Delphi/VCL and Lazarus/LCL.
It checks No, Enter, Escape, title-bar close and explicit Yes on the dialog
helper; no operation is submitted or broadcast. Result windows close normally.
Native captures are written beside the test log for both compiler targets.

Set `PASCAL_WALLET_ADVANCED_UI_ONLY=1` for the Advanced Operation presentation
checks. They open the actual form through its explicit nil-node constructor,
inspect all six tabs at 100% and 150% font scale, and capture each tab. Assertions
cover native group-box content bounds, caption/font fit, sibling overlap, footer
buttons and the Delist tab's published-field binding. No wallet is loaded and
Execute is never invoked; these are presentation checks, not transaction tests.
Run the full presentation suite on a 1920x1080 or larger viewport. The fixed-size
Advanced Operation dialog has a 1140x975 client area at 150%, before its native
frame; a 1024x720 desktop cannot contain it. Geometry is logged before the
work-area containment assertion.

## Send PASC presentation

Set `PASCAL_WALLET_SEND_UI_ONLY=1` to run
[UWalletSendPresentationChecks](UWalletSendPresentationChecks.pas) through the
same automatic demo and private-desktop broker. Leave other focused selector
variables unset. For example, after building the chosen compiler target:

```powershell
$previous = $env:PASCAL_WALLET_SEND_UI_ONLY
$env:PASCAL_WALLET_SEND_UI_ONLY = '1'
try {
  & $Broker $WalletGridRefreshExe '<local-folder>'
} finally {
  $env:PASCAL_WALLET_SEND_UI_ONLY = $previous
}
```

Use a fresh output directory and copy
[mainnet-blocks-0-4.bin](../../fixtures/mainnet-blocks-0-4.bin) beside the probe
EXE, together with its matching OpenSSL runtime. This case keeps offline mode,
validates public genesis into disposable data and generates its own test key.
It calls the real Send dialog, Continue/validation and dynamic route methods,
then cancels at confirmation. It never invokes Finish, signs or broadcasts.

Six branches cover the default fee, a custom fee, and public, sender-key,
recipient-key and password-encrypted payloads, including fee/payload combinations.
They visit Selected Accounts, Details, Custom Fee, Encryption, Password, Content
and Confirmation as applicable. Send PASC deliberately does not inject the
separate fee-payer screen; the case checks that it remains absent.

At 100% and 150% font scale, assertions check native visibility, group-box
content bounds, caption/edit font fit, sibling overlap, tab order and populated
selected/confirmation grids. Native BMPs are saved beside the detailed
`<main-log-stem>.send-ui.log`, named by route, step and scale. Inspect those BMPs
and each compiler's log before reporting verification. Successful execution writes
`PASS native Send PASC presentation` and exits zero; a failed assertion exits
nonzero. Run both the actual Delphi/VCL and Lazarus/LCL targets.
