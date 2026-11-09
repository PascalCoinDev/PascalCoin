# Native component checks

These Delphi probes and shared checks test Sphere 10 Pascal Framework
components. Each probe is mirrored in standalone PascalFramework. No probe
needs wallet data, network access or OpenSSL.

## Build and run

Build each probe's `.dproj` in Delphi for Win64 Debug. Run `ScopeProbe` and
`GridLoadingProbe` from Delphi, and the others through the
[private-desktop broker](../README.md#private-desktop-broker), which never switches the input desktop.

For `NativeThrottleProbe`, run these commands from the repository root with
Free Pascal on PATH:

```powershell
New-Item src\tests\experimental\lib\native-throttle\broker,<new-folder> -ItemType Directory -Force
fpc -FEsrc\tests\experimental\lib\native-throttle\broker -FUsrc\tests\experimental\lib\native-throttle\broker src\tests\experimental\native\PrivateDesktopBroker.lpr
.\src\tests\experimental\lib\native-throttle\broker\PrivateDesktopBroker.exe `
  .\src\tests\experimental\lib\native-throttle\Win64\Debug\bin\NativeThrottleProbe.exe `
  <new-folder>\trace.log
```

For other broker runs, substitute the probe's EXE and log path. Pass a log path
in a test folder, or point `TEMP` and `TMP` at one.

| Probe | Arguments | Report | Exit codes |
|---|---|---|---|
| `ScopeProbe` | — | PASS and FAIL lines on standard output | 0 pass, 1 failure |
| `GridLoadingProbe` | Optional log path | Log, default `%TEMP%\GridLoadingProbe-<PID>.log`, with bitmap captures beside it | 0 pass, 1 assertion failure, 2 initialization failure, 64 extra argument |
| `NativeThrottleProbe` | Trace-file path | Trace, with `result.json` beside it | Nonzero on failure |
| `DataRowVariantProbe`, `GridThrottleProbe` | Optional log path | PASS and FAIL log, default `%TEMP%\<ProbeName>-<PID>.log` | 0 pass, 1 assertion or runtime failure, 64 extra argument |
| `ReadWriteLockProbe` | Log path | PASS and FAIL log | 0 pass, 1 failure, 64 missing or extra argument |

## Managed scopes and disposal bags

[ScopeProbe.dproj](ScopeProbe.dproj) runs the same 63 scope, disposal-bag and
lock-scope assertions as the FPCUnit fixtures. Use Delphi 13, Win64 Debug or
Release. Delphi reports leaks at shutdown. FPC can run with heap tracing
(`-gh`).

The shared checks are in [UScope.Checks](../../framework/UScope.Checks.pas),
[UDisposables.Scope.Checks](../../../UDisposables.Scope.Checks.pas) and
[UReadWriteScope.Checks](../../framework/UReadWriteScope.Checks.pas).

## Read-write locks

[ReadWriteLockProbe.dproj](ReadWriteLockProbe.dproj) checks the `TReadWriteLock`
contract in seven groups, including recursion, promotion, timeouts and
wrong-thread release. It builds into
`src/tests/experimental/lib/native-read-write/<Platform>/<Config>/bin`.

## Custom loading cards

[GridLoadingProbe.dproj](GridLoadingProbe.dproj) is a VCL application that
checks custom grid loading cards, including artwork, input gating, error
precedence and focus in modeless windows. It builds into
`src/tests/experimental/lib/Delphi/<Platform>/grid-loading/<Config>`.

[UVisualGrid.Refresh.Tests](../../framework/UVisualGrid.Refresh.Tests.pas)
covers the same API in Lazarus.

## Five-second throttle

[NativeThrottleProbe.dproj](NativeThrottleProbe.dproj) exercises
`TThrottledEvent` and its VCL timer. Its [source](NativeThrottleProbe.dpr) runs
ten scenarios at once in about 24 seconds. Input every 100 ms or every second
must give one leading call, then one final call after five quiet seconds. Other
scenarios cover finished-only and periodic modes, and recovery after a callback
exception, cancellation or mode change.

FPCUnit timer regressions are in each library's `UCommon.UI.Tests.pas`.

## Data-row Variant compatibility

[DataRowVariantProbe.dproj](DataRowVariantProbe.dproj) checks positional
data-row access and native text in Delphi. Its
[source](DataRowVariantProbe.dpr) covers `Integer(Row._(0))`, row copies,
synchronous `TVisualGrid.Cells[x,y]` and Unicode text. It builds into
`src/tests/experimental/lib/Delphi/<Platform>/data-row-variant/<Config>`.

FPC 3.2.0 and 3.2.2 dispatch regressions are in the `UCommon.Data.Tests`
fixture.

## Per-grid request admission

[GridThrottleProbe.dproj](GridThrottleProbe.dproj) checks that each VCL grid
admits requests through its own five-second timer and that the latest
parameters win. A full run of its [source](GridThrottleProbe.dpr) covers eight
groups. Set the selector beside a group to run only that group:

- Refresh suspension (`PASCAL_GRID_SUSPENSION_ONLY=1`)
- Parentless and hidden populated grids
- Mixed inputs and independent timers
- Busy single and latest requests
- Held result and completion input
- Cancel, mode and disposal
- Completion without timer dispatch (`PASCAL_GRID_COMPLETION_ONLY=1`)
- Cancellation before execution (`PASCAL_GRID_EARLY_CANCEL_ONLY=1`)

`PASCAL_GRID_LOADING_ONLY=1` runs four queued-loading checks that the full run
omits. Unset selectors for a full run. Selectors also work through the broker.
For example, run only cancellation before execution directly from the
repository root:

```powershell
$env:PASCAL_GRID_EARLY_CANCEL_ONLY = '1'
.\src\tests\experimental\lib\Delphi\Win64\grid-throttle\Debug\GridThrottleProbe.exe <new-folder>\GridEarlyCancel.log
```

The probe builds into
`src/tests/experimental/lib/Delphi/<Platform>/grid-throttle/<Config>`.

[`RefreshSuspended`](../../../../libraries/sphere10/UVisualGrid.pas) pauses a
grid's request admission. Use it on the UI thread. While suspended, the grid
keeps the latest request, asks active work to cancel and ignores its results and
errors. To resume at once, cancel `RefreshThrottle` while still suspended, then
clear `RefreshSuspended`. One latest query starts after any canceled worker
retires.

## Shared grid checks

- `UVisualGrid.Paging.Checks` tests all six paging-mode changes from a scrolled
  result.
- `UVisualGrid.Loading.Checks` has five FPCUnit cases for loading and error
  cards during throttled layout changes and after provider failures.
- `UVisualGrid.Selection.Checks` tests four row-click and selection-clearing
  cases. Run them with `PASCAL_GRID_SELECTION_ONLY=1` on PascalFramework's
  `GridViewportProbe`, or through FPCUnit `TVisualGridSelectionEventTests`.
  Mouse cases use no global input.
- The [worker checks](../../framework/UVisualGrid.Worker.Checks.pas) cover
  cancellation before execution and three public `CancelActiveFetch` cases. Run
  the three with `PASCAL_GRID_ACTIVE_CANCEL_ONLY=1` on `GridViewportProbe`, or
  as the active-cancellation methods in `TVisualGridQueuedCompletionTests`.
  With `VISUALGRID_TEST_WORKER_START` defined, the selector also tests calling
  `CancelActiveFetch` before `Start`.

`GridThrottleProbe` and the
[experimental FPCUnit runner](../../RunExperimentalWalletTests.ps1) define
`VISUALGRID_TEST_WORKER_START`. Normal applications and packages omit it. Use
separate compiled-unit output for builds with and without it.

## Recorded results

`DataRowVariantProbe` has passed in Delphi, as has `GridThrottleProbe` in full
and with its suspension, loading and early-cancel selectors.
