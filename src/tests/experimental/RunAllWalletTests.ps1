<#
.SYNOPSIS
Runs the complete Windows wallet unit and native regression suites.
.DESCRIPTION
The controller integration fixture runs in its own process because it owns
process-lifetime application state. Native checks use fresh data and private
desktops. The optional large-chain fixture and Delphi-only probes are separate.
#>
# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [string] $FpcPath,
    [string] $LazarusDirectory,
    [string] $OutputDirectory,
    [string] $WalletExecutable
)

$ErrorActionPreference = 'Stop'
if ([Environment]::OSVersion.Platform -ne [PlatformID]::Win32NT) {
    throw 'The full native suite requires Windows. Run ThreadingTests.lpr for portable lock coverage.'
}

# CI exposes the installed tools on PATH; explicit paths remain available locally.
if (-not $FpcPath) {
    $command = Get-Command fpc -ErrorAction SilentlyContinue
    if ($command) { $FpcPath = $command.Source }
    elseif (Test-Path -LiteralPath '<fpc>\bin\x86_64-win64\fpc.exe') {
        $FpcPath = '<fpc>\bin\x86_64-win64\fpc.exe'
    } else { throw 'Pass -FpcPath or add fpc to PATH.' }
}
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
if (-not $LazarusDirectory) {
    $command = Get-Command lazbuild -ErrorAction SilentlyContinue
    if ($command) { $LazarusDirectory = Split-Path -Parent $command.Source }
    elseif (Test-Path -LiteralPath '<lazarus>\lcl') {
        $LazarusDirectory = '<lazarus>'
    } else { throw 'Pass -LazarusDirectory or add lazbuild to PATH.' }
}
$lazarus = (Resolve-Path -LiteralPath $LazarusDirectory).Path
if (-not (Test-Path -LiteralPath (Join-Path $lazarus 'lcl') -PathType Container)) {
    throw "Lazarus LCL directory not found under $lazarus. Pass -LazarusDirectory explicitly."
}
$repository = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../../..')).Path
if (-not $WalletExecutable) {
    $WalletExecutable = Join-Path $repository 'src/PascalCoinWalletExperimental.exe'
}
$shippingWallet = (Resolve-Path -LiteralPath $WalletExecutable).Path
if (-not $OutputDirectory) {
    $runId = (Get-Date -Format 'yyyyMMdd-HHmmss') + '-' + [guid]::NewGuid().ToString('N').Substring(0, 8)
    $OutputDirectory = Join-Path $PSScriptRoot "lib/all-$runId"
}
$output = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $output) { throw "Choose a new output directory: $output" }
New-Item -ItemType Directory -Path $output | Out-Null

# Every registering unit must belong to the general or isolated controller runner.
$general = [IO.File]::ReadAllText((Join-Path $PSScriptRoot 'ExperimentalWalletTests.lpr'))
$isolated = [IO.File]::ReadAllText((Join-Path $PSScriptRoot 'ui/WalletGridRefreshTests.lpr'))
$fixtureFiles = @()
foreach ($directory in @('core', 'startup', 'operations', 'ui', 'framework')) {
    $fixtureFiles += Get-ChildItem -LiteralPath (Join-Path $PSScriptRoot $directory) -Filter '*.pas' -File
}
foreach ($name in @('UCommon.Tests.pas', 'UCommon.Collections.Tests.pas', 'UMemory.Tests.pas')) {
    $fixtureFiles += Get-Item -LiteralPath (Join-Path $PSScriptRoot "../$name")
}
$registeredUnits = @()
foreach ($file in $fixtureFiles) {
    $text = [IO.File]::ReadAllText($file.FullName)
    if ($text -notmatch '(?im)^\s*RegisterTests?\s*\(') { continue }
    if ($text -notmatch '(?im)^\s*unit\s+([A-Za-z0-9_.]+)\s*;') {
        throw "Cannot identify registered fixture unit: $($file.FullName)"
    }
    $unitName = $Matches[1]
    $pattern = '(?<![A-Za-z0-9_.])' + [regex]::Escape($unitName) + '(?![A-Za-z0-9_.])'
    $runner = if ($general -match $pattern) { 'general' }
              elseif ($isolated -match $pattern) { 'isolated-controller' }
              else { throw "Registered fixture unit is missing from the test runners: $unitName" }
    $registeredUnits += [pscustomobject]@{ Unit = $unitName; Runner = $runner }
}
$registeredUnits | Sort-Object Unit | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $output 'fixture-registration.json')

$phases = @(
    @{ Name = 'unit'; Script = 'RunExperimentalWalletTests.ps1'; Lazarus = $true },
    @{ Name = 'grid-integration'; Script = 'RunWalletGridRefreshTests.ps1'; Lazarus = $true },
    @{ Name = 'grid-native'; Script = 'RunWalletGridRefreshTests.ps1'; Lazarus = $true; Switch = '-Demo' },
    @{ Name = 'network-toggle'; Script = 'RunWalletGridRefreshTests.ps1'; Lazarus = $true; Switch = '-NetworkOff' },
    @{ Name = 'buffered-backlog'; Script = 'RunWalletGridRefreshTests.ps1'; Lazarus = $true; Switch = '-Backlog' },
    @{ Name = 'startup'; Script = 'native/RunWalletStartupNativeTests.ps1'; Lazarus = $true; Shipping = $true },
    @{ Name = 'download-stop'; Script = 'RunWalletDownloadStopTests.ps1'; Lazarus = $true },
    @{ Name = 'safebox-recovery'; Script = 'RunWalletSafeboxRecoveryTests.ps1'; Lazarus = $false },
    @{ Name = 'classic'; Script = 'native/RunWalletRegressionTests.ps1'; Lazarus = $true; Probe = 'classic' },
    @{ Name = 'connection-restart'; Script = 'native/RunWalletRegressionTests.ps1'; Lazarus = $true; Probe = 'connection-restart' },
    @{ Name = 'pending-protocol'; Script = 'native/RunWalletRegressionTests.ps1'; Lazarus = $true; Probe = 'pending-protocol' },
    @{ Name = 'buffered-frames'; Script = 'native/RunWalletRegressionTests.ps1'; Lazarus = $true; Probe = 'buffered-frames' },
    @{ Name = 'node-sync'; Script = 'native/RunWalletRegressionTests.ps1'; Lazarus = $true; Probe = 'node-sync' },
    @{ Name = 'mempool-lock'; Script = 'native/RunWalletRegressionTests.ps1'; Lazarus = $true; Probe = 'mempool-lock' }
)
$coverage = @{
    RegisteredFixtureUnits = @($registeredUnits | Sort-Object Unit)
    NativePhases = @($phases | Where-Object Name -ne 'unit' | ForEach-Object { $_.Name })
    SharedHelpersInUnitFixtures = @('UAccountCachePolicyChecks', 'UChangeAccountInfoReplayChecks',
        'UDownloadLifetimeChecks', 'ULegacyConnectionLifetimeChecks', 'UMempoolSanitizeChecks',
        'UOperationReadHashChecks', 'UOrderedCardinalChecks',
        'UPendingCatchupChecks', 'UPendingSaveChecks', 'UStoredHistoryChecks')
    EquivalentNodeLifecycleScenarios = 'UNodeSynchronizationTests mirrors the eight native UNodeSynchronizationChecks cases'
    SeparateInputs = @('Delphi-only probes require Delphi',
        'BlockThroughputProbe requires an owned public-chain dataset',
        'LargeBufferedBlockBacklogProbe requires an owned matching-chain dataset')
}
$coverage | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $output 'test-coverage.json')
$shell = (Get-Process -Id $PID).Path
$results = @()
foreach ($phase in $phases) {
    $arguments = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File',
        (Join-Path $PSScriptRoot $phase.Script), '-FpcPath', $compiler,
        '-OutputDirectory', (Join-Path $output $phase.Name))
    if ($phase.Lazarus) { $arguments += @('-LazarusDirectory', $lazarus) }
    if ($phase.Shipping) { $arguments += @('-WalletExecutable', $shippingWallet) }
    if ($phase.Probe) { $arguments += @('-Probe', $phase.Probe) }
    if ($phase.Switch) { $arguments += $phase.Switch }
    Write-Host "Running wallet tests: $($phase.Name)"
    $started = Get-Date
    & $shell @arguments 2>&1 | Tee-Object -FilePath (Join-Path $output ($phase.Name + '-runner.log'))
    $status = $LASTEXITCODE
    $results += [pscustomobject]@{
        Phase = $phase.Name
        ExitCode = $status
        Seconds = [Math]::Round(((Get-Date) - $started).TotalSeconds, 2)
    }
    $results | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $output 'all-tests.json')
}
$results | Format-Table -AutoSize
Write-Host "Wallet regression evidence: $output"
if (@($results | Where-Object ExitCode -ne 0).Count -ne 0) { exit 1 }
exit 0
