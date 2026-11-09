<#
.SYNOPSIS
Builds and runs main-wallet refresh and received-block checks on a private desktop.
.DESCRIPTION
The default target is the dedicated FPCUnit fixture. Use -Demo to run the native
interactive demo's automatic scenarios instead. Add -AllDemoCases to include
every distinct native presentation/logging/dialog group in fresh processes.
Use -DemoCase <name> with -Demo to run one of those groups instead.
Native demo checks use the shipping Release optimization level so layout bugs
that disappear in less optimized builds remain covered.
Use -NetworkOff for the real
connectivity toggle and deferred-refresh lifecycle regression. Use -Backlog for
the actual buffered-response processor, outgoing/incoming transport ownership,
explicit shutdown, inherited data sources and committed footer/status checks
using five historical blocks. One build runs eight fresh private processes.
Use -LargeBacklog with -LargeDataRoot for the opt-in matching public-chain
fixture. It reuses an explicitly owned Data junction while creating fresh
settings and one watch-only key; it is not part of the eight small scenarios.
The other targets use typed
mock sources and real wallet controls. Network cases restrict peers to loopback;
all targets use fresh isolated wallet settings. Existing wallet keys and the input desktop
are never used by this script.
#>
# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [string] $FpcPath = $(if ($c = Get-Command fpc -ErrorAction SilentlyContinue) { $c.Source } else { '' }),
    [string] $LazarusDirectory = $(if ($c = Get-Command lazbuild -ErrorAction SilentlyContinue) { Split-Path -Parent $c.Source } else { '' }),
    [string] $OutputDirectory,
    [switch] $Demo,
    [switch] $AllDemoCases,
    [string] $DemoCase,
    [switch] $NetworkOff,
    [switch] $Backlog,
    [switch] $LargeBacklog,
    [string] $LargeDataRoot
)

$ErrorActionPreference = 'Stop'
if (@($Demo, $NetworkOff, $Backlog, $LargeBacklog).Where({ $_.IsPresent }).Count -gt 1) {
    throw 'Choose only one of -Demo, -NetworkOff, -Backlog or -LargeBacklog.'
}
if (($AllDemoCases -or $DemoCase) -and -not $Demo) { throw '-AllDemoCases and -DemoCase require -Demo.' }
if ($AllDemoCases -and $DemoCase) { throw 'Choose only one of -AllDemoCases or -DemoCase.' }
$demoSelectors = [ordered]@{
    'selection' = @('PASCAL_WALLET_SELECTION_ONLY')
    'safebox-info' = @('PASCAL_WALLET_SAFEBOX_LOG_ONLY')
    'sync-paint' = @('PASCAL_WALLET_SYNC_PAINT_ONLY')
    'tools' = @('PASCAL_WALLET_TOOLS_ONLY')
    'history-depth' = @('PASCAL_WALLET_TOOLS_ONLY', 'PASCAL_WALLET_HISTORY_DEPTH_ONLY')
    'operation-input' = @('PASCAL_WALLET_TOOLS_ONLY', 'PASCAL_WALLET_OPERATION_INPUT_ONLY')
    'block-tools' = @('PASCAL_WALLET_TOOLS_ONLY', 'PASCAL_WALLET_BLOCK_TOOLS_ONLY')
    'change-info-approval' = @('PASCAL_WALLET_CHANGE_INFO_APPROVAL_ONLY')
    'change-info-ui' = @('PASCAL_WALLET_CHANGE_INFO_UI_ONLY')
    'send-ui' = @('PASCAL_WALLET_SEND_UI_ONLY')
    'operation-messages' = @('PASCAL_WALLET_OPERATION_MESSAGE_ONLY')
    'advanced-ui' = @('PASCAL_WALLET_ADVANCED_UI_ONLY')
}
$allDemoCaseNames = @('default') + @($demoSelectors.Keys)
if ($DemoCase) {
    $DemoCase = $DemoCase.ToLowerInvariant()
    if ($DemoCase -notin $allDemoCaseNames) {
        throw "Unknown demo case '$DemoCase'. Choose: $($allDemoCaseNames -join ', ')."
    }
}
$useDemoSelectors = $AllDemoCases -or [bool]$DemoCase
$ownedLargeRoot = $null
if ($LargeBacklog) {
    if ([string]::IsNullOrWhiteSpace($LargeDataRoot)) { throw '-LargeBacklog requires -LargeDataRoot.' }
    $ownedLargeRoot = (Resolve-Path -LiteralPath $LargeDataRoot).Path
    if (-not (Test-Path -LiteralPath (Join-Path $ownedLargeRoot 'LargeBacklog.owned') -PathType Leaf)) {
        throw 'The large fixture requires its explicitly owned data marker.'
    }
} elseif ($LargeDataRoot) { throw '-LargeDataRoot is only valid with -LargeBacklog.' }
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
$lazarus = (Resolve-Path -LiteralPath $LazarusDirectory).Path
$source = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..\..')).Path
$targetInfo = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if ($LASTEXITCODE -ne 0 -or $targetInfo.Count -ne 2 -or $targetInfo[1] -ne 'win64') {
    throw 'These native grid refresh checks require a Windows x64 FPC compiler.'
}
$target = $targetInfo -join '-'
$runId = (Get-Date -Format 'yyyyMMdd-HHmmss') + '-' + [guid]::NewGuid().ToString('N').Substring(0, 8)
if (-not $OutputDirectory) { $OutputDirectory = Join-Path $PSScriptRoot "lib\grid-refresh-$runId" }
$output = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $output) { throw "Choose a new output directory: $output" }
$units = Join-Path $output 'units'
$brokerUnits = Join-Path $output 'broker-units'
New-Item -ItemType Directory -Path $units, $brokerUnits -Force | Out-Null

$gridDirectory = Join-Path $PSScriptRoot 'native\grid-refresh'
$backlogDirectory = Join-Path $PSScriptRoot 'native\network-backlog'
$program = if ($Demo) { Join-Path $gridDirectory 'WalletGridRefreshDemo.lpr' }
           elseif ($NetworkOff) { Join-Path $gridDirectory 'WalletNetworkOffProbe.lpr' }
           elseif ($Backlog) { Join-Path $backlogDirectory 'BufferedBlockBacklogProbe.lpr' }
           elseif ($LargeBacklog) { Join-Path $backlogDirectory 'LargeBufferedBlockBacklogProbe.lpr' }
           else { Join-Path $PSScriptRoot 'ui\WalletGridRefreshTests.lpr' }
$name = [IO.Path]::GetFileNameWithoutExtension($program)
$executable = Join-Path $output "$name.exe"
$optimization = if ($Demo) { '-O2' } else { '-O1' }
$arguments = @('-B', '-MDelphi', $optimization, '-gl', '-Cr', '-Co', '-WG',
    '-dBorland', '-dVer150', '-dDelphi7', '-dCompiler6_Up', '-dPUREPASCAL',
    "-FU$units", "-FE$output", "-o$executable",
    "-Fi$source\libraries\generics.collections\inc",
    "-Fu$lazarus\lcl\units\$target", "-Fu$lazarus\lcl\units\$target\win32",
    "-Fu$lazarus\components\lazutils\lib\$target", "-Fu$lazarus\packager\units\$target",
    "-Fu$gridDirectory", "-Fu$backlogDirectory", "-Fu$PSScriptRoot\ui", "-Fu$PSScriptRoot\framework")
$directories = @('core', 'gui-experimental', 'gui-experimental\core.utils',
    'gui-experimental\wizards\wallet', 'gui-experimental\wizards\operations', 'gui-classic',
    'libraries\simplebaselib4pascal', 'libraries\hashlib4pascal', 'libraries\cryptolib4pascal',
    'libraries\synapse', 'libraries\sphere10', 'libraries\generics.collections',
    'libraries\pascalcoin', 'libraries\paszlib', 'libraries\abstractmem', 'libraries\regex',
    'libraries\gnugettext')
foreach ($directory in $directories) { $arguments += "-Fu$source\$directory" }
if ($LargeBacklog) { $arguments += '-dLARGE_BACKLOG_PROBE' }
$arguments += $program
$arguments | Set-Content -LiteralPath (Join-Path $output 'compiler-arguments.txt')
& $compiler @arguments *> (Join-Path $output 'build.log')
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath (Join-Path $output 'build.log') -Tail 60
    exit $LASTEXITCODE
}
# Prefer the deployed runtime, then the repository's canonical Windows DLL.
$crypto = Join-Path $source 'libcrypto-1_1-x64.dll'
if (-not (Test-Path -LiteralPath $crypto)) {
    $crypto = Join-Path (Split-Path -Parent $source) 'windows-dll\libcrypto-1_1-x64.dll'
}
if (-not (Test-Path -LiteralPath $crypto)) { throw "OpenSSL runtime not found: $crypto" }
Copy-Item -LiteralPath $crypto -Destination $output
if ($Demo) {
    # Tools and operation-dialog cases validate the repository's historic genesis.
    $fixture = Join-Path $PSScriptRoot 'fixtures/mainnet-blocks-0-4.bin'
    Copy-Item -LiteralPath $fixture -Destination $output
    Get-FileHash -LiteralPath $fixture -Algorithm SHA256 | Select-Object Algorithm, Hash |
        ConvertTo-Json | Set-Content -LiteralPath (Join-Path $output 'fixture.json')
}

$broker = Join-Path $output 'PrivateDesktopBroker.exe'
& $compiler '-MDelphi' '-O2' '-gl' "-FU$brokerUnits" "-o$broker" `
    (Join-Path $PSScriptRoot 'native\PrivateDesktopBroker.lpr') *> (Join-Path $output 'broker-build.log')
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath (Join-Path $output 'broker-build.log') -Tail 40
    exit $LASTEXITCODE
}
$temporaryData = Join-Path $output 'temporary-data'
New-Item -ItemType Directory -Path $temporaryData | Out-Null
$previousTemp = $env:TEMP
$previousTmp = $env:TMP
$previousBacklogCase = $env:PASCALCOIN_BACKLOG_CASE
$previousLargeDataRoot = $env:PASCALCOIN_LARGE_BACKLOG_ROOT
$selectorNames = @($demoSelectors.Values | ForEach-Object { $_ }) + @(
    'PASCAL_WALLET_KEY_NOTIFICATIONS_ONLY', 'PASCAL_WALLET_CONSECUTIVE_SELECTION_ONLY',
    'PASCAL_WALLET_REFRESH_LIFECYCLE_ONLY', 'PASCAL_WALLET_SELECTION_COMPLETION_ONLY')
$selectorNames = @($selectorNames | Sort-Object -Unique)
$previousSelectors = @{}
if ($useDemoSelectors) {
    foreach ($name in $selectorNames) {
        $previousSelectors[$name] = [Environment]::GetEnvironmentVariable($name, 'Process')
    }
}
$status = 0
$overallStatus = 0
$caseResults = @()
try {
    $env:TEMP = $temporaryData
    $env:TMP = $temporaryData
    if ($LargeBacklog) { $env:PASCALCOIN_LARGE_BACKLOG_ROOT = $ownedLargeRoot }
    $cases = if ($Backlog) {
        @('direct', 'synchronization', 'outgoing', 'outgoing-shutdown', 'incoming',
          'incoming-settings', 'incoming-shutdown', 'connected-miner')
    } elseif ($AllDemoCases) {
        $allDemoCaseNames
    } elseif ($DemoCase) {
        @($DemoCase)
    } else { @('') }
    foreach ($case in $cases) {
        $suffix = if ($case) { "-$case" } else { '' }
        $log = Join-Path $output "tests$suffix.log"
        $brokerLog = Join-Path $output "broker$suffix.log"
        if ($Backlog) { $env:PASCALCOIN_BACKLOG_CASE = $case }
        if ($useDemoSelectors) {
            # Focused selectors replace the default branch; clear all before each child.
            foreach ($name in $selectorNames) {
                [Environment]::SetEnvironmentVariable($name, $null, 'Process')
            }
            foreach ($name in $demoSelectors[$case]) {
                [Environment]::SetEnvironmentVariable($name, '1', 'Process')
            }
        }
        & $broker $executable $log *> $brokerLog
        $status = $LASTEXITCODE
        if (Test-Path -LiteralPath $log) { Get-Content -LiteralPath $log }
        elseif ($status -eq 0) {
            Write-Error 'The grid refresh check did not produce its result log.' -ErrorAction Continue
            $status = 1
        }
        if ($status -eq 0 -and -not $Demo -and -not $LargeBacklog) {
            $heap = "$log.heap.log"
            if (-not (Test-Path -LiteralPath $heap) -or
                -not ([IO.File]::ReadAllText($heap) -match '(?m)^0 unfreed memory blocks')) {
                Write-Error 'The grid refresh fixture did not produce a clean heap report.' -ErrorAction Continue
                $status = 1
            }
        }
        $caseResults += [pscustomobject]@{ Case = $case; ExitCode = $status; Log = $log }
        $caseResults | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $output 'cases.json')
        if ($status -ne 0) {
            $overallStatus = $status
            Get-Content -LiteralPath $brokerLog -Tail 25
            if (-not $AllDemoCases) { break }
        }
    }
} finally {
    $env:TEMP = $previousTemp
    $env:TMP = $previousTmp
    $env:PASCALCOIN_BACKLOG_CASE = $previousBacklogCase
    $env:PASCALCOIN_LARGE_BACKLOG_ROOT = $previousLargeDataRoot
    if ($useDemoSelectors) {
        foreach ($name in $selectorNames) {
            [Environment]::SetEnvironmentVariable($name, $previousSelectors[$name], 'Process')
        }
    }
}
Write-Host "Wallet grid refresh evidence: $output"
exit $overallStatus
