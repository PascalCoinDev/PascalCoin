<#
.SYNOPSIS
Runs a distinct native wallet regression on an isolated Windows desktop.
.DESCRIPTION
Uses the existing acceptance helpers, fresh data and the private desktop broker.
The protocol probe compiles a verified visibility-only copy of UNetProtocol;
shipping source and method bodies remain unchanged.
#>
# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('classic', 'connection-restart', 'pending-protocol', 'buffered-frames', 'node-sync', 'mempool-lock')]
    [string] $Probe,
    [string] $FpcPath = '<fpc>\bin\x86_64-win64\fpc.exe',
    [string] $LazarusDirectory = '<lazarus>',
    [Parameter(Mandatory = $true)]
    [string] $OutputDirectory
)

$ErrorActionPreference = 'Stop'
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
$lazarus = (Resolve-Path -LiteralPath $LazarusDirectory).Path
$source = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../../..')).Path
$targetInfo = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if ($LASTEXITCODE -ne 0 -or $targetInfo.Count -ne 2 -or $targetInfo[1] -ne 'win64') {
    throw 'The native wallet regressions require a Windows x64 FPC compiler.'
}
$target = $targetInfo -join '-'
$programs = @{
    'classic' = 'classic/ClassicWalletProbe.lpr'
    'connection-restart' = 'connection-restart/ConnectionRestartProbe.lpr'
    'pending-protocol' = 'pending-protocol/PendingProtocolProbe.lpr'
    'buffered-frames' = 'network-backlog/BufferedBlockFrameProbe.lpr'
    'node-sync' = 'node-sync/WalletNodeSynchronizationProbe.lpr'
    'mempool-lock' = 'mempool-lock/MempoolLockProbe.lpr'
}
$program = Join-Path $PSScriptRoot $programs[$Probe]
$output = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $output) { throw "Choose a new output directory: $output" }
$units = Join-Path $output 'units'
$brokerUnits = Join-Path $output 'broker-units'
$temporaryData = Join-Path $output 'temporary-data'
New-Item -ItemType Directory -Path $units, $brokerUnits, $temporaryData -Force | Out-Null
$executable = Join-Path $output ([IO.Path]::GetFileNameWithoutExtension($program) + '.exe')
$fixture = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../fixtures/mainnet-blocks-0-4.bin')).Path
Copy-Item -LiteralPath $fixture -Destination $output
$crypto = @((Join-Path $source 'libcrypto-1_1-x64.dll'),
    (Join-Path (Split-Path -Parent $source) 'windows-dll/libcrypto-1_1-x64.dll')) |
    Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
if (-not $crypto) { throw 'The bundled OpenSSL 1.1 x64 runtime is missing.' }
Copy-Item -LiteralPath $crypto -Destination $output

$arguments = @('-B', '-Mdelphi', '-O1', '-gl',
    '-dBorland', '-dVer150', '-dDelphi7', '-dCompiler6_Up', '-dPUREPASCAL',
    "-FU$units", "-FE$output", "-o$executable",
    "-Fi$source/libraries/generics.collections/inc")
if ($Probe -ne 'classic') {
    $arguments += @('-Cr', '-Co')
}
# Classic retains its shipping arithmetic settings; the other core probes are checked.
if ($Probe -in @('classic', 'node-sync')) { $arguments += '-WG' }
if ($Probe -eq 'pending-protocol') {
    $accessOutput = Join-Path $output 'protocol-access'
    $accessCore = & (Join-Path $PSScriptRoot 'pending-protocol/NewPendingProtocolAccessCopy.ps1') -OutputDirectory $accessOutput
    $arguments += @('-dPASCAL_PROTOCOL_INTERNAL_CHECKS', "-Fu$accessCore")
}
$arguments += @("-Fu$([IO.Path]::GetDirectoryName($program))",
    "-Fu$lazarus/lcl/units/$target/win32", "-Fu$lazarus/lcl/units/$target",
    "-Fu$lazarus/components/lazutils/lib/$target", "-Fu$lazarus/packager/units/$target")
$directories = @('core', 'gui-experimental', 'gui-experimental/core.utils',
    'gui-experimental/wizards/wallet', 'gui-experimental/wizards/operations', 'gui-classic',
    'libraries/simplebaselib4pascal', 'libraries/hashlib4pascal', 'libraries/cryptolib4pascal',
    'libraries/synapse', 'libraries/sphere10', 'libraries/generics.collections',
    'libraries/pascalcoin', 'libraries/paszlib', 'libraries/abstractmem', 'libraries/regex',
    'libraries/gnugettext')
if ($Probe -eq 'classic') {
    # Both GUIs contain UFRMWallet; keep the Classic probe on its shipping graph.
    $directories = @('gui-classic') + @($directories | Where-Object {
        $_ -ne 'gui-classic' -and $_ -notlike 'gui-experimental*'
    })
}
foreach ($directory in $directories) { $arguments += "-Fu$source/$directory" }
$arguments += $program
$arguments | Set-Content -LiteralPath (Join-Path $output 'compiler-arguments.txt')
& $compiler @arguments *> (Join-Path $output 'build.log')
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath (Join-Path $output 'build.log') -Tail 60
    exit $LASTEXITCODE
}
$broker = Join-Path $output 'PrivateDesktopBroker.exe'
& $compiler '-Mdelphi' '-O2' '-gl' "-FU$brokerUnits" "-o$broker" `
    (Join-Path $PSScriptRoot 'PrivateDesktopBroker.lpr') *> (Join-Path $output 'broker-build.log')
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath (Join-Path $output 'broker-build.log') -Tail 40
    exit $LASTEXITCODE
}
$log = Join-Path $output 'tests.log'
$previousTemp, $previousTmp = $env:TEMP, $env:TMP
$previousFixture = $env:PASCAL_CLASSIC_BLOCK_FIXTURE
$previousProtocolCase = $env:PASCAL_PENDING_PROTOCOL_ONLY
$previousPowCase = $env:PASCAL_PENDING_POW_NONEMPTY_ONLY
try {
    $env:TEMP = $temporaryData
    $env:TMP = $temporaryData
    $env:PASCAL_CLASSIC_BLOCK_FIXTURE = $fixture
    Remove-Item Env:PASCAL_PENDING_PROTOCOL_ONLY -ErrorAction SilentlyContinue
    Remove-Item Env:PASCAL_PENDING_POW_NONEMPTY_ONLY -ErrorAction SilentlyContinue
    & $broker $executable $log *> (Join-Path $output 'broker.log')
    $status = $LASTEXITCODE
} finally {
    $env:TEMP, $env:TMP = $previousTemp, $previousTmp
    $env:PASCAL_CLASSIC_BLOCK_FIXTURE = $previousFixture
    $env:PASCAL_PENDING_PROTOCOL_ONLY = $previousProtocolCase
    $env:PASCAL_PENDING_POW_NONEMPTY_ONLY = $previousPowCase
}
if (Test-Path -LiteralPath $log) {
    $report = [IO.File]::ReadAllText($log)
    Write-Output $report
    if ($report -match '(?m)^FAIL') { $status = 1 }
    if ($Probe -eq 'pending-protocol' -and
        [regex]::Matches($report, '(?m)^PASS pending protocol case=\d+\s*$').Count -ne 9) {
        Write-Error 'All nine pending protocol cases must run.' -ErrorAction Continue
        $status = 1
    }
} else {
    Write-Error 'The native regression did not produce its report.' -ErrorAction Continue
    $status = 1
}
$heap = "$log.heap.log"
if ($status -eq 0 -and (-not (Test-Path -LiteralPath $heap) -or
        [IO.File]::ReadAllText($heap) -notmatch '(?m)^0 unfreed memory blocks')) {
    Write-Error 'The native regression did not produce a clean heap report.' -ErrorAction Continue
    $status = 1
}
if ($Probe -eq 'pending-protocol') {
    # Recompute both fixed digests independently of the Pascal serializer.
    $python = @('python', 'python3') | ForEach-Object {
        Get-Command $_ -CommandType Application -ErrorAction SilentlyContinue
    } | Where-Object Source -NotLike '*\WindowsApps\*' | Select-Object -First 1
    if (-not $python) { throw 'Python 3 is required for the independent proof-of-work vectors.' }
    & $python.Source (Join-Path $PSScriptRoot 'pending-protocol/ProofOfWorkVectors.py') `
        *> (Join-Path $output 'proof-of-work-vectors.json')
    if ($LASTEXITCODE -ne 0) { $status = 1 }
}
@{
    Probe = $Probe
    ExitCode = $status
    ExecutableSha256 = (Get-FileHash -LiteralPath $executable -Algorithm SHA256).Hash
    FixtureSha256 = (Get-FileHash -LiteralPath $fixture -Algorithm SHA256).Hash
    CheckedArithmetic = ($Probe -ne 'classic')
    VisibilityOnlyProtocolCopy = ($Probe -eq 'pending-protocol')
} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $output 'verification.json')
if ($status -ne 0) { Get-Content -LiteralPath (Join-Path $output 'broker.log') -Tail 25 }
Write-Host "Native $Probe regression evidence: $output"
exit $status
