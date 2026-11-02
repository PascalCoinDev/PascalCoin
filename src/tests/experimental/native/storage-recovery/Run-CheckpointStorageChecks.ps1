<#
.SYNOPSIS
Tests real checkpoint I/O failures and local safebox recovery with tiny fixtures.
.DESCRIPTION
Uses a fresh output/TEMP directory and private desktop. No real wallet data,
public peers or installed wallet processes are used.
#>
# Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [string] $FpcPath = '<fpc>\bin\x86_64-win64\fpc.exe',
    [Parameter(Mandatory = $true)] [string] $OutputDirectory
)

$ErrorActionPreference = 'Stop'
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
$target = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if ($LASTEXITCODE -ne 0 -or $target.Count -ne 2 -or $target[0] -ne 'x86_64' -or $target[1] -ne 'win64') {
    throw 'The native storage runner requires a Windows x64 Free Pascal target.'
}
$source = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../../../..')).Path
$tests = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../..')).Path
$output = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $output) { throw "Choose a new output directory: $output" }
$units = Join-Path $output 'units'
$brokerUnits = Join-Path $output 'broker-units'
$temporary = Join-Path $output 'temporary-data'
New-Item -ItemType Directory -Path $units, $brokerUnits, $temporary -Force | Out-Null
$executable = Join-Path $output 'CheckpointStorageProbe.exe'
$arguments = @('-B', '-MDelphi', '-O2', '-gl', '-Cr', '-Co', '-Ci',
    '-dBorland', '-dVer150', '-dDelphi7', '-dCompiler6_Up', '-dPUREPASCAL',
    "-FU$units", "-FE$output", "-o$executable",
    "-Fi$source\libraries\generics.collections\inc", "-Fu$PSScriptRoot")
foreach ($directory in @('core', 'libraries\simplebaselib4pascal',
        'libraries\hashlib4pascal', 'libraries\cryptolib4pascal', 'libraries\synapse',
        'libraries\sphere10', 'libraries\generics.collections',
        'libraries\pascalcoin', 'libraries\paszlib', 'libraries\abstractmem', 'libraries\regex')) {
    $arguments += "-Fu$source\$directory"
}
$arguments += Join-Path $PSScriptRoot 'CheckpointStorageProbe.lpr'
$arguments | Set-Content -LiteralPath (Join-Path $output 'compiler-arguments.txt')
$previousTemp, $previousTmp = $env:TEMP, $env:TMP
try {
    $env:TEMP, $env:TMP = $temporary, $temporary
    & $compiler @arguments *> (Join-Path $output 'build.log')
    if ($LASTEXITCODE -ne 0) {
        Get-Content -LiteralPath (Join-Path $output 'build.log') -Tail 60
        exit $LASTEXITCODE
    }
    $crypto = Join-Path $source 'libcrypto-1_1-x64.dll'
    if (-not (Test-Path -LiteralPath $crypto)) {
        $crypto = Join-Path (Split-Path -Parent $source) 'windows-dll\libcrypto-1_1-x64.dll'
    }
    Copy-Item -LiteralPath $crypto -Destination $output
    Copy-Item -LiteralPath (Join-Path $tests 'fixtures/mainnet-blocks-0-4.bin') -Destination $output
    $broker = Join-Path $output 'PrivateDesktopBroker.exe'
    & $compiler '-MDelphi' '-O2' '-gl' "-FU$brokerUnits" "-o$broker" `
        (Join-Path $PSScriptRoot '../PrivateDesktopBroker.lpr') *> (Join-Path $output 'broker-build.log')
    if ($LASTEXITCODE -ne 0) {
        Get-Content -LiteralPath (Join-Path $output 'broker-build.log') -Tail 40
        exit $LASTEXITCODE
    }
    $log = Join-Path $output 'tests.log'
    $previousErrorAction = $ErrorActionPreference
    try {
        # Windows PowerShell wraps native stderr as error records. Preserve its
        # exit code and full diagnostics even when the child reports a failure.
        $ErrorActionPreference = 'Continue'
        & $broker $executable $log *> (Join-Path $output 'broker.log')
        $status = $LASTEXITCODE
    } finally {
        $ErrorActionPreference = $previousErrorAction
    }
    if (-not (Test-Path -LiteralPath $log)) { throw 'Storage checks did not produce a result log.' }
    $text = [IO.File]::ReadAllText($log)
    Write-Output $text
    if ($text -notmatch '(?m)^PASS 27\s*$' -or $text -match '(?m)^FAIL') { $status = 1 }
    $heap = "$log.heap.log"
    if ($status -eq 0 -and (-not (Test-Path -LiteralPath $heap) -or
            [IO.File]::ReadAllText($heap) -notmatch '(?m)^0 unfreed memory blocks')) {
        Write-Error 'Storage checks did not produce a clean heap report.' -ErrorAction Continue
        $status = 1
    }
    if ($status -ne 0) {
        Get-Content -LiteralPath (Join-Path $output 'broker.log') -Tail 30
        Get-ChildItem -LiteralPath $output -Filter 'tests.case-*.log' -File | ForEach-Object {
            if ([IO.File]::ReadAllText($_.FullName) -match '(?m)^FAIL') { Get-Content -LiteralPath $_.FullName }
        }
    }
    Write-Host "Checkpoint storage evidence: $output"
    exit $status
} finally {
    $env:TEMP, $env:TMP = $previousTemp, $previousTmp
}
