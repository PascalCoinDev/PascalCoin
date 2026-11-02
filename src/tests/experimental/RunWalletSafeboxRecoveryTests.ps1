<#
.SYNOPSIS
Builds and tests interrupted safebox recovery using real loopback connections.
.DESCRIPTION
The probe tests late replies, sole-peer reconnection, connection leases, worker
handoff and strict rejection of invalid packets/chunks. It uses fresh data on an
owned private desktop and never connects to public peers or opens a user's wallet.
#>
# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [string] $FpcPath = '<fpc>\bin\x86_64-win64\fpc.exe',
    [string] $OutputDirectory
)

$ErrorActionPreference = 'Stop'
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
$source = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..\..')).Path
$targetInfo = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if ($LASTEXITCODE -ne 0 -or $targetInfo.Count -ne 2 -or $targetInfo[1] -ne 'win64') {
    throw 'The safebox recovery check requires a Windows x64 FPC compiler.'
}
if (-not $OutputDirectory) {
    $OutputDirectory = Join-Path $PSScriptRoot ('lib\safebox-recovery-' + [guid]::NewGuid().ToString('N'))
}
$output = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $output) { throw "Choose a new output directory: $output" }
$units = Join-Path $output 'units'
$brokerUnits = Join-Path $output 'broker-units'
$temporaryData = Join-Path $output 'temporary-data'
New-Item -ItemType Directory -Path $units, $brokerUnits, $temporaryData -Force | Out-Null
$probeDirectory = Join-Path $PSScriptRoot 'native\safebox-recovery'
$executable = Join-Path $output 'SafeboxRecoveryProbe.exe'
$arguments = @('-B', '-MDelphi', '-O2', '-gl', '-Cr', '-Co', '-Ci',
    '-dBorland', '-dVer150', '-dDelphi7', '-dCompiler6_Up', '-dPUREPASCAL',
    "-FU$units", "-FE$output", "-o$executable",
    "-Fi$source\libraries\generics.collections\inc", "-Fu$probeDirectory")
$directories = @('core', 'libraries\simplebaselib4pascal', 'libraries\hashlib4pascal',
    'libraries\cryptolib4pascal', 'libraries\synapse', 'libraries\sphere10',
    'libraries\generics.collections', 'libraries\pascalcoin', 'libraries\paszlib',
    'libraries\abstractmem', 'libraries\regex')
foreach ($directory in $directories) { $arguments += "-Fu$source\$directory" }
$arguments += Join-Path $probeDirectory 'SafeboxRecoveryProbe.lpr'
$arguments | Set-Content -LiteralPath (Join-Path $output 'compiler-arguments.txt')

$previousTemp, $previousTmp = $env:TEMP, $env:TMP
try {
    $env:TEMP = $temporaryData
    $env:TMP = $temporaryData
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
    $broker = Join-Path $output 'PrivateDesktopBroker.exe'
    & $compiler '-MDelphi' '-O2' '-gl' "-FU$brokerUnits" "-o$broker" `
        (Join-Path $PSScriptRoot 'native\PrivateDesktopBroker.lpr') *> (Join-Path $output 'broker-build.log')
    if ($LASTEXITCODE -ne 0) {
        Get-Content -LiteralPath (Join-Path $output 'broker-build.log') -Tail 40
        exit $LASTEXITCODE
    }
    $log = Join-Path $output 'tests.log'
    & $broker $executable $log *> (Join-Path $output 'broker.log')
    $status = $LASTEXITCODE
    if (Test-Path -LiteralPath $log) {
        $text = [IO.File]::ReadAllText($log)
        Write-Output $text
        if ($text -notmatch '(?m)^RESULT cases=8 failures=0\s*$' -or $text -match '(?m)^FAIL') {
            $status = 1
        }
    } else {
        Write-Error 'The safebox recovery check did not produce its result log.' -ErrorAction Continue
        $status = 1
    }
    $heap = "$log.heap.log"
    if ($status -eq 0 -and (-not (Test-Path -LiteralPath $heap) -or
            [IO.File]::ReadAllText($heap) -notmatch '(?m)^0 unfreed memory blocks')) {
        Write-Error 'The safebox recovery check did not produce a clean heap report.' -ErrorAction Continue
        $status = 1
    }
    if ($status -ne 0) { Get-Content -LiteralPath (Join-Path $output 'broker.log') -Tail 25 }
    Write-Host "Safebox recovery evidence: $output"
    exit $status
} finally {
    $env:TEMP, $env:TMP = $previousTemp, $previousTmp
}
