<#
.SYNOPSIS
Builds and tests safebox-download cancellation on an isolated private desktop.
.DESCRIPTION
Two controlled real download workers contend for the owner's chunk-list lock
while shutdown joins them. Fresh settings disable peers and use an ephemeral
listener. The test never opens the user's wallet or switches the input desktop.
#>
# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [string] $FpcPath = $(if ($c = Get-Command fpc -ErrorAction SilentlyContinue) { $c.Source } else { '' }),
    [string] $LazarusDirectory = $(if ($c = Get-Command lazbuild -ErrorAction SilentlyContinue) { Split-Path -Parent $c.Source } else { '' }),
    [string] $OutputDirectory
)

$ErrorActionPreference = 'Stop'
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
$lazarus = (Resolve-Path -LiteralPath $LazarusDirectory).Path
$source = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..\..')).Path
$targetInfo = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if ($LASTEXITCODE -ne 0 -or $targetInfo.Count -ne 2 -or $targetInfo[1] -ne 'win64') {
    throw 'The native download-stop check requires a Windows x64 FPC compiler.'
}
$target = $targetInfo -join '-'
if (-not $OutputDirectory) {
    $OutputDirectory = Join-Path $PSScriptRoot ('lib\download-stop-' + [guid]::NewGuid().ToString('N'))
}
$output = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $output) { throw "Choose a new output directory: $output" }
$units = Join-Path $output 'units'
$brokerUnits = Join-Path $output 'broker-units'
New-Item -ItemType Directory -Path $units, $brokerUnits -Force | Out-Null
$probeDirectory = Join-Path $PSScriptRoot 'native\download-stop'
$executable = Join-Path $output 'DownloadSafeboxStopProbe.exe'
$arguments = @('-B', '-MDelphi', '-O1', '-gl', '-Cr', '-Co', '-WG',
    '-dBorland', '-dVer150', '-dDelphi7', '-dCompiler6_Up', '-dPUREPASCAL',
    "-FU$units", "-FE$output", "-o$executable",
    "-Fi$source\libraries\generics.collections\inc",
    "-Fu$lazarus\lcl\units\$target", "-Fu$lazarus\lcl\units\$target\win32",
    "-Fu$lazarus\components\lazutils\lib\$target", "-Fu$lazarus\packager\units\$target",
    "-Fu$probeDirectory")
$directories = @('core', 'gui-experimental', 'gui-experimental\core.utils',
    'gui-experimental\wizards\wallet', 'gui-experimental\wizards\operations', 'gui-classic',
    'libraries\simplebaselib4pascal', 'libraries\hashlib4pascal', 'libraries\cryptolib4pascal',
    'libraries\synapse', 'libraries\sphere10', 'libraries\generics.collections',
    'libraries\pascalcoin', 'libraries\paszlib', 'libraries\abstractmem', 'libraries\regex',
    'libraries\gnugettext')
foreach ($directory in $directories) { $arguments += "-Fu$source\$directory" }
$arguments += Join-Path $probeDirectory 'DownloadSafeboxStopProbe.lpr'
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

$broker = Join-Path $output 'PrivateDesktopBroker.exe'
& $compiler '-MDelphi' '-O2' '-gl' "-FU$brokerUnits" "-o$broker" `
    (Join-Path $PSScriptRoot 'native\PrivateDesktopBroker.lpr') *> (Join-Path $output 'broker-build.log')
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath (Join-Path $output 'broker-build.log') -Tail 40
    exit $LASTEXITCODE
}
$log = Join-Path $output 'tests.log'
$temporaryData = Join-Path $output 'temporary-data'
New-Item -ItemType Directory -Path $temporaryData | Out-Null
$previousTemp = $env:TEMP
$previousTmp = $env:TMP
try {
    $env:TEMP = $temporaryData
    $env:TMP = $temporaryData
    & $broker $executable $log *> (Join-Path $output 'broker.log')
    $status = $LASTEXITCODE
} finally {
    $env:TEMP = $previousTemp
    $env:TMP = $previousTmp
}
if (Test-Path -LiteralPath $log) { Get-Content -LiteralPath $log }
elseif ($status -eq 0) {
    Write-Error 'The download-stop check did not produce its result log.' -ErrorAction Continue
    $status = 1
}
$heap = "$log.heap.log"
if ($status -eq 0 -and (-not (Test-Path -LiteralPath $heap) -or
        -not ([IO.File]::ReadAllText($heap) -match '(?m)^0 unfreed memory blocks'))) {
    Write-Error 'The download-stop check did not produce a clean heap report.' -ErrorAction Continue
    $status = 1
}
if ($status -ne 0) { Get-Content -LiteralPath (Join-Path $output 'broker.log') -Tail 25 }
Write-Host "Wallet download-stop evidence: $output"
exit $status
