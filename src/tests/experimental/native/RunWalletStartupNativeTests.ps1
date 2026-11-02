param(
    [string] $FpcPath = '<fpc>\bin\x86_64-win64\fpc.exe',
    [string] $LazarusDirectory = '<lazarus>',
    [string] $OutputDirectory,
    [string] $WalletExecutable
)

$ErrorActionPreference = 'Stop'
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
$lazarus = (Resolve-Path -LiteralPath $LazarusDirectory).Path
$source = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..\..\..')).Path
if (-not $WalletExecutable) { $WalletExecutable = Join-Path $source 'PascalCoinWalletExperimental.exe' }
$shippingExecutable = (Resolve-Path -LiteralPath $WalletExecutable).Path
$targetInfo = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if ($LASTEXITCODE -ne 0 -or $targetInfo.Count -ne 2 -or $targetInfo[1] -ne 'win64') {
    throw 'The native wallet startup probe requires a Windows x64 FPC compiler.'
}
$target = "$($targetInfo[0])-$($targetInfo[1])"
if (-not $OutputDirectory) {
    $OutputDirectory = Join-Path $PSScriptRoot ('..\lib\native-' + [guid]::NewGuid().ToString('N'))
}
New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null
$output = (Resolve-Path -LiteralPath $OutputDirectory).Path
$brokerUnits = Join-Path $output 'broker-units'
$probeUnits = Join-Path $output 'probe-units'
New-Item -ItemType Directory -Path $brokerUnits, $probeUnits -Force | Out-Null
$brokerExecutable = Join-Path $output 'PrivateDesktopBroker.exe'
$probeExecutable = Join-Path $output 'WalletStartupProbe.exe'
$brokerBuildLog = Join-Path $output 'broker-build.log'
$probeBuildLog = Join-Path $output 'probe-build.log'
$brokerRunLog = Join-Path $output 'broker-run.log'

& $compiler '-MDelphi' '-O1' '-gl' "-FU$brokerUnits" "-FE$output" "-o$brokerExecutable" `
    "$PSScriptRoot\PrivateDesktopBroker.lpr" *> $brokerBuildLog
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $brokerBuildLog -Tail 50
    exit $LASTEXITCODE
}
$probeArguments = @(
    '-B', '-MDelphi', '-O1', '-gl', '-WG',
    '-dBorland', '-dVer150', '-dDelphi7', '-dCompiler6_Up', '-dPUREPASCAL',
    "-Fu$lazarus\lcl\units\$target", "-Fu$lazarus\lcl\units\$target\win32",
    "-Fu$lazarus\components\lazutils\lib\$target", "-Fu$lazarus\packager\units\$target",
    "-Fi$source\libraries\generics.collections\inc", "-FU$probeUnits", "-FE$output", "-o$probeExecutable"
)
$unitDirectories = @(
    'core', 'gui-experimental', 'libraries\simplebaselib4pascal', 'libraries\hashlib4pascal',
    'libraries\cryptolib4pascal', 'libraries\synapse', 'libraries\sphere10',
    'libraries\generics.collections', 'libraries\pascalcoin', 'gui-experimental\core.utils',
    'gui-experimental\wizards\wallet', 'gui-experimental\wizards\operations',
    'libraries\paszlib', 'gui-classic', 'libraries\abstractmem', 'libraries\regex'
)
foreach ($directory in $unitDirectories) {
    $probeArguments += "-Fu$source\$directory"
}
$probeArguments += "$PSScriptRoot\WalletStartupProbe.lpr"
& $compiler @probeArguments *> $probeBuildLog
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $probeBuildLog -Tail 60
    exit $LASTEXITCODE
}
# Copy the existing local runtime dependency; never relocate or replace it.
# Prefer the deployed runtime, then the repository's canonical Windows DLL.
$crypto = Join-Path $source 'libcrypto-1_1-x64.dll'
if (-not (Test-Path -LiteralPath $crypto)) {
    $crypto = Join-Path (Split-Path -Parent $source) 'windows-dll\libcrypto-1_1-x64.dll'
}
if (-not (Test-Path -LiteralPath $crypto)) { throw "OpenSSL runtime not found: $crypto" }
Copy-Item -LiteralPath $crypto -Destination $output
# The broker refuses to reuse its data directories and never switches desktops.
$testTemp = Join-Path $output 'temporary-data'
New-Item -ItemType Directory -Path $testTemp -Force | Out-Null
$previousTemp = $env:TEMP
$previousTmp = $env:TMP
try {
    $env:TEMP = $testTemp
    $env:TMP = $testTemp
    & $brokerExecutable $probeExecutable $output '--wallet-startup' *> $brokerRunLog
    $nativeExitCode = $LASTEXITCODE
} finally {
    $env:TEMP = $previousTemp
    $env:TMP = $previousTmp
}
Get-Content -LiteralPath $brokerRunLog
foreach ($mode in @('loaded', 'close-loading', 'close-before-load', 'slow-close')) {
    $log = Join-Path $output "$mode.log"
    if (Test-Path -LiteralPath $log) { Get-Content -LiteralPath $log }
}
if ($nativeExitCode -eq 0) {
  try {
    $env:TEMP = $testTemp
    $env:TMP = $testTemp
    foreach ($mode in @('actual-first-run', 'actual-no-dialogs')) {
        & $brokerExecutable $shippingExecutable $output "--$mode" >> $brokerRunLog
        if ($LASTEXITCODE -ne 0) { $nativeExitCode = $LASTEXITCODE }
        Get-Content -LiteralPath (Join-Path $output "$mode.log")
        if ($nativeExitCode -ne 0) { break }
    }
  } finally {
    $env:TEMP = $previousTemp
    $env:TMP = $previousTmp
  }
}
Write-Host "Native wallet startup evidence: $output"
exit $nativeExitCode
