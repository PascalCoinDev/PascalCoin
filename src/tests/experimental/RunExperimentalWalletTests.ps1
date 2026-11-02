param(
    [string] $FpcPath = '<fpc>\bin\x86_64-win64\fpc.exe',
    [string] $LazarusDirectory = '<lazarus>',
    [string] $OutputDirectory
)

$ErrorActionPreference = 'Stop'
$compiler = (Resolve-Path -LiteralPath $FpcPath).Path
$lazarus = (Resolve-Path -LiteralPath $LazarusDirectory).Path
$repository = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..\..\..')).Path
$targetInfo = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if ($LASTEXITCODE -ne 0 -or $targetInfo.Count -ne 2 -or $targetInfo[1] -notin @('win64', 'win32')) {
    throw 'This native Lazarus wallet suite requires a Windows Free Pascal target.'
}
$target = $targetInfo -join '-'
$runId = (Get-Date -Format 'yyyyMMdd-HHmmss') + '-' + [guid]::NewGuid().ToString('N').Substring(0, 8)
if (-not $OutputDirectory) {
    $OutputDirectory = Join-Path $PSScriptRoot "lib\$target\$runId"
}
$outputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $outputDirectory) { throw "Choose a new output directory: $outputDirectory" }
$units = Join-Path $outputDirectory 'units'
$brokerUnits = Join-Path $outputDirectory 'broker-units'
New-Item -ItemType Directory -Path $units, $brokerUnits -Force | Out-Null

# Tests use generated keys; OpenSSL must be resolvable next to their executable.
$cryptoName = if ($targetInfo[0] -eq 'x86_64') { 'libcrypto-1_1-x64.dll' } else { 'libcrypto-1_1.dll' }
# Prefer the deployed runtime, then the repository's canonical bundled copy.
$crypto = @((Join-Path $repository "src\$cryptoName"), (Join-Path $repository "windows-dll\$cryptoName")) |
    Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
if (-not $crypto) { throw "Place the existing OpenSSL 1.1 runtime ($cryptoName) in src or windows-dll." }
Copy-Item -LiteralPath $crypto -Destination (Join-Path $outputDirectory $cryptoName)
# Public historical blocks let checkpoint recovery exercise real local replay.
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures\mainnet-blocks-0-4.bin') -Destination $outputDirectory

$arguments = @('-B', '-Mdelphi', '-gl', '-Cr', '-Co', '-dVISUALGRID_TEST_WORKER_START',
    '-dBorland', '-dVer150', '-dDelphi7', '-dCompiler6_Up', '-dPUREPASCAL',
    "-FU$units", "-FE$outputDirectory", "-Fi$repository\src\libraries\generics.collections\inc",
    "-Fu$lazarus\lcl\units\$target\win32", "-Fu$lazarus\lcl\units\$target",
    "-Fu$lazarus\components\lazutils\lib\$target", "-Fu$lazarus\packager\units\$target")
$sourcePaths = @('core', 'gui-experimental', 'gui-experimental\core.utils',
    'gui-experimental\wizards\wallet', 'gui-experimental\wizards\operations', 'gui-classic',
    'libraries\simplebaselib4pascal', 'libraries\hashlib4pascal', 'libraries\cryptolib4pascal',
    'libraries\synapse', 'libraries\sphere10', 'libraries\generics.collections',
    'libraries\pascalcoin', 'libraries\paszlib', 'libraries\abstractmem', 'libraries\regex',
    'libraries\gnugettext')
foreach ($path in $sourcePaths) { $arguments += "-Fu$repository\src\$path" }
foreach ($path in @('startup', 'operations', 'ui', 'framework', 'core', 'native\mempool-lock', 'native\block-throughput', 'native\network-lifetime', 'native\ordered-cardinal', 'native\stored-history', 'native\storage-recovery', 'native\abstractmem-storage')) { $arguments += "-Fu$PSScriptRoot\$path" }
$arguments += "-Fu$repository\src\tests"
$arguments += "$PSScriptRoot\ExperimentalWalletTests.lpr"
$arguments | Set-Content -LiteralPath (Join-Path $outputDirectory 'compiler-arguments.txt')
& $compiler @arguments *> (Join-Path $outputDirectory 'build.log')
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath (Join-Path $outputDirectory 'build.log') -Tail 60
    exit $LASTEXITCODE
}

$broker = Join-Path $outputDirectory 'PrivateDesktopBroker.exe'
& $compiler '-Mdelphi' '-O2' '-gl' "-FU$brokerUnits" "-o$broker" `
    "$PSScriptRoot\native\PrivateDesktopBroker.lpr" *> (Join-Path $outputDirectory 'broker-build.log')
if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath (Join-Path $outputDirectory 'broker-build.log') -Tail 60
    exit $LASTEXITCODE
}
$results = Join-Path $outputDirectory 'tests.log'
$testTemp = Join-Path $outputDirectory 'temporary-data'
New-Item -ItemType Directory -Path $testTemp -Force | Out-Null
$previousTemp = $env:TEMP
$previousTmp = $env:TMP
try {
    $env:TEMP = $testTemp
    $env:TMP = $testTemp
    & $broker (Join-Path $outputDirectory 'ExperimentalWalletTests.exe') $results *> (Join-Path $outputDirectory 'broker.log')
    $status = $LASTEXITCODE
} finally {
    $env:TEMP = $previousTemp
    $env:TMP = $previousTmp
}
if (Test-Path -LiteralPath $results) { Get-Content -LiteralPath $results }
if ($status -eq 0) {
    $heapReport = "$results.heap.log"
    if (-not (Test-Path -LiteralPath $heapReport) -or
        -not ([IO.File]::ReadAllText($heapReport) -match '(?m)^0 unfreed memory blocks')) {
        Write-Error 'The experimental wallet suite did not produce a clean heap report.' -ErrorAction Continue
        $status = 1
    }
}
if ($status -ne 0) {
    Get-Content -LiteralPath (Join-Path $outputDirectory 'broker.log') -Tail 40
    if (Test-Path -LiteralPath "$results.progress.log") {
        Get-Content -LiteralPath "$results.progress.log" -Tail 30
    }
}
Write-Host "Experimental wallet test evidence: $outputDirectory"
exit $status
