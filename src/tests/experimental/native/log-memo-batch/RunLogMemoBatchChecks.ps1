# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [string]$FpcPath=$(if ($c = Get-Command fpc -ErrorAction SilentlyContinue) { $c.Source } else { '' }),
    [string]$LazarusDirectory=$(if ($c = Get-Command lazbuild -ErrorAction SilentlyContinue) { Split-Path -Parent $c.Source } else { '' }),
    [Parameter(Mandatory=$true)][string]$OutputDirectory,
    [switch]$Classic
)
$ErrorActionPreference='Stop'
$compiler=(Resolve-Path -LiteralPath $FpcPath).Path
$lazarus=(Resolve-Path -LiteralPath $LazarusDirectory).Path
$source=(Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../../../..')).Path
$targetInfo=((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if($LASTEXITCODE-ne0-or$targetInfo.Count-ne2-or$targetInfo[0]-ne'x86_64'-or$targetInfo[1]-ne'win64'){
    throw 'The native memo regression requires Windows x64 Free Pascal.'
}
$target=$targetInfo -join '-'
$output=[IO.Path]::GetFullPath($OutputDirectory)
if(Test-Path -LiteralPath $output){throw 'Choose a fresh output directory.'}
foreach($directory in @('units','broker-units','temporary-data')){
    New-Item -ItemType Directory -Path (Join-Path $output $directory) -Force|Out-Null
}
$programName=if($Classic){'ClassicLogMemoBatchProbe'}else{'LogMemoBatchProbe'}
$exe=Join-Path $output ($programName+'.exe')
$arguments=@('-B','-MDelphi','-Scghi','-O2','-WG','-dRELEASE','-dLCL','-dLCLwin32',
    '-dBorland','-dVer150','-dDelphi7','-dCompiler6_Up','-dPUREPASCAL',
    ('-FU'+(Join-Path $output 'units')),('-FE'+$output),('-o'+$exe),
    ('-Fi'+(Join-Path $source 'libraries/generics.collections/inc')),('-Fu'+$PSScriptRoot),
    ('-Fu'+$lazarus+'/lcl/units/'+$target+'/win32'),('-Fu'+$lazarus+'/lcl/units/'+$target),
    ('-Fu'+$lazarus+'/components/lazutils/lib/'+$target),('-Fu'+$lazarus+'/packager/units/'+$target))
$directories=@('core','gui-experimental','gui-experimental/core.utils','gui-experimental/wizards/wallet',
    'gui-experimental/wizards/operations','gui-classic','libraries/simplebaselib4pascal','libraries/hashlib4pascal',
    'libraries/cryptolib4pascal','libraries/synapse','libraries/sphere10','libraries/generics.collections',
    'libraries/pascalcoin','libraries/paszlib','libraries/abstractmem','libraries/regex','libraries/gnugettext')
if($Classic){$directories=@('gui-classic')+@($directories|Where-Object{$_-ne'gui-classic'-and$_-notlike'gui-experimental*'})}
foreach($directory in $directories){
    $arguments+='-Fu'+(Join-Path $source $directory)
}
$arguments+=Join-Path $PSScriptRoot ($programName+'.lpr')
$arguments|Set-Content -LiteralPath (Join-Path $output 'compiler-arguments.txt')
& $compiler @arguments *> (Join-Path $output 'build.log')
if($LASTEXITCODE-ne0){Get-Content -LiteralPath (Join-Path $output 'build.log') -Tail 40;throw 'Native memo probe did not compile.'}
# Use the tracked runtime so these probes also run from a clean checkout.
$crypto=Join-Path (Split-Path -Parent $source) 'windows-dll/libcrypto-1_1-x64.dll'
Copy-Item -LiteralPath $crypto -Destination $output
$broker=Join-Path $output 'LogMemoBatchBroker.exe'
& $compiler '-MDelphi' '-O2' ('-FU'+(Join-Path $output 'broker-units')) ('-o'+$broker) (Join-Path $PSScriptRoot 'LogMemoBatchBroker.lpr') *> (Join-Path $output 'broker-build.log')
if($LASTEXITCODE-ne0){throw 'Native memo probe broker did not compile.'}
$oldTemp,$oldTmp=$env:TEMP,$env:TMP
try{
    $env:TEMP=Join-Path $output 'temporary-data';$env:TMP=$env:TEMP
    $log=Join-Path $output 'checks.log'
    & $broker $exe $log *> (Join-Path $output 'runtime-broker.log')
    $code=$LASTEXITCODE
}finally{$env:TEMP,$env:TMP=$oldTemp,$oldTmp}
if(-not(Test-Path -LiteralPath $log)){throw 'Native memo probe omitted its result log.'}
$text=[IO.File]::ReadAllText($log)
Write-Output $text
if($code-ne0-or$text-match'(?m)^FAIL'-or$text-notmatch'(?m)^PASS 8\s*$'){
    throw 'Native memo batch checks failed; evidence is retained in the output directory.'
}
