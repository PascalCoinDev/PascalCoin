# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [Parameter(Mandatory=$true)][string]$OutputDirectory,
    [string]$FpcPath='fpc'
)
$ErrorActionPreference='Stop'
$compiler=(Get-Command $FpcPath -ErrorAction Stop).Source
$target=((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
if($LASTEXITCODE-ne0-or$target.Count-ne2-or$target[0]-ne'x86_64'-or$target[1]-ne'win64'){
    throw 'This native runner requires Windows x64 Free Pascal.'
}
$source=(Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../../../..')).Path
$output=[IO.Path]::GetFullPath($OutputDirectory)
if(Test-Path -LiteralPath $output){throw 'Choose a fresh output directory.'}
foreach($directory in @('units','temporary-data')){New-Item -ItemType Directory -Path (Join-Path $output $directory) -Force|Out-Null}
$exe=Join-Path $output 'BufferedAdoptionProbe.exe'
$arguments=@('-B','-MDelphi','-O2','-gl','-Cr','-Co','-Ci','-dBorland','-dVer150','-dDelphi7','-dCompiler6_Up','-dPUREPASCAL',('-FU'+(Join-Path $output 'units')),('-FE'+$output),('-o'+$exe),('-Fi'+(Join-Path $source 'libraries\generics.collections\inc')),('-Fu'+$PSScriptRoot))
foreach($directory in @('core','libraries\simplebaselib4pascal','libraries\hashlib4pascal','libraries\cryptolib4pascal','libraries\synapse','libraries\sphere10','libraries\generics.collections','libraries\pascalcoin','libraries\paszlib','libraries\abstractmem','libraries\regex')){
    $arguments+='-Fu'+(Join-Path $source $directory)
}
$arguments+=Join-Path $PSScriptRoot 'BufferedAdoptionProbe.lpr'
$arguments|Set-Content -LiteralPath (Join-Path $output 'compiler-arguments.txt')
& $compiler @arguments *> (Join-Path $output 'build.log')
if($LASTEXITCODE-ne0){Get-Content -LiteralPath (Join-Path $output 'build.log') -Tail 40;throw 'Buffered adoption probe did not compile.'}
Copy-Item -LiteralPath (Join-Path $source 'libcrypto-1_1-x64.dll') -Destination $output
Copy-Item -LiteralPath (Join-Path $source 'tests\experimental\fixtures\mainnet-blocks-0-4.bin') -Destination $output
$oldTemp,$oldTmp=$env:TEMP,$env:TMP
try{
    $env:TEMP=Join-Path $output 'temporary-data';$env:TMP=$env:TEMP
    $log=Join-Path $output 'checks.log'
    & $exe $log *> (Join-Path $output 'stdout.log')
    $code=$LASTEXITCODE
}finally{$env:TEMP,$env:TMP=$oldTemp,$oldTmp}
if(-not(Test-Path -LiteralPath $log)){Get-Content -LiteralPath (Join-Path $output 'stdout.log');throw 'Buffered adoption probe omitted its log.'}
$text=[IO.File]::ReadAllText($log)
$heap=[IO.File]::ReadAllText($log+'.heap.log')
Write-Output $text
if($code-ne0-or$text-match'(?m)^FAIL'-or$text-notmatch'(?m)^PASS 11\s*$'-or$heap-notmatch'(?m)^0 unfreed memory blocks'){
    throw 'Buffered adoption checks failed; retained evidence is in the output directory.'
}
