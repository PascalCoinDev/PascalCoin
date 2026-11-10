param([Parameter(Mandatory=$true)][string]$OutputDirectory)
$ErrorActionPreference = 'Stop'

# Keep shipping visibility unchanged. Extended native checks compile this copy
# after moving three declarations; every other source character must match.
$destination = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $destination) { throw 'Choose a new output directory.' }
$source = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..\..\..\..\core\UNetProtocol.pas'))
$text = [IO.File]::ReadAllText($source)
$pattern = '(?m)^    (?:Procedure|Function) (?:DoProcess_GetPendingOperations_Request\(|DoProcess_GetPendingOperations;|ReadTcpClientBuffer\()[^\r\n]*\r?\n'
$declarations = [regex]::Matches($text, $pattern)
if ($declarations.Count -ne 3) { throw 'Expected exactly three protocol declarations.' }
$copy = $text
$lines = @()
foreach ($declaration in $declarations) {
    $copy = $copy.Replace($declaration.Value, '')
    $lines += $declaration.Value.TrimEnd([char[]]"`r`n")
}
$withoutDeclarations = $copy
$anchor = [regex]::Match($copy, '(?m)^  protected\r?\n(?=    // Existing processing owners finish locally held block frames without)')
if (-not $anchor.Success) { throw 'Expected the protected transport section.' }
$newline = if ($anchor.Value.EndsWith("`r`n")) { "`r`n" } else { "`n" }
$insert = $anchor.Value + ($lines -join $newline) + $newline
$copy = $copy.Remove($anchor.Index, $anchor.Length).Insert($anchor.Index, $insert)
if (([regex]::Matches($copy, $pattern).Count -ne 3) -or
    ([regex]::Replace($copy, $pattern, '') -cne $withoutDeclarations)) {
    throw 'Protocol access copy changed more than declaration visibility.'
}
$coreDirectory = Join-Path $destination 'src\core'
New-Item -ItemType Directory -Path $coreDirectory | Out-Null
$target = Join-Path $coreDirectory 'UNetProtocol.pas'
[IO.File]::WriteAllText($target, $copy, [Text.UTF8Encoding]::new($false))
Copy-Item -LiteralPath (Join-Path (Split-Path (Split-Path $source)) 'config.inc') -Destination (Join-Path $destination 'src\config.inc')
[pscustomobject]@{
    source = $source
    sourceSha256 = (Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash
    copy = $target
    copySha256 = (Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash
    movedDeclarations = 3
    otherSourceTextUnchanged = $true
} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $destination 'source-equivalence.json')
Write-Output $coreDirectory
