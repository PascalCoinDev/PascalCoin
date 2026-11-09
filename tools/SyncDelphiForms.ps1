<#
.SYNOPSIS
Generates VCL DFM companions from the canonical Lazarus LFM resources.
.DESCRIPTION
Maps the supported LCL controls and properties to VCL, preserves PNG artwork
bytes, and uses the existing LCL/Win32 image-list readers and serializer for
native image-list streams. Original LFM files are never modified. The native
converter is compiled into the ignored tools/lib directory when needed.

Use -AssetsOnly or -PropertiesOnly to repair existing DFM companions without
regenerating their other layout settings. Both switches may be combined.
.EXAMPLE
.\SyncDelphiForms.ps1 -AssetsOnly -PropertiesOnly
#>
# Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
param(
    [string] $SourceDirectory = (Join-Path $PSScriptRoot '..\src\gui-experimental'),
    [switch] $AssetsOnly,
    [switch] $PropertiesOnly,
    [string] $ImageListConverter,
    [string] $FpcPath = $(if ($c = Get-Command fpc -ErrorAction SilentlyContinue) { $c.Source } else { '' }),
    [string] $LazarusDirectory = $(if ($c = Get-Command lazbuild -ErrorAction SilentlyContinue) { Split-Path -Parent $c.Source } else { '' })
)

$ErrorActionPreference = 'Stop'
$source = (Resolve-Path -LiteralPath $SourceDirectory).Path
$encoding = [Text.UTF8Encoding]::new($true)
$converterSource = Join-Path $PSScriptRoot 'ConvertLazarusImageList.lpr'
$conversionOutput = Join-Path $PSScriptRoot 'lib\delphi-forms'
$conversionWork = Join-Path $conversionOutput ('assets-' + [guid]::NewGuid().ToString('N'))
$script:DelphiImageListConverter = $ImageListConverter
$script:DelphiPictureCount = 0
$script:DelphiImageListCount = 0

function ConvertFrom-ResourceHex([string] $Hex) {
    $compact = $Hex -replace '\s', ''
    if (($compact.Length % 2) -ne 0) { throw 'A resource contains an incomplete hex byte.' }
    $bytes = [byte[]]::new($compact.Length / 2)
    for ($index = 0; $index -lt $bytes.Length; $index++) {
        $bytes[$index] = [Convert]::ToByte($compact.Substring($index * 2, 2), 16)
    }
    return ,$bytes
}

function Get-ImageListConverter {
    if ($script:DelphiImageListConverter) {
        return (Resolve-Path -LiteralPath $script:DelphiImageListConverter).Path
    }
    $compiler = (Get-Command $FpcPath -ErrorAction Stop).Source
    $lazarus = (Resolve-Path -LiteralPath $LazarusDirectory).Path
    $targetInfo = ((& $compiler '-iTP' '-iTO') -join ' ').Trim() -split '\s+'
    if ($LASTEXITCODE -ne 0 -or $targetInfo.Count -ne 2 -or $targetInfo[1] -notmatch '^win(32|64)$') {
        throw 'Image-list conversion requires a Windows Free Pascal compiler and matching Lazarus units.'
    }
    $target = $targetInfo -join '-'
    $units = Join-Path $conversionOutput ('units-' + $target)
    New-Item -ItemType Directory -Path $conversionOutput, $units -Force | Out-Null
    $executable = Join-Path $conversionOutput 'ConvertLazarusImageList.exe'
    if (-not (Test-Path -LiteralPath $executable) -or
        (Get-Item -LiteralPath $executable).LastWriteTimeUtc -lt (Get-Item -LiteralPath $converterSource).LastWriteTimeUtc) {
        $arguments = @(
            '-MDelphi', '-O1', '-gl',
            "-Fu$lazarus\lcl\units\$target", "-Fu$lazarus\lcl\units\$target\win32",
            "-Fu$lazarus\components\lazutils\lib\$target", "-Fu$lazarus\packager\units\$target",
            "-FU$units", "-FE$conversionOutput", "-o$executable", $converterSource
        )
        & $compiler @arguments *> (Join-Path $conversionOutput 'converter-build.log')
        if ($LASTEXITCODE -ne 0) {
            Get-Content -LiteralPath (Join-Path $conversionOutput 'converter-build.log') -Tail 40
            throw 'The native image-list converter failed to compile.'
        }
    }
    $script:DelphiImageListConverter = $executable
    return $executable
}

function ConvertTo-DelphiAsset([byte[]] $Bytes, [string] $Property, [string] $AssetName) {
    if ($Property -eq 'Picture.Data' -and $Bytes.Length -gt 5) {
        $nameLength = [int]$Bytes[0]
        if ($Bytes.Length -gt ($nameLength + 5)) {
            $className = [Text.Encoding]::ASCII.GetString($Bytes, 1, $nameLength)
            if ($className -eq 'TPortableNetworkGraphic') {
                $dataStart = 1 + $nameLength + 4
                $declaredSize = [BitConverter]::ToUInt32($Bytes, 1 + $nameLength)
                if ($declaredSize -ne ($Bytes.Length - $dataStart)) {
                    throw "PNG length mismatch in $AssetName."
                }
                $signature = [BitConverter]::ToString($Bytes, $dataStart, 8)
                if ($signature -ne '89-50-4E-47-0D-0A-1A-0A') { throw "Invalid PNG in $AssetName." }
                # VCL TPicture stores its graphic class name followed by the
                # PNG itself. Lazarus adds a size prefix which VCL does not use.
                $vclName = [Text.Encoding]::ASCII.GetBytes('TPngImage')
                $result = [byte[]]::new(1 + $vclName.Length + $declaredSize)
                $result[0] = $vclName.Length
                [Array]::Copy($vclName, 0, $result, 1, $vclName.Length)
                [Array]::Copy($Bytes, $dataStart, $result, 1 + $vclName.Length, $declaredSize)
                $script:DelphiPictureCount++
                return ,$result
            }
        }
    }
    if ($Property -eq 'Bitmap' -and $Bytes.Length -gt 2 -and
        $Bytes[0] -eq 0x4C -and ($Bytes[1] -eq 0x69 -or $Bytes[1] -eq 0x7A)) {
        $converter = Get-ImageListConverter
        New-Item -ItemType Directory -Path $conversionWork -Force | Out-Null
        $inputFile = Join-Path $conversionWork ($AssetName + '.lazarus.bin')
        $outputFile = Join-Path $conversionWork ($AssetName + '.vcl.bin')
        [IO.File]::WriteAllBytes($inputFile, $Bytes)
        & $converter $inputFile $outputFile | Write-Host
        if ($LASTEXITCODE -ne 0) { throw "Image-list conversion failed for $AssetName." }
        $script:DelphiImageListCount++
        return ,([IO.File]::ReadAllBytes($outputFile))
    }
    return ,$Bytes
}

function Convert-DelphiAssets([string] $Text, [string] $BaseName) {
    $assetIndex = [int[]]@(0)
    $pattern = '(?m)^(?<indent>[ \t]*)(?<property>Picture\.Data|Bitmap)\s*=\s*\{(?<hex>[0-9A-Fa-f\s]+)\}'
    return [regex]::Replace($Text, $pattern, {
        param($match)
        $assetIndex[0]++
        $bytes = ConvertFrom-ResourceHex $match.Groups['hex'].Value
        $property = $match.Groups['property'].Value
        $converted = ConvertTo-DelphiAsset $bytes $property ($BaseName + '-' + $assetIndex[0])
        if ([Convert]::ToBase64String($bytes) -eq [Convert]::ToBase64String($converted)) { return $match.Value }
        $hex = [BitConverter]::ToString($converted).Replace('-', '')
        $indent = $match.Groups['indent'].Value
        $builder = [Text.StringBuilder]::new()
        [void]$builder.Append($indent + $property + " = {`r`n")
        for ($index = 0; $index -lt $hex.Length; $index += 64) {
            [void]$builder.Append($indent + '  ' + $hex.Substring($index, [Math]::Min(64, $hex.Length - $index)) + "`r`n")
        }
        [void]$builder.Append($indent + '}')
        return $builder.ToString()
    })
}

function Convert-DelphiProperties([string] $Text) {
    $inputLines = $Text -split '\r?\n'
    $outputLines = [Collections.Generic.List[string]]::new()
    $objects = [Collections.Generic.List[object]]::new()
    for ($index = 0; $index -lt $inputLines.Length; $index++) {
        $line = $inputLines[$index]
        if ($line -match '^(\s*)(?:object|inherited)\s+\w+:\s*(\w+)') {
            $kind = $Matches[2]
            $indent = $Matches[1].Length
            $hasClick = $false
            $hasPasswordChar = $false
            for ($next = $index + 1; $next -lt $inputLines.Length; $next++) {
                if ($inputLines[$next] -match ('^' + (' ' * $indent) + 'end\s*$')) { break }
                if ($inputLines[$next] -match ('^' + (' ' * ($indent + 2)) + 'OnClick\s*=')) { $hasClick = $true }
                if ($inputLines[$next] -match ('^' + (' ' * ($indent + 2)) + 'PasswordChar\s*=')) { $hasPasswordChar = $true }
            }
            $objects.Add([pscustomobject]@{ Kind = $kind; Indent = $indent; HasClick = $hasClick; HasPasswordChar = $hasPasswordChar })
            $outputLines.Add($line)
            continue
        }
        if ($objects.Count -gt 0) {
            $current = $objects[$objects.Count - 1]
            if ($current.Kind -in @('TCheckBox', 'TRadioButton') -and $line -match '^\s*OnChange\s*=') {
                # VCL check/radio state changes use OnClick. If both were
                # supplied, keep the existing click handler rather than write
                # duplicate event properties. The current account selector's
                # two handlers both call its same SearchFiltered method.
                if ($current.HasClick) { continue }
                $line = $line -replace '^(\s*)OnChange\s*=', '${1}OnClick ='
            }
            if ($current.Kind -in @('TSpeedButton', 'TGroupBox', 'TComboBox') -and $line -match '^\s*AutoSize\s*=') { continue }
            if ($current.Kind -in @('TOpenDialog', 'TSaveDialog') -and $line -match '^\s*(Width|Height)\s*=') { continue }
            if ($current.Kind -eq 'TRadioGroup' -and $line -match '^\s*AutoFill\s*=') { continue }
            if ($current.Kind -eq 'TDrawGrid' -and $line -match '^\s*(ExtendedSelect|TitleFont\.\w+)\s*=') { continue }
            if ($current.Kind -eq 'TEdit' -and $line -match '^\s*EchoMode\s*=\s*(\w+)') {
                if ($Matches[1] -eq 'emPassword' -and -not $current.HasPasswordChar) {
                    $outputLines.Add((' ' * ($current.Indent + 2)) + "PasswordChar = '*'")
                }
                continue
            }
            if ($line -match '^(\s*)end\s*$' -and $Matches[1].Length -eq $current.Indent) {
                $objects.RemoveAt($objects.Count - 1)
            }
        }
        $outputLines.Add($line)
    }
    return $outputLines -join "`r`n"
}

# Keep Lazarus resources canonical; generate paired VCL resources for Delphi.
foreach ($file in Get-ChildItem -LiteralPath $source -Filter '*.lfm' -Recurse) {
    if ($file.FullName -match '[\\/](backup|\.backup)[\\/]') { continue }
    if ($file.Name -eq 'UFRMWallet.lfm') { continue } # Historical, unused wallet.
    $destination = [IO.Path]::ChangeExtension($file.FullName, '.dfm')
    if ($AssetsOnly -or $PropertiesOnly) {
        if (-not (Test-Path -LiteralPath $destination)) { throw "No paired DFM exists: $destination" }
        $text = [IO.File]::ReadAllText($destination)
        $converted = $text
        if ($PropertiesOnly) { $converted = Convert-DelphiProperties $converted }
        if ($AssetsOnly) { $converted = Convert-DelphiAssets $converted $file.BaseName }
        if ($converted -cne $text) { [IO.File]::WriteAllText($destination, $converted, $encoding) }
        continue
    }
    $objects = [Collections.Generic.List[object]]::new()
    $lines = [Collections.Generic.List[string]]::new()
    foreach ($line in [IO.File]::ReadAllLines($file.FullName)) {
        if ($line -match '^(\s*)(?:object|inherited)\s+\w+:\s*(\w+)') {
            $kind = $Matches[2]
            $indent = $Matches[1].Length
            $objects.Add([pscustomobject]@{ Kind = $kind; Indent = $indent })
            $line = $line.Replace(': TPairSplitterSide', ': TPanel').Replace(': TPairSplitter', ': TPanel')
            $line = $line.Replace(': TFloatSpinEdit', ': TNumberBox')
            $line = $line.Replace(': TApplicationProperties', ': TApplicationEvents')
            $lines.Add($line)
            if ($kind -eq 'TPairSplitter') {
                $lines.Add((' ' * ($indent + 2)) + 'BevelOuter = bvNone')
            } elseif ($kind -eq 'TPairSplitterSide') {
                $lines.Add((' ' * ($indent + 2)) + 'BevelOuter = bvNone')
                $align = if ($line -match 'PairSplitterSide1:') { 'alLeft' } else { 'alClient' }
                $lines.Add((' ' * ($indent + 2)) + 'Align = ' + $align)
            } elseif ($kind -eq 'TFloatSpinEdit') {
                $lines.Add((' ' * ($indent + 2)) + 'Mode = nbmFloat')
            }
            continue
        }
        if ($line -match '^\s*(LCLVersion|BorderSpacing\.\w+|AnchorSide\w+\.\w+|ChildSizing\.\w+)\s*=') { continue }
        if ($objects.Count -gt 0) {
            $current = $objects[$objects.Count - 1]
            if ($objects.Count -gt 1 -and $line -match '^\s*Client(Width|Height)\s*=') { continue }
            if ($current.Kind -eq 'TPairSplitter' -and $line -match '^\s*Position\s*=') { continue }
            if ($current.Kind -eq 'TFloatSpinEdit') {
                $line = $line -replace '^(\s*)Increment\s*=', '${1}SmallStep ='
                $line = $line -replace '^(\s*)DecimalPlaces\s*=', '${1}Decimal ='
            }
            if ($file.Name -eq 'UFRMMainForm.lfm' -and $objects.Count -eq 1) {
                $line = $line.Replace('FormStyle = fsMDIForm', 'FormStyle = fsNormal')
            }
            if ($line -match '^(\s*)end\s*$' -and $Matches[1].Length -eq $current.Indent) {
                $objects.RemoveAt($objects.Count - 1)
            }
        }
        $line = $line.Replace('ssAutoBoth', 'ssBoth').Replace('ssAutoVertical', 'ssVertical').Replace('ssAutoHorizontal', 'ssHorizontal')
        $lines.Add($line)
    }
    $text = ($lines -join "`r`n") + "`r`n"
    $text = Convert-DelphiProperties $text
    [IO.File]::WriteAllText($destination, (Convert-DelphiAssets $text $file.BaseName), $encoding)
}
Write-Host ("Converted {0} PNG picture streams and {1} image lists; original LFM assets remain unchanged." -f
    $script:DelphiPictureCount, $script:DelphiImageListCount)
