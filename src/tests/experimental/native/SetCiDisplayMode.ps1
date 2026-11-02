<#
.SYNOPSIS
Prepares an ephemeral GitHub Windows runner for native 150-percent UI checks.
.DESCRIPTION
Only GitHub-hosted runners may change mode. InspectOnly reads modes without
changing the local display. No registry, DPI or persistent display settings change.
#>
param(
    [switch] $InspectOnly,
    [string] $ReportPath
)

$ErrorActionPreference = 'Stop'
if ([Environment]::OSVersion.Platform -ne [PlatformID]::Win32NT) {
    throw 'This native test display helper requires Windows.'
}
if (-not $InspectOnly -and
    ($env:RUNNER_ENVIRONMENT -ne 'github-hosted' -or $env:GITHUB_ACTIONS -ne 'true')) {
    throw 'Display changes are restricted to ephemeral GitHub-hosted runners. Use -InspectOnly locally.'
}

if (-not ('PascalCoinCiDisplay' -as [type])) {
    Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;

public static class PascalCoinCiDisplay {
    // DEVMODEW has a fixed 220-byte layout on both Windows architectures.
    // See Microsoft wingdi.h/DEVMODEW; unused display/printer fields stay opaque.
    [StructLayout(LayoutKind.Explicit, Size = 220)]
    public struct Mode {
        [FieldOffset(68)] public ushort Size;
        [FieldOffset(70)] public ushort DriverExtra;
        [FieldOffset(72)] public uint Fields;
        [FieldOffset(168)] public uint BitsPerPixel;
        [FieldOffset(172)] public uint Width;
        [FieldOffset(176)] public uint Height;
        [FieldOffset(184)] public uint Frequency;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Rect {
        public int Left;
        public int Top;
        public int Right;
        public int Bottom;
    }

    [DllImport("user32.dll", EntryPoint = "EnumDisplaySettingsW",
        CharSet = CharSet.Unicode, ExactSpelling = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool NativeEnumDisplaySettings(string device, int index, ref Mode mode);

    [DllImport("user32.dll", EntryPoint = "ChangeDisplaySettingsExW",
        CharSet = CharSet.Unicode, ExactSpelling = true)]
    private static extern int NativeChangeDisplaySettings(string device, ref Mode mode,
        IntPtr window, uint flags, IntPtr parameter);

    [DllImport("user32.dll")]
    [return: MarshalAs(UnmanagedType.Bool)]
    public static extern bool SystemParametersInfoW(uint action, uint parameter,
        out Rect rectangle, uint flags);

    [DllImport("user32.dll")]
    [return: MarshalAs(UnmanagedType.Bool)]
    public static extern bool SetProcessDPIAware();

    public static Mode ReadMode(int index) {
        var mode = new Mode();
        mode.Size = (ushort)Marshal.SizeOf(typeof(Mode));
        if (!NativeEnumDisplaySettings(null, index, ref mode)) {
            throw new InvalidOperationException("Cannot read the current display mode.");
        }
        return mode;
    }

    public static Mode[] AvailableModes() {
        var modes = new System.Collections.Generic.List<Mode>();
        for (var index = 0; ; index++) {
            var mode = new Mode();
            mode.Size = (ushort)Marshal.SizeOf(typeof(Mode));
            if (!NativeEnumDisplaySettings(null, index, ref mode)) {
                return modes.ToArray();
            }
            modes.Add(mode);
        }
    }

    public static int ApplyMode(Mode mode, uint flags) {
        // Keep the native null device pointer inside C#: PowerShell can coerce
        // a null string argument to an invalid empty display-device name.
        mode.Size = (ushort)Marshal.SizeOf(typeof(Mode));
        return NativeChangeDisplaySettings(null, ref mode, IntPtr.Zero, flags, IntPtr.Zero);
    }

    public static Rect WorkArea() {
        Rect rectangle;
        if (!SystemParametersInfoW(0x0030, 0, out rectangle, 0)) {
            throw new InvalidOperationException("Cannot read the native work area.");
        }
        return rectangle;
    }
}
'@
}

# This affects only this helper process, so reported rectangles use device pixels.
[void][PascalCoinCiDisplay]::SetProcessDPIAware()
$before = [PascalCoinCiDisplay]::ReadMode(-1)
$beforeArea = [PascalCoinCiDisplay]::WorkArea()
$modes = @([PascalCoinCiDisplay]::AvailableModes() | Where-Object {
    $_.BitsPerPixel -ge 32 -and $_.Width -ge 1920 -and $_.Height -ge 1080
})

$changed = $false
try {
    if (-not $InspectOnly -and
        ($before.Width -lt 1920 -or $before.Height -lt 1080)) {
        # Prefer Full HD; retain a driver's supported alternative if it tests valid.
        $requested = [PascalCoinCiDisplay]::ReadMode(-1)
        $requested.Width = 1920
        $requested.Height = 1080
        $candidates = @($requested) + @($modes | Sort-Object Width, Height, Frequency)
        $selected = $false
        foreach ($candidate in $candidates) {
            $candidate.Fields = 0x001C0000 # DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT
            $candidate.DriverExtra = 0
            $candidate.BitsPerPixel = 32
            $test = [PascalCoinCiDisplay]::ApplyMode($candidate, 2)
            if ($test -ne 0) { continue }
            # Flags=0 applies dynamically; CDS_UPDATEREGISTRY is intentionally absent.
            $apply = [PascalCoinCiDisplay]::ApplyMode($candidate, 0)
            if ($apply -eq 0) {
                $selected = $true
                $changed = $true
                break
            }
        }
        if (-not $selected) { throw 'No supported Full HD test display mode could be applied.' }
    }

    $after = [PascalCoinCiDisplay]::ReadMode(-1)
    $area = [PascalCoinCiDisplay]::WorkArea()
    if (-not $InspectOnly -and
        (($area.Right - $area.Left) -lt 1200 -or ($area.Bottom - $area.Top) -lt 1020)) {
        throw 'The native test work area is too small for the fixed-size 150-percent dialogs.'
    }
} finally {
    $after = [PascalCoinCiDisplay]::ReadMode(-1)
    $area = [PascalCoinCiDisplay]::WorkArea()
    $report = [ordered]@{
        InspectOnly = [bool]$InspectOnly
        Changed = $changed
        Before = @{ Width = $before.Width; Height = $before.Height; WorkArea = $beforeArea }
        After = @{ Width = $after.Width; Height = $after.Height; WorkArea = $area }
        SupportedFullHdModes = @($modes | Select-Object Width, Height, BitsPerPixel, Frequency -Unique)
    }
    $json = $report | ConvertTo-Json -Depth 5
    if ($ReportPath) { $json | Set-Content -LiteralPath $ReportPath }
    Write-Output $json
}
