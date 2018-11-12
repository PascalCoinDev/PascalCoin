unit ULogicalCPUCount;

// http://wiki.lazarus.freepascal.org/Example_of_multi-threaded_application:_array_of_threads#1._Detect_number_of_cores_available.

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe
  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}
{$MODE DELPHI}
{$HINTS OFF}
{$ENDIF FPC}

interface

{$IFDEF FPC}
{$IFDEF LINUX}

{$linklib c}
uses
  ctypes;
{$ENDIF LINUX}
{$IFDEF WINDOWS}

uses
  Windows;

{$ENDIF WINDOWS}
{$IF DEFINED(DARWIN) OR DEFINED(FREEBSD)}

uses
  ctypes, sysctl;
{$ENDIF}
{$ENDIF FPC}

type
  TLogicalCPUCount = class sealed(TObject)

  public
    //returns number of cores: a computer with two hyperthreaded cores will report 4
    class function GetLogicalCPUCount(): Int32; static;

  end;

implementation

{$IF DEFINED(LINUX)}
const
  _SC_NPROCESSORS_ONLN = 83;

function sysconf(i: cint): clong; cdecl; external Name 'sysconf';
{$ENDIF LINUX}

{ TLogicalCPUCount }

class function TLogicalCPUCount.GetLogicalCPUCount(): Int32;
{$IFDEF WINDOWS}
var
  LIdx: Int32;
  LProcessAffinityMask, LSystemAffinityMask: DWORD_PTR;
  LMask: DWORD;
  LSystemInfo: SYSTEM_INFO;
{$ENDIF WINDOWS}
{$IF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
var
  LMib: array[0..1] of cint;
  Llen, Lt: cint;
{$ENDIF}

begin
{$IFNDEF FPC}
  // For Delphi
  Result := CPUCount;
{$ELSE}
{$IF DEFINED(WINDOWS)}
  //returns total number of processors available to system including logical hyperthreaded processors
  if GetProcessAffinityMask(GetCurrentProcess, LProcessAffinityMask,
    LSystemAffinityMask) then
  begin
    Result := 0;
    for LIdx := 0 to 31 do
    begin
      LMask := DWORD(1) shl LIdx;
      if (LProcessAffinityMask and LMask) <> 0 then
      begin
        System.Inc(Result);
      end;
    end;
  end
  else
  begin
    // can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(LSystemInfo);
    Result := LSystemInfo.dwNumberOfProcessors;
  end;
  {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}

  LMib[0] := CTL_HW;
  LMib[1] := HW_NCPU;
  Llen := System.SizeOf(Lt);
  {$IF DEFINED(VER3_0_0) OR DEFINED(VER3_0_2)}
  fpsysctl(PChar(@LMib), 2, @Lt, @Llen, nil, 0);
  {$ELSE}
  fpsysctl(@LMib, 2, @Lt, @Llen, nil, 0);
  {$ENDIF}
  Result := Lt;

  {$ELSEIF DEFINED(LINUX)}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ELSE}
  // Fallback for other platforms
  Result := 1;
{$ENDIF WINDOWS}
{$ENDIF FPC}
end;

end.
