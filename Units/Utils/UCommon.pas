{ Copyright (c) 2017 PascalCoin Developers

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  CREDITS:
  [2017-06-29] Herman Schoenfeld (herman@sphere10.com): Created unit, added IFF functions
  [2017-08-10] Herman Schoenfeld (herman@sphere10.com): Added String2Hex, BinStrComp functions
}

unit UCommon;


{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface


{ Converts a string to hexidecimal format }
function String2Hex(const Buffer: AnsiString): AnsiString;

{ Binary-safe StrComp replacement. StrComp will return 0 for when str1 and str2 both start with NUL character. }
function BinStrComp(const Str1, Str2 : AnsiString): Integer;

{ Language-level tools }
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Cardinal): Cardinal; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Integer): Integer; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Int64): Int64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: UInt64): UInt64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Double): Double; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: string): string; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: variant): variant; overload;

type
  { TArrayTool }
  TArrayTool<T> = class
    public
      class procedure Swap(var Values : array of T; Item1Index, Item2Index : SizeInt);
    end;


implementation

uses
  Classes, SysUtils, Math;

function String2Hex(const Buffer: AnsiString): AnsiString;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Buffer) do
    Result := LowerCase(Result + IntToHex(Ord(Buffer[n]), 2));
end;

function BinStrComp(const Str1, Str2: AnsiString): integer;
var Str1Len, Str2Len, i : Integer;
begin
   Str1Len := Length(Str1);
   Str2Len := Length(Str2);
   if (Str1Len < Str2Len) then
     Result := -1
   else if (Str1Len > Str2Len) then
     Result := 1
   else begin
     Result := 0;
     For i:= 1 to Str1Len do begin
       if Str1[i] < Str2[i] then begin
         Result := -1;
         break;
       end else if Str1[i] > Str2[i] then begin
         Result := 1;
         break;
       end
     end;
   end;
End;

{%region Language-level tools }
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Cardinal): Cardinal;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Integer): Integer;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Int64): Int64;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: UInt64): UInt64;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Double): Double;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: string): string;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: variant): variant;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;
{%endregion}

{%region TArrayTool }

class procedure TArrayTool<T>.Swap(var Values : array of T; Item1Index, Item2Index : SizeInt);
var temp : T; len, recSize : SizeInt; itemSize : SizeInt;
begin
  len := Length(Values);
  recSize := SizeOf(T);
  if (Item1Index < 0) OR (Item1Index > len) then Raise Exception.Create('Invalid Parameter: Item1Index out of bounds');
  if (Item2Index < 0) OR (Item2Index > len) then Raise Exception.Create('Invalid Parameter: Item2Index out of bounds');
  temp := Values[Item1Index];
  Values[Item1Index] := Values[Item2Index];
  Values[Item2Index] := temp;
end;

{%endregion}


end.

