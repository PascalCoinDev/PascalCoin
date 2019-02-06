{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpStringUtils;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  StrUtils,
  ClpBits,
  ClpCryptoLibTypes;

type
  TStringUtils = class sealed(TObject)
  public
    class function GetStringHashCode(const Input: string): Int32; static;
    class function SplitString(const Input: string; Delimiter: Char)
      : TCryptoLibStringArray; static;
    class function BeginsWith(const Input, SubString: string;
      IgnoreCase: Boolean; Offset: Int32 = 1): Boolean; static;

    class function EndsWith(const Input, SubString: String; IgnoreCase: Boolean)
      : Boolean; static;

    class function LastIndexOf(const Input, SubString: string;
      IgnoreCase: Boolean): Int32; overload; static; inline;

    class function LastIndexOf(const Input, SubString: string;
      StartIndex, Count: Int32; IgnoreCase: Boolean): Int32; overload; static;

  end;

implementation

{ TStringUtils }

class function TStringUtils.EndsWith(const Input, SubString: String;
  IgnoreCase: Boolean): Boolean;
var
  SubStringLength: Int32;
  TempString: String;
begin
  SubStringLength := System.Length(SubString);
  Result := SubStringLength > 0;
  if Result then
  begin
    TempString := System.Copy(Input, System.Length(Input) - SubStringLength + 1,
      SubStringLength);
    Result := System.Length(TempString) = SubStringLength;
    if Result then
    begin
      if IgnoreCase then
      begin
        Result := CompareText(TempString, SubString) = 0
      end
      else
      begin
        Result := TempString = SubString;
      end;
    end;
  end;
end;

class function TStringUtils.GetStringHashCode(const Input: string): Int32;
var
  LowPoint, HighPoint: Int32;
  LResult: UInt32;
begin
  LResult := 0;
{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(Input);
  HighPoint := System.High(Input);
{$ELSE}
  LowPoint := 1;
  HighPoint := System.Length(Input);
{$ENDIF DELPHIXE3_UP}
  while LowPoint <= HighPoint do
  begin
    LResult := TBits.RotateLeft32(LResult, 5);
    LResult := LResult xor UInt32(Input[LowPoint]);
    System.Inc(LowPoint);
  end;
  Result := Int32(LResult);
end;

class function TStringUtils.LastIndexOf(const Input, SubString: string;
  StartIndex, Count: Int32; IgnoreCase: Boolean): Int32;
var
  I, L, LS, M: Int32;
  S: String;
  P: PChar;

begin
  Result := -1;
  LS := System.Length(Input);
  L := System.Length(SubString);
  if (L = 0) or (L > LS) then
  begin
    Exit;
  end;
  P := PChar(SubString);
  S := Input;
  I := StartIndex + 1; // 1 based
  if (I > LS) then
  begin
    I := LS;
  end;
  I := I - L + 1;
  M := StartIndex - Count + 1; // 1 based
  if M < 1 then
  begin
    M := 1;
  end;

  while (Result = -1) and (I >= M) do
  begin
    if IgnoreCase then
    begin
      if (StrLiComp(PChar(@S[I]), P, L) = 0) then
      begin
        Result := I - 1;
      end;
    end
    else
    begin
      if (StrLComp(PChar(@S[I]), P, L) = 0) then
      begin
        Result := I - 1;
      end;
    end;
    Dec(I);
  end;
end;

class function TStringUtils.LastIndexOf(const Input, SubString: string;
  IgnoreCase: Boolean): Int32;
begin
  Result := LastIndexOf(Input, SubString, System.Length(Input) - 1,
    System.Length(Input), IgnoreCase);
end;

class function TStringUtils.SplitString(const Input: string; Delimiter: Char)
  : TCryptoLibStringArray;
var
  PosStart, PosDel, SplitPoints, I, LowPoint, HighPoint, Len: Int32;
begin
  Result := Nil;
  if Input <> '' then
  begin
    { Determine the length of the resulting array }
{$IFDEF DELPHIXE3_UP}
    LowPoint := System.Low(Input);
    HighPoint := System.High(Input);
{$ELSE}
    LowPoint := 1;
    HighPoint := System.Length(Input);
{$ENDIF DELPHIXE3_UP}
    SplitPoints := 0;
    for I := LowPoint to HighPoint do
    begin
      if (Delimiter = Input[I]) then
        System.Inc(SplitPoints);
    end;

    System.SetLength(Result, SplitPoints + 1);

    { Split the string and fill the resulting array }

    I := 0;
    Len := System.Length(Delimiter);
    PosStart := 1;
    PosDel := System.Pos(Delimiter, Input);
    while PosDel > 0 do
    begin
      Result[I] := System.Copy(Input, PosStart, PosDel - PosStart);
      PosStart := PosDel + Len;
      PosDel := PosEx(Delimiter, Input, PosStart);
      System.Inc(I);
    end;
    Result[I] := System.Copy(Input, PosStart, System.Length(Input));
  end;
end;

class function TStringUtils.BeginsWith(const Input, SubString: string;
  IgnoreCase: Boolean; Offset: Int32): Boolean;
var
  L: Int32;
  PtrInput, PtrSubString: PChar;
begin
  L := System.Length(SubString);
  Result := L > 0;
  PtrInput := PChar(Input);
  System.Inc(PtrInput, Offset - 1);
  PtrSubString := PChar(SubString);
  if Result then
  begin
    if IgnoreCase then
    begin
      Result := StrLiComp(PtrSubString, PtrInput, L) = 0
    end
    else
    begin
      Result := StrLComp(PtrSubString, PtrInput, L) = 0
    end;
  end;
end;

end.
