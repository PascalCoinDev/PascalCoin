unit HlpArrayUtils;

{$I HashLib.inc}

interface

uses
  SysUtils,
  HlpHashLibTypes;

type
  TArrayUtils = class sealed(TObject)

  public

    class function AreEqual(const ABuffer1, ABuffer2: THashLibByteArray)
      : Boolean; overload; static;

    class function ConstantTimeAreEqual(const ABuffer1,
      ABuffer2: THashLibByteArray): Boolean; static;

    class procedure Fill(const ABuffer: THashLibByteArray; AFrom, ATo: Int32;
      AFiller: Byte); overload; static;

    class procedure Fill(const ABuffer: THashLibUInt32Array; AFrom, ATo: Int32;
      AFiller: UInt32); overload; static;

    class procedure Fill(const ABuffer: THashLibUInt64Array; AFrom, ATo: Int32;
      AFiller: UInt64); overload; static;

    class procedure ZeroFill(const ABuffer: THashLibByteArray);
      overload; static;

    class procedure ZeroFill(const ABuffer: THashLibUInt32Array);
      overload; static;

    class procedure ZeroFill(const ABuffer: THashLibUInt64Array);
      overload; static;

  end;

implementation

{ TArrayUtils }

class function TArrayUtils.AreEqual(const ABuffer1,
  ABuffer2: THashLibByteArray): Boolean;
begin
  if System.Length(ABuffer1) <> System.Length(ABuffer2) then
  begin
    Result := false;
    Exit;
  end;
  Result := CompareMem(ABuffer1, ABuffer2, System.Length(ABuffer1) *
    System.SizeOf(Byte));
end;

{$B+}

class function TArrayUtils.ConstantTimeAreEqual(const ABuffer1,
  ABuffer2: THashLibByteArray): Boolean;
var
  LIdx: Int32;
  LDiff: UInt32;
begin
  LDiff := UInt32(System.Length(ABuffer1)) xor UInt32(System.Length(ABuffer2));
  LIdx := 0;
  while (LIdx <= System.High(ABuffer1)) and (LIdx <= System.High(ABuffer2)) do
  begin
    LDiff := LDiff or (UInt32(ABuffer1[LIdx] xor ABuffer2[LIdx]));
    System.Inc(LIdx);
  end;
  Result := LDiff = 0;
end;

{$B-}

class procedure TArrayUtils.Fill(const ABuffer: THashLibByteArray;
  AFrom, ATo: Int32; AFiller: Byte);
begin
  if ABuffer <> Nil then
  begin
    System.FillChar(ABuffer[AFrom], (ATo - AFrom) *
      System.SizeOf(Byte), AFiller);
  end;
end;

class procedure TArrayUtils.Fill(const ABuffer: THashLibUInt32Array;
  AFrom, ATo: Int32; AFiller: UInt32);
begin
  if ABuffer <> Nil then
  begin
{$IFDEF FPC}
    System.FillDWord(ABuffer[AFrom], (ATo - AFrom), AFiller);
{$ELSE}
    while AFrom < ATo do
    begin
      ABuffer[AFrom] := AFiller;
      System.Inc(AFrom);
    end;
{$ENDIF}
  end;
end;

class procedure TArrayUtils.Fill(const ABuffer: THashLibUInt64Array;
  AFrom, ATo: Int32; AFiller: UInt64);
begin
  if ABuffer <> Nil then
  begin
{$IFDEF FPC}
    System.FillQWord(ABuffer[AFrom], (ATo - AFrom), AFiller);
{$ELSE}
    while AFrom < ATo do
    begin
      ABuffer[AFrom] := AFiller;
      System.Inc(AFrom);
    end;
{$ENDIF}
  end;
end;

class procedure TArrayUtils.ZeroFill(const ABuffer: THashLibByteArray);
begin
  TArrayUtils.Fill(ABuffer, 0, System.Length(ABuffer), Byte(0));
end;

class procedure TArrayUtils.ZeroFill(const ABuffer: THashLibUInt32Array);
begin
  TArrayUtils.Fill(ABuffer, 0, System.Length(ABuffer), UInt32(0));
end;

class procedure TArrayUtils.ZeroFill(const ABuffer: THashLibUInt64Array);
begin
  TArrayUtils.Fill(ABuffer, 0, System.Length(ABuffer), UInt64(0));
end;

end.
