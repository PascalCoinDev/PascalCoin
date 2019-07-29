unit HlpConverters;

{$I HashLib.inc}

interface

uses
  Classes,
  StrUtils,
  SysUtils,
  HlpHashLibTypes,
  HlpBits,
  HlpBitConverter;

resourcestring
  SEncodingInstanceNil = 'Encoding Instance Cannot Be Nil';

type
  TConverters = class sealed(TObject)

  strict private
    class function SplitString(const AInput: String; ADelimiter: Char)
      : THashLibStringArray; static;

{$IFDEF DEBUG}
    class procedure Check(const AInput: THashLibByteArray;
      AInputSize, AOutputSize: Int32); overload; static;
{$ENDIF DEBUG}
    class procedure swap_copy_str_to_u32(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32); static;

    class procedure swap_copy_str_to_u64(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32); static;

  public

    class function be2me_32(AInput: UInt32): UInt32; static; inline;

    class function be2me_64(AInput: UInt64): UInt64; static; inline;

    class function le2me_32(AInput: UInt32): UInt32; static; inline;

    class function le2me_64(AInput: UInt64): UInt64; static; inline;

    class procedure be32_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class procedure le32_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class procedure be64_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class procedure le64_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class function ReadBytesAsUInt32LE(AInput: PByte; AIndex: Int32): UInt32;
      static; inline;

    class function ReadBytesAsUInt64LE(AInput: PByte; AIndex: Int32): UInt64;
      static; inline;

    class function ReadUInt32AsBytesLE(AInput: UInt32): THashLibByteArray;
      overload; static; inline;

    class function ReadUInt64AsBytesLE(AInput: UInt64): THashLibByteArray;
      overload; static; inline;

    class procedure ReadUInt32AsBytesLE(AInput: UInt32;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class procedure ReadUInt32AsBytesBE(AInput: UInt32;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class procedure ReadUInt64AsBytesLE(AInput: UInt64;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class procedure ReadUInt64AsBytesBE(AInput: UInt64;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class function ConvertStringToBytes(const AInput: String;
      const AEncoding: TEncoding): THashLibByteArray; overload; static;

    class function ConvertBytesToString(const AInput: THashLibByteArray;
      const AEncoding: TEncoding): String; overload; static;

    class function ConvertHexStringToBytes(const AInput: String)
      : THashLibByteArray; static; inline;

    class function ConvertBytesToHexString(const AInput: THashLibByteArray;
      AGroup: Boolean): String; static;

  end;

implementation

{ TConverters }

{$IFDEF DEBUG}

class procedure TConverters.Check(const AInput: THashLibByteArray;
  AInputSize, AOutputSize: Int32);
begin
  System.Assert(((System.length(AInput) * AInputSize) mod AOutputSize) = 0);
end;

{$ENDIF DEBUG}

class procedure TConverters.swap_copy_str_to_u32(ASource: Pointer;
  ASourceIndex: Int32; ADestination: Pointer; ADestinationIndex: Int32;
  ASize: Int32);
var
  LPtrSourceStart, LPtrDestinationStart, LPtrSourceEnd: PCardinal;
  LPtrByteSourceStart: PByte;
  LLength: Int32;
begin
  // if all pointers and length are 32-bits aligned
  if ((Int32(PByte(ADestination) - PByte(0)) or (PByte(ASource) - PByte(0)) or
    ASourceIndex or ADestinationIndex or ASize) and 3) = 0 then
  begin
    // copy memory as 32-bit words
    LPtrSourceStart := PCardinal(PByte(ASource) + ASourceIndex);
    LPtrSourceEnd := PCardinal((PByte(ASource) + ASourceIndex) + ASize);
    LPtrDestinationStart := PCardinal(PByte(ADestination) + ADestinationIndex);
    while LPtrSourceStart < LPtrSourceEnd do
    begin
      LPtrDestinationStart^ := TBits.ReverseBytesUInt32(LPtrSourceStart^);
      System.Inc(LPtrDestinationStart);
      System.Inc(LPtrSourceStart);
    end;
  end
  else
  begin
    LPtrByteSourceStart := (PByte(ASource) + ASourceIndex);
    LLength := ASize + ADestinationIndex;
    while ADestinationIndex < LLength do
    begin
      PByte(ADestination)[ADestinationIndex xor 3] := LPtrByteSourceStart^;
      System.Inc(LPtrByteSourceStart);
      System.Inc(ADestinationIndex);
    end;
  end;
end;

class procedure TConverters.swap_copy_str_to_u64(ASource: Pointer;
  ASourceIndex: Int32; ADestination: Pointer; ADestinationIndex: Int32;
  ASize: Int32);
var
  LPtrSourceStart, LPtrDestinationStart, LPtrSourceEnd: PUInt64;
  LPtrByteSourceStart: PByte;
  LLength: Int32;
begin
  // if all pointers and length are 64-bits aligned
  if ((Int32(PByte(ADestination) - PByte(0)) or (PByte(ASource) - PByte(0)) or
    ASourceIndex or ADestinationIndex or ASize) and 7) = 0 then
  begin
    // copy aligned memory block as 64-bit integers
    LPtrSourceStart := PUInt64(PByte(ASource) + ASourceIndex);
    LPtrSourceEnd := PUInt64((PByte(ASource) + ASourceIndex) + ASize);
    LPtrDestinationStart := PUInt64(PByte(ADestination) + ADestinationIndex);
    while LPtrSourceStart < LPtrSourceEnd do
    begin
      LPtrDestinationStart^ := TBits.ReverseBytesUInt64(LPtrSourceStart^);
      System.Inc(LPtrDestinationStart);
      System.Inc(LPtrSourceStart);
    end;
  end
  else
  begin
    LPtrByteSourceStart := (PByte(ASource) + ASourceIndex);
    LLength := ASize + ADestinationIndex;
    while ADestinationIndex < LLength do
    begin
      PByte(ADestination)[ADestinationIndex xor 7] := LPtrByteSourceStart^;
      System.Inc(LPtrByteSourceStart);
      System.Inc(ADestinationIndex);
    end;
  end;
end;

class function TConverters.be2me_32(AInput: UInt32): UInt32;
begin
  if TBitConverter.IsLittleEndian then
  begin
    result := TBits.ReverseBytesUInt32(AInput)
  end
  else
  begin
    result := AInput;
  end;
end;

class function TConverters.be2me_64(AInput: UInt64): UInt64;
begin
  if TBitConverter.IsLittleEndian then
  begin
    result := TBits.ReverseBytesUInt64(AInput)
  end
  else
  begin
    result := AInput;
  end;
end;

class procedure TConverters.be32_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
  if TBitConverter.IsLittleEndian then
  begin
    swap_copy_str_to_u32(ASource, ASourceIndex, ADestination,
      ADestinationIndex, ASize)
  end
  else
  begin
    System.Move(Pointer(PByte(ASource) + ASourceIndex)^,
      Pointer(PByte(ADestination) + ADestinationIndex)^, ASize);
  end;
end;

class procedure TConverters.be64_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
  if TBitConverter.IsLittleEndian then
  begin
    swap_copy_str_to_u64(ASource, ASourceIndex, ADestination,
      ADestinationIndex, ASize)
  end
  else
  begin
    System.Move(Pointer(PByte(ASource) + ASourceIndex)^,
      Pointer(PByte(ADestination) + ADestinationIndex)^, ASize);
  end;
end;

class function TConverters.le2me_32(AInput: UInt32): UInt32;
begin
  if not TBitConverter.IsLittleEndian then
  begin
    result := TBits.ReverseBytesUInt32(AInput)
  end
  else
  begin
    result := AInput;
  end;
end;

class function TConverters.le2me_64(AInput: UInt64): UInt64;
begin
  if not TBitConverter.IsLittleEndian then
  begin
    result := TBits.ReverseBytesUInt64(AInput)
  end
  else
  begin
    result := AInput;
  end;
end;

class procedure TConverters.le32_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
  if TBitConverter.IsLittleEndian then
  begin
    System.Move(Pointer(PByte(ASource) + ASourceIndex)^,
      Pointer(PByte(ADestination) + ADestinationIndex)^, ASize)
  end
  else
  begin
    swap_copy_str_to_u32(ASource, ASourceIndex, ADestination,
      ADestinationIndex, ASize);
  end;
end;

class procedure TConverters.le64_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
  if TBitConverter.IsLittleEndian then
  begin
    System.Move(Pointer(PByte(ASource) + ASourceIndex)^,
      Pointer(PByte(ADestination) + ADestinationIndex)^, ASize)
  end
  else
  begin
    swap_copy_str_to_u64(ASource, ASourceIndex, ADestination,
      ADestinationIndex, ASize);
  end;
end;

class function TConverters.ReadBytesAsUInt32LE(AInput: PByte;
  AIndex: Int32): UInt32;
begin
{$IFDEF FPC}
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput[AIndex], result, System.SizeOf(UInt32));
{$ELSE}
  result := PCardinal(AInput + AIndex)^;
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
{$ELSE}
  // Delphi does not handle unaligned memory access on ARM Devices properly.
  System.Move(AInput[AIndex], result, System.SizeOf(UInt32));
{$ENDIF FPC}
  result := le2me_32(result);
end;

class function TConverters.ReadBytesAsUInt64LE(AInput: PByte;
  AIndex: Int32): UInt64;
begin
{$IFDEF FPC}
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput[AIndex], result, System.SizeOf(UInt64));
{$ELSE}
  result := PUInt64(AInput + AIndex)^;
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
{$ELSE}
  // Delphi does not handle unaligned memory access on ARM Devices properly.
  System.Move(AInput[AIndex], result, System.SizeOf(UInt64));
{$ENDIF FPC}
  result := le2me_64(result);
end;

class function TConverters.ReadUInt32AsBytesLE(AInput: UInt32)
  : THashLibByteArray;
begin
  result := THashLibByteArray.Create(Byte(AInput), Byte(AInput shr 8),
    Byte(AInput shr 16), Byte(AInput shr 24));
end;

class function TConverters.ReadUInt64AsBytesLE(AInput: UInt64)
  : THashLibByteArray;
begin
  result := THashLibByteArray.Create(Byte(AInput), Byte(AInput shr 8),
    Byte(AInput shr 16), Byte(AInput shr 24), Byte(AInput shr 32),
    Byte(AInput shr 40), Byte(AInput shr 48), Byte(AInput shr 56));
end;

class procedure TConverters.ReadUInt32AsBytesLE(AInput: UInt32;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
  AOutput[AIndex] := Byte(AInput);
  AOutput[AIndex + 1] := Byte(AInput shr 8);
  AOutput[AIndex + 2] := Byte(AInput shr 16);
  AOutput[AIndex + 3] := Byte(AInput shr 24);
end;

class procedure TConverters.ReadUInt32AsBytesBE(AInput: UInt32;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
  AOutput[AIndex] := Byte(AInput shr 24);
  AOutput[AIndex + 1] := Byte(AInput shr 16);
  AOutput[AIndex + 2] := Byte(AInput shr 8);
  AOutput[AIndex + 3] := Byte(AInput);
end;

class procedure TConverters.ReadUInt64AsBytesLE(AInput: UInt64;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
  AOutput[AIndex] := Byte(AInput);
  AOutput[AIndex + 1] := Byte(AInput shr 8);
  AOutput[AIndex + 2] := Byte(AInput shr 16);
  AOutput[AIndex + 3] := Byte(AInput shr 24);
  AOutput[AIndex + 4] := Byte(AInput shr 32);
  AOutput[AIndex + 5] := Byte(AInput shr 40);
  AOutput[AIndex + 6] := Byte(AInput shr 48);
  AOutput[AIndex + 7] := Byte(AInput shr 56);
end;

class procedure TConverters.ReadUInt64AsBytesBE(AInput: UInt64;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
  AOutput[AIndex] := Byte(AInput shr 56);
  AOutput[AIndex + 1] := Byte(AInput shr 48);
  AOutput[AIndex + 2] := Byte(AInput shr 40);
  AOutput[AIndex + 3] := Byte(AInput shr 32);
  AOutput[AIndex + 4] := Byte(AInput shr 24);
  AOutput[AIndex + 5] := Byte(AInput shr 16);
  AOutput[AIndex + 6] := Byte(AInput shr 8);
  AOutput[AIndex + 7] := Byte(AInput);
end;

class function TConverters.ConvertBytesToHexString(const AInput
  : THashLibByteArray; AGroup: Boolean): String;
var
  LIdx: Int32;
  LHexString, LWorkString: String;
  LTempArray: THashLibStringArray;
begin
  LHexString := UpperCase(TBitConverter.ToString(AInput));

  if System.length(AInput) = 1 then
  begin
    result := LHexString;
    Exit;
  end;

  if System.length(AInput) = 2 then
  begin
    result := StringReplace(LHexString, '-', '', [rfIgnoreCase, rfReplaceAll]);
    Exit;
  end;

  if (AGroup) then
  begin
{$IFDEF DEBUG}
    Check(AInput, 1, 4);
{$ENDIF DEBUG}
    LWorkString := UpperCase(TBitConverter.ToString(AInput));

    LTempArray := TConverters.SplitString(LWorkString, '-');
    LHexString := '';
    LIdx := 0;

    while LIdx < (System.length(LTempArray) shr 2) do
    begin
      if (LIdx <> 0) then
      begin
        LHexString := LHexString + '-';
      end;
      LHexString := LHexString + LTempArray[LIdx * 4] + LTempArray[LIdx * 4 + 1]
        + LTempArray[LIdx * 4 + 2] + LTempArray[LIdx * 4 + 3];
      System.Inc(LIdx);
    end;
  end
  else
  begin
    LHexString := StringReplace(LHexString, '-', '',
      [rfIgnoreCase, rfReplaceAll]);
  end;
  result := LHexString;
end;

class function TConverters.ConvertHexStringToBytes(const AInput: String)
  : THashLibByteArray;
var
  LInput: String;
begin
  LInput := AInput;
  LInput := StringReplace(LInput, '-', '', [rfIgnoreCase, rfReplaceAll]);

{$IFDEF DEBUG}
  System.Assert(System.length(LInput) and 1 = 0);
{$ENDIF DEBUG}
  System.SetLength(result, System.length(LInput) shr 1);

{$IFNDEF NEXTGEN}
  HexToBin(PChar(LInput), @result[0], System.length(result));
{$ELSE}
  HexToBin(PChar(LInput), 0, result, 0, System.length(LInput));
{$ENDIF !NEXTGEN}
end;

class function TConverters.ConvertStringToBytes(const AInput: String;
  const AEncoding: TEncoding): THashLibByteArray;
begin
  if AEncoding = Nil then
  begin
    raise EArgumentNilHashLibException.CreateRes(@SEncodingInstanceNil);
  end;

{$IFDEF FPC}
  result := AEncoding.GetBytes(UnicodeString(AInput));
{$ELSE}
  result := AEncoding.GetBytes(AInput);
{$ENDIF FPC}
end;

class function TConverters.ConvertBytesToString(const AInput: THashLibByteArray;
  const AEncoding: TEncoding): String;
begin
  if AEncoding = Nil then
  begin
    raise EArgumentNilHashLibException.CreateRes(@SEncodingInstanceNil);
  end;

{$IFDEF FPC}
  result := String(AEncoding.GetString(AInput));
{$ELSE}
  result := AEncoding.GetString(AInput);
{$ENDIF FPC}
end;

class function TConverters.SplitString(const AInput: String; ADelimiter: Char)
  : THashLibStringArray;
var
  LPosStart, LPosSkip, LSplitPoints, LIdx, LLowIndex, LHighIndex,
    LLength: Int32;
begin
  result := Nil;
  if AInput <> '' then
  begin
    { Determine the length of the resulting array }
    LSplitPoints := 0;
{$IFDEF DELPHIXE3_UP}
    LLowIndex := System.Low(AInput);
    LHighIndex := System.High(AInput);
{$ELSE}
    LLowIndex := 1;
    LHighIndex := System.length(AInput);
{$ENDIF DELPHIXE3_UP}
    for LIdx := LLowIndex to LHighIndex do
    begin
      if (ADelimiter = AInput[LIdx]) then
      begin
        System.Inc(LSplitPoints);
      end;
    end;

    System.SetLength(result, LSplitPoints + 1);

    { Split the string and fill the resulting array }

    LIdx := 0;
    LLength := System.length(ADelimiter);
{$IFDEF DELPHIXE3_UP}
    LPosStart := System.Low(AInput);
    LHighIndex := System.High(AInput);
{$ELSE}
    LPosStart := 1;
    LHighIndex := System.length(AInput);
{$ENDIF DELPHIXE3_UP}
    LPosSkip := System.Pos(ADelimiter, AInput);
    while LPosSkip > 0 do
    begin
      result[LIdx] := System.Copy(AInput, LPosStart, LPosSkip - LPosStart);
      LPosStart := LPosSkip + LLength;
      LPosSkip := PosEx(ADelimiter, AInput, LPosStart);
      System.Inc(LIdx);
    end;
    result[LIdx] := System.Copy(AInput, LPosStart, LHighIndex);
  end;
end;

end.
