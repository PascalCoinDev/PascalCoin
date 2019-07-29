unit HlpXXHash64;

{$I HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpNullable,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TXXHash64 = class sealed(THash, IHash64, IHashWithKey, ITransformBlock)

  strict private
  var
    FKey, FHash: UInt64;

  const
    CKEY = UInt64(0);

{$IFDEF FPC}
    // to bypass Internal error (200706094) on FPC, We use "Typed Constant".
    PRIME64_1: UInt64 = (11400714785074694791);
    PRIME64_2: UInt64 = (14029467366897019727);
    PRIME64_3: UInt64 = (1609587929392839161);
    PRIME64_4: UInt64 = (9650029242287828579);
    PRIME64_5: UInt64 = (2870177450012600261);
{$ELSE}
    PRIME64_1 = UInt64(11400714785074694791);
    PRIME64_2 = UInt64(14029467366897019727);
    PRIME64_3 = UInt64(1609587929392839161);
    PRIME64_4 = UInt64(9650029242287828579);
    PRIME64_5 = UInt64(2870177450012600261);
{$ENDIF FPC}
    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

  type

    TXXH_State = record

    private
    var
      FTotalLength, FV1, FV2, FV3, FV4: UInt64;
      FMemorySize: UInt32;
      FMemory: THashLibByteArray;

      function Clone(): TXXH_State; inline;

    end;

  strict private
  var
    FState: TXXH_State;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TXXHash64.TXXH_State }

function TXXHash64.TXXH_State.Clone(): TXXH_State;
begin
  result := Default (TXXH_State);
  result.FTotalLength := FTotalLength;
  result.FMemorySize := FMemorySize;
  result.FV1 := FV1;
  result.FV2 := FV2;
  result.FV3 := FV3;
  result.FV4 := FV4;
  result.FMemory := System.Copy(FMemory);
end;

{ TXXHash64 }

function TXXHash64.Clone(): IHash;
var
  LHashInstance: TXXHash64;
begin
  LHashInstance := TXXHash64.Create();
  LHashInstance.FKey := FKey;
  LHashInstance.FHash := FHash;
  LHashInstance.FState := FState.Clone();
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TXXHash64.Create;
begin
  Inherited Create(8, 32);
  FKey := CKEY;
  System.SetLength(FState.FMemory, 32);
end;

function TXXHash64.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt64AsBytesLE(FKey);
end;

function TXXHash64.GetKeyLength: TNullableInteger;
begin
  result := 8;
end;

procedure TXXHash64.Initialize;
begin
  FHash := 0;
  FState.FV1 := FKey + PRIME64_1 + PRIME64_2;
  FState.FV2 := FKey + PRIME64_2;
  FState.FV3 := FKey + 0;
  FState.FV4 := FKey - PRIME64_1;
  FState.FTotalLength := 0;
  FState.FMemorySize := 0;
end;

procedure TXXHash64.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := CKEY;
  end
  else
  begin
    if System.Length(AValue) <> KeyLength.value then
    begin
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    end;
    FKey := TConverters.ReadBytesAsUInt64LE(PByte(AValue), 0);
  end;
end;

procedure TXXHash64.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  V1, V2, V3, V4: UInt64;
  LPtrLimit, LPtrEnd, LPtrADataStart, LPtrMemoryStart, LPtrMemory: PByte;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LPtrADataStart := PByte(AData) + AIndex;
  LPtrMemory := PByte(FState.FMemory);
  FState.FTotalLength := FState.FTotalLength + UInt64(ALength);

  if ((FState.FMemorySize + UInt32(ALength)) < UInt32(32)) then
  begin

    LPtrMemoryStart := PByte(FState.FMemory) + FState.FMemorySize;

    System.Move(LPtrADataStart^, LPtrMemoryStart^, ALength);

    FState.FMemorySize := FState.FMemorySize + UInt32(ALength);
    Exit;
  end;

  LPtrEnd := LPtrADataStart + UInt32(ALength);

  if FState.FMemorySize > 0 then
  begin
    LPtrMemoryStart := PByte(FState.FMemory) + FState.FMemorySize;
    System.Move(LPtrADataStart^, LPtrMemoryStart^, 32 - FState.FMemorySize);

    FState.FV1 := PRIME64_1 * TBits.RotateLeft64(FState.FV1 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(LPtrMemory, 0), 31);
    FState.FV2 := PRIME64_1 * TBits.RotateLeft64(FState.FV2 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(LPtrMemory, 8), 31);
    FState.FV3 := PRIME64_1 * TBits.RotateLeft64(FState.FV3 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(LPtrMemory, 16), 31);
    FState.FV4 := PRIME64_1 * TBits.RotateLeft64(FState.FV4 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(LPtrMemory, 24), 31);

    LPtrADataStart := LPtrADataStart + (32 - FState.FMemorySize);
    FState.FMemorySize := 0;
  end;

  if LPtrADataStart <= (LPtrEnd - 32) then
  begin
    V1 := FState.FV1;
    V2 := FState.FV2;
    V3 := FState.FV3;
    V4 := FState.FV4;

    LPtrLimit := LPtrEnd - 32;
    repeat
      V1 := PRIME64_1 * TBits.RotateLeft64
        (V1 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE
        (LPtrADataStart, 0), 31);
      V2 := PRIME64_1 * TBits.RotateLeft64
        (V2 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE
        (LPtrADataStart, 8), 31);
      V3 := PRIME64_1 * TBits.RotateLeft64
        (V3 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE
        (LPtrADataStart, 16), 31);
      V4 := PRIME64_1 * TBits.RotateLeft64
        (V4 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE
        (LPtrADataStart, 24), 31);

      System.Inc(LPtrADataStart, 32);
    until not(LPtrADataStart <= LPtrLimit);

    FState.FV1 := V1;
    FState.FV2 := V2;
    FState.FV3 := V3;
    FState.FV4 := V4;
  end;

  if LPtrADataStart < LPtrEnd then
  begin
    LPtrMemoryStart := PByte(FState.FMemory);
    System.Move(LPtrADataStart^, LPtrMemoryStart^, LPtrEnd - LPtrADataStart);
    FState.FMemorySize := LPtrEnd - LPtrADataStart;
  end;
end;

function TXXHash64.TransformFinal: IHashResult;
var
  V1, V2, V3, V4: UInt64;
  LPtrEnd, LPtrBuffer: PByte;
begin

  if FState.FTotalLength >= UInt64(32) then
  begin
    V1 := FState.FV1;
    V2 := FState.FV2;
    V3 := FState.FV3;
    V4 := FState.FV4;

    FHash := TBits.RotateLeft64(V1, 1) + TBits.RotateLeft64(V2, 7) +
      TBits.RotateLeft64(V3, 12) + TBits.RotateLeft64(V4, 18);

    V1 := TBits.RotateLeft64(V1 * PRIME64_2, 31) * PRIME64_1;
    FHash := (FHash xor V1) * PRIME64_1 + PRIME64_4;

    V2 := TBits.RotateLeft64(V2 * PRIME64_2, 31) * PRIME64_1;
    FHash := (FHash xor V2) * PRIME64_1 + PRIME64_4;

    V3 := TBits.RotateLeft64(V3 * PRIME64_2, 31) * PRIME64_1;
    FHash := (FHash xor V3) * PRIME64_1 + PRIME64_4;

    V4 := TBits.RotateLeft64(V4 * PRIME64_2, 31) * PRIME64_1;
    FHash := (FHash xor V4) * PRIME64_1 + PRIME64_4;
  end
  else
    FHash := FKey + PRIME64_5;

  System.Inc(FHash, FState.FTotalLength);

  LPtrBuffer := PByte(FState.FMemory);
  LPtrEnd := LPtrBuffer + FState.FMemorySize;

  while (LPtrBuffer + 8) <= LPtrEnd do
  begin
    FHash := FHash xor (PRIME64_1 * TBits.RotateLeft64(PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(LPtrBuffer, 0), 31));
    FHash := TBits.RotateLeft64(FHash, 27) * PRIME64_1 + PRIME64_4;
    System.Inc(LPtrBuffer, 8);
  end;

  if (LPtrBuffer + 4) <= LPtrEnd then
  begin
    FHash := FHash xor TConverters.ReadBytesAsUInt32LE(LPtrBuffer, 0) *
      PRIME64_1;
    FHash := TBits.RotateLeft64(FHash, 23) * PRIME64_2 + PRIME64_3;
    System.Inc(LPtrBuffer, 4);
  end;

  while LPtrBuffer < LPtrEnd do
  begin
    FHash := FHash xor LPtrBuffer^ * PRIME64_5;
    FHash := TBits.RotateLeft64(FHash, 11) * PRIME64_1;
    System.Inc(LPtrBuffer);
  end;

  FHash := FHash xor (FHash shr 33);
  FHash := FHash * PRIME64_2;
  FHash := FHash xor (FHash shr 29);
  FHash := FHash * PRIME64_3;
  FHash := FHash xor (FHash shr 32);

  result := THashResult.Create(FHash);
  Initialize();
end;

end.
