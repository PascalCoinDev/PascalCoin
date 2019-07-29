unit HlpMurmur2_64;

{$I HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpMultipleTransformNonBlock,
  HlpNullable;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TMurmur2_64 = class sealed(TMultipleTransformNonBlock, IHash64, IHashWithKey,
    ITransformBlock)

  strict private
  var
    FKey, FWorkingKey: UInt32;

  const
    CKEY = UInt32($0);
{$IFDEF FPC}
    // to bypass Internal error (200706094) on FPC, We use "Typed Constant".

    M: UInt64 = UInt64($C6A4A7935BD1E995);

{$ELSE}
    M = UInt64($C6A4A7935BD1E995);
{$ENDIF FPC}
    R = Int32(47);

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

  strict protected
    function ComputeAggregatedBytes(const AData: THashLibByteArray)
      : IHashResult; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TMurmur2_64 }

function TMurmur2_64.Clone(): IHash;
var
  LHashInstance: TMurmur2_64;
begin
  LHashInstance := TMurmur2_64.Create();
  LHashInstance.FKey := FKey;
  LHashInstance.FWorkingKey := FWorkingKey;
  FBuffer.Position := 0;
  LHashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TMurmur2_64.ComputeAggregatedBytes(const AData: THashLibByteArray)
  : IHashResult;
var
  LLength, LCurrentIndex: Int32;
  LH, LK: UInt64;
  LPtrData: PByte;
begin
  LLength := System.length(AData);
  LPtrData := PByte(AData);

  if (LLength = 0) then
  begin
    result := THashResult.Create(UInt64(0));
    Exit;
  end;

  LH := FWorkingKey xor UInt64(LLength);
  LCurrentIndex := 0;

  while (LLength >= 8) do
  begin

    LK := TConverters.ReadBytesAsUInt64LE(LPtrData, LCurrentIndex);

    LK := LK * M;
    LK := LK xor (LK shr R);
    LK := LK * M;

    LH := LH xor LK;
    LH := LH * M;

    System.Inc(LCurrentIndex, 8);
    System.Dec(LLength, 8);

  end;

  case LLength of
    7:
      begin
        LH := LH xor ((UInt64(AData[LCurrentIndex]) shl 48));
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 40);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 32);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 24);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 16);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 8);
        System.Inc(LCurrentIndex);

        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

    6:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 40);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 32);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 24);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 16);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 8);
        System.Inc(LCurrentIndex);

        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

    5:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 32);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 24);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 16);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 8);
        System.Inc(LCurrentIndex);

        LH := LH xor UInt64(AData[LCurrentIndex]);
        LH := LH * M;
      end;

    4:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 24);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 16);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 8);
        System.Inc(LCurrentIndex);

        LH := LH xor UInt64(AData[LCurrentIndex]);
        LH := LH * M;
      end;

    3:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 16);
        System.Inc(LCurrentIndex);

        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 8);
        System.Inc(LCurrentIndex);

        LH := LH xor UInt64(AData[LCurrentIndex]);
        LH := LH * M;
      end;

    2:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex]) shl 8);
        System.Inc(LCurrentIndex);

        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

    1:
      begin
        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

  end;

  LH := LH xor (LH shr R);
  LH := LH * M;
  LH := LH xor (LH shr R);

  result := THashResult.Create(LH);
end;

constructor TMurmur2_64.Create;
begin
  Inherited Create(8, 8);
  FKey := CKEY;
end;

function TMurmur2_64.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(FKey);
end;

function TMurmur2_64.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmur2_64.Initialize;
begin
  FWorkingKey := FKey;
  Inherited Initialize();
end;

procedure TMurmur2_64.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := CKEY;
  end
  else
  begin
    if System.length(AValue) <> KeyLength.value then
    begin
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    end;
    FKey := TConverters.ReadBytesAsUInt32LE(PByte(AValue), 0);
  end;
end;

end.
