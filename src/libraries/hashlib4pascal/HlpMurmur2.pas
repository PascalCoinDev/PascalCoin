unit HlpMurmur2;

{$I HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpConverters,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpMultipleTransformNonBlock,
  HlpNullable;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TMurmur2 = class sealed(TMultipleTransformNonBlock, IHash32, IHashWithKey,
    ITransformBlock)

  strict private
  var
    FKey, FWorkingKey, FH: UInt32;

  const
    CKEY = UInt32($0);
    M = UInt32($5BD1E995);
    R = Int32(24);

    function InternalComputeBytes(const AData: THashLibByteArray): Int32;
    procedure TransformUInt32Fast(ABlock: UInt32); inline;
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

{ TMurmur2 }

constructor TMurmur2.Create;
begin
  Inherited Create(4, 4);
  FKey := CKEY;
end;

function TMurmur2.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(FKey);
end;

procedure TMurmur2.SetKey(const AValue: THashLibByteArray);
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
    FKey := TConverters.ReadBytesAsUInt32LE(PByte(AValue), 0);
  end;
end;

procedure TMurmur2.TransformUInt32Fast(ABlock: UInt32);
begin
  ABlock := ABlock * M;
  ABlock := ABlock xor (ABlock shr R);
  ABlock := ABlock * M;

  FH := FH * M;
  FH := FH xor ABlock;
end;

function TMurmur2.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmur2.Initialize;
begin
  FWorkingKey := FKey;
  inherited Initialize();
end;

function TMurmur2.InternalComputeBytes(const AData: THashLibByteArray): Int32;
var
  LLength, LCurrentIndex: Int32;
  LBlock: UInt32;
  LPtrData: PByte;
begin
  LLength := System.Length(AData);
  LPtrData := PByte(AData);

  if (LLength = 0) then
  begin
    result := 0;
    Exit;
  end;

  FH := FWorkingKey xor UInt32(LLength);
  LCurrentIndex := 0;

  while (LLength >= 4) do
  begin
    LBlock := TConverters.ReadBytesAsUInt32LE(LPtrData, LCurrentIndex);
    TransformUInt32Fast(LBlock);
    System.Inc(LCurrentIndex, 4);
    System.Dec(LLength, 4);
  end;

  case LLength of
    3:
      begin
        FH := FH xor (AData[LCurrentIndex + 2] shl 16);

        FH := FH xor (AData[LCurrentIndex + 1] shl 8);

        FH := FH xor (AData[LCurrentIndex]);

        FH := FH * M;
      end;

    2:
      begin
        FH := FH xor (AData[LCurrentIndex + 1] shl 8);

        FH := FH xor (AData[LCurrentIndex]);

        FH := FH * M;
      end;

    1:
      begin
        FH := FH xor (AData[LCurrentIndex]);

        FH := FH * M;
      end;
  end;

  FH := FH xor (FH shr 13);

  FH := FH * M;
  FH := FH xor (FH shr 15);

  result := Int32(FH);
end;

function TMurmur2.Clone(): IHash;
var
  LHashInstance: TMurmur2;
begin
  LHashInstance := TMurmur2.Create();
  LHashInstance.FKey := FKey;
  LHashInstance.FWorkingKey := FWorkingKey;
  LHashInstance.FH := FH;
  FBuffer.Position := 0;
  LHashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TMurmur2.ComputeAggregatedBytes(const AData: THashLibByteArray)
  : IHashResult;
begin
  result := THashResult.Create(InternalComputeBytes(AData));
end;

end.
