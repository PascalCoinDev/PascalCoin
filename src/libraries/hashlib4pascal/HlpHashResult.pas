unit HlpHashResult;

{$I HashLib.inc}

interface

uses
  SysUtils,
{$IFDEF FPC}
  base64,
{$ELSE}
{$IFDEF HAS_DELPHI_NET_ENCODING}
  System.NetEncoding,
{$ELSE}
  Classes,
  EncdDecd,
{$ENDIF HAS_DELPHI_NET_ENCODING}
{$ENDIF FPC}
  HlpBits,
  HlpHashLibTypes,
  HlpIHashResult,
  HlpConverters,
  HlpArrayUtils;

resourcestring
  SImpossibleRepresentationInt32 =
    'Current Data Structure cannot be Represented as an "Int32" Type.';
  SImpossibleRepresentationUInt8 =
    'Current Data Structure cannot be Represented as an "UInt8" Type.';
  SImpossibleRepresentationUInt16 =
    'Current Data Structure cannot be Represented as an "UInt16" Type.';
  SImpossibleRepresentationUInt32 =
    'Current Data Structure cannot be Represented as an "UInt32" Type.';
  SImpossibleRepresentationUInt64 =
    'Current Data Structure cannot be Represented as an "UInt64" Type.';

type
  THashResult = class sealed(TInterfacedObject, IHashResult)

  strict private
  var
    FHash: THashLibByteArray;

  public

    constructor Create(AHash: Int32); overload;
    constructor Create(AHash: UInt8); overload;
    constructor Create(AHash: UInt16); overload;
    constructor Create(AHash: UInt32); overload;
    constructor Create(AHash: UInt64); overload;
    constructor Create(const AHash: THashLibByteArray); overload;

    function GetBytes(): THashLibByteArray;
    function GetUInt8(): UInt8;
    function GetUInt16(): UInt16;
    function GetUInt32(): UInt32;
    function GetInt32(): Int32;
    function GetUInt64(): UInt64;
    function ToString(AGroup: Boolean = False): String; reintroduce;
    function Equals(const AHashResult: IHashResult): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ THashResult }

constructor THashResult.Create(AHash: UInt64);
begin
  Inherited Create();
  FHash := THashLibByteArray.Create(Byte(AHash shr 56), Byte(AHash shr 48),
    Byte(AHash shr 40), Byte(AHash shr 32), Byte(AHash shr 24),
    Byte(AHash shr 16), Byte(AHash shr 8), Byte(AHash));
end;

constructor THashResult.Create(const AHash: THashLibByteArray);
begin
  Inherited Create();
  FHash := AHash;
end;

constructor THashResult.Create(AHash: UInt32);
begin
  Inherited Create();
  FHash := THashLibByteArray.Create(Byte(AHash shr 24), Byte(AHash shr 16),
    Byte(AHash shr 8), Byte(AHash));
end;

constructor THashResult.Create(AHash: UInt8);
begin
  Inherited Create();
  FHash := THashLibByteArray.Create(AHash);
end;

constructor THashResult.Create(AHash: UInt16);
begin
  Inherited Create();
  FHash := THashLibByteArray.Create(Byte(AHash shr 8), Byte(AHash));
end;

constructor THashResult.Create(AHash: Int32);
begin
  Inherited Create();
  FHash := THashLibByteArray.Create(Byte(TBits.Asr32(AHash, 24)),
    Byte(TBits.Asr32(AHash, 16)), Byte(TBits.Asr32(AHash, 8)), Byte(AHash));
end;

function THashResult.Equals(const AHashResult: IHashResult): Boolean;
begin
  result := TArrayUtils.ConstantTimeAreEqual(AHashResult.GetBytes(), FHash);
end;

function THashResult.GetBytes: THashLibByteArray;
begin
  result := FHash;
end;

function THashResult.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}

var
  LResult: UInt32;
  LIdx, LTop: Int32;
  LTemp: String;
{$IFDEF HAS_DELPHI_NET_ENCODING}
  LTempHolder: THashLibByteArray;
{$ELSE}
{$IFDEF DELPHI}
  LTempHolder: TBytesStream;
{$ENDIF DELPHI}
{$ENDIF HAS_DELPHI_NET_ENCODING}
{$IFDEF FPC}
  LTempHolder: String;
{$ENDIF FPC}
begin

{$IFDEF HAS_DELPHI_NET_ENCODING}
  LTempHolder := Self.FHash;
{$ELSE}
{$IFDEF DELPHI}
  LTempHolder := TBytesStream.Create(Self.FHash);
{$ENDIF DELPHI}
{$ENDIF HAS_DELPHI_NET_ENCODING}
{$IFDEF FPC}
  LTempHolder := EncodeStringBase64(TConverters.ConvertBytesToString(Self.FHash,
    TEncoding.UTF8));
{$ENDIF FPC}
{$IFDEF HAS_DELPHI_NET_ENCODING}
  LTemp := StringReplace(TNetEncoding.base64.EncodeBytesToString(LTempHolder),
    sLineBreak, '', [rfReplaceAll]);
{$ELSE}
{$IFDEF DELPHI}
  try
    LTemp := StringReplace(String(EncodeBase64(LTempHolder.Memory,
      LTempHolder.Size)), sLineBreak, '', [rfReplaceAll]);
  finally
    LTempHolder.Free;
  end;
{$ENDIF DELPHI}
{$ENDIF HAS_DELPHI_NET_ENCODING}
{$IFDEF FPC}
  LTemp := LTempHolder;
{$ENDIF FPC}
  LTemp := AnsiUpperCase(LTemp);

  LResult := 0;
{$IFDEF DELPHIXE3_UP}
  LIdx := System.Low(LTemp);
  LTop := System.High(LTemp);
{$ELSE}
  LIdx := 1;
  LTop := System.Length(LTemp);
{$ENDIF DELPHIXE3_UP}
  while LIdx <= LTop do
  begin
    LResult := TBits.RotateLeft32(LResult, 5);
    LResult := LResult xor UInt32(LTemp[LIdx]);
    System.Inc(LIdx);
  end;

  result := LResult;
end;

function THashResult.GetInt32: Int32;
begin
  if (System.Length(FHash) <> 4) then
  begin
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationInt32);
  end;
  result := Int32((Int32(FHash[0]) shl 24) or (Int32(FHash[1]) shl 16) or
    (Int32(FHash[2]) shl 8) or (Int32(FHash[3])));
end;

function THashResult.GetUInt8: UInt8;
begin
  if (System.Length(FHash) <> 1) then
  begin
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt8);
  end;
  result := (UInt8(FHash[0]));
end;

function THashResult.GetUInt16: UInt16;
begin
  if (System.Length(FHash) <> 2) then
  begin
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt16);
  end;
  result := (UInt16(FHash[0]) shl 8) or (UInt16(FHash[1]));
end;

function THashResult.GetUInt32: UInt32;
begin
  if (System.Length(FHash) <> 4) then
  begin
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt32);
  end;
  result := (UInt32(FHash[0]) shl 24) or (UInt32(FHash[1]) shl 16) or
    (UInt32(FHash[2]) shl 8) or (UInt32(FHash[3]));
end;

function THashResult.GetUInt64: UInt64;
begin
  if (System.Length(FHash) <> 8) then
  begin
    raise EInvalidOperationHashLibException.CreateRes
      (@SImpossibleRepresentationUInt64);
  end;
  result := (UInt64(FHash[0]) shl 56) or (UInt64(FHash[1]) shl 48) or
    (UInt64(FHash[2]) shl 40) or (UInt64(FHash[3]) shl 32) or
    (UInt64(FHash[4]) shl 24) or (UInt64(FHash[5]) shl 16) or
    (UInt64(FHash[6]) shl 8) or (UInt64(FHash[7]));
end;

function THashResult.ToString(AGroup: Boolean): String;
begin
  result := TConverters.ConvertBytesToHexString(FHash, AGroup);
end;

end.
