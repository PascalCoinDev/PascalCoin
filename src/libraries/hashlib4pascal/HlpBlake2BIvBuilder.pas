unit HlpBlake2BIvBuilder;

{$I HashLib.inc}

interface

uses
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpConverters,
  HlpBlake2BTreeConfig,
  HlpIBlake2BConfig,
  HlpIBlake2BTreeConfig,
  HlpHashLibTypes;

resourcestring
  SInvalidHashSize =
    '"HashSize" Must Be Greater Than 0 And Less Than or Equal To 64';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 64';
  SInvalidPersonalisationLength =
    '"Personalisation" Length Must Be Equal To 16';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 16';
  STreeIncorrectInnerHashSize =
    'Tree Inner Hash Size Must Not Be Greater Than 64';

type
  TBlake2BIvBuilder = class sealed(TObject)

  strict private
    class var

      FSequentialTreeConfig: IBlake2BTreeConfig;

    class procedure VerifyConfigB(const AConfig: IBlake2BConfig;
      const ATreeConfig: IBlake2BTreeConfig; AIsSequential: Boolean); static;

    class constructor Blake2BIvBuilder();

  public
    class function ConfigB(const AConfig: IBlake2BConfig;
      var ATreeConfig: IBlake2BTreeConfig): THashLibUInt64Array; static;

  end;

implementation

{ TBlake2BIvBuilder }

class procedure TBlake2BIvBuilder.VerifyConfigB(const AConfig: IBlake2BConfig;
  const ATreeConfig: IBlake2BTreeConfig; AIsSequential: Boolean);
begin

  // digest length
  if ((AConfig.HashSize <= 0) or (AConfig.HashSize > 64)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidHashSize);
  end;

  // Key length
  if (AConfig.Key <> Nil) then
  begin
    if (System.Length(AConfig.Key) > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidKeyLength);
    end;
  end;

  // Salt length
  if (AConfig.Salt <> Nil) then
  begin
    if (System.Length(AConfig.Salt) <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidSaltLength);
    end;
  end;

  // Personalisation length
  if (AConfig.Personalisation <> Nil) then
  begin
    if (System.Length(AConfig.Personalisation) <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@SInvalidPersonalisationLength);
    end;
  end;

  // Tree InnerHashSize
  if (ATreeConfig <> Nil) then
  begin

    if ((not AIsSequential) and ((ATreeConfig.InnerHashSize <= 0))) then
    begin
      raise EArgumentOutOfRangeHashLibException.Create
        ('treeConfig.TreeIntermediateHashSize');
    end;

    if (ATreeConfig.InnerHashSize > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@STreeIncorrectInnerHashSize);
    end;
  end;

end;

class constructor TBlake2BIvBuilder.Blake2BIvBuilder;
begin
  FSequentialTreeConfig := TBlake2BTreeConfig.Create();
  FSequentialTreeConfig.FanOut := 1;
  FSequentialTreeConfig.MaxDepth := 1;
  FSequentialTreeConfig.LeafSize := 0;
  FSequentialTreeConfig.NodeOffset := 0;
  FSequentialTreeConfig.NodeDepth := 0;
  FSequentialTreeConfig.InnerHashSize := 0;
  FSequentialTreeConfig.IsLastNode := False;
end;

class function TBlake2BIvBuilder.ConfigB(const AConfig: IBlake2BConfig;
  var ATreeConfig: IBlake2BTreeConfig): THashLibUInt64Array;
var
  LIsSequential: Boolean;
  LBuffer: THashLibByteArray;
begin
  LIsSequential := ATreeConfig = Nil;
  if (LIsSequential) then
  begin
    ATreeConfig := FSequentialTreeConfig;
  end;

  VerifyConfigB(AConfig, ATreeConfig, LIsSequential);

  System.SetLength(LBuffer, 64);

  LBuffer[0] := AConfig.HashSize;
  LBuffer[1] := System.Length(AConfig.Key);

  if ATreeConfig <> Nil then
  begin
    LBuffer[2] := ATreeConfig.FanOut;
    LBuffer[3] := ATreeConfig.MaxDepth;
    TConverters.ReadUInt32AsBytesLE(ATreeConfig.LeafSize, LBuffer, 4);
    TConverters.ReadUInt64AsBytesLE(ATreeConfig.NodeOffset, LBuffer, 8);
    LBuffer[16] := ATreeConfig.NodeDepth;
    LBuffer[17] := ATreeConfig.InnerHashSize;
  end;

  if AConfig.Salt <> Nil then
  begin
    System.Move(AConfig.Salt[0], LBuffer[32], 16 * System.SizeOf(Byte));
  end;

  if AConfig.Personalisation <> Nil then
  begin
    System.Move(AConfig.Personalisation[0], LBuffer[48],
      16 * System.SizeOf(Byte));
  end;

  System.SetLength(Result, 8);
  TConverters.le64_copy(PByte(LBuffer), 0, PUInt64(Result), 0,
    System.Length(LBuffer) * System.SizeOf(Byte));
end;

end.
