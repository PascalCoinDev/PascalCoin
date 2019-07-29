unit HlpBlake2SIvBuilder;

{$I HashLib.inc}

interface

uses
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpConverters,
  HlpBlake2STreeConfig,
  HlpIBlake2SConfig,
  HlpIBlake2STreeConfig,
  HlpHashLibTypes;

resourcestring
  SInvalidHashSize =
    '"HashSize" Must Be Greater Than 0 And Less Than or Equal To 32';
  SInvalidKeyLength = '"Key" Length Must Not Be Greater Than 32';
  SInvalidPersonalisationLength = '"Personalisation" Length Must Be Equal To 8';
  SInvalidSaltLength = '"Salt" Length Must Be Equal To 8';
  STreeIncorrectInnerHashSize =
    'Tree Inner Hash Size Must Not Be Greater Than 32';

type
  TBlake2SIvBuilder = class sealed(TObject)

  strict private
    class var

      FSequentialTreeConfig: IBlake2STreeConfig;

    class procedure VerifyConfigS(const AConfig: IBlake2SConfig;
      const ATreeConfig: IBlake2STreeConfig; AIsSequential: Boolean); static;

    class constructor Blake2SIvBuilder();

  public
    class function ConfigS(const AConfig: IBlake2SConfig;
      var ATreeConfig: IBlake2STreeConfig): THashLibUInt32Array; static;

  end;

implementation

{ TBlake2SIvBuilder }

class procedure TBlake2SIvBuilder.VerifyConfigS(const AConfig: IBlake2SConfig;
  const ATreeConfig: IBlake2STreeConfig; AIsSequential: Boolean);
begin

  // digest length
  if ((AConfig.HashSize <= 0) or (AConfig.HashSize > 32)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidHashSize);
  end;

  // Key length
  if (AConfig.Key <> Nil) then
  begin
    if (System.Length(AConfig.Key) > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidKeyLength);
    end;
  end;

  // Salt length
  if (AConfig.Salt <> Nil) then
  begin
    if (System.Length(AConfig.Salt) <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidSaltLength);
    end;
  end;

  // Personalisation length
  if (AConfig.Personalisation <> Nil) then
  begin
    if (System.Length(AConfig.Personalisation) <> 8) then
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

    if (ATreeConfig.InnerHashSize > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@STreeIncorrectInnerHashSize);
    end;
  end;

end;

class constructor TBlake2SIvBuilder.Blake2SIvBuilder;
begin
  FSequentialTreeConfig := TBlake2STreeConfig.Create();
  FSequentialTreeConfig.FanOut := 1;
  FSequentialTreeConfig.MaxDepth := 1;
  FSequentialTreeConfig.LeafSize := 0;
  FSequentialTreeConfig.NodeOffset := 0;
  FSequentialTreeConfig.NodeDepth := 0;
  FSequentialTreeConfig.InnerHashSize := 0;
  FSequentialTreeConfig.IsLastNode := False;
end;

class function TBlake2SIvBuilder.ConfigS(const AConfig: IBlake2SConfig;
  var ATreeConfig: IBlake2STreeConfig): THashLibUInt32Array;
var
  LIsSequential: Boolean;
  LBuffer: THashLibByteArray;
begin
  LIsSequential := ATreeConfig = Nil;
  if (LIsSequential) then
  begin
    ATreeConfig := FSequentialTreeConfig;
  end;

  VerifyConfigS(AConfig, ATreeConfig, LIsSequential);

  System.SetLength(LBuffer, 32);

  LBuffer[0] := AConfig.HashSize;
  LBuffer[1] := System.Length(AConfig.Key);

  if ATreeConfig <> Nil then
  begin
    LBuffer[2] := ATreeConfig.FanOut;
    LBuffer[3] := ATreeConfig.MaxDepth;
    TConverters.ReadUInt32AsBytesLE(ATreeConfig.LeafSize, LBuffer, 4);
    LBuffer[8] := Byte(ATreeConfig.NodeOffset);
    LBuffer[9] := Byte(ATreeConfig.NodeOffset shr 8);
    LBuffer[10] := Byte(ATreeConfig.NodeOffset shr 16);
    LBuffer[11] := Byte(ATreeConfig.NodeOffset shr 24);
    LBuffer[12] := Byte(ATreeConfig.NodeOffset shr 32);
    LBuffer[13] := Byte(ATreeConfig.NodeOffset shr 40);
    LBuffer[14] := ATreeConfig.NodeDepth;
    LBuffer[15] := ATreeConfig.InnerHashSize;
  end;

  if AConfig.Salt <> Nil then
  begin
    System.Move(AConfig.Salt[0], LBuffer[16], 8 * System.SizeOf(Byte));
  end;

  if AConfig.Personalisation <> Nil then
  begin
    System.Move(AConfig.Personalisation[0], LBuffer[24],
      8 * System.SizeOf(Byte));
  end;

  System.SetLength(Result, 8);
  TConverters.le32_copy(PByte(LBuffer), 0, PCardinal(Result), 0,
    System.Length(LBuffer) * System.SizeOf(Byte));
end;

end.
