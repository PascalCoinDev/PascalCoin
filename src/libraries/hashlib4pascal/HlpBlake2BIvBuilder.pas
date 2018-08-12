unit HlpBlake2BIvBuilder;

{$I HashLib.inc}

interface

uses
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

type
  TBlake2BIvBuilder = class sealed(TObject)

  strict private
    class var

      FSequentialTreeConfig: IBlake2BTreeConfig;

    class constructor Blake2BIvBuilder();

  public
    class function ConfigB(const config: IBlake2BConfig;
      const treeConfig: IBlake2BTreeConfig): THashLibUInt64Array; static;

    class procedure ConfigBSetNode(const rawConfig: THashLibUInt64Array;
      depth: Byte; nodeOffset: UInt64); static; inline;

  end;

implementation

{ TBlake2BIvBuilder }

class constructor TBlake2BIvBuilder.Blake2BIvBuilder;
begin
  FSequentialTreeConfig := TBlake2BTreeConfig.Create();
  FSequentialTreeConfig.IntermediateHashSize := 0;
  FSequentialTreeConfig.LeafSize := 0;
  FSequentialTreeConfig.FanOut := 1;
  FSequentialTreeConfig.MaxHeight := 1;
end;

class function TBlake2BIvBuilder.ConfigB(const config: IBlake2BConfig;
  const treeConfig: IBlake2BTreeConfig): THashLibUInt64Array;
var
  isSequential: Boolean;
  LtreeConfig: IBlake2BTreeConfig;
  rawConfig: THashLibUInt64Array;
begin
  LtreeConfig := treeConfig;
  isSequential := LtreeConfig = Nil;
  if (isSequential) then
  begin
    LtreeConfig := FSequentialTreeConfig;
  end;
  System.SetLength(rawConfig, 8);

  // digest length
  if ((config.HashSize <= 0) or (config.HashSize > 64)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidHashSize);
  end;

  rawConfig[0] := rawConfig[0] or (UInt64(UInt32(config.HashSize)));

  // Key length
  if (config.Key <> Nil) then
  begin
    if (System.Length(config.Key) > 64) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidKeyLength);
    end;
    rawConfig[0] := rawConfig[0] or
      UInt64(UInt32(System.Length(config.Key)) shl 8);
  end;

  // FanOut
  rawConfig[0] := rawConfig[0] or (UInt32(LtreeConfig.FanOut) shl 16);
  // Depth
  rawConfig[0] := rawConfig[0] or (UInt32(LtreeConfig.MaxHeight) shl 24);
  // Leaf length
  rawConfig[0] := rawConfig[0] or
    ((UInt64(UInt32(LtreeConfig.LeafSize))) shl 32);

  // Inner length
  if ((not isSequential) and ((LtreeConfig.IntermediateHashSize <= 0) or
    (LtreeConfig.IntermediateHashSize > 64))) then
  begin
    raise EArgumentOutOfRangeHashLibException.Create
      ('treeConfig.TreeIntermediateHashSize');
  end;
  rawConfig[2] := rawConfig[2] or
    (UInt32(LtreeConfig.IntermediateHashSize) shl 8);

  // Salt
  if (config.Salt <> Nil) then
  begin
    if (System.Length(config.Salt) <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidSaltLength);
    end;
    rawConfig[4] := TConverters.ReadBytesAsUInt64LE(PByte(config.Salt), 0);
    rawConfig[5] := TConverters.ReadBytesAsUInt64LE(PByte(config.Salt), 8);
  end;

  // Personalisation
  if (config.Personalisation <> Nil) then
  begin
    if (System.Length(config.Personalisation) <> 16) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@SInvalidPersonalisationLength);
    end;
    rawConfig[6] := TConverters.ReadBytesAsUInt64LE
      (PByte(config.Personalisation), 0);
    rawConfig[7] := TConverters.ReadBytesAsUInt64LE
      (PByte(config.Personalisation), 8);
  end;

  result := rawConfig;
end;

class procedure TBlake2BIvBuilder.ConfigBSetNode(const rawConfig
  : THashLibUInt64Array; depth: Byte; nodeOffset: UInt64);
begin
  rawConfig[1] := nodeOffset;
  rawConfig[2] := (rawConfig[2] and (not UInt64($FF))) or depth;
end;

end.
