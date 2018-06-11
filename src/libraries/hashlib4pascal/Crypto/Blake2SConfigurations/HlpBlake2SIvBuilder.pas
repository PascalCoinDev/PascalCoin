unit HlpBlake2SIvBuilder;

{$I ..\..\Include\HashLib.inc}

interface

uses
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

type
  TBlake2SIvBuilder = class sealed(TObject)

  strict private
    class var

      FSequentialTreeConfig: IBlake2STreeConfig;

    class constructor Blake2SIvBuilder();

  public
    class function ConfigS(config: IBlake2SConfig;
      treeConfig: IBlake2STreeConfig): THashLibUInt32Array; static;

    // class procedure ConfigSSetNode(rawConfig: THashLibUInt32Array; depth: Byte;
    // nodeOffset: UInt32); static; inline;

  end;

implementation

{ TBlake2SIvBuilder }

class constructor TBlake2SIvBuilder.Blake2SIvBuilder;
begin
  FSequentialTreeConfig := TBlake2STreeConfig.Create();
  FSequentialTreeConfig.IntermediateHashSize := 0;
  FSequentialTreeConfig.LeafSize := 0;
  FSequentialTreeConfig.FanOut := 1;
  FSequentialTreeConfig.MaxHeight := 1;
end;

class function TBlake2SIvBuilder.ConfigS(config: IBlake2SConfig;
  treeConfig: IBlake2STreeConfig): THashLibUInt32Array;
var
  isSequential: Boolean;
  rawConfig: THashLibUInt32Array;
begin
  isSequential := treeConfig = Nil;
  if (isSequential) then
  begin
    treeConfig := FSequentialTreeConfig;
  end;
  System.SetLength(rawConfig, 8);

  // digest length
  if ((config.HashSize <= 0) or (config.HashSize > 32)) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidHashSize);
  end;

  rawConfig[0] := rawConfig[0] or (UInt32(config.HashSize));

  // Key length
  if (config.Key <> Nil) then
  begin
    if (System.Length(config.Key) > 32) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidKeyLength);
    end;
    rawConfig[0] := rawConfig[0] or UInt32(System.Length(config.Key)) shl 8;
  end;

  // FanOut
  rawConfig[0] := rawConfig[0] or (UInt32(treeConfig.FanOut) shl 16);
  // Depth
  rawConfig[0] := rawConfig[0] or (UInt32(treeConfig.MaxHeight) shl 24);
  // Leaf length
  rawConfig[0] := rawConfig[0] or
    ((UInt64(UInt32(treeConfig.LeafSize))) shl 32);

  // Inner length
  if ((not isSequential) and ((treeConfig.IntermediateHashSize <= 0) or
    (treeConfig.IntermediateHashSize > 32))) then
  begin
    raise EArgumentOutOfRangeHashLibException.Create
      ('treeConfig.TreeIntermediateHashSize');
  end;
  rawConfig[2] := rawConfig[2] or
    (UInt32(treeConfig.IntermediateHashSize) shl 8);

  // Salt
  if (config.Salt <> Nil) then
  begin
    if (System.Length(config.Salt) <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes(@SInvalidSaltLength);
    end;
    rawConfig[4] := TConverters.ReadBytesAsUInt32LE(PByte(config.Salt), 0);
    rawConfig[5] := TConverters.ReadBytesAsUInt32LE(PByte(config.Salt), 4);
  end;

  // Personalisation
  if (config.Personalisation <> Nil) then
  begin
    if (System.Length(config.Personalisation) <> 8) then
    begin
      raise EArgumentOutOfRangeHashLibException.CreateRes
        (@SInvalidPersonalisationLength);
    end;
    rawConfig[6] := TConverters.ReadBytesAsUInt32LE
      (PByte(config.Personalisation), 0);
    rawConfig[7] := TConverters.ReadBytesAsUInt32LE
      (PByte(config.Personalisation), 4);
  end;

  result := rawConfig;
end;

// class procedure TBlake2SIvBuilder.ConfigSSetNode(rawConfig: THashLibUInt32Array;
// depth: Byte; nodeOffset: UInt32);
// begin
// rawConfig[1] := nodeOffset;
// rawConfig[2] := (rawConfig[2] and (not UInt32($FF))) or depth;
// end;

end.
