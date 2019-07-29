unit HlpBlake2S;

{$I HashLib.inc}

interface

uses
  SysUtils,
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpBits,
  HlpHash,
  HlpHashResult,
  HlpIHashResult,
  HlpIBlake2SConfig,
  HlpBlake2SConfig,
  HlpIBlake2STreeConfig,
  HlpBlake2SIvBuilder,
  HlpIHash,
  HlpIHashInfo,
  HlpConverters,
  HlpArrayUtils,
  HlpHashLibTypes;

resourcestring
  SInvalidConfigLength = 'Config Length Must Be 8 Words';
  SConfigNil = 'Config Cannot Be Nil';

type
  TBlake2S = class sealed(THash, ICryptoNotBuildIn, ITransformBlock)
  strict private

{$REGION 'Consts'}
  const

{$IFNDEF USE_UNROLLED_VARIANT}
    NumberOfRounds = Int32(10);
{$ENDIF USE_UNROLLED_VARIANT}
    BlockSizeInBytes = Int32(64);

    IV0 = UInt32($66A09E667);
    IV1 = UInt32($BB67AE85);
    IV2 = UInt32($3C6EF372);
    IV3 = UInt32($A54FF53A);
    IV4 = UInt32($510E527F);
    IV5 = UInt32($9B05688C);
    IV6 = UInt32($1F83D9AB);
    IV7 = UInt32($5BE0CD19);

{$IFNDEF USE_UNROLLED_VARIANT}
    Sigma: array [0 .. 9, 0 .. 15] of Byte = ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15), (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5,
      3), (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
      (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
      (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
      (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
      (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
      (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
      (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
      (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0));
{$ENDIF USE_UNROLLED_VARIANT}
{$ENDREGION}
    class var

      FDefaultConfig: IBlake2SConfig;

  var
    FM: array [0 .. 15] of UInt32;
    FRawConfig, FState: THashLibUInt32Array;
    FKey, FBuffer: THashLibByteArray;
{$IFNDEF USE_UNROLLED_VARIANT}
    FV: array [0 .. 15] of UInt32;
{$ENDIF USE_UNROLLED_VARIANT}
    FFilledBufferCount, FHashSize, FBlockSize: Int32;
    FCounter0, FCounter1, FFinalizationFlag0, FFinalizationFlag1: UInt32;
    FTreeConfig: IBlake2STreeConfig;

    class constructor Blake2SConfig();

{$IFNDEF USE_UNROLLED_VARIANT}
    procedure G(a, b, c, d, r, i: Int32); inline;
{$ENDIF USE_UNROLLED_VARIANT}
    procedure Compress(ABlock: PByte; AStart: Int32);

    procedure Finish(); inline;

  strict protected

    function GetName: String; override;

  public
    constructor Create(); overload;
    constructor Create(const AConfig: IBlake2SConfig); overload;
    constructor Create(const AConfig: IBlake2SConfig;
      const ATreeConfig: IBlake2STreeConfig); overload;
    procedure Initialize; override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ADataLength: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;

  end;

implementation

{ TBlake2S }

class constructor TBlake2S.Blake2SConfig;
begin
  FDefaultConfig := TBlake2SConfig.Create();
end;

constructor TBlake2S.Create();
begin
  Create(TBlake2SConfig.Create() as IBlake2SConfig);
end;

{$IFNDEF USE_UNROLLED_VARIANT}

procedure TBlake2S.G(a, b, c, d, r, i: Int32);
begin
  FV[a] := FV[a] + (FV[b] + FM[Sigma[r][2 * i + 0]]);
  FV[d] := TBits.RotateRight32(FV[d] xor FV[a], 16);
  FV[c] := FV[c] + FV[d];
  FV[b] := TBits.RotateRight32(FV[b] xor FV[c], 12);
  FV[a] := FV[a] + (FV[b] + FM[Sigma[r][2 * i + 1]]);
  FV[d] := TBits.RotateRight32(FV[d] xor FV[a], 8);
  FV[c] := FV[c] + FV[d];
  FV[b] := TBits.RotateRight32(FV[b] xor FV[c], 7);
end;

{$ENDIF USE_UNROLLED_VARIANT}

function TBlake2S.Clone(): IHash;
var
  LHashInstance: TBlake2S;
begin
  LHashInstance := TBlake2S.Create(TBlake2SConfig.Create(FHashSize)
    as IBlake2SConfig);
  System.Move(FM, LHashInstance.FM, System.SizeOf(FM));
  LHashInstance.FRawConfig := System.Copy(FRawConfig);
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FKey := System.Copy(FKey);
  LHashInstance.FBuffer := System.Copy(FBuffer);
{$IFNDEF USE_UNROLLED_VARIANT}
  System.Move(FV, LHashInstance.FV, System.SizeOf(FV));
{$ENDIF USE_UNROLLED_VARIANT}
  LHashInstance.FFilledBufferCount := FFilledBufferCount;
  LHashInstance.FCounter0 := FCounter0;
  LHashInstance.FCounter1 := FCounter1;
  LHashInstance.FFinalizationFlag0 := FFinalizationFlag0;
  LHashInstance.FFinalizationFlag1 := FFinalizationFlag1;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

procedure TBlake2S.Compress(ABlock: PByte; AStart: Int32);
var
{$IFDEF USE_UNROLLED_VARIANT}
  m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, v0, v1,
    v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: UInt32;

{$ELSE}
  i, r: Int32;

{$ENDIF USE_UNROLLED_VARIANT}
begin
  TConverters.le32_copy(ABlock, AStart, @(FM[0]), 0, FBlockSize);

{$IFDEF USE_UNROLLED_VARIANT}
  m0 := FM[0];
  m1 := FM[1];
  m2 := FM[2];
  m3 := FM[3];
  m4 := FM[4];
  m5 := FM[5];
  m6 := FM[6];
  m7 := FM[7];
  m8 := FM[8];
  m9 := FM[9];
  m10 := FM[10];
  m11 := FM[11];
  m12 := FM[12];
  m13 := FM[13];
  m14 := FM[14];
  m15 := FM[15];

  v0 := FState[0];
  v1 := FState[1];
  v2 := FState[2];
  v3 := FState[3];
  v4 := FState[4];
  v5 := FState[5];
  v6 := FState[6];
  v7 := FState[7];

  v8 := IV0;
  v9 := IV1;
  v10 := IV2;
  v11 := IV3;
  v12 := IV4 xor FCounter0;
  v13 := IV5 xor FCounter1;
  v14 := IV6 xor FFinalizationFlag0;
  v15 := IV7 xor FFinalizationFlag1;

  // Rounds
  // *
  // Round 1.
  v0 := v0 + m0;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m2;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m4;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m6;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m5;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m7;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m3;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m1;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m8;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m10;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m12;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m14;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m13;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m15;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m11;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m9;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 2.
  v0 := v0 + m14;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m4;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m9;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m13;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m15;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m6;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m8;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m10;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m1;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m0;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m11;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m5;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m7;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m3;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m2;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m12;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 3.
  v0 := v0 + m11;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m12;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m5;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m15;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m2;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m13;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m0;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m8;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m10;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m3;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m7;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m9;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m1;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m4;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m6;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m14;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 4.
  v0 := v0 + m7;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m3;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m13;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m11;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m12;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m14;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m1;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m9;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m2;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m5;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m4;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m15;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m0;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m8;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m10;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m6;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 5.
  v0 := v0 + m9;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m5;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m2;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m10;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m4;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m15;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m7;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m0;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m14;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m11;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m6;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m3;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m8;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m13;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m12;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m1;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 6.
  v0 := v0 + m2;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m6;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m0;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m8;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m11;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m3;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m10;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m12;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m4;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m7;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m15;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m1;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m14;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m9;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m5;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m13;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 7.
  v0 := v0 + m12;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m1;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m14;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m4;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m13;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m10;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m15;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m5;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m0;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m6;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m9;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m8;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m2;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m11;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m3;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m7;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 8.
  v0 := v0 + m13;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m7;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m12;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m3;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m1;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m9;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m14;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m11;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m5;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m15;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m8;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m2;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m6;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m10;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m4;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m0;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 9.
  v0 := v0 + m6;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m14;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m11;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m0;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m3;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m8;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m9;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m15;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m12;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m13;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m1;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m10;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m4;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m5;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m7;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m2;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);

  // Round 10.
  v0 := v0 + m10;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 16);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 12);
  v1 := v1 + m8;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 16);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 12);
  v2 := v2 + m7;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 16);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 12);
  v3 := v3 + m1;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 16);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 12);
  v2 := v2 + m6;
  v2 := v2 + v6;
  v14 := v14 xor v2;
  v14 := TBits.RotateRight32(v14, 8);
  v10 := v10 + v14;
  v6 := v6 xor v10;
  v6 := TBits.RotateRight32(v6, 7);
  v3 := v3 + m5;
  v3 := v3 + v7;
  v15 := v15 xor v3;
  v15 := TBits.RotateRight32(v15, 8);
  v11 := v11 + v15;
  v7 := v7 xor v11;
  v7 := TBits.RotateRight32(v7, 7);
  v1 := v1 + m4;
  v1 := v1 + v5;
  v13 := v13 xor v1;
  v13 := TBits.RotateRight32(v13, 8);
  v9 := v9 + v13;
  v5 := v5 xor v9;
  v5 := TBits.RotateRight32(v5, 7);
  v0 := v0 + m2;
  v0 := v0 + v4;
  v12 := v12 xor v0;
  v12 := TBits.RotateRight32(v12, 8);
  v8 := v8 + v12;
  v4 := v4 xor v8;
  v4 := TBits.RotateRight32(v4, 7);
  v0 := v0 + m15;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 16);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 12);
  v1 := v1 + m9;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 16);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 12);
  v2 := v2 + m3;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 16);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 12);
  v3 := v3 + m13;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 16);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 12);
  v2 := v2 + m12;
  v2 := v2 + v7;
  v13 := v13 xor v2;
  v13 := TBits.RotateRight32(v13, 8);
  v8 := v8 + v13;
  v7 := v7 xor v8;
  v7 := TBits.RotateRight32(v7, 7);
  v3 := v3 + m0;
  v3 := v3 + v4;
  v14 := v14 xor v3;
  v14 := TBits.RotateRight32(v14, 8);
  v9 := v9 + v14;
  v4 := v4 xor v9;
  v4 := TBits.RotateRight32(v4, 7);
  v1 := v1 + m14;
  v1 := v1 + v6;
  v12 := v12 xor v1;
  v12 := TBits.RotateRight32(v12, 8);
  v11 := v11 + v12;
  v6 := v6 xor v11;
  v6 := TBits.RotateRight32(v6, 7);
  v0 := v0 + m11;
  v0 := v0 + v5;
  v15 := v15 xor v0;
  v15 := TBits.RotateRight32(v15, 8);
  v10 := v10 + v15;
  v5 := v5 xor v10;
  v5 := TBits.RotateRight32(v5, 7);
  // */
  // Finalization

  FState[0] := FState[0] xor (v0 xor v8);
  FState[1] := FState[1] xor (v1 xor v9);
  FState[2] := FState[2] xor (v2 xor v10);
  FState[3] := FState[3] xor (v3 xor v11);
  FState[4] := FState[4] xor (v4 xor v12);
  FState[5] := FState[5] xor (v5 xor v13);
  FState[6] := FState[6] xor (v6 xor v14);
  FState[7] := FState[7] xor (v7 xor v15);

{$ELSE}
  FV[0] := FState[0];
  FV[1] := FState[1];
  FV[2] := FState[2];
  FV[3] := FState[3];
  FV[4] := FState[4];
  FV[5] := FState[5];
  FV[6] := FState[6];
  FV[7] := FState[7];

  FV[8] := IV0;
  FV[9] := IV1;
  FV[10] := IV2;
  FV[11] := IV3;
  FV[12] := IV4 xor FCounter0;
  FV[13] := IV5 xor FCounter1;

  FV[14] := IV6 xor FFinalizationFlag0;

  FV[15] := IV7 xor FFinalizationFlag1;

  for r := 0 to System.Pred(NumberOfRounds) do

  begin
    G(0, 4, 8, 12, r, 0);
    G(1, 5, 9, 13, r, 1);
    G(2, 6, 10, 14, r, 2);
    G(3, 7, 11, 15, r, 3);
    G(0, 5, 10, 15, r, 4);
    G(1, 6, 11, 12, r, 5);
    G(2, 7, 8, 13, r, 6);
    G(3, 4, 9, 14, r, 7);

  end;

  for i := 0 to 7 do
  begin
    FState[i] := FState[i] xor (FV[i] xor FV[i + 8]);
  end;

{$ENDIF USE_UNROLLED_VARIANT}
end;

constructor TBlake2S.Create(const AConfig: IBlake2SConfig);
begin
  Create(AConfig, Nil);
end;

constructor TBlake2S.Create(const AConfig: IBlake2SConfig;
  const ATreeConfig: IBlake2STreeConfig);
var
  LConfig: IBlake2SConfig;
begin
  LConfig := AConfig;
  FTreeConfig := ATreeConfig;
  FBlockSize := BlockSizeInBytes;

  if (LConfig = Nil) then
  begin
    LConfig := FDefaultConfig;
  end;

  FRawConfig := TBlake2SIvBuilder.ConfigS(LConfig, FTreeConfig);
  if ((LConfig.Key <> Nil) and (System.Length(LConfig.Key) <> 0)) then
  begin
    FKey := System.Copy(LConfig.Key, System.Low(LConfig.Key),
      System.Length(LConfig.Key));
    System.SetLength(FKey, FBlockSize);
  end;

  FHashSize := LConfig.HashSize;

  System.SetLength(FState, 8);

  Inherited Create(FHashSize, FBlockSize);
end;

procedure TBlake2S.Finish;
var
  LCount: Int32;
begin
  // Last compression
  FCounter0 := FCounter0 + UInt32(FFilledBufferCount);

  FFinalizationFlag0 := System.High(UInt32);

  if (FTreeConfig.IsLastNode) then
  begin
    FFinalizationFlag1 := System.High(UInt32);
  end;

  LCount := System.Length(FBuffer) - FFilledBufferCount;

  if LCount > 0 then
  begin
    TArrayUtils.Fill(FBuffer, FFilledBufferCount,
      LCount + FFilledBufferCount, Byte(0));
  end;

  Compress(PByte(FBuffer), 0);
end;

procedure TBlake2S.Initialize;
var
  LIdx: Int32;
begin
  if (FRawConfig = Nil) then
  begin
    raise EArgumentNilHashLibException.CreateRes(@SConfigNil);
  end;
  if (System.Length(FRawConfig) <> 8) then
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidConfigLength);
  end;

  FState[0] := IV0;
  FState[1] := IV1;
  FState[2] := IV2;
  FState[3] := IV3;
  FState[4] := IV4;
  FState[5] := IV5;
  FState[6] := IV6;
  FState[7] := IV7;

  FCounter0 := 0;
  FCounter1 := 0;
  FFinalizationFlag0 := 0;
  FFinalizationFlag1 := 0;

  FFilledBufferCount := 0;

  System.SetLength(FBuffer, BlockSizeInBytes);

  TArrayUtils.ZeroFill(FBuffer);

  System.FillChar(FM, System.SizeOf(FM), UInt32(0));

{$IFNDEF USE_UNROLLED_VARIANT}
  System.FillChar(FV, System.SizeOf(FV), UInt32(0));
{$ENDIF USE_UNROLLED_VARIANT}
  for LIdx := 0 to 7 do
  begin
    FState[LIdx] := FState[LIdx] xor FRawConfig[LIdx];
  end;

  if (FKey <> Nil) then
  begin
    TransformBytes(FKey, 0, System.Length(FKey));
  end;
end;

procedure TBlake2S.TransformBytes(const AData: THashLibByteArray;
  AIndex, ADataLength: Int32);
var
  LOffset, LBufferRemaining: Int32;
begin
  LOffset := AIndex;
  LBufferRemaining := BlockSizeInBytes - FFilledBufferCount;

  if ((FFilledBufferCount > 0) and (ADataLength > LBufferRemaining)) then
  begin
    if LBufferRemaining > 0 then
    begin
      System.Move(AData[LOffset], FBuffer[FFilledBufferCount],
        LBufferRemaining);
    end;
    FCounter0 := FCounter0 + UInt32(BlockSizeInBytes);
    if (FCounter0 = 0) then
    begin
      System.Inc(FCounter1);
    end;
    Compress(PByte(FBuffer), 0);
    LOffset := LOffset + LBufferRemaining;
    ADataLength := ADataLength - LBufferRemaining;
    FFilledBufferCount := 0;
  end;

  while (ADataLength > BlockSizeInBytes) do
  begin
    FCounter0 := FCounter0 + UInt32(BlockSizeInBytes);
    if (FCounter0 = 0) then
    begin
      System.Inc(FCounter1);
    end;
    Compress(PByte(AData), LOffset);
    LOffset := LOffset + BlockSizeInBytes;
    ADataLength := ADataLength - BlockSizeInBytes;
  end;

  if (ADataLength > 0) then
  begin
    System.Move(AData[LOffset], FBuffer[FFilledBufferCount], ADataLength);
    FFilledBufferCount := FFilledBufferCount + ADataLength;
  end;
end;

function TBlake2S.TransformFinal: IHashResult;
var
  LBuffer: THashLibByteArray;
begin
  Finish();
  System.SetLength(LBuffer, FHashSize);
  TConverters.le32_copy(PCardinal(FState), 0, PByte(LBuffer), 0,
    System.Length(LBuffer));
  Result := THashResult.Create(LBuffer);
  Initialize();
end;

function TBlake2S.GetName: String;
begin
  Result := Format('%s_%u', [Self.ClassName, Self.HashSize * 8]);
end;

end.
