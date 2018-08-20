{
  Copyright (c) Herman Schoenfeld 2018

  RandomHash reference implementation (PIP-0009).

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

unit URandomHash;

{%region Compiler Directives}
{$IFDEF FPC}
  {$UNDEF DELPHI}
  {$MODE delphi}
  {$DEFINE USE_UNROLLED_VARIANT}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}
  {$POINTERMATH ON}
  {$WARNINGS OFF}
  {$HINTS OFF}
  {$NOTES OFF}
  {$OPTIMIZATION LEVEL3}
  {$OPTIMIZATION PEEPHOLE}
  {$OPTIMIZATION REGVAR}
  {$OPTIMIZATION LOOPUNROLL}
  {$OPTIMIZATION STRENGTH}
  {$OPTIMIZATION CSE}
  {$OPTIMIZATION DFA}
  {$IFDEF CPUI386}
    {$OPTIMIZATION USEEBP}
  {$ENDIF}
  {$IFDEF CPUX86_64}
    {$OPTIMIZATION USERBP}
  {$ENDIF}
{$ELSE}
  {$DEFINE USE_UNROLLED_VARIANT}
  {$DEFINITIONINFO ON}
  {$HINTS OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}
  {$POINTERMATH ON}
  {$STRINGCHECKS OFF}
  {$WARN DUPLICATE_CTOR_DTOR OFF}
  // 2010 only
  {$IF CompilerVersion = 21.0}
  {$DEFINE DELPHI2010}
  {$IFEND}
  // 2010 and Above
  {$IF CompilerVersion >= 21.0}
  {$DEFINE DELPHI2010_UP}
  {$IFEND}
  // XE and Above
  {$IF CompilerVersion >= 22.0}
  {$DEFINE DELPHIXE_UP}
  {$IFEND}
  // XE2 and Above
  {$IF CompilerVersion >= 23.0}
  {$DEFINE DELPHIXE2_UP}
  {$DEFINE HAS_UNITSCOPE}
  {$IFEND}
  // XE3 and Below
  {$IF CompilerVersion <= 24.0}
  {$DEFINE DELPHIXE3_DOWN}
  {$IFEND}
  // XE3 and Above
  {$IF CompilerVersion >= 24.0}
  {$DEFINE DELPHIXE3_UP}
  {$LEGACYIFEND ON}
  {$ZEROBASEDSTRINGS OFF}
  {$IFEND}
  // XE7 and Above
  {$IF CompilerVersion >= 28.0}
  {$DEFINE DELPHIXE7_UP}
  {$IFEND}
  // 10.2 Tokyo and Above
  {$IF CompilerVersion >= 32.0}
  {$DEFINE DELPHI10.2_TOKYO_UP}
  {$IFEND}
  // 2010 and Above
  {$IFNDEF DELPHI2010_UP}
  {$MESSAGE ERROR 'This Library requires Delphi 2010 or higher.'}
  {$ENDIF}
  // 10.2 Tokyo and Above
  {$IFDEF DELPHI10.2_TOKYO_UP}
  {$WARN COMBINING_SIGNED_UNSIGNED OFF}
  {$WARN COMBINING_SIGNED_UNSIGNED64 OFF}
  {$ENDIF}
{$ENDIF}
{%endregion}

interface

uses Generics.Collections, SysUtils, HlpIHash, HlpBits, HlpHashFactory;

type

  { TRandomHash }

  TRandomHash = class sealed(TObject)
    const
      N = 5;            // Number of hashing rounds required to compute a nonce (total rounds = 2^N - 1)
      M = 10 * 1024;    // 10KB The memory expansion unit (in bytes)

    {$IFNDEF UNITTESTS}private{$ELSE}public{$ENDIF}
      FMurmurHash3_x86_32 : IHash;
      FHashAlg : array[0..17] of IHash;  // declared here to avoid race-condition during mining
      function ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes; inline;
      function MemTransform1(const AChunk: TBytes): TBytes; {$IFDEF FPC}inline;{$ENDIF}
      function MemTransform2(const AChunk: TBytes): TBytes; inline;
      function MemTransform3(const AChunk: TBytes): TBytes; inline;
      function MemTransform4(const AChunk: TBytes): TBytes; inline;
      function MemTransform5(const AChunk: TBytes): TBytes; inline;
      function MemTransform6(const AChunk: TBytes): TBytes; inline;
      function MemTransform7(const AChunk: TBytes): TBytes; inline;
      function MemTransform8(const AChunk: TBytes): TBytes; inline;
      function Expand(const AInput: TBytes; AExpansionFactor: Int32) : TBytes;
      function Compress(const AInputs: TArray<TBytes>): TBytes; inline;
      function ChangeNonce(const ABlockHeader: TBytes; ANonce: Int32): TBytes; inline;
      function Checksum(const AInput: TBytes): UInt32; overload; inline;
      function Checksum(const AInput: TArray<TBytes>): UInt32; overload; inline;
      function Hash(const ABlockHeader: TBytes; ARound: Int32) : TArray<TBytes>; overload;
    public
      constructor Create;
      destructor Destroy; override;
      function Hash(const ABlockHeader: TBytes): TBytes; overload; inline;
      class function Compute(const ABlockHeader: TBytes): TBytes; overload; static; inline;
  end;

  { ERandomHash }

  ERandomHash = class(Exception);

  { TMersenne32 }

  TMersenne32 = class
    private const
      // Define MT19937 constants (32-bit RNG)
      N = 624;
      M = 397;
      R = 31;
      A = $9908B0DF;
      F = 1812433253;
      U = 11;
      S = 7;
      B = $9D2C5680;
      T = 15;
      C = $EFC60000;
      L = 18;
      MASK_LOWER = UInt32((UInt64(1) shl R) - 1);
      MASK_UPPER = UInt32(UInt64(1) shl R);
    private
      FIndex: Word;
      Fmt: array [0..N-1] of UInt32;
      procedure Twist(); inline;
    public
      constructor Create(ASeed: UInt32);
      procedure Initialize(ASeed: UInt32); inline;
      function NextInt32: UInt32; inline;
      function NextUInt32: UInt32; inline;
      function NextSingle: Single; inline;
      function NextUSingle: Single; inline;
  end;

resourcestring
  SUnSupportedHash = 'Unsupported Hash Selected';
  SInvalidRound = 'Round must be between 0 and N inclusive';

implementation

uses UMemory;

{ TRandomHash }

constructor TRandomHash.Create;
begin
  FMurmurHash3_x86_32 := THashFactory.THash32.CreateMurmurHash3_x86_32();
  FHashAlg[0] := THashFactory.TCrypto.CreateSHA2_256();
  FHashAlg[1] := THashFactory.TCrypto.CreateSHA2_384();
  FHashAlg[2] := THashFactory.TCrypto.CreateSHA2_512();
  FHashAlg[3] := THashFactory.TCrypto.CreateSHA3_256();
  FHashAlg[4] := THashFactory.TCrypto.CreateSHA3_384();
  FHashAlg[5] := THashFactory.TCrypto.CreateSHA3_512();
  FHashAlg[6] := THashFactory.TCrypto.CreateRIPEMD160();
  FHashAlg[7] := THashFactory.TCrypto.CreateRIPEMD256();
  FHashAlg[8] := THashFactory.TCrypto.CreateRIPEMD320();
  FHashAlg[9] := THashFactory.TCrypto.CreateBlake2B_512();
  FHashAlg[10] := THashFactory.TCrypto.CreateBlake2S_256();
  FHashAlg[11] := THashFactory.TCrypto.CreateTiger2_5_192();
  FHashAlg[12] := THashFactory.TCrypto.CreateSnefru_8_256();
  FHashAlg[13] := THashFactory.TCrypto.CreateGrindahl512();
  FHashAlg[14] := THashFactory.TCrypto.CreateHaval_5_256();
  FHashAlg[15] := THashFactory.TCrypto.CreateMD5();
  FHashAlg[16] := THashFactory.TCrypto.CreateRadioGatun32();
  FHashAlg[17] := THashFactory.TCrypto.CreateWhirlPool();
end;

destructor TRandomHash.Destroy;
var i : integer;
begin
 FMurmurHash3_x86_32 := nil;
 for i := Low(FHashAlg) to High(FHashAlg) do
   FHashAlg[i] := nil;
 inherited Destroy;
end;

class function TRandomHash.Compute(const ABlockHeader: TBytes): TBytes;
var
  LHasher : TRandomHash;
  LDisposables : TDisposables;
begin
 LHasher := LDisposables.AddObject( TRandomHash.Create ) as TRandomHash;
 Result := LHasher.Hash(ABlockHeader);
end;

function TRandomHash.Hash(const ABlockHeader: TBytes): TBytes;
var
  LAllOutputs: TArray<TBytes>;
  LSeed: UInt32;
begin
  LAllOutputs := Hash(ABlockHeader, N);
  Result := FHashAlg[0].ComputeBytes(Compress(LAllOutputs)).GetBytes;
end;

function TRandomHash.Hash(const ABlockHeader: TBytes; ARound: Int32) : TArray<TBytes>;
var
  LRoundOutputs: TList<TBytes>;
  LSeed: UInt32;
  LGen: TMersenne32;
  LRoundInput, LOtherNonceHeader, LOutput, LBytes: TBytes;
  LParentOutputs, LNeighborOutputs, LToArray: TArray<TBytes>;
  LHashFunc: IHash;
  i: Int32;
  LDisposables : TDisposables;
begin
  if (ARound < 1) or (ARound > N) then
    raise EArgumentOutOfRangeException.CreateRes(@SInvalidRound);

  LRoundOutputs := LDisposables.AddObject( TList<TBytes>.Create() ) as TList<TBytes>;
  LGen := LDisposables.AddObject( TMersenne32.Create(0) ) as TMersenne32;
  if ARound = 1 then  begin
    LSeed := Checksum(ABlockHeader);
    LGen.Initialize(LSeed);
    LRoundInput := ABlockHeader;
  end else begin
    LParentOutputs := Hash(ABlockHeader, ARound - 1);
    LSeed := Checksum(LParentOutputs);
    LGen.Initialize(LSeed);
    LRoundOutputs.AddRange( LParentOutputs );
    LOtherNonceHeader := ChangeNonce(ABlockHeader, LGen.NextUInt32);
    LNeighborOutputs := Hash(LOtherNonceHeader, ARound - 1);
    LRoundOutputs.AddRange(LNeighborOutputs);
    LRoundInput := Compress( LRoundOutputs.ToArray );
  end;

  LHashFunc := FHashAlg[LGen.NextUInt32 mod 18];
  LOutput := LHashFunc.ComputeBytes(LRoundInput).GetBytes;
  LOutput := Expand(LOutput, N - ARound);
  LRoundOutputs.Add(LOutput);

  Result := LRoundOutputs.ToArray;
end;

function TRandomHash.ChangeNonce(const ABlockHeader: TBytes;  ANonce: Int32): TBytes;
var
  LHeaderLength : Integer;
begin
  // NOTE: NONCE is last 4 bytes of header!

  // Clone the original header
  Result := Copy(ABlockHeader);

  // If digest not big enough to contain a nonce, just return the clone
  LHeaderLength := Length(ABlockHeader);
  if LHeaderLength < 4 then
    exit;

  // Overwrite the nonce in little-endian
  Result[LHeaderLength - 4] := Byte(ANonce);
  Result[LHeaderLength - 3] := (ANonce SHR 8) AND 255;
  Result[LHeaderLength - 2] := (ANonce SHR 16) AND 255;
  Result[LHeaderLength - 1] := (ANonce SHR 24) AND 255;
end;

function TRandomHash.Checksum(const AInput: TBytes): UInt32;
begin
  Result := FMurmurHash3_x86_32.ComputeBytes(AInput).GetUInt32;
end;

function TRandomHash.Checksum(const AInput : TArray<TBytes>): UInt32;
var
  i: Int32;
begin
  FMurmurHash3_x86_32.Initialize;
  for i := Low(AInput) to High(AInput) do
  begin
    FMurmurHash3_x86_32.TransformBytes(AInput[i]);
  end;
  Result := FMurmurHash3_x86_32.TransformFinal.GetUInt32;
end;

function TRandomHash.Compress(const AInputs : TArray<TBytes>): TBytes;
var
  i: Int32;
  LSeed: UInt32;
  LSource: TBytes;
  LGen: TMersenne32;
  LDisposables : TDisposables;
begin
  SetLength(Result, 100);
  LSeed := Checksum(AInputs);
  LGen := LDisposables.AddObject( TMersenne32.Create( LSeed ) ) as TMersenne32;
  for i := 0 to 99 do
  begin
    LSource := AInputs[LGen.NextUInt32 mod Length(AInputs)];
    Result[i] := LSource[LGen.NextUInt32 mod Length(LSource)];
  end;
end;

function TRandomHash.ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes;
begin
  SetLength(Result, Length(AChunk1) + Length(AChunk2));
  Move(AChunk1[0], Result[0], Length(AChunk1));
  Move(AChunk2[0], Result[Length(AChunk1)], Length(AChunk2));
end;

function TRandomHash.MemTransform1(const AChunk: TBytes): TBytes;
var
  i, LChunkLength : UInt32;
  LState : UInt32;

  function XorShift32 : UInt32; {$IFDEF FPC}inline;{$ENDIF}
  begin
    LState := LState XOR (LState SHL 13);
    LState := LState XOR (LState SHR 17);
    LState := LState XOR (LState SHL 5);
    Result := LState;
  end;

begin
  // Seed XorShift32 with non-zero seed (checksum of input or 1)
  LState := Checksum(AChunk);
  if LState = 0 then
    LState := 1;

  // Select random bytes from input using XorShift32 RNG
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := AChunk[XorShift32 MOD LChunkLength];
end;

function TRandomHash.MemTransform2(const AChunk: TBytes): TBytes;
var
  i, LChunkLength, LPivot, LOdd: Int32;
begin
  LChunkLength := Length(AChunk);
  LPivot := LChunkLength SHR 1;
  LOdd := LChunkLength MOD 2;
  SetLength(Result, LChunkLength);
  Move(AChunk[LPivot + LOdd], Result[0], LPivot);
  Move(AChunk[0], Result[LPivot + LOdd], LPivot);
  // Set middle-byte for odd-length arrays
  if LOdd = 1 then
    Result[LPivot] := AChunk[LPivot];
end;

function TRandomHash.MemTransform3(const AChunk: TBytes): TBytes;
var
  i, LChunkLength: Int32;
begin
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := AChunk[LChunkLength - i - 1];
end;

function TRandomHash.MemTransform4(const AChunk: TBytes): TBytes;
var
  i, LChunkLength, LPivot, LOdd: Int32;
begin
  LChunkLength := Length(AChunk);
  LPivot := LChunkLength SHR 1;
  LOdd := LChunkLength MOD 2;
  SetLength(Result, LChunkLength);
  for i := 0 to Pred(LPivot) do
  begin
    Result[(i * 2)] := AChunk[i];
    Result[(i * 2) + 1] := AChunk[i + LPivot + LOdd];
  end;
  // Set final byte for odd-lengths
  if LOdd = 1 THEN
    Result[High(Result)] := AChunk[LPivot];
end;

function TRandomHash.MemTransform5(const AChunk: TBytes): TBytes;
var
  i, LChunkLength, LPivot, LOdd: Int32;
begin
  LChunkLength := Length(AChunk);
  LPivot := LChunkLength SHR 1;
  LOdd := LChunkLength MOD 2;
  SetLength(Result, LChunkLength);
  for i := Low(AChunk) to Pred(LPivot) do
  begin
    Result[(i * 2)] := AChunk[i + LPivot + LOdd];
    Result[(i * 2) + 1] := AChunk[i];
  end;
  // Set final byte for odd-lengths
  if LOdd = 1 THEN
    Result[High(Result)] := AChunk[LPivot];
end;

function TRandomHash.MemTransform6(const AChunk: TBytes): TBytes;
var
  i, LChunkLength, LPivot, LOdd: Int32;
begin
  LChunkLength := Length(AChunk);
  LPivot := LChunkLength SHR 1;
  LOdd := LChunkLength MOD 2;
  SetLength(Result, LChunkLength);
  for i := 0 to Pred(LPivot) do
  begin
    Result[i] := AChunk[(i * 2)] xor AChunk[(i * 2) + 1];
    Result[i + LPivot + LOdd] := AChunk[i] xor AChunk[LChunkLength - i - 1];
  end;
  // Set middle-byte for odd-lengths
  if LOdd = 1 THEN
    Result[LPivot] := AChunk[High(AChunk)];
end;

function TRandomHash.MemTransform7(const AChunk: TBytes): TBytes;
var
  i, LChunkLength: Int32;
begin
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := TBits.RotateLeft8(AChunk[i], LChunkLength - i);
end;

function TRandomHash.MemTransform8(const AChunk: TBytes): TBytes;
var
  i, LChunkLength: Int32;
begin
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := TBits.RotateRight8(AChunk[i], LChunkLength - i);
end;

function TRandomHash.Expand(const AInput: TBytes; AExpansionFactor: Int32): TBytes;
var
  LSize, LBytesToAdd: Int32;
  LOutput, LNextChunk: TBytes;
  LRandom, LSeed: UInt32;
  LGen: TMersenne32;
  LDisposables : TDisposables;
begin
  LSeed := Checksum(AInput);
  LGen := LDisposables.AddObject( TMersenne32.Create (LSeed) ) as TMersenne32;
  LSize := Length(AInput) + (AExpansionFactor * M);
  LOutput := Copy(AInput);
  LBytesToAdd := LSize - Length(AInput);

  while LBytesToAdd > 0 do
  begin
    LNextChunk := Copy(LOutput);
    if Length(LNextChunk) > LBytesToAdd then
      SetLength(LNextChunk, LBytesToAdd);

    LRandom := LGen.NextUInt32;
    case LRandom mod 8 of
      0: LOutput := ContencateByteArrays(LOutput, MemTransform1(LNextChunk));
      1: LOutput := ContencateByteArrays(LOutput, MemTransform2(LNextChunk));
      2: LOutput := ContencateByteArrays(LOutput, MemTransform3(LNextChunk));
      3: LOutput := ContencateByteArrays(LOutput, MemTransform4(LNextChunk));
      4: LOutput := ContencateByteArrays(LOutput, MemTransform5(LNextChunk));
      5: LOutput := ContencateByteArrays(LOutput, MemTransform6(LNextChunk));
      6: LOutput := ContencateByteArrays(LOutput, MemTransform7(LNextChunk));
      7: LOutput := ContencateByteArrays(LOutput, MemTransform8(LNextChunk));
    end;
    LBytesToAdd := LBytesToAdd - Length(LNextChunk);
  end;
  Result := LOutput;
end;

{ TMersenne32 }

constructor TMersenne32.Create(ASeed: UInt32);
begin
  Initialize(ASeed);
end;

procedure TMersenne32.Initialize(ASeed: UInt32);
var
  i: Int32;
begin
  Fmt[0] := ASeed;
  for i := 1 to Pred(N) do
    Fmt[i] := F * (Fmt[i - 1] xor (Fmt[i - 1] shr 30)) + i;
  FIndex := N;
end;

procedure TMersenne32.Twist;
var
  i: Int32;
begin
  for i := 0 to N - M - 1 do
    Fmt[i] := Fmt[i + M] xor (((Fmt[i] and MASK_UPPER) or (Fmt[i + 1] and MASK_LOWER)) shr 1) xor (UInt32(-(Fmt[i + 1] and 1)) and A);

  for i := N - M to N - 2 do
  begin
    Fmt[i] := Fmt[i + (M - N)] xor (((Fmt[i] and MASK_UPPER) or (Fmt[i + 1] and MASK_LOWER)) shr 1) xor (UInt32(-(Fmt[i + 1] and 1)) and A);
    Fmt[N - 1] := Fmt[M - 1] xor (((Fmt[N - 1] and MASK_UPPER) or (Fmt[0] and MASK_LOWER)) shr 1) xor (UInt32(-(Fmt[0] and 1)) and A);
  end;
  FIndex := 0;
end;

function TMersenne32.NextInt32: UInt32;
begin
  Result := Int32(NextUInt32);
end;

function TMersenne32.NextUInt32: UInt32;
var
  i: Int32;
begin
  i := FIndex;
  if FIndex >= N then
  begin
    Twist;
    i := FIndex;
  end;
  Result := Fmt[i];
  FIndex := i + 1;
  Result := Result xor (Fmt[i] shr U);
  Result := Result xor (Result shl S) and B;
  Result := Result xor (Result shl T) and C;
  Result := Result xor (Result shr L);
end;

function TMersenne32.NextSingle: Single;
begin
  Result := NextUInt32 * 4.6566128730773926E-010;
end;

function TMersenne32.NextUSingle: Single;
begin
  Result := NextUInt32 * 2.32830643653869628906E-10;
end;

end.
