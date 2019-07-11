unit URandomHash2;

{ Copyright (c) 2018 by Herman Schoenfeld

  PIP-0009: RandomHash2 Reference Implementation

  Notes:
  1. TRandomHash2 - standard use hasher
  2. TRandomHashFast - optimized, mining hasher
  3. TMersenne32 - mersenne twister random number generator
  4. TXorShift32 - lightning fast random number generator (used in memtransform0)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

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

uses {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF}, SysUtils, HlpIHash, HlpBits, HlpHashFactory;

type

  { TRandomHash2 }

  TRandomHash2 = class sealed(TObject)
    const
      N = 5;      // Max-number of hashing rounds required to compute a nonce, total rounds = 2^N
      J = 3;       // Max-number of dependent neighbouring nonces required to evaluate a nonce round
      M = 1024;    // The memory expansion unit (in bytes), total bytes per nonce = M * (2^N (N-2) + 2)
      NUM_HASH_ALGO = 18;

    {$IFNDEF UNITTESTS}private{$ELSE}public{$ENDIF}
      FMurmurHash3_x86_32 : IHash;
      FHashAlg : array[0..17] of IHash;  // declared here to avoid race-condition during mining
      function ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes; inline;
      function MemTransform1(const AChunk: TBytes): TBytes; inline;
      function MemTransform2(const AChunk: TBytes): TBytes; inline;
      function MemTransform3(const AChunk: TBytes): TBytes; inline;
      function MemTransform4(const AChunk: TBytes): TBytes; inline;
      function MemTransform5(const AChunk: TBytes): TBytes; inline;
      function MemTransform6(const AChunk: TBytes): TBytes; inline;
      function MemTransform7(const AChunk: TBytes): TBytes; inline;
      function MemTransform8(const AChunk: TBytes): TBytes; inline;
      function Expand(const AInput: TBytes; AExpansionFactor: Int32) : TBytes;
      function Compress(const AInputs: TArray<TBytes>): TBytes; inline;
      function ChangeNonce(const ABlockHeader: TBytes; ANonce: UInt32): TBytes; inline;
      function Checksum(const AInput: TBytes): UInt32; overload; inline;
      function Checksum(const AInput: TArray<TBytes>): UInt32; overload; inline;
      function Hash(const ABlockHeader: TBytes; ARound: Int32; out AFoundLastRound : Int32) : TArray<TBytes>; overload;
    public
      constructor Create;
      destructor Destroy; override;
      function Hash(const ABlockHeader: TBytes): TBytes; overload; inline;
      class function Compute(const ABlockHeader: TBytes): TBytes; overload; static; inline;
  end;

 { ERandomHash }

  ERandomHash2 = class(Exception);

resourcestring
  SUnSupportedHash = 'Unsupported Hash Selected';
  SInvalidRound = 'Round must be between 0 and N inclusive';
  SOverlappingArgs = 'Overlapping read/write regions';
  SBufferTooSmall = 'Buffer too small to apply memory transform';
  SBlockHeaderTooSmallForNonce = 'Buffer too small to contain nonce';

implementation

uses UCommon, UMemory, URandomHash;


{ TRandomHash2 }

constructor TRandomHash2.Create;
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

destructor TRandomHash2.Destroy;
var i : integer;
begin
 FMurmurHash3_x86_32 := nil;
 for i := Low(FHashAlg) to High(FHashAlg) do
   FHashAlg[i] := nil;
 inherited Destroy;
end;

class function TRandomHash2.Compute(const ABlockHeader: TBytes): TBytes;
var
  LHasher : TRandomHash2;
  LDisposables : TDisposables;
begin
 LHasher := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
 Result := LHasher.Hash(ABlockHeader);
end;

function TRandomHash2.Hash(const ABlockHeader: TBytes): TBytes;
var
  LAllOutputs: TArray<TBytes>;
  LSeed: UInt32;
  LLastRound : Int32;
begin
  LLastRound := 0;
  LAllOutputs := Hash(ABlockHeader, N, LLastRound);
  if LLastRound <= 0 then
    raise ERandomHash2.Create('Internal Error: 984F52997131417E8D63C43BD686F5B2'); // Should have found final round!
  Result := FHashAlg[0].ComputeBytes(Compress(LAllOutputs)).GetBytes;
end;

function TRandomHash2.Hash(const ABlockHeader: TBytes; ARound: Int32; out AFoundLastRound : Int32) : TArray<TBytes>;
var
  LRoundOutputs: TList<TBytes>;
  LNeighbourLastRound : Int32;
  LSeed, LNumNeighbours: UInt32;
  LGen: TMersenne32;
  LRoundInput, LNeighbourNonceHeader, LOutput, LBytes: TBytes;
  LParentOutputs, LNeighborOutputs, LToArray: TArray<TBytes>;
  LHashFunc: IHash;
  i: Int32;
  LDisposables : TDisposables;
begin
  if (ARound < 1) or (ARound > N) then
    raise EArgumentOutOfRangeException.CreateRes(@SInvalidRound);

  LRoundOutputs := LDisposables.AddObject( TList<TBytes>.Create() ) as TList<TBytes>;
  LGen := LDisposables.AddObject( TMersenne32.Create(0) ) as TMersenne32;
  if ARound = 1 then begin
    LSeed := Checksum(ABlockHeader);
    LGen.Initialize(LSeed);
    LRoundInput := ABlockHeader;
  end else begin
    LParentOutputs := Hash(ABlockHeader, ARound - 1, AFoundLastRound);
    if AFoundLastRound > 0 then
      // Previous round was the final round, so just return it's value
      Exit(LParentOutputs);

    // Add parent round outputs to this round outputs
    LSeed := Checksum(LParentOutputs);
    LGen.Initialize(LSeed);
    LRoundOutputs.AddRange( LParentOutputs );

    // Add neighbouring nonce outputs to this round outputs
    LNumNeighbours := (LGen.NextUInt32 MOD J)+1;
    for i := 0 to LNumNeighbours do begin
      LNeighbourNonceHeader := ChangeNonce(ABlockHeader, LGen.NextUInt32);
      LNeighborOutputs := Hash(LNeighbourNonceHeader, ARound - 1, LNeighbourLastRound);
      LRoundOutputs.AddRange(LNeighborOutputs);
    end;

    // Compress the parent/neighbouring outputs to form this rounds input
    LRoundInput := Compress( LRoundOutputs.ToArray );
  end;

  // Select a random hash function and hash the input
  // add this round output to outputs
  LHashFunc := FHashAlg[LGen.NextUInt32 mod NUM_HASH_ALGO];
  LOutput := LHashFunc.ComputeBytes(LRoundInput).GetBytes;

  // Determine if final round (last byte of hash mod N = 0)
  if (ARound = N) OR (LOutput[High(LOutput)] MOD N = 0) then
    AFoundLastRound := ARound;

  // Memory-expand the hash, add to output list and return
  LOutput := Expand(LOutput, N - ARound);
  LRoundOutputs.Add(LOutput);
  Result := LRoundOutputs.ToArray;
end;

function TRandomHash2.ChangeNonce(const ABlockHeader: TBytes;  ANonce: UInt32): TBytes;
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

function TRandomHash2.Checksum(const AInput: TBytes): UInt32;
var
  LSeed : UInt32;
  LLen, i, j : UInt32;
begin
  if Length(AInput) < 4 then
    raise ERandomHash2.Create('Can''t checksum arrays smaller than 4 bytes');

  // Seed the XorShift32 RNG with last little-endian DWORD of input
  LLen := Length(AInput);
  LSeed := AInput[LLen - 4] OR
          (AInput[LLen - 3] SHL 8) OR
          (AInput[LLen - 2] SHL 16) OR
          (AInput[LLen - 1] SHL 24);
  if LSeed = 0 then LSeed := 1;

  // The "checksum" is determined by picking random values from the input and
  // re-seeding using those values (and bit-rotating with other entropy sources).
  for i := 0 to 24 do begin
    // Build a new 32bit seed by selecting 4 random bytes selected using
    // the existing 32bit seed. The bytes are aggregated in LE sequence.
    // Note: XorShift alters LSeed each call
    LSeed := AInput[TXorShift32.Next(LSeed) MOD LLen] OR
            (AInput[TXorShift32.Next(LSeed) MOD LLen] SHL 8) OR
            (AInput[TXorShift32.Next(LSeed) MOD LLen] SHL 16) OR
            (AInput[TXorShift32.Next(LSeed) MOD LLen] SHL 24);
    if LSeed = 0 then LSeed := 1;

    // Bit-Rotate the new 32-byte seed using i and input length
    LSeed := TBits.RotateLeft32(LSeed, (LLen + i) MOD 32);
  end;
  Result := LSeed;
end;

function TRandomHash2.Checksum(const AInput : TArray<TBytes>): UInt32;
var
  i: Int32;
begin
  // shortcut just checksum last item
  Result := Checksum(AInput[High(AInput)]);
end;

function TRandomHash2.Compress(const AInputs : TArray<TBytes>): TBytes;
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

function TRandomHash2.ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes;
begin
  SetLength(Result, Length(AChunk1) + Length(AChunk2));
  Move(AChunk1[0], Result[0], Length(AChunk1));
  Move(AChunk2[0], Result[Length(AChunk1)], Length(AChunk2));
end;

function TRandomHash2.MemTransform1(const AChunk: TBytes): TBytes;
var
  i, LChunkLength : UInt32;
  LState : UInt32;
begin
  // Seed XorShift32 with non-zero seed (checksum of input or 1)
  LState := Checksum(AChunk);
  if LState = 0 then
    LState := 1;

  // Select random bytes from input using XorShift32 RNG
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := AChunk[TXorShift32.Next(LState) MOD LChunkLength];
end;

function TRandomHash2.MemTransform2(const AChunk: TBytes): TBytes;
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

function TRandomHash2.MemTransform3(const AChunk: TBytes): TBytes;
var
  i, LChunkLength: Int32;
begin
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := AChunk[LChunkLength - i - 1];
end;

function TRandomHash2.MemTransform4(const AChunk: TBytes): TBytes;
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

function TRandomHash2.MemTransform5(const AChunk: TBytes): TBytes;
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

function TRandomHash2.MemTransform6(const AChunk: TBytes): TBytes;
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

function TRandomHash2.MemTransform7(const AChunk: TBytes): TBytes;
var
  i, LChunkLength: Int32;
begin
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := TBits.RotateLeft8(AChunk[i], LChunkLength - i);
end;

function TRandomHash2.MemTransform8(const AChunk: TBytes): TBytes;
var
  i, LChunkLength: Int32;
begin
  LChunkLength := Length(AChunk);
  SetLength(Result, LChunkLength);
  for i := 0 to High(AChunk) do
    Result[i] := TBits.RotateRight8(AChunk[i], LChunkLength - i);
end;

function TRandomHash2.Expand(const AInput: TBytes; AExpansionFactor: Int32): TBytes;
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

end.
