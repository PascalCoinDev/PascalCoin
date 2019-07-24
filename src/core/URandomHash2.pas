unit URandomHash2;

{ Copyright (c) 2018 by Herman Schoenfeld

  RandomHash2 Reference Implementation

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
      MIN_N = 2; // Min-number of hashing rounds required to compute a nonce, min total rounds = J^MIN_N
      MAX_N = 4; // Max-number of hashing rounds required to compute a nonce, max total rounds = J^MAX_N
      MIN_J = 0; // Min-number of dependent neighbouring nonces required to evaluate a nonce round
      MAX_J = 8; // Max-number of dependent neighbouring nonces required to evaluate a nonce round
      M = 256;    // The memory expansion unit (in bytes), max total bytes per nonce = M * ((MAX_J+1)^MAX_N (MAX_N-2) + 2)
      NUM_HASH_ALGO = 18;

      public type

        TCachedHash = record
          Nonce : UInt32;
          Header : TBytes;
          Hash : TBytes;
        end;

      {$IFNDEF UNITTESTS}private{$ELSE}public{$ENDIF}
      FMurmurHash3_x86_32 : IHash;
      FHashAlg : array[0..17] of IHash;  // declared here to avoid race-condition during mining
      FCachedHeaderTemplate : TBytes;
      FCachedHashes : TList<TCachedHash>;

      function GetCachedHashes : TArray<TCachedHash>; inline;
      function ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes; inline;
      function MemTransform1(const AChunk: TBytes): TBytes; inline;
      function MemTransform2(const AChunk: TBytes): TBytes; inline;
      function MemTransform3(const AChunk: TBytes): TBytes; inline;
      function MemTransform4(const AChunk: TBytes): TBytes; inline;
      function MemTransform5(const AChunk: TBytes): TBytes; inline;
      function MemTransform6(const AChunk: TBytes): TBytes; inline;
      function MemTransform7(const AChunk: TBytes): TBytes; inline;
      function MemTransform8(const AChunk: TBytes): TBytes; inline;
      function Expand(const AInput: TBytes; AExpansionFactor: Int32; ASeed : UInt32) : TBytes;
      function Compress(const AInputs: TArray<TBytes>; ASeed : UInt32): TBytes; inline;
      function SetLastDWordLE(const ABytes: TBytes; AValue: UInt32): TBytes; inline;
      function GetLastDWordLE(const ABytes: TBytes) : UInt32; inline;
      function ComputeVeneerRound(const ARoundOutputs : TArray<TBytes>) : TBytes; inline;
      function CalculateRoundOutputs(const ABlockHeader: TBytes; ARound: Int32; out ARoundOutputs : TArray<TBytes>) : Boolean; overload;
    public
      constructor Create;
      destructor Destroy; override;
      property CachedHashes : TArray<TCachedHash> read GetCachedHashes;
      function HasCachedHash : Boolean; inline;
      function PopCachedHash : TCachedHash; inline;
      function PeekCachedHash : TCachedHash; inline;
      function TryHash(const ABlockHeader: TBytes; AMaxRound : UInt32; out AHash : TBytes) : Boolean;
      function Hash(const ABlockHeader: TBytes): TBytes; overload; inline;
      class function Compute(const ABlockHeader: TBytes): TBytes; overload; static; inline;
  end;

 { ERandomHash2 }

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
  SetLength(Self.FCachedHeaderTemplate, 0);
  FCachedHashes := TList<TCachedHash>.Create;
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
 FCachedHashes.Clear;
 FreeAndNil(FCachedHashes);
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

function TRandomHash2.TryHash(const ABlockHeader: TBytes; AMaxRound : UInt32; out AHash : TBytes) : Boolean;
var
  LOutputs: TArray<TBytes>;
  LSeed: UInt32;
begin
  if NOT CalculateRoundOutputs(ABlockHeader, AMaxRound, LOutputs) then
    Exit(False);
  AHash := ComputeVeneerRound(LOutputs);
  Result := True;
end;

function TRandomHash2.Hash(const ABlockHeader: TBytes): TBytes;
begin
  if NOT TryHash(ABlockHeader, MAX_N, Result) then
    raise ERandomHash2.Create('Internal Error: 984F52997131417E8D63C43BD686F5B2'); // Should have found final round!
end;

function TRandomHash2.ComputeVeneerRound(const ARoundOutputs : TArray<TBytes>) : TBytes;
var
  LSeed : UInt32;
begin
  LSeed := GetLastDWordLE(ARoundOutputs[High(ARoundOutputs)]);
  // Final "veneer" round of RandomHash is a SHA2-256 of compression of prior round outputs
  Result := FHashAlg[0].ComputeBytes(Compress(ARoundOutputs, LSeed)).GetBytes;
end;

function TRandomHash2.CalculateRoundOutputs(const ABlockHeader: TBytes; ARound: Int32; out ARoundOutputs : TArray<TBytes>) : Boolean;
var
  LRoundOutputs: TList<TBytes>;
  LNeighbourWasLastRound : Boolean;
  LSeed, LNumNeighbours: UInt32;
  LGen: TMersenne32;
  LRoundInput, LNeighbourNonceHeader, LOutput : TBytes;
  LCachedHash : TCachedHash;
  LParentOutputs, LNeighborOutputs, LToArray, LBuffs2: TArray<TBytes>;
  LHashFunc: IHash;
  i: Int32;
  LDisposables : TDisposables;
  LBuff : TBytes;
begin
  if (ARound < 1) or (ARound > MAX_N) then
    raise EArgumentOutOfRangeException.CreateRes(@SInvalidRound);

  LRoundOutputs := LDisposables.AddObject( TList<TBytes>.Create() ) as TList<TBytes>;
  LGen := LDisposables.AddObject( TMersenne32.Create(0) ) as TMersenne32;
  if ARound = 1 then begin
    LRoundInput := FHashAlg[0].ComputeBytes(ABlockHeader).GetBytes;
    LSeed := GetLastDWordLE( LRoundInput );
    LGen.Initialize(LSeed);
  end else begin
    if CalculateRoundOutputs(ABlockHeader, ARound - 1, LParentOutputs) = True then begin
      // Previous round was the final round, so just return it's value
      ARoundOutputs := LParentOutputs;
      Exit(True);
    end;

    // Add parent round outputs to this round outputs
    LSeed := GetLastDWordLE( LParentOutputs[High(LParentOutputs)] );
    LGen.Initialize(LSeed);
    LRoundOutputs.AddRange( LParentOutputs );

    // Add neighbouring nonce outputs to this round outputs
    LNumNeighbours := (LGen.NextUInt32 MOD (MAX_J - MIN_J)) + MIN_J;
    for i := 1 to LNumNeighbours do begin
      LNeighbourNonceHeader := SetLastDWordLE(ABlockHeader, LGen.NextUInt32); // change nonce
      LNeighbourWasLastRound := CalculateRoundOutputs(LNeighbourNonceHeader, ARound - 1, LNeighborOutputs);
      LRoundOutputs.AddRange(LNeighborOutputs);

      // If neighbour was a fully evaluated nonce, cache it for re-use
      if LNeighbourWasLastRound then begin
        LCachedHash.Nonce := GetLastDWordLE(LNeighbourNonceHeader);
        LCachedHash.Header := LNeighbourNonceHeader;
        LCachedHash.Hash := ComputeVeneerRound(LNeighborOutputs);
        // if header is different (other than nonce), clear cache
        if NOT BytesEqual(FCachedHeaderTemplate, LCachedHash.Header, 0, 32 - 4) then begin
          FCachedHashes.Clear;
          FCachedHeaderTemplate := SetLastDWordLE(LCachedHash.Header, 0);
        end;
        FCachedHashes.Add(LCachedHash);
      end;
    end;
    // Compress the parent/neighbouring outputs to form this rounds input
    LRoundInput := Compress( LRoundOutputs.ToArray, LGen.NextUInt32 );
  end;

  // Select a random hash function and hash the input to find the output
  LHashFunc := FHashAlg[LGen.NextUInt32 mod NUM_HASH_ALGO];
  LOutput := LHashFunc.ComputeBytes(LRoundInput).GetBytes;

  // Memory-expand the output, add to output list and return output list
  LOutput := Expand(LOutput, MAX_N - ARound, LGen.NextUInt32);
  LRoundOutputs.Add(LOutput);
  ARoundOutputs := LRoundOutputs.ToArray;

  // Determine if final round
  Result := (ARound = MAX_N) OR ((ARound >= MIN_N) AND (GetLastDWordLE(LOutput) MOD MAX_N = 0));
end;

function TRandomHash2.SetLastDWordLE(const ABytes: TBytes; AValue: UInt32): TBytes;
var
  ABytesLength : Integer;
begin
  // Clone the original header
  Result := Copy(ABytes);

  // If digest not big enough to contain a nonce, just return the clone
  ABytesLength := Length(ABytes);
  if ABytesLength < 4 then
    exit;

  // Overwrite the nonce in little-endian
  Result[ABytesLength - 4] := Byte(AValue);
  Result[ABytesLength - 3] := (AValue SHR 8) AND 255;
  Result[ABytesLength - 2] := (AValue SHR 16) AND 255;
  Result[ABytesLength - 1] := (AValue SHR 24) AND 255;
end;

function TRandomHash2.GetLastDWordLE(const ABytes: TBytes) : UInt32;
var LLen : Integer;
begin
  LLen := Length(ABytes);
  if LLen < 4 then
   raise EArgumentOutOfRangeException.CreateRes(@SBlockHeaderTooSmallForNonce);

  // Last 4 bytes are nonce (LE)
  Result := ABytes[LLen - 4] OR
           (ABytes[LLen - 3] SHL 8) OR
           (ABytes[LLen - 2] SHL 16) OR
           (ABytes[LLen - 1] SHL 24);
end;


function TRandomHash2.Compress(const AInputs : TArray<TBytes>; ASeed : UInt32): TBytes;
var
  i: Int32;
  LSource: TBytes;
  LGen: TMersenne32;
  LDisposables : TDisposables;
begin
  SetLength(Result, 100);
  LGen := LDisposables.AddObject( TMersenne32.Create( ASeed ) ) as TMersenne32;
  for i := 0 to 99 do
  begin
    LSource := AInputs[LGen.NextUInt32 mod Length(AInputs)];
    Result[i] := LSource[LGen.NextUInt32 mod Length(LSource)];
  end;
end;

function TRandomHash2.GetCachedHashes : TArray<TCachedHash>;
begin
  Result := FCachedHashes.ToArray;
end;

function TRandomHash2.HasCachedHash : Boolean;
begin
  Result := FCachedHashes.Count > 0;
end;

function TRandomHash2.PopCachedHash : TCachedHash;
begin
  Result := FCachedHashes.Last;
  FCachedHashes.Delete(FCachedHashes.Count - 1);
end;

function TRandomHash2.PeekCachedHash : TCachedHash;
begin
  Result := FCachedHashes.Last;
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
  // Seed XorShift32 with last byte
  LState := GetLastDWordLE(AChunk);
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

function TRandomHash2.Expand(const AInput: TBytes; AExpansionFactor: Int32; ASeed : UInt32): TBytes;
var
  LSize, LBytesToAdd: Int32;
  LOutput, LNextChunk: TBytes;
  LRandom: UInt32;
  LGen: TMersenne32;
  LDisposables : TDisposables;
begin
  LGen := LDisposables.AddObject( TMersenne32.Create (ASeed) ) as TMersenne32;
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
