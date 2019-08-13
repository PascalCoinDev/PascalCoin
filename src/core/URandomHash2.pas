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

uses {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF}, UCommon, SysUtils, HlpIHash, HlpBits, HlpHashFactory;

type

  { TRandomHash2 }

  TRandomHash2 = class sealed(TObject)
    const
      MIN_N = 2; // Min-number of hashing rounds required to compute a nonce
      MAX_N = 4; // Max-number of hashing rounds required to compute a nonce
      MIN_J = 1; // Min-number of dependent neighbouring nonces required to evaluate a nonce round
      MAX_J = 8; // Max-number of dependent neighbouring nonces required to evaluate a nonce round
      M = 64;    // The memory expansion unit (in bytes)
      NUM_HASH_ALGO = 18;

    {$IFNDEF UNITTESTS}private{$ELSE}public{$ENDIF}
      FHashAlg : array[0..17] of IHash;  // declared here to avoid race-condition during mining
      FMemStats : TStatistics;
      FCaptureMemStats : Boolean;

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
      function ComputeVeneerRound(const ARoundOutputs : TArray<TBytes>) : TBytes; inline;
      function CalculateRoundOutputs(const ABlockHeader: TBytes; ARound: Int32; out ARoundOutputs : TArray<TBytes>) : Boolean; overload;
    public
      constructor Create;
      destructor Destroy; override;
      property CaptureMemStats : Boolean read FCaptureMemStats write FCaptureMemStats;
      function TryHash(const ABlockHeader: TBytes; AMaxRound : UInt32; out AHash : TBytes) : Boolean;
      function Hash(const ABlockHeader: TBytes): TBytes; overload; inline;
      class function Compute(const ABlockHeader: TBytes): TBytes; overload; static; inline;
  end;

 { TRandomHash2Fast }

  TRandomHash2Fast = class sealed(TObject)
    const
      MIN_N = 2; // Min-number of hashing rounds required to compute a nonce
      MAX_N = 4; // Max-number of hashing rounds required to compute a nonce
      MIN_J = 1; // Min-number of dependent neighbouring nonces required to evaluate a nonce round
      MAX_J = 8; // Max-number of dependent neighbouring nonces required to evaluate a nonce round
      M = 64;    // The memory expansion unit (in bytes)
      NUM_HASH_ALGO = 18;

      public type

        TCachedHash = record
          Nonce : UInt32;
          Level : Integer;
          Header : TBytes;
          RoundOutputs : TArray<TBytes>;
        end;
        PCachedHash = ^TCachedHash;

        TCache = class
          private
            FEnablePartiallyComputed : boolean;          
            FHeaderTemplate : TBytes;
            FComputed : TList<TCachedHash>;
            FPartiallyComputed : TList<TCachedHash>;
            procedure PreProcessNewHash(const AHeader : TBytes);            
          public
            constructor Create;
            destructor Destroy;
            property EnablePartiallyComputed : boolean read FEnablePartiallyComputed write FEnablePartiallyComputed;
            procedure AddPartiallyComputed(const AHeader : TBytes; ALevel : Integer; const AOutputs : TArray<TBytes>); inline;
            procedure AddFullyComputed(const AHeader : TBytes; ALevel : Integer; const AHash : TBytes); inline;
            function HasComputedHash : Boolean; inline;
            function PopComputedHash : TCachedHash; inline;
            function HasNextPartiallyComputedHash : Boolean; inline; 
            function PeekNextPartiallyComputedHash : TCachedHash; inline;
            function PopNextPartiallyComputedHash : TCachedHash; inline;
            function ComputeMemorySize : Integer;
        end;

    {$IFNDEF UNITTESTS}private{$ELSE}public{$ENDIF}
      FHashAlg : array[0..17] of IHash;  // declared here to avoid race-condition during mining
      FCache : TCache;
      FMemStats : TStatistics;
      FCaptureMemStats : Boolean;
      FEnableCaching : Boolean;

      function ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes; inline;
      procedure MemTransform1(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      procedure MemTransform2(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      procedure MemTransform3(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      procedure MemTransform4(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      procedure MemTransform5(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      procedure MemTransform6(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      procedure MemTransform7(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      procedure MemTransform8(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer); inline;
      function Expand(const AInput: TBytes; AExpansionFactor: Int32; ASeed : UInt32) : TBytes;
      function Compress(const AInputs: TArray<TBytes>; ASeed : UInt32): TBytes;
      function ComputeVeneerRound(const ARoundOutputs : TArray<TBytes>) : TBytes;
      function CalculateRoundOutputs(const ABlockHeader: TBytes; ARound: Int32; APCachedHash : PCachedHash; out ARoundOutputs : TArray<TBytes>) : Boolean; overload;
    public
      constructor Create;
      destructor Destroy; override;
      property Cache : TCache read FCache;
      property EnableCaching : Boolean read FEnableCaching write FEnableCaching;
      property CaptureMemStats : Boolean read FCaptureMemStats write FCaptureMemStats;
      property MemStats : TStatistics read FMemStats;
      function TryHash(const ABlockHeader: TBytes; AMaxRound : UInt32; out AHash : TBytes) : Boolean;
      function Hash(const ABlockHeader: TBytes): TBytes; overload;
      function ResumeHash(const ACachedHash : TCachedHash): TBytes; overload;
      class function Compute(const ABlockHeader: TBytes): TBytes; overload; static;
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

uses UMemory, URandomHash;

{ TRandomHash2 }

constructor TRandomHash2.Create;
begin
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
  FMemStats.Reset;
end;

destructor TRandomHash2.Destroy;
var i : integer;
begin
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

function TRandomHash2.ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes;
begin
  SetLength(Result, Length(AChunk1) + Length(AChunk2));
  Move(AChunk1[0], Result[0], Length(AChunk1));
  Move(AChunk2[0], Result[Length(AChunk1)], Length(AChunk2));
end;

{ TRandomHash2Fast }

constructor TRandomHash2Fast.Create;
begin
  FEnableCaching := False;
  FCache := TCache.Create;
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
  FMemStats.Reset;
end;

destructor TRandomHash2Fast.Destroy;
var i : integer;
begin
 FCache.Destroy;
 inherited Destroy;
end;

class function TRandomHash2Fast.Compute(const ABlockHeader: TBytes): TBytes;
var
  LHasher : TRandomHash2Fast;
  LDisposables : TDisposables;
begin
 LHasher := LDisposables.AddObject( TRandomHash2Fast.Create ) as TRandomHash2Fast;
 Result := LHasher.Hash(ABlockHeader);
end;

function TRandomHash2Fast.TryHash(const ABlockHeader: TBytes; AMaxRound : UInt32; out AHash : TBytes) : Boolean;
var
  LOutputs: TArray<TBytes>;
  LSeed: UInt32;
begin
  if NOT CalculateRoundOutputs(ABlockHeader, AMaxRound, nil, LOutputs) then
    Exit(False);
  AHash := ComputeVeneerRound(LOutputs);
  Result := True;
end;

function TRandomHash2Fast.Hash(const ABlockHeader: TBytes): TBytes;
begin
  if NOT TryHash(ABlockHeader, MAX_N, Result) then
    raise ERandomHash2.Create('Internal Error: 974F52882131417E8D63A43BD686E5B2'); // Should have found final round!
end;


function TRandomHash2Fast.ResumeHash(const ACachedHash : TCachedHash): TBytes;
var
  LOutputs: TArray<TBytes>;
  LSeed: UInt32;
begin
  if NOT CalculateRoundOutputs(ACachedHash.Header, MAX_N, @ACachedHash, LOutputs) then
    raise ERandomHash2.Create('Internal Error: 274E52882131417E8C63A43BD686E5B4'); // Should have found final round!
  Result := ComputeVeneerRound(LOutputs);
end;

function TRandomHash2Fast.ComputeVeneerRound(const ARoundOutputs : TArray<TBytes>) : TBytes;
var
  LSeed : UInt32;
  LSize, i : Integer;
begin
  LSeed := GetLastDWordLE(ARoundOutputs[High(ARoundOutputs)]);
  // Final "veneer" round of RandomHash is a SHA2-256 of compression of prior round outputs
  Result := FHashAlg[0].ComputeBytes(Compress(ARoundOutputs, LSeed)).GetBytes;
  if FCaptureMemStats then begin
    LSize := 0;
    for i :=  Low(ARoundOutputs) to High(ARoundOutputs) do
      Inc(LSize, Length(ARoundOutputs[i]));
    if FEnableCaching then
      Inc(LSize, FCache.ComputeMemorySize);
    FMemStats.AddDatum(LSize);
  end;
end;

function TRandomHash2Fast.CalculateRoundOutputs(const ABlockHeader: TBytes; ARound: Int32; APCachedHash : PCachedHash; out ARoundOutputs : TArray<TBytes>) : Boolean;
var
  LRoundOutputs: TList<TBytes>;
  LNeighbourWasLastRound : Boolean;
  LSeed, LNumNeighbours: UInt32;
  LGen: TMersenne32;
  LRoundInput, LNeighbourNonceHeader, LOutput, LXX : TBytes;
  LCachedHash : TCachedHash;
  LParentOutputs, LNeighborOutputs, LToArray, LBuffs2: TArray<TBytes>;
  LHashFunc: IHash;
  i: Int32;
  LDisposables : TDisposables;
  LBuff : TBytes;
begin
  if (ARound < 1) or (ARound > MAX_N) then
    raise EArgumentOutOfRangeException.CreateRes(@SInvalidRound);   

  if Assigned(APCachedHash) AND (APCachedHash^.Level = ARound) AND (BytesEqual(APCachedHash^.Header, ABlockHeader))  then begin
    ARoundOutputs := APCachedHash^.RoundOutputs;
    Exit(False); // assume partially evaluated 
  end;

  LRoundOutputs := LDisposables.AddObject( TList<TBytes>.Create() ) as TList<TBytes>;
  LGen := LDisposables.AddObject( TMersenne32.Create(0) ) as TMersenne32;
  if ARound = 1 then begin
    LRoundInput := FHashAlg[0].ComputeBytes(ABlockHeader).GetBytes;
    LSeed := GetLastDWordLE( LRoundInput );
    LGen.Initialize(LSeed);
  end else begin
    if CalculateRoundOutputs(ABlockHeader, ARound - 1, APCachedHash, LParentOutputs) = True then begin
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
      LNeighbourWasLastRound := CalculateRoundOutputs(LNeighbourNonceHeader, ARound - 1, nil, LNeighborOutputs);
      LRoundOutputs.AddRange(LNeighborOutputs);

      // If neighbour was a fully evaluated nonce, cache it for re-use
      if FEnableCaching then 
        if LNeighbourWasLastRound then
          FCache.AddFullyComputed(LNeighbourNonceHeader, ARound - 1, ComputeVeneerRound(LNeighborOutputs) )
        else
          FCache.AddPartiallyComputed(LNeighbourNonceHeader, ARound - 1, LNeighborOutputs );
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

function TRandomHash2Fast.Compress(const AInputs : TArray<TBytes>; ASeed : UInt32): TBytes;
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

function TRandomHash2Fast.Expand(const AInput: TBytes; AExpansionFactor: Int32; ASeed : UInt32): TBytes;
var
  LOutput: TBytes;
  LReadEnd, LCopyLen, LInputSize : UInt32;
  LGen: TMersenne32;
  LDisposables : TDisposables;
begin
  LInputSize := Length (AInput);
  LGen := LDisposables.AddObject( TMersenne32.Create ( ASeed ) ) as TMersenne32;
  SetLength(LOutput, LInputSize + (AExpansionFactor * M));

  // Copy the genesis blob
  Move(AInput[0], LOutput[0], LInputSize);
  LReadEnd := LInputSize - 1;
  LCopyLen := LInputSize;

  while LReadEnd < Pred(Length(LOutput)) do
  begin
    if (LReadEnd + 1 + LCopyLen) > Length(LOutput) then
      LCopyLen := Length(LOutput) - (LReadEnd + 1);
    case LGen.NextUInt32 mod 8 of
      0: MemTransform1(LOutput, 0, LReadEnd+1, LCopyLen);
      1: MemTransform2(LOutput, 0, LReadEnd+1, LCopyLen);
      2: MemTransform3(LOutput, 0, LReadEnd+1, LCopyLen);
      3: MemTransform4(LOutput, 0, LReadEnd+1, LCopyLen);
      4: MemTransform5(LOutput, 0, LReadEnd+1, LCopyLen);
      5: MemTransform6(LOutput, 0, LReadEnd+1, LCopyLen);
      6: MemTransform7(LOutput, 0, LReadEnd+1, LCopyLen);
      7: MemTransform8(LOutput, 0, LReadEnd+1, LCopyLen);
    end;
    Inc(LReadEnd, LCopyLen);
    Inc(LCopyLen, LCopyLen);
  end;
  Result := LOutput;
end;

procedure TRandomHash2Fast.MemTransform1(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LReadEnd, LWriteEnd : UInt32;
  LState : UInt32;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);

  // Seed XorShift32 with last byte
  LState := GetDWordLE(ABuffer, LReadEnd-3);
  if LState = 0 then
    LState := 1;

  // Select random bytes from input using XorShift32 RNG
  for i := AWriteStart to LWriteEnd do
    ABuffer[i] := ABuffer[AReadStart + (TXorShift32.Next(LState) MOD ALength)];
end;

procedure TRandomHash2Fast.MemTransform2(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LReadEnd, LWriteEnd, LPivot, LOdd: Int32;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);
  if LWriteEnd >= Length(ABuffer) then
    raise EArgumentOutOfRangeException.Create(SBufferTooSmall);

  LPivot := ALength SHR 1;
  LOdd := ALength MOD 2;
  Move(ABuffer[AReadStart + LPivot + LOdd], ABuffer[AWriteStart], LPivot);
  Move(ABuffer[AReadStart], ABuffer[AWriteStart + LPivot + LOdd], LPivot);
  // Set middle-byte for odd-length arrays
  if LOdd = 1 then
    ABuffer[AWriteStart + LPivot] := ABuffer[AReadStart + LPivot];
end;

procedure TRandomHash2Fast.MemTransform3(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LReadEnd, LWriteEnd: Int32;
  LReadPtr, LWritePtr : PByte;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);
  if LWriteEnd >= Length(ABuffer) then
    raise EArgumentOutOfRangeException.Create(SBufferTooSmall);

  LReadPtr := PByte(ABuffer) + AReadStart;
  LWritePtr := PByte(ABuffer) + LWriteEnd;
  for i := 0 to Pred(ALength) do begin
    LWritePtr^ := LReadPtr^;
    Inc(LReadPtr);
    Dec(LWritePtr);
  end;
end;

procedure TRandomHash2Fast.MemTransform4(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LReadEnd, LWriteEnd, LPivot, LOdd: Int32;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);
  if LWriteEnd >= Length(ABuffer) then
    raise EArgumentOutOfRangeException.Create(SBufferTooSmall);

  LPivot := ALength SHR 1;
  LOdd := ALength MOD 2;
  for i := 0 to Pred(LPivot) do
  begin
    ABuffer[AWriteStart + (i * 2)] := ABuffer[AReadStart + i];
    ABuffer[AWriteStart + (i * 2) + 1] := ABuffer[AReadStart + i + LPivot + LOdd];
  end;
  // Set final byte for odd-lengths
  if LOdd = 1 THEN
    ABuffer[LWriteEnd] := ABuffer[AReadStart + LPivot];
end;

procedure TRandomHash2Fast.MemTransform5(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LReadEnd, LWriteEnd, LPivot, LOdd: Int32;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);
  if LWriteEnd >= Length(ABuffer) then
    raise EArgumentOutOfRangeException.Create(SBufferTooSmall);

  LPivot := ALength SHR 1;
  LOdd := ALength MOD 2;
  for i := 0 to Pred(LPivot) do
  begin
    ABuffer[AWriteStart + (i * 2)] := ABuffer[AReadStart + i + LPivot + LOdd];
    ABuffer[AWriteStart + (i * 2) + 1] := ABuffer[AReadStart + i];
  end;
  // Set final byte for odd-lengths
  if LOdd = 1 THEN
    ABuffer[LWriteEnd] := ABuffer[AReadStart + LPivot];
end;

procedure TRandomHash2Fast.MemTransform6(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LReadEnd, LWriteEnd, LPivot, LOdd: Int32;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);
  if LWriteEnd >= Length(ABuffer) then
    raise EArgumentOutOfRangeException.Create(SBufferTooSmall);

  LPivot := ALength SHR 1;
  LOdd := ALength MOD 2;
  for i := 0 to Pred(LPivot) do
  begin
    ABuffer[AWriteStart + i] := ABuffer[AReadStart + (i * 2)] xor ABuffer[AReadStart + (i * 2) + 1];
    ABuffer[AWriteStart + i + LPivot + LOdd] := ABuffer[AReadStart + i] xor ABuffer[AReadStart + ALength - i - 1];
  end;
  // Set middle-byte for odd-lengths
  if LOdd = 1 THEN
    ABuffer[AWriteStart + LPivot] := ABuffer[AReadStart + ALength - 1];
end;

procedure TRandomHash2Fast.MemTransform7(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LReadEnd, LWriteEnd: Int32;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);
  if LWriteEnd >= Length(ABuffer) then
    raise EArgumentOutOfRangeException.Create(SBufferTooSmall);

  for i := 0 to Pred(ALength) do
    ABuffer[AWriteStart + i] := TBits.RotateLeft8(ABuffer[AReadStart + i], ALength - i);
end;

procedure TRandomHash2Fast.MemTransform8(var ABuffer: TBytes; AReadStart, AWriteStart, ALength : Integer);
var
  i, LChunkLength, LReadEnd, LWriteEnd: Int32;
begin
  LReadEnd := AReadStart + ALength - 1;
  LWriteEnd := AWriteStart + ALength - 1;
  if LReadEnd >= AWriteStart then
    raise EArgumentOutOfRangeException.Create(SOverlappingArgs);
  if LWriteEnd >= Length(ABuffer) then
    raise EArgumentOutOfRangeException.Create(SBufferTooSmall);

  for i := 0 to Pred(ALength) do
    ABuffer[AWriteStart + i] := TBits.RotateRight8(ABuffer[AReadStart + i], ALength - i);
end;

function TRandomHash2Fast.ContencateByteArrays(const AChunk1, AChunk2: TBytes): TBytes;
begin
  SetLength(Result, Length(AChunk1) + Length(AChunk2));
  Move(AChunk1[0], Result[0], Length(AChunk1));
  Move(AChunk2[0], Result[Length(AChunk1)], Length(AChunk2));
end;

{ TRandomHash2Fast.TCache }

constructor TRandomHash2Fast.TCache.Create;
var i : Integer;
begin
  FEnablePartiallyComputed := false;
  FComputed := TList<TCachedHash>.Create;
  FComputed.Capacity := 100;
  SetLength(Self.FHeaderTemplate, 0);
  FPartiallyComputed := TList<TCachedHash>.Create;
  FPartiallyComputed.Capacity := 1000;
end;

destructor TRandomHash2Fast.TCache.Destroy;
var i : Integer;
begin
  FComputed.Clear;
  FreeAndNil(FComputed);
  FPartiallyComputed.Clear;
  FreeAndNil(FPartiallyComputed);
end;

procedure TRandomHash2Fast.TCache.AddPartiallyComputed(const AHeader : TBytes; ALevel : Integer; const AOutputs : TArray<TBytes>);
var
  LItem : TCachedHash;
begin
  PreProcessNewHash(AHeader);
  if NOT FEnablePartiallyComputed then
    Exit;

  // Only keep 10 level 3's partially calculated
  if (ALevel < 3) OR (FPartiallyComputed.Count > 10) then Exit;
    
  LItem.Nonce := GetLastDWordLE(AHeader);
  LItem.Header := AHeader;  
  LItem.Level := ALevel;
  LItem.RoundOutputs := AOutputs;
  FPartiallyComputed.Add(LItem);
end;

procedure TRandomHash2Fast.TCache.AddFullyComputed(const AHeader : TBytes; ALevel : Integer; const AHash : TBytes);
var
  LItem : TCachedHash;
begin
  PreProcessNewHash(AHeader);
  LItem.Nonce := GetLastDWordLE(AHeader);
  LItem.Header := AHeader;
  LItem.Level := ALevel;
  LItem.RoundOutputs := TArray<TBytes>.Create( AHash );
  FComputed.Add(LItem);
end;

procedure TRandomHash2Fast.TCache.PreProcessNewHash(const AHeader : TBytes);
var i : integer;
begin
  // if header is a new template, flush cache
  if NOT BytesEqual(FHeaderTemplate, AHeader, 0, 32 - 4) then begin
    FComputed.Clear;
    FComputed.Capacity := 100;    
    FPartiallyComputed.Clear;
    FPartiallyComputed.Capacity := 1000;
    FHeaderTemplate := SetLastDWordLE(AHeader, 0);
  end;  
end;
     
function TRandomHash2Fast.TCache.HasComputedHash : Boolean;
begin
  Result := FComputed.Count > 0;
end;

function TRandomHash2Fast.TCache.PopComputedHash : TCachedHash;
begin
  Result := FComputed.Last;
  FComputed.Delete(FComputed.Count - 1);
end;

function TRandomHash2Fast.TCache.HasNextPartiallyComputedHash : boolean;
begin
  Result := FPartiallyComputed.Count > 0;
end;

function TRandomHash2Fast.TCache.PeekNextPartiallyComputedHash : TCachedHash;
var i : Integer;
begin
  if FPartiallyComputed.Count > 0 then begin
    Result := FPartiallyComputed[0];
    Exit;
  end;
  raise Exception.Create('Cache is empty');
end;

function TRandomHash2Fast.TCache.PopNextPartiallyComputedHash : TCachedHash;
var 
  i : Integer;
  LList : TList<TCachedHash>;
begin
  if FPartiallyComputed.Count > 0 then begin
    Result := FPartiallyComputed[0];
    FPartiallyComputed.Delete(0);
    Exit;
  end;
  raise Exception.Create('Cache is empty');
end;

function TRandomHash2Fast.TCache.ComputeMemorySize : Integer;
var 
  i,j,k : Integer;
  LList : TList<TCachedHash>;
  LItem : TCachedHash;
begin
  Result := Length(FHeaderTemplate);
  Inc(Result, FComputed.Count * (4 + 4 + 32));
  for j := 0 to FPartiallyComputed.Count - 1 do begin
    LItem := FPartiallyComputed[j];
    Inc(Result, 4);
    Inc(Result, 4);
    Inc(Result, 32);            
    for k := Low(LItem.RoundOutputs) to High(LItem.RoundOutputs) do
      Inc(Result, Length(LItem.RoundOutputs[k])); 
  end;
end;


end.
