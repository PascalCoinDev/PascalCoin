{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpDigestRandomGenerator;

{$I CryptoLib.inc}

interface

uses
  SyncObjs,
  ClpIDigest,
  ClpConverters,
  ClpCryptoLibTypes,
  ClpIDigestRandomGenerator,
  ClpIRandomGenerator;

type
  /// **
  // * Random generation based on the digest with counter. Calling AddSeedMaterial will
  // * always increase the entropy of the hash.
  // * <p>
  // * Internal access to the digest is synchronized so a single one of these can be shared.
  // * </p>
  // */
  TDigestRandomGenerator = class sealed(TInterfacedObject,
    IDigestRandomGenerator, IRandomGenerator)

  strict private
  const
    CYCLE_COUNT = Int64(10);

  var
    FstateCounter, FseedCounter: Int64;
    Fdigest: IDigest;
    Fstate, Fseed: TCryptoLibByteArray;

    procedure CycleSeed(); inline;
    procedure GenerateState(); inline;
    procedure DigestAddCounter(seedVal: Int64); inline;
    procedure DigestUpdate(const inSeed: TCryptoLibByteArray); inline;
    procedure DigestDoFinal(const result: TCryptoLibByteArray); inline;

    class var

      FLock: TCriticalSection;

    class procedure Boot(); static;
    class constructor CreateDigestRandomGenerator();
    class destructor DestroyDigestRandomGenerator();

  public

    constructor Create(const digest: IDigest);
    procedure AddSeedMaterial(const inSeed: TCryptoLibByteArray);
      overload; inline;
    procedure AddSeedMaterial(rSeed: Int64); overload; inline;
    procedure NextBytes(const bytes: TCryptoLibByteArray); overload; inline;
    procedure NextBytes(const bytes: TCryptoLibByteArray;
      start, len: Int32); overload;

  end;

implementation

{ TDigestRandomGenerator }

procedure TDigestRandomGenerator.DigestAddCounter(seedVal: Int64);
var
  bytes: TCryptoLibByteArray;
begin
  System.SetLength(bytes, 8);
  bytes := TConverters.ReadUInt64AsBytesLE(UInt64(seedVal));
  Fdigest.BlockUpdate(bytes, 0, System.Length(bytes));
end;

procedure TDigestRandomGenerator.DigestUpdate(const inSeed
  : TCryptoLibByteArray);
begin
  Fdigest.BlockUpdate(inSeed, 0, System.Length(inSeed));
end;

procedure TDigestRandomGenerator.DigestDoFinal(const result
  : TCryptoLibByteArray);
begin
  Fdigest.DoFinal(result, 0);
end;

procedure TDigestRandomGenerator.AddSeedMaterial(rSeed: Int64);
begin

  FLock.Acquire;
  try
    DigestAddCounter(rSeed);
    DigestUpdate(Fseed);
    DigestDoFinal(Fseed);
  finally
    FLock.Release;
  end;
end;

class procedure TDigestRandomGenerator.Boot;
begin
  if FLock = Nil then
  begin
    FLock := TCriticalSection.Create;
  end;
end;

procedure TDigestRandomGenerator.AddSeedMaterial(const inSeed
  : TCryptoLibByteArray);
begin

  FLock.Acquire;
  try
    DigestUpdate(inSeed);
    DigestUpdate(Fseed);
    DigestDoFinal(Fseed);
  finally
    FLock.Release;
  end;
end;

constructor TDigestRandomGenerator.Create(const digest: IDigest);
begin
  Inherited Create();
  Fdigest := digest;
  System.SetLength(Fseed, digest.GetDigestSize);
  FseedCounter := 1;
  System.SetLength(Fstate, digest.GetDigestSize);
  FstateCounter := 1;
end;

class constructor TDigestRandomGenerator.CreateDigestRandomGenerator;
begin
  TDigestRandomGenerator.Boot;
end;

procedure TDigestRandomGenerator.CycleSeed;
begin
  DigestUpdate(Fseed);
  DigestAddCounter(FseedCounter);
  System.Inc(FseedCounter);
  DigestDoFinal(Fseed);
end;

class destructor TDigestRandomGenerator.DestroyDigestRandomGenerator;
begin
  FLock.Free;
end;

procedure TDigestRandomGenerator.GenerateState;
begin
  DigestAddCounter(FstateCounter);
  System.Inc(FstateCounter);
  DigestUpdate(Fstate);
  DigestUpdate(Fseed);
  DigestDoFinal(Fstate);

  if ((FstateCounter mod CYCLE_COUNT) = 0) then
  begin
    CycleSeed();
  end;
end;

procedure TDigestRandomGenerator.NextBytes(const bytes: TCryptoLibByteArray);
begin
  NextBytes(bytes, 0, System.Length(bytes));
end;

procedure TDigestRandomGenerator.NextBytes(const bytes: TCryptoLibByteArray;
  start, len: Int32);
var
  stateOff, endPoint: Int32;
  I: Integer;
begin

  FLock.Acquire;
  try
    stateOff := 0;
    GenerateState();
    endPoint := start + len;

    for I := start to System.Pred(endPoint) do
    begin
      if (stateOff = System.Length(Fstate)) then
      begin
        GenerateState();
        stateOff := 0;
      end;
      bytes[I] := Fstate[stateOff];
      System.Inc(stateOff);
    end;

  finally
    FLock.Release;
  end;
end;

end.
