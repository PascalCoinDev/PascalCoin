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

unit ClpCryptoApiRandomGenerator;

{$I CryptoLib.inc}

interface

uses
  SyncObjs,
  ClpCryptoLibTypes,
  ClpIRandomNumberGenerator,
  ClpRandomNumberGenerator,
  ClpICryptoApiRandomGenerator,
  ClpIRandomGenerator;

resourcestring
  SNegativeOffset = 'Start Offset Cannot be Negative, "Start"';
  SArrayTooSmall = 'Byte Array Too Small For Requested Offset and Length';

type
  /// <summary>
  /// Uses TRandomNumberGenerator.Create() to Get randomness generator
  /// </summary>
  TCryptoApiRandomGenerator = class(TInterfacedObject,
    ICryptoApiRandomGenerator, IRandomGenerator)

  strict private
  var
    FrndProv: IRandomNumberGenerator;

    class var

      FLock: TCriticalSection;

    class procedure Boot(); static;
    class constructor CreateCryptoApiRandomGenerator();
    class destructor DestroyCryptoApiRandomGenerator();

  public
    /// <summary>
    /// Uses TRandomNumberGenerator.CreateRNG() to Get randomness generator
    /// </summary>
    constructor Create(); overload;
    constructor Create(const rng: IRandomNumberGenerator); overload;

    /// <summary>Add more seed material to the generator.</summary>
    /// <param name="seed">A byte array to be mixed into the generator's state.</param>
    procedure AddSeedMaterial(const seed: TCryptoLibByteArray);
      overload; virtual;

    /// <summary>Add more seed material to the generator.</summary>
    /// <param name="seed">A long value to be mixed into the generator's state.</param>
    procedure AddSeedMaterial(seed: Int64); overload; virtual;

    /// <summary>Fill byte array with random values.</summary>
    /// <param name="bytes">Array to be filled.</param>
    procedure NextBytes(const bytes: TCryptoLibByteArray); overload; virtual;

    /// <summary>Fill byte array with random values.</summary>
    /// <param name="bytes">Array to receive bytes.</param>
    /// <param name="start">Index to start filling at.</param>
    /// <param name="len">Length of segment to fill.</param>
    procedure NextBytes(const bytes: TCryptoLibByteArray; start, len: Int32);
      overload; virtual;

  end;

implementation

{ TCryptoApiRandomGenerator }

procedure TCryptoApiRandomGenerator.AddSeedMaterial(seed: Int64);
begin
  // We don't care about the seed
end;

class procedure TCryptoApiRandomGenerator.Boot;
begin
  if FLock = Nil then
  begin
    FLock := TCriticalSection.Create;
  end;
end;

procedure TCryptoApiRandomGenerator.AddSeedMaterial
  (const seed: TCryptoLibByteArray);
begin
  // We don't care about the seed
end;

constructor TCryptoApiRandomGenerator.Create(const rng: IRandomNumberGenerator);
begin
  Inherited Create();
  FrndProv := rng;
end;

class constructor TCryptoApiRandomGenerator.CreateCryptoApiRandomGenerator;
begin
  TCryptoApiRandomGenerator.Boot;
end;

class destructor TCryptoApiRandomGenerator.DestroyCryptoApiRandomGenerator;
begin
  FLock.Free;
end;

constructor TCryptoApiRandomGenerator.Create;
begin
  Create(TRandomNumberGenerator.CreateRNG());
end;

procedure TCryptoApiRandomGenerator.NextBytes(const bytes: TCryptoLibByteArray);
begin

  FLock.Acquire;
  try
    FrndProv.GetBytes(bytes);
  finally
    FLock.Release;
  end;
end;

procedure TCryptoApiRandomGenerator.NextBytes(const bytes: TCryptoLibByteArray;
  start, len: Int32);
var
  tmpBuf: TCryptoLibByteArray;
begin
  if (start < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNegativeOffset);
  end;
  if (System.Length(bytes) < (start + len)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SArrayTooSmall);

  end;

  if ((System.Length(bytes) = len) and (start = 0)) then
  begin
    NextBytes(bytes);
  end
  else
  begin
    System.SetLength(tmpBuf, len);
    NextBytes(tmpBuf);

    System.Move(tmpBuf[0], bytes[start], len * System.SizeOf(Byte));

  end;
end;

end.
