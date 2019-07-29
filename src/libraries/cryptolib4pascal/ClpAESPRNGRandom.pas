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

unit ClpAESPRNGRandom;

{$I CryptoLib.inc}

interface

uses
  SyncObjs,
  ClpAesEngine,
  ClpIAesEngine,
  ClpIBlockCipher,
  ClpIKeyParameter,
  ClpKeyParameter,
  ClpIBufferedCipher,
  ClpIBufferedBlockCipher,
  ClpBufferedBlockCipher,
  ClpArrayUtils,
  ClpOSRandom,
  ClpCryptoLibTypes;

resourcestring
  SInvalidAESRNGSeedLength =
    'AES RNG Seed Length must be either one of these "128/192/256 bits".';

type
  IAESPRNGRandom = interface(IInterface)
    ['{DE2F9387-CD63-475F-AD4B-EA0692790FB2}']

    procedure FillBytes(const data: TCryptoLibByteArray);
    procedure FillNonZeroBytes(const data: TCryptoLibByteArray);

  end;

type
  TAESPRNGRandom = class sealed(TInterfacedObject, IAESPRNGRandom)

  strict private
  const
    COUNTER_SIZE = Int32(16);
    class var

      FDefaultInstance: IAESPRNGRandom;

  var
    FLock: TCriticalSection;
    FCounter: TCryptoLibByteArray;
    FAESRNGSeedLength, FBytesSinceSeed, FReseedAfterBytes: Int32;
    FCipher: IBufferedCipher;

    class function GetDefaultInstance: IAESPRNGRandom; static; inline;

    class procedure GetRawEntropy(const AEntropy: TCryptoLibByteArray); inline;

    class procedure Boot(); static;
    class constructor CreateAESPRNGRandom();
    class destructor DestroyAESPRNGRandom();

    class procedure ValidateAESRNGSeedLength(ASeedLength: Int32);

    constructor Create(const AAESRNGSeed: TCryptoLibByteArray;
      AReseedAfterBytes: Int32); overload;

    procedure DoIncrementCounter();

    procedure DoSeed(const AAESRNGSeed: TCryptoLibByteArray);

  public
    constructor Create(AAESRNGSeedLength: Int32 = 32;
      AReseedAfterBytes: Int32 = 1024 * 1024); overload;

    destructor Destroy; override;

    procedure FillBytes(const data: TCryptoLibByteArray); overload;
    procedure FillNonZeroBytes(const data: TCryptoLibByteArray); overload;

    class procedure GetBytes(const data: TCryptoLibByteArray); overload; static;
    class procedure GetNonZeroBytes(const data: TCryptoLibByteArray);
      overload; static;

    class property DefaultInstance: IAESPRNGRandom read GetDefaultInstance;

  end;

implementation

{ TAESPRNGRandom }

class function TAESPRNGRandom.GetDefaultInstance: IAESPRNGRandom;
begin
  result := FDefaultInstance;
end;

class procedure TAESPRNGRandom.ValidateAESRNGSeedLength(ASeedLength: Int32);
begin
  if ((ASeedLength < 16) or (ASeedLength > 32) or ((ASeedLength and 7) <> 0))
  then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidAESRNGSeedLength);
  end;
end;

class procedure TAESPRNGRandom.GetRawEntropy(const AEntropy
  : TCryptoLibByteArray);
begin
  TOSRandom.GetBytes(AEntropy);
end;

class procedure TAESPRNGRandom.Boot;
begin
  if FDefaultInstance = Nil then
  begin
    FDefaultInstance := TAESPRNGRandom.Create();
  end;
end;

procedure TAESPRNGRandom.DoIncrementCounter;
var
  i: Int32;
begin
  for i := System.Low(FCounter) to System.High(FCounter) do
  begin
    System.Inc(FCounter[i]);
    // Check whether we need to loop again to carry the one.
    if (FCounter[i] <> 0) then
    begin
      break;
    end;
  end;
end;

procedure TAESPRNGRandom.DoSeed(const AAESRNGSeed: TCryptoLibByteArray);
var
  LKeyParameter: IKeyParameter;
begin
  LKeyParameter := TKeyParameter.Create(AAESRNGSeed);
  FLock.Acquire;
  try
    FCipher.Init(True, LKeyParameter);
    FBytesSinceSeed := 0;
  finally
    FLock.Release;
  end;
end;

constructor TAESPRNGRandom.Create(const AAESRNGSeed: TCryptoLibByteArray;
  AReseedAfterBytes: Int32);
var
  LAesEngine: IAesEngine;
  LBlockCipher: IBlockCipher;
  LAESRNGSeed: TCryptoLibByteArray;
begin
  Inherited Create();
  LAESRNGSeed := System.Copy(AAESRNGSeed);
  FLock := TCriticalSection.Create;
  // Set up engine
  LAesEngine := TAesEngine.Create();
  LBlockCipher := LAesEngine as IBlockCipher; // ECB no padding
  FCipher := TBufferedBlockCipher.Create(LBlockCipher) as IBufferedBlockCipher;
  System.SetLength(FCounter, COUNTER_SIZE);
  FAESRNGSeedLength := System.Length(LAESRNGSeed);
  FReseedAfterBytes := AReseedAfterBytes;
  ValidateAESRNGSeedLength(FAESRNGSeedLength);
  DoSeed(LAESRNGSeed);
  TArrayUtils.ZeroFill(LAESRNGSeed); // clear key from memory
end;

constructor TAESPRNGRandom.Create(AAESRNGSeedLength, AReseedAfterBytes: Int32);
var
  LSeed: TCryptoLibByteArray;
begin
  System.SetLength(LSeed, AAESRNGSeedLength);
  GetRawEntropy(LSeed); // pure entropy from OS
  Create(LSeed, AReseedAfterBytes);
  TArrayUtils.ZeroFill(LSeed); // clear seed from memory
end;

destructor TAESPRNGRandom.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TAESPRNGRandom.FillBytes(const data: TCryptoLibByteArray);
var
  LDataLength, LDatum, LResultLength: Int32;
  LSeed, LResult: TCryptoLibByteArray;
begin
  LDataLength := System.Length(data);
  if LDataLength <= 0 then
  begin
    Exit;
  end;

  if (FBytesSinceSeed > FReseedAfterBytes) then
  begin
    System.SetLength(LSeed, FAESRNGSeedLength);
    GetRawEntropy(LSeed); // pure entropy from OS
    DoSeed(LSeed);
    TArrayUtils.ZeroFill(LSeed); // clear seed from memory
  end;

  LDatum := 0;

  FLock.Acquire;
  try
    while (LDataLength shr 4) > 0 do
    begin
      DoIncrementCounter;
      LResultLength := FCipher.DoFinal(FCounter, data, LDatum);

      System.Inc(LDatum, LResultLength);
      System.Inc(FBytesSinceSeed, LResultLength);
      System.Dec(LDataLength, LResultLength);
    end;

    if LDataLength > 0 then
    begin
      DoIncrementCounter;
      LResult := FCipher.DoFinal(FCounter);
      System.Move(LResult[0], data[LDatum], LDataLength * System.SizeOf(Byte));
      System.Inc(FBytesSinceSeed, LDataLength);
    end;

  finally
    FLock.Release;
  end;
end;

procedure TAESPRNGRandom.FillNonZeroBytes(const data: TCryptoLibByteArray);
begin
  repeat
    FillBytes(data);
  until (TArrayUtils.NoZeroes(data));
end;

class constructor TAESPRNGRandom.CreateAESPRNGRandom;
begin
  TAESPRNGRandom.Boot();
end;

class destructor TAESPRNGRandom.DestroyAESPRNGRandom;
begin
  FDefaultInstance := Nil;
end;

class procedure TAESPRNGRandom.GetBytes(const data: TCryptoLibByteArray);
begin
  DefaultInstance.FillBytes(data);
end;

class procedure TAESPRNGRandom.GetNonZeroBytes(const data: TCryptoLibByteArray);
begin
  DefaultInstance.FillNonZeroBytes(data);
end;

end.
