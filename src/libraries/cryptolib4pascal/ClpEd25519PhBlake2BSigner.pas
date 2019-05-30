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

unit ClpEd25519PhBlake2BSigner;

{$I CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpIEd25519Blake2B,
  ClpEd25519Blake2B,
  ClpICipherParameters,
  ClpIEd25519PhBlake2BSigner,
  ClpIEd25519Blake2BPrivateKeyParameters,
  ClpIEd25519Blake2BPublicKeyParameters,
  ClpEd25519Blake2BPrivateKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SNotInitializedForSigning =
    'Ed25519PhBlake2BSigner not Initialised for Signature Generation.';
  SNotInitializedForVerifying =
    'Ed25519PhBlake2BSigner not Initialised for Verification';
  SPreHashDigestFailed = 'PreHash Digest Failed';

type
  TEd25519PhBlake2BSigner = class(TInterfacedObject, IEd25519PhBlake2BSigner)

  strict private
  var
    FPreHash: IDigest;
    FContext: TCryptoLibByteArray;
    FforSigning: Boolean;
    FEd25519Blake2BInstance: IEd25519Blake2B;
    FPrivateKey: IEd25519Blake2BPrivateKeyParameters;
    FPublicKey: IEd25519Blake2BPublicKeyParameters;

  strict protected
    function GetAlgorithmName: String; virtual;

  public
    constructor Create(const context: TCryptoLibByteArray);
    destructor Destroy(); override;

    procedure Init(forSigning: Boolean;
      const parameters: ICipherParameters); virtual;
    procedure Update(b: Byte); virtual;
    procedure BlockUpdate(const buf: TCryptoLibByteArray;
      off, len: Int32); virtual;
    function GenerateSignature(): TCryptoLibByteArray; virtual;
    function VerifySignature(const signature: TCryptoLibByteArray)
      : Boolean; virtual;
    procedure Reset(); virtual;

    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

{ TEd25519PhBlake2BSigner }

procedure TEd25519PhBlake2BSigner.BlockUpdate(const buf: TCryptoLibByteArray;
  off, len: Int32);
begin
  FPreHash.BlockUpdate(buf, off, len);
end;

constructor TEd25519PhBlake2BSigner.Create(const context: TCryptoLibByteArray);
begin
  Inherited Create();
  FContext := System.Copy(context);
  FEd25519Blake2BInstance := TEd25519Blake2B.Create();
  FPreHash := FEd25519Blake2BInstance.CreatePreHash();
end;

destructor TEd25519PhBlake2BSigner.Destroy;
begin
  inherited Destroy;
end;

function TEd25519PhBlake2BSigner.GetAlgorithmName: String;
begin
  Result := 'Ed25519PhBlake2B';
end;

procedure TEd25519PhBlake2BSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
begin
  FforSigning := forSigning;

  if (forSigning) then
  begin
    // TODO Allow IAsymmetricCipherKeyPair to be an ICipherParameters?

    FPrivateKey := parameters as IEd25519Blake2BPrivateKeyParameters;
    FPublicKey := FPrivateKey.GeneratePublicKey();
  end
  else
  begin
    FPrivateKey := Nil;
    FPublicKey := parameters as IEd25519Blake2BPublicKeyParameters;
  end;

  Reset();
end;

procedure TEd25519PhBlake2BSigner.Reset;
begin
  FPreHash.Reset();
end;

procedure TEd25519PhBlake2BSigner.Update(b: Byte);
begin
  FPreHash.Update(b);
end;

function TEd25519PhBlake2BSigner.GenerateSignature: TCryptoLibByteArray;
var
  signature, msg: TCryptoLibByteArray;
begin
  if ((not FforSigning) or (FPrivateKey = Nil)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForSigning);
  end;
  System.SetLength(msg, TEd25519Blake2B.PreHashSize);

  if ((TEd25519Blake2B.PreHashSize) <> (FPreHash.DoFinal(msg, 0))) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SPreHashDigestFailed);
  end;

  System.SetLength(signature,
    TEd25519Blake2BPrivateKeyParameters.SignatureSize);

  FPrivateKey.Sign(TEd25519Blake2B.TEd25519Algorithm.Ed25519Ph, FPublicKey,
    FContext, msg, 0, TEd25519Blake2B.PreHashSize, signature, 0);
  Result := signature;
end;

function TEd25519PhBlake2BSigner.VerifySignature(const signature
  : TCryptoLibByteArray): Boolean;
var
  pk: TCryptoLibByteArray;
begin
  if ((FforSigning) or (FPublicKey = Nil)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForVerifying);
  end;
  if (TEd25519Blake2B.SignatureSize <> System.Length(signature)) then
  begin
    Result := false;
    Exit;
  end;
  pk := FPublicKey.GetEncoded();
  Result := FEd25519Blake2BInstance.VerifyPrehash(signature, 0, pk, 0, FContext,
    FPreHash);
end;

end.
