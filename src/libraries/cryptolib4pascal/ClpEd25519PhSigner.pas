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

unit ClpEd25519PhSigner;

{$I CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpIEd25519,
  ClpEd25519,
  ClpICipherParameters,
  ClpIEd25519PhSigner,
  ClpIEd25519PrivateKeyParameters,
  ClpIEd25519PublicKeyParameters,
  ClpEd25519PrivateKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SNotInitializedForSigning =
    'Ed25519PhSigner not Initialised for Signature Generation.';
  SNotInitializedForVerifying =
    'Ed25519PhSigner not Initialised for Verification';
  SPreHashDigestFailed = 'PreHash Digest Failed';

type
  TEd25519PhSigner = class(TInterfacedObject, IEd25519PhSigner)

  strict private
  var
    FPreHash: IDigest;
    FContext: TCryptoLibByteArray;
    FforSigning: Boolean;
    FEd25519Instance: IEd25519;
    FPrivateKey: IEd25519PrivateKeyParameters;
    FPublicKey: IEd25519PublicKeyParameters;

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

{ TEd25519PhSigner }

procedure TEd25519PhSigner.BlockUpdate(const buf: TCryptoLibByteArray;
  off, len: Int32);
begin
  FPreHash.BlockUpdate(buf, off, len);
end;

constructor TEd25519PhSigner.Create(const context: TCryptoLibByteArray);
begin
  Inherited Create();
  FContext := System.Copy(context);
  FEd25519Instance := TEd25519.Create();
  FPreHash := FEd25519Instance.CreatePreHash();
end;

destructor TEd25519PhSigner.Destroy;
begin
  inherited Destroy;
end;

function TEd25519PhSigner.GetAlgorithmName: String;
begin
  Result := 'Ed25519Ph';
end;

procedure TEd25519PhSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
begin
  FforSigning := forSigning;

  if (forSigning) then
  begin
    // TODO Allow IAsymmetricCipherKeyPair to be an ICipherParameters?

    FPrivateKey := parameters as IEd25519PrivateKeyParameters;
    FPublicKey := FPrivateKey.GeneratePublicKey();
  end
  else
  begin
    FPrivateKey := Nil;
    FPublicKey := parameters as IEd25519PublicKeyParameters;
  end;

  Reset();
end;

procedure TEd25519PhSigner.Reset;
begin
  FPreHash.Reset();
end;

procedure TEd25519PhSigner.Update(b: Byte);
begin
  FPreHash.Update(b);
end;

function TEd25519PhSigner.GenerateSignature: TCryptoLibByteArray;
var
  signature, msg: TCryptoLibByteArray;
begin
  if ((not FforSigning) or (FPrivateKey = Nil)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForSigning);
  end;
  System.SetLength(msg, TEd25519.PreHashSize);

  if ((TEd25519.PreHashSize) <> (FPreHash.DoFinal(msg, 0))) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SPreHashDigestFailed);
  end;

  System.SetLength(signature, TEd25519PrivateKeyParameters.SignatureSize);

  FPrivateKey.Sign(TEd25519.TEd25519Algorithm.Ed25519Ph, FPublicKey, FContext,
    msg, 0, TEd25519.PreHashSize, signature, 0);
  Result := signature;
end;

function TEd25519PhSigner.VerifySignature(const signature
  : TCryptoLibByteArray): Boolean;
var
  pk: TCryptoLibByteArray;
begin
  if ((FforSigning) or (FPublicKey = Nil)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForVerifying);
  end;
  if (TEd25519.SignatureSize <> System.Length(signature)) then
  begin
    Result := false;
    Exit;
  end;
  pk := FPublicKey.GetEncoded();
  Result := FEd25519Instance.VerifyPrehash(signature, 0, pk, 0, FContext,
    FPreHash);
end;

end.
