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

unit ClpEd25519Signer;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpIEd25519,
  ClpEd25519,
  ClpICipherParameters,
  ClpIEd25519Signer,
  ClpIEd25519PrivateKeyParameters,
  ClpIEd25519PublicKeyParameters,
  ClpEd25519PrivateKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SNotInitializedForSigning =
    'Ed25519Signer not Initialised for Signature Generation.';
  SNotInitializedForVerifying =
    'Ed25519Signer not Initialised for Verification';

type
  TEd25519Signer = class(TInterfacedObject, IEd25519Signer)

  strict private
  var
    FBuffer: TMemoryStream;
    FforSigning: Boolean;
    FEd25519Instance: IEd25519;
    FPrivateKey: IEd25519PrivateKeyParameters;
    FPublicKey: IEd25519PublicKeyParameters;

    function Aggregate: TCryptoLibByteArray; inline;

  strict protected
    function GetAlgorithmName: String; virtual;

  public
    constructor Create();
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

{ TEd25519Signer }

function TEd25519Signer.Aggregate: TCryptoLibByteArray;
begin
  FBuffer.Position := 0;
  System.SetLength(Result, FBuffer.Size);
  FBuffer.Read(Result[0], FBuffer.Size);
end;

procedure TEd25519Signer.BlockUpdate(const buf: TCryptoLibByteArray;
  off, len: Int32);
begin
  FBuffer.Write(buf[off], len);
end;

constructor TEd25519Signer.Create;
begin
  Inherited Create();
  FBuffer := TMemoryStream.Create();
  FEd25519Instance := TEd25519.Create();
end;

destructor TEd25519Signer.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

function TEd25519Signer.GetAlgorithmName: String;
begin
  Result := 'Ed25519';
end;

procedure TEd25519Signer.Init(forSigning: Boolean;
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

procedure TEd25519Signer.Reset;
begin
  FBuffer.Clear;
  FBuffer.SetSize(Int64(0));
end;

procedure TEd25519Signer.Update(b: Byte);
begin
  FBuffer.Write(TCryptoLibByteArray.Create(b)[0], 1);
end;

function TEd25519Signer.GenerateSignature: TCryptoLibByteArray;
var
  signature, buf: TCryptoLibByteArray;
  count: Int32;
begin
  if ((not FforSigning) or (FPrivateKey = Nil)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForSigning);
  end;

  System.SetLength(signature, TEd25519PrivateKeyParameters.SignatureSize);
  buf := Aggregate();
  count := System.Length(buf);

  FPrivateKey.Sign(TEd25519.TEd25519Algorithm.Ed25519, FPublicKey, Nil, buf, 0,
    count, signature, 0);
  Reset();
  Result := signature;
end;

function TEd25519Signer.VerifySignature(const signature
  : TCryptoLibByteArray): Boolean;
var
  buf, pk: TCryptoLibByteArray;
  count: Int32;
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
  buf := Aggregate();
  count := System.Length(buf);
  Result := FEd25519Instance.Verify(signature, 0, pk, 0, buf, 0, count);
  Reset();
end;

end.
