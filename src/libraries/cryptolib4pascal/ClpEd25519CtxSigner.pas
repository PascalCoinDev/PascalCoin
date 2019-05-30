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

unit ClpEd25519CtxSigner;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpIEd25519,
  ClpEd25519,
  ClpICipherParameters,
  ClpIEd25519CtxSigner,
  ClpIEd25519PrivateKeyParameters,
  ClpIEd25519PublicKeyParameters,
  ClpEd25519PrivateKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SNotInitializedForSigning =
    'Ed25519CtxSigner not Initialised for Signature Generation.';
  SNotInitializedForVerifying =
    'Ed25519CtxSigner not Initialised for Verification';

type
  TEd25519CtxSigner = class(TInterfacedObject, IEd25519CtxSigner)

  strict private
  var
    FContext: TCryptoLibByteArray;
    FBuffer: TMemoryStream;
    FforSigning: Boolean;
    FEd25519Instance: IEd25519;
    FPrivateKey: IEd25519PrivateKeyParameters;
    FPublicKey: IEd25519PublicKeyParameters;

    function Aggregate: TCryptoLibByteArray; inline;

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

{ TEd25519CtxSigner }

function TEd25519CtxSigner.Aggregate: TCryptoLibByteArray;
begin
  FBuffer.Position := 0;
  System.SetLength(Result, FBuffer.Size);
  FBuffer.Read(Result[0], FBuffer.Size);
end;

procedure TEd25519CtxSigner.BlockUpdate(const buf: TCryptoLibByteArray;
  off, len: Int32);
begin
  FBuffer.Write(buf[off], len);
end;

constructor TEd25519CtxSigner.Create(const context: TCryptoLibByteArray);
begin
  Inherited Create();
  FBuffer := TMemoryStream.Create();
  FContext := System.Copy(context);
  FEd25519Instance := TEd25519.Create();
end;

destructor TEd25519CtxSigner.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

function TEd25519CtxSigner.GetAlgorithmName: String;
begin
  Result := 'Ed25519ctx';
end;

procedure TEd25519CtxSigner.Init(forSigning: Boolean;
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

procedure TEd25519CtxSigner.Reset;
begin
  FBuffer.Clear;
  FBuffer.SetSize(Int64(0));
end;

procedure TEd25519CtxSigner.Update(b: Byte);
begin
  FBuffer.Write(TCryptoLibByteArray.Create(b)[0], 1);
end;

function TEd25519CtxSigner.GenerateSignature: TCryptoLibByteArray;
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

  FPrivateKey.Sign(TEd25519.TEd25519Algorithm.Ed25519ctx, FPublicKey, FContext,
    buf, 0, count, signature, 0);
  Reset();
  Result := signature;
end;

function TEd25519CtxSigner.VerifySignature(const signature
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
  Result := FEd25519Instance.Verify(signature, 0, pk, 0, FContext, buf,
    0, count);
  Reset();
end;

end.
