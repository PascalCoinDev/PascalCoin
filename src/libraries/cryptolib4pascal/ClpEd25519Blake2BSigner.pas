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

unit ClpEd25519Blake2BSigner;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpIEd25519Blake2B,
  ClpEd25519Blake2B,
  ClpICipherParameters,
  ClpIEd25519Blake2BSigner,
  ClpIEd25519Blake2BPrivateKeyParameters,
  ClpIEd25519Blake2BPublicKeyParameters,
  ClpEd25519Blake2BPrivateKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SNotInitializedForSigning =
    'Ed25519Blake2BSigner not Initialised for Signature Generation.';
  SNotInitializedForVerifying =
    'Ed25519Blake2BSigner not Initialised for Verification';

type
  TEd25519Blake2BSigner = class(TInterfacedObject, IEd25519Blake2BSigner)

  strict private
  var
    FBuffer: TMemoryStream;
    FforSigning: Boolean;
    FEd25519Blake2BInstance: IEd25519Blake2B;
    FPrivateKey: IEd25519Blake2BPrivateKeyParameters;
    FPublicKey: IEd25519Blake2BPublicKeyParameters;

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

{ TEd25519Blake2BSigner }

function TEd25519Blake2BSigner.Aggregate: TCryptoLibByteArray;
begin
  FBuffer.Position := 0;
  System.SetLength(Result, FBuffer.Size);
  FBuffer.Read(Result[0], FBuffer.Size);
end;

procedure TEd25519Blake2BSigner.BlockUpdate(const buf: TCryptoLibByteArray;
  off, len: Int32);
begin
  FBuffer.Write(buf[off], len);
end;

constructor TEd25519Blake2BSigner.Create;
begin
  Inherited Create();
  FBuffer := TMemoryStream.Create();
  FEd25519Blake2BInstance := TEd25519Blake2B.Create();
end;

destructor TEd25519Blake2BSigner.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

function TEd25519Blake2BSigner.GetAlgorithmName: String;
begin
  Result := 'Ed25519Blake2B';
end;

procedure TEd25519Blake2BSigner.Init(forSigning: Boolean;
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

procedure TEd25519Blake2BSigner.Reset;
begin
  FBuffer.Clear;
  FBuffer.SetSize(Int64(0));
end;

procedure TEd25519Blake2BSigner.Update(b: Byte);
begin
  FBuffer.Write(TCryptoLibByteArray.Create(b)[0], 1);
end;

function TEd25519Blake2BSigner.GenerateSignature: TCryptoLibByteArray;
var
  signature, buf: TCryptoLibByteArray;
  count: Int32;
begin
  if ((not FforSigning) or (FPrivateKey = Nil)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForSigning);
  end;

  System.SetLength(signature,
    TEd25519Blake2BPrivateKeyParameters.SignatureSize);
  buf := Aggregate();
  count := System.Length(buf);

  FPrivateKey.Sign(TEd25519Blake2B.TEd25519Algorithm.Ed25519, FPublicKey, Nil,
    buf, 0, count, signature, 0);
  Reset();
  Result := signature;
end;

function TEd25519Blake2BSigner.VerifySignature(const signature
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
  if (TEd25519Blake2B.SignatureSize <> System.Length(signature)) then
  begin
    Result := false;
    Exit;
  end;
  pk := FPublicKey.GetEncoded();
  buf := Aggregate();
  count := System.Length(buf);
  Result := FEd25519Blake2BInstance.Verify(signature, 0, pk, 0, buf, 0, count);
  Reset();
end;

end.
