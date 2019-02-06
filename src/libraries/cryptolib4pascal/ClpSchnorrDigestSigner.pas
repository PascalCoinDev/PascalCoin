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

unit ClpSchnorrDigestSigner;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  SysUtils,
  Classes,
  ClpISchnorr,
  ClpISchnorrExt,
  ClpISignersEncodings,
  ClpISchnorrDigestSigner,
  ClpIDigest,
  ClpBigInteger,
  ClpCryptoLibTypes,
  ClpIParametersWithRandom,
  ClpSignersEncodings,
  ClpIAsymmetricKeyParameter,
  ClpICipherParameters,
  ClpISigner;

resourcestring
  SPrivateKey = 'Signing Requires Private Key.';
  SPublicKey = 'Verification Requires Public Key.';
  SSchnorrDigestSignerNotInitializedForSignatureGeneration =
    'SchnorrDigestSigner not Initialized for Signature Generation.';
  SSchnorrDigestSignerNotInitializedForVerification =
    'SchnorrDigestSigner not Initialized for Verification';
  SEncodingError = 'Unable to Encode Signature';

type
  TSchnorrDigestSigner = class(TInterfacedObject, ISigner, ISchnorrDigestSigner)

  strict private
  var
    FDigest: IDigest;
    FSchnorr: ISchnorr;
    FForSigning: Boolean;
    FEncoding: ISchnorrEncoding;
    FBuffer: TMemoryStream;

    function Aggregate: TCryptoLibByteArray; inline;

  strict protected

    function GetOrder(): TBigInteger; virtual;

  public
    // constructor Create(const Schnorr: ISchnorr; const digest: IDigest);
    // overload;
    constructor Create(const Schnorr: ISchnorrExt; const digest: IDigest;
      const encoding: ISchnorrEncoding);
    destructor Destroy(); override;

    function GetAlgorithmName: String; virtual;
    property AlgorithmName: String read GetAlgorithmName;

    procedure Init(forSigning: Boolean;
      const parameters: ICipherParameters); virtual;

    /// <summary>
    /// update the internal digest with the byte b
    /// </summary>
    procedure Update(input: Byte); virtual;

    /// <summary>
    /// update the internal digest with the byte array in
    /// </summary>
    procedure BlockUpdate(const input: TCryptoLibByteArray;
      inOff, length: Int32); virtual;

    /// <summary>
    /// Generate a signature for the message we've been loaded with using the
    /// key we were initialised with.
    /// </summary>
    function GenerateSignature(): TCryptoLibByteArray; virtual;

    /// <returns>
    /// true if the internal state represents the signature described in the
    /// passed in array.
    /// </returns>
    function VerifySignature(const signature: TCryptoLibByteArray)
      : Boolean; virtual;

    /// <summary>
    /// Reset the internal state
    /// </summary>
    procedure Reset(); virtual;

  end;

implementation

{ TSchnorrDigestSigner }

function TSchnorrDigestSigner.Aggregate: TCryptoLibByteArray;
begin
  FBuffer.Position := 0;
  System.SetLength(Result, FBuffer.Size);
  FBuffer.Read(Result[0], FBuffer.Size);
end;

procedure TSchnorrDigestSigner.BlockUpdate(const input: TCryptoLibByteArray;
  inOff, length: Int32);
begin
  FBuffer.Write(input[inOff], length);
end;

// constructor TSchnorrDigestSigner.Create(const Schnorr: ISchnorr;
// const digest: IDigest);
// begin
// Inherited Create();
// FSchnorr := Schnorr;
// FDigest := digest;
// FBuffer := TMemoryStream.Create();
// end;

constructor TSchnorrDigestSigner.Create(const Schnorr: ISchnorrExt;
  const digest: IDigest; const encoding: ISchnorrEncoding);
begin
  Inherited Create();
  FSchnorr := Schnorr;
  FDigest := digest;
  FEncoding := encoding;
  FBuffer := TMemoryStream.Create();
end;

destructor TSchnorrDigestSigner.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

function TSchnorrDigestSigner.GenerateSignature: TCryptoLibByteArray;
var
  sig: TCryptoLibGenericArray<TBigInteger>;
begin
  if ((not FForSigning)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SSchnorrDigestSignerNotInitializedForSignatureGeneration);
  end;

  sig := FSchnorr.GenerateSignature(Aggregate());

  try
    Result := FEncoding.Encode(GetOrder(), sig[0], sig[1]);
  except
    raise EInvalidOperationCryptoLibException.CreateRes(@SEncodingError);
  end;
end;

function TSchnorrDigestSigner.GetAlgorithmName: String;
begin
  Result := FDigest.AlgorithmName + 'with' + FSchnorr.AlgorithmName;
end;

function TSchnorrDigestSigner.GetOrder: TBigInteger;
begin
  if Supports(FSchnorr, ISchnorrExt) then
  begin
    Result := (FSchnorr as ISchnorrExt).Order;
  end
  else
  begin
    Result := Default (TBigInteger);
  end;
end;

procedure TSchnorrDigestSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
var
  k: IAsymmetricKeyParameter;
  withRandom: IParametersWithRandom;
begin
  FForSigning := forSigning;

  if (Supports(parameters, IParametersWithRandom, withRandom)) then
  begin
    k := withRandom.parameters as IAsymmetricKeyParameter;
  end
  else
  begin
    k := parameters as IAsymmetricKeyParameter;
  end;

  if ((forSigning) and (not k.IsPrivate)) then
  begin
    raise EInvalidKeyCryptoLibException.CreateRes(@SPrivateKey);
  end;

  if ((not forSigning) and (k.IsPrivate)) then
  begin
    raise EInvalidKeyCryptoLibException.CreateRes(@SPublicKey);
  end;

  Reset();

  FSchnorr.Init(forSigning, parameters, FDigest);
end;

procedure TSchnorrDigestSigner.Reset;
begin
  FDigest.Reset;
  FBuffer.Clear;
  FBuffer.SetSize(Int64(0));
end;

procedure TSchnorrDigestSigner.Update(input: Byte);
begin
  FBuffer.Write(TCryptoLibByteArray.Create(input)[0], 1);
end;

function TSchnorrDigestSigner.VerifySignature(const signature
  : TCryptoLibByteArray): Boolean;
var
  sig: TCryptoLibGenericArray<TBigInteger>;
begin
  if (FForSigning) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SSchnorrDigestSignerNotInitializedForVerification);
  end;

  try
    sig := FEncoding.Decode(GetOrder(), signature);
    Result := FSchnorr.VerifySignature(Aggregate(), sig[0], sig[1]);
  except
    Result := false;
  end;

end;

end.
