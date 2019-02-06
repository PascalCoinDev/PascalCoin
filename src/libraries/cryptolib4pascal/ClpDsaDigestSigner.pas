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

unit ClpDsaDigestSigner;

{$I ..\..\Include\CryptoLib.inc}

interface

uses

  SysUtils,
  ClpIDsa,
  ClpIDsaExt,
  ClpIDigest,
  ClpBigInteger,
  ClpCryptoLibTypes,
  ClpIParametersWithRandom,
  ClpSignersEncodings,
  ClpIAsymmetricKeyParameter,
  ClpICipherParameters,
  ClpISigner,
  ClpISignersEncodings,
  ClpIDsaDigestSigner;

resourcestring
  SPrivateKey = 'Signing Requires Private Key.';
  SPublicKey = 'Verification Requires Public Key.';
  SDsaDigestSignerNotInitializedForSignatureGeneration =
    'DSADigestSigner not Initialized for Signature Generation.';
  SDsaDigestSignerNotInitializedForVerification =
    'DSADigestSigner not Initialized for Verification';
  SEncodingError = 'Unable to Encode Signature';

type
  TDsaDigestSigner = class(TInterfacedObject, ISigner, IDsaDigestSigner)

  strict private
  var
    Fdigest: IDigest;
    Fdsa: IDsa;
    Fencoding: IDsaEncoding;
    FforSigning: Boolean;

  strict protected

    function GetOrder(): TBigInteger; virtual;

  public
    constructor Create(const dsa: IDsa; const digest: IDigest); overload;
    constructor Create(const dsa: IDsaExt; const digest: IDigest;
      const encoding: IDsaEncoding); overload;

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

{ TDsaDigestSigner }

procedure TDsaDigestSigner.BlockUpdate(const input: TCryptoLibByteArray;
  inOff, length: Int32);
begin
  Fdigest.BlockUpdate(input, inOff, length);
end;

constructor TDsaDigestSigner.Create(const dsa: IDsa; const digest: IDigest);
begin
  Inherited Create();
  Fdsa := dsa;
  Fdigest := digest;
  Fencoding := TStandardDsaEncoding.Instance;
end;

constructor TDsaDigestSigner.Create(const dsa: IDsaExt; const digest: IDigest;
  const encoding: IDsaEncoding);
begin
  Inherited Create();
  Fdsa := dsa;
  Fdigest := digest;
  Fencoding := encoding;
end;

function TDsaDigestSigner.GenerateSignature: TCryptoLibByteArray;
var
  hash: TCryptoLibByteArray;
  sig: TCryptoLibGenericArray<TBigInteger>;
begin
  if ((not FforSigning)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SDsaDigestSignerNotInitializedForSignatureGeneration);
  end;

  System.SetLength(hash, Fdigest.GetDigestSize);

  Fdigest.DoFinal(hash, 0);

  sig := Fdsa.GenerateSignature(hash);

  try
    Result := Fencoding.Encode(GetOrder(), sig[0], sig[1]);
  except
    raise EInvalidOperationCryptoLibException.CreateRes(@SEncodingError);
  end;
end;

function TDsaDigestSigner.GetAlgorithmName: String;
begin
  Result := Fdigest.AlgorithmName + 'with' + Fdsa.AlgorithmName;
end;

function TDsaDigestSigner.GetOrder: TBigInteger;
begin
  if Supports(Fdsa, IDsaExt) then
  begin
    Result := (Fdsa as IDsaExt).Order;
  end
  else
  begin
    Result := Default (TBigInteger);
  end;
end;

procedure TDsaDigestSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
var
  k: IAsymmetricKeyParameter;
  withRandom: IParametersWithRandom;
begin
  FforSigning := forSigning;

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

  Fdsa.Init(forSigning, parameters);
end;

procedure TDsaDigestSigner.Reset;
begin
  Fdigest.Reset;
end;

procedure TDsaDigestSigner.Update(input: Byte);
begin
  Fdigest.Update(input);
end;

function TDsaDigestSigner.VerifySignature(const signature
  : TCryptoLibByteArray): Boolean;
var
  hash: TCryptoLibByteArray;
  sig: TCryptoLibGenericArray<TBigInteger>;
begin
  if (FforSigning) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SDsaDigestSignerNotInitializedForVerification);
  end;

  System.SetLength(hash, Fdigest.GetDigestSize);

  Fdigest.DoFinal(hash, 0);

  try
    sig := Fencoding.Decode(GetOrder(), signature);
    Result := Fdsa.VerifySignature(hash, sig[0], sig[1]);
  except
    Result := false;
  end;

end;

end.
