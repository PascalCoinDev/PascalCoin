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

unit ClpECNRSigner;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIDsaExt,
  ClpIECC,
  ClpIECNRSigner,
  ClpBigInteger,
  ClpBigIntegers,
  ClpISecureRandom,
  ClpIECKeyParameters,
  ClpIParametersWithRandom,
  ClpICipherParameters,
  ClpIECKeyPairGenerator,
  ClpECKeyPairGenerator,
  ClpECKeyGenerationParameters,
  ClpIECKeyGenerationParameters,
  ClpIECPrivateKeyParameters,
  ClpIECPublicKeyParameters,
  ClpIAsymmetricCipherKeyPair,
  ClpSecureRandom,
  ClpECAlgorithms,
  ClpCryptoLibTypes;

resourcestring
  SECPublicKeyNotFound = 'EC Public Key Required for Verification';
  SECPrivateKeyNotFound = 'EC Private Key Required for Signing';
  SNotInitializedForSigning = 'Not Initialised For Signing';
  SNotInitializedForVerifying = 'Not Initialised For Verifying';
  SNotInitializedForVerifyingRecovery =
    'Not Initialised For Verifying/Recovery';
  SInputTooLargeForECNRKey = 'Input Too Large For ECNR Key.';

type

  /// <summary>
  /// EC-NR as described in IEEE 1363-2000 - a signature algorithm for Elliptic Curve which
  /// also offers message recovery.
  /// </summary>
  TECNRSigner = class sealed(TInterfacedObject, IDsaExt, IECNRSigner)

  strict private
  var
    FForSigning: Boolean;
    FKey: IECKeyParameters;
    FRandom: ISecureRandom;

    function GetAlgorithmName: String;
    function GetOrder: TBigInteger;

    function ExtractT(const pubKey: IECPublicKeyParameters;
      const r, s: TBigInteger): TBigInteger;

  public

    property Order: TBigInteger read GetOrder;
    property AlgorithmName: String read GetAlgorithmName;

    /// <summary>
    /// Initialise the signer.
    /// </summary>
    /// <param name="forSigning">
    /// forSigning true if we are generating a signature, false for
    /// verification or if we want to use the signer for message recovery.
    /// </param>
    /// <param name="parameters">
    /// key parameters for signature generation.
    /// </param>
    procedure Init(forSigning: Boolean;
      const parameters: ICipherParameters); virtual;

    /// <summary>
    /// <para>
    /// Section 7.2.5 ECSP-NR, pg 34
    /// </para>
    /// <para>
    /// generate a signature for the given message using the key we were <br />
    /// initialised with. Generally, the order of the curve should be at <br />
    /// least as long as the hash of the message of interest, and with <br />
    /// ECNR it *must* be at least as long.
    /// </para>
    /// </summary>
    /// <param name="&amp;message">
    /// the digest to be signed.
    /// </param>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if the digest is longer than the key allows
    /// </exception>
    function GenerateSignature(const &message: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    /// <summary>
    /// <para>
    /// Section 7.2.6 ECVP-NR, pg 35
    /// </para>
    /// <para>
    /// return true if the value r and s represent a signature for the <br />
    /// message passed in. Generally, the order of the curve should be at
    /// <br />least as long as the hash of the message of interest, and
    /// with <br />ECNR, it *must* be at least as long. But just in case
    /// the signer <br />applied mod(n) to the longer digest, this
    /// implementation will <br />apply mod(n) during verification.
    /// </para>
    /// </summary>
    /// <param name="&amp;message">
    /// the digest to be verified.
    /// </param>
    /// <param name="r">
    /// the r value of the signature.
    /// </param>
    /// <param name="s">
    /// the s value of the signature.
    /// </param>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if the digest is longer than the key allows
    /// </exception>
    function VerifySignature(const &message: TCryptoLibByteArray;
      const r, s: TBigInteger): Boolean;

    /// <summary>
    /// Returns the data used for the signature generation, assuming the
    /// public key passed to Init() is correct.
    /// </summary>
    /// <returns>
    /// null if r and s are not valid.
    /// </returns>
    function GetRecoveredMessage(const r, s: TBigInteger): TCryptoLibByteArray;

  end;

implementation

{ TECNRSigner }

function TECNRSigner.ExtractT(const pubKey: IECPublicKeyParameters;
  const r, s: TBigInteger): TBigInteger;
var
  n, x: TBigInteger;
  G, W, P: IECPoint;
begin
  n := pubKey.parameters.n;

  // r in the range [1,n-1]
  if ((r.CompareTo(TBigInteger.ONE) < 0) or (r.CompareTo(n) >= 0)) then
  begin
    result := Default (TBigInteger);
    Exit;
  end;

  // s in the range [0,n-1]           NB: ECNR spec says 0
  if ((s.CompareTo(TBigInteger.ZERO) < 0) or (s.CompareTo(n) >= 0)) then
  begin
    result := Default (TBigInteger);
    Exit;
  end;

  // compute P = sG + rW

  G := pubKey.parameters.G;
  W := pubKey.Q;
  // calculate P using Bouncy math
  P := TECAlgorithms.SumOfTwoMultiplies(G, s, W, r).Normalize();

  // components must be bogus.
  if (P.IsInfinity) then
  begin
    result := Default (TBigInteger);
    Exit;
  end;

  x := P.AffineXCoord.ToBigInteger();

  result := r.Subtract(x).&Mod(n);
end;

function TECNRSigner.GenerateSignature(const &message: TCryptoLibByteArray)
  : TCryptoLibGenericArray<TBigInteger>;
var
  n, e, r, s, Vx, x, u: TBigInteger;
  privKey: IECPrivateKeyParameters;
  tempPair: IAsymmetricCipherKeyPair;
  keyGen: IECKeyPairGenerator;
  V: IECPublicKeyParameters;
begin
  if (not FForSigning) then
  begin
    // not properly initialized... deal with it
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForSigning);
  end;

  n := Order;

  e := TBigInteger.Create(1, &message);

  privKey := FKey as IECPrivateKeyParameters;

  if (e.CompareTo(n) >= 0) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputTooLargeForECNRKey);
  end;

  repeat // generate r
    // generate another, but very temporary, key pair using
    // the same EC parameters
    keyGen := TECKeyPairGenerator.Create();

    keyGen.Init(TECKeyGenerationParameters.Create(privKey.parameters, FRandom)
      as IECKeyGenerationParameters);

    tempPair := keyGen.GenerateKeyPair();

    V := tempPair.Public as IECPublicKeyParameters; // get temp's public key
    Vx := V.Q.AffineXCoord.ToBigInteger(); // get the point's x coordinate

    r := Vx.Add(e).&Mod(n);
  until (not(r.SignValue = 0));

  // generate s
  x := privKey.D; // private key value
  u := (tempPair.Private as IECPrivateKeyParameters).D;
  // temp's private key value
  s := u.Subtract(r.Multiply(x)).&Mod(n);

  result := TCryptoLibGenericArray<TBigInteger>.Create(r, s);
end;

function TECNRSigner.GetAlgorithmName: String;
begin
  result := 'ECNR';
end;

function TECNRSigner.GetOrder: TBigInteger;
begin
  result := FKey.parameters.n;
end;

procedure TECNRSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
var
  rParam: IParametersWithRandom;
  Lparameters: ICipherParameters;
begin
  FForSigning := forSigning;
  Lparameters := parameters;
  if (forSigning) then
  begin

    if (Supports(Lparameters, IParametersWithRandom, rParam)) then
    begin
      FRandom := rParam.random;
      Lparameters := rParam.parameters;
    end
    else
    begin
      FRandom := TSecureRandom.Create();
    end;

    if (not(Supports(Lparameters, IECPrivateKeyParameters))) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SECPrivateKeyNotFound);
    end;

    FKey := Lparameters as IECPrivateKeyParameters;
  end
  else
  begin
    if (not(Supports(Lparameters, IECPublicKeyParameters))) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SECPublicKeyNotFound);
    end;

    FKey := Lparameters as IECPublicKeyParameters;
  end;
end;

function TECNRSigner.VerifySignature(const &message: TCryptoLibByteArray;
  const r, s: TBigInteger): Boolean;
var
  pubKey: IECPublicKeyParameters;
  n, e, t: TBigInteger;
  nBitLength, eBitLength: Int32;
begin
  if (FForSigning) then
  begin
    // not properly initialized... deal with it
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForVerifying);
  end;

  pubKey := FKey as IECPublicKeyParameters;
  n := pubKey.parameters.n;
  nBitLength := n.BitLength;

  e := TBigInteger.Create(1, &message);
  eBitLength := e.BitLength;

  if (eBitLength > nBitLength) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputTooLargeForECNRKey);
  end;

  t := ExtractT(pubKey, r, s);

  result := (t.IsInitialized) and (t.Equals(e.&Mod(n)));
end;

function TECNRSigner.GetRecoveredMessage(const r, s: TBigInteger)
  : TCryptoLibByteArray;
var
  t: TBigInteger;
begin
  if (FForSigning) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForVerifyingRecovery);
  end;

  t := ExtractT(FKey as IECPublicKeyParameters, r, s);

  if (t.IsInitialized) then
  begin
    result := TBigIntegers.AsUnsignedByteArray(t);
    Exit;
  end;

  result := Nil;
end;

end.
