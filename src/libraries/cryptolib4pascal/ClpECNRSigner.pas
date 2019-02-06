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

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIDsaExt,
  ClpIECC,
  ClpIECNRSigner,
  ClpBigInteger,
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
  SInputTooLargeForECNRKey = 'Input Too Large For ECNR Key.';

type

  /// <summary>
  /// EC-NR as described in IEEE 1363-2000
  /// </summary>
  TECNRSigner = class sealed(TInterfacedObject, IDsaExt, IECNRSigner)

  strict private
  var
    FforSigning: Boolean;
    Fkey: IECKeyParameters;
    Frandom: ISecureRandom;

    function GetAlgorithmName: String; virtual;
    function GetOrder: TBigInteger; virtual;

  public

    property Order: TBigInteger read GetOrder;
    property AlgorithmName: String read GetAlgorithmName;

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

  end;

implementation

{ TECNRSigner }

function TECNRSigner.GenerateSignature(const &message: TCryptoLibByteArray)
  : TCryptoLibGenericArray<TBigInteger>;
var
  n, e, r, s, Vx, x, u: TBigInteger;
  nBitLength, eBitLength: Int32;
  privKey: IECPrivateKeyParameters;
  tempPair: IAsymmetricCipherKeyPair;
  keyGen: IECKeyPairGenerator;
  V: IECPublicKeyParameters;
begin
  if (not FforSigning) then
  begin
    // not properly initialized... deal with it
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForSigning);
  end;

  n := Order;
  nBitLength := n.BitLength;

  e := TBigInteger.Create(1, &message);
  eBitLength := e.BitLength;

  privKey := Fkey as IECPrivateKeyParameters;

  if (eBitLength > nBitLength) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputTooLargeForECNRKey);
  end;

  repeat // generate r
    // generate another, but very temporary, key pair using
    // the same EC parameters
    keyGen := TECKeyPairGenerator.Create();

    keyGen.Init(TECKeyGenerationParameters.Create(privKey.parameters, Frandom)
      as IECKeyGenerationParameters);

    tempPair := keyGen.GenerateKeyPair();

    // BigInteger Vx := tempPair.getPublic().getW().getAffineX();
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
  result := Fkey.parameters.n;
end;

procedure TECNRSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
var
  rParam: IParametersWithRandom;
  Lparameters: ICipherParameters;
begin
  FforSigning := forSigning;
  Lparameters := parameters;
  if (forSigning) then
  begin

    if (Supports(Lparameters, IParametersWithRandom, rParam)) then
    begin
      Frandom := rParam.random;
      Lparameters := rParam.parameters;
    end
    else
    begin
      Frandom := TSecureRandom.Create();
    end;

    if (not(Supports(Lparameters, IECPrivateKeyParameters))) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SECPrivateKeyNotFound);
    end;

    Fkey := Lparameters as IECPrivateKeyParameters;
  end
  else
  begin
    if (not(Supports(Lparameters, IECPublicKeyParameters))) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SECPublicKeyNotFound);
    end;

    Fkey := Lparameters as IECPublicKeyParameters;
  end;
end;

function TECNRSigner.VerifySignature(const &message: TCryptoLibByteArray;
  const r, s: TBigInteger): Boolean;
var
  pubKey: IECPublicKeyParameters;
  n, e, x, t: TBigInteger;
  nBitLength, eBitLength: Int32;
  G, W, P: IECPoint;
begin
  if (FforSigning) then
  begin
    // not properly initialized... deal with it
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForVerifying);
  end;

  pubKey := Fkey as IECPublicKeyParameters;
  n := pubKey.parameters.n;
  nBitLength := n.BitLength;

  e := TBigInteger.Create(1, &message);
  eBitLength := e.BitLength;

  if (eBitLength > nBitLength) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputTooLargeForECNRKey);
  end;

  // r in the range [1,n-1]
  if ((r.CompareTo(TBigInteger.One) < 0) or (r.CompareTo(n) >= 0)) then
  begin
    result := false;
    Exit;
  end;

  // s in the range [0,n-1]           NB: ECNR spec says 0
  if ((s.CompareTo(TBigInteger.Zero) < 0) or (s.CompareTo(n) >= 0)) then
  begin
    result := false;
    Exit;
  end;

  // compute P = sG + rW

  G := pubKey.parameters.G;
  W := pubKey.Q;
  // calculate P using ECAlgorithms Math
  P := TECAlgorithms.SumOfTwoMultiplies(G, s, W, r).Normalize();

  if (P.IsInfinity) then
  begin
    result := false;
    Exit;
  end;

  x := P.AffineXCoord.ToBigInteger();
  t := r.Subtract(x).&Mod(n);

  result := t.Equals(e);
end;

end.
