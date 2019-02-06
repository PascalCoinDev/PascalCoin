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

unit ClpECSchnorrSipaSigner;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIDigest,
  ClpISchnorrExt,
  ClpIECC,
  ClpIECSchnorrSipaSigner,
  ClpBigInteger,
  ClpBigIntegers,
  ClpISecureRandom,
  ClpIECKeyParameters,
  ClpIParametersWithRandom,
  ClpICipherParameters,
  ClpIECPrivateKeyParameters,
  ClpIECPublicKeyParameters,
  ClpSecureRandom,
  ClpECAlgorithms,
  ClpDigestUtilities,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SECPublicKeyNotFound = 'EC Public Key Required for Verification';
  SECPrivateKeyNotFound = 'EC Private Key Required for Signing';
  SNotInitializedForSigning = 'Not Initialised For Signing';
  SNotInitializedForVerifying = 'Not Initialised For Verifying';
  SSignatureGenerationError = 'An Error Occurred During Signature Generation';
  SOnlyFPCurvesAllowed =
    'Only FP (Prime Field) Curves are Allowed for This Schnorr Implementation';

type

  /// <summary>
  /// <para>
  /// Schnorr Signature as described in <see href="https://github.com/sipa/bips/blob/bip-schnorr/bip-schnorr.mediawiki">
  /// bip-schnorr</see>
  /// </para>
  /// <para>
  /// This <c>Schnorr</c> implementation only allows <c>FP(Prime Field)</c>
  /// Curves.
  /// </para>
  /// </summary>
  TECSchnorrSipaSigner = class sealed(TInterfacedObject, ISchnorrExt,
    IECSchnorrSipaSigner)

  strict private
  var
    FForSigning: Boolean;
    FKey: IECKeyParameters;
    FRandom: ISecureRandom;
    FDigest: IDigest;

    function GetAlgorithmName: String; virtual;
    function GetOrder: TBigInteger; virtual;

    function GetPP: TBigInteger; inline;
    function GetG: IECPoint; inline;
    function GetCurve: IECCurve; inline;

    property PP: TBigInteger read GetPP;
    property G: IECPoint read GetG;
    property Curve: IECCurve read GetCurve;

    class procedure ValidateAllowedCurves(const ACurve: IECCurve);
      static; inline;

    procedure Reset();

  public

    property Order: TBigInteger read GetOrder;
    property AlgorithmName: String read GetAlgorithmName;

    procedure Init(forSigning: Boolean; const parameters: ICipherParameters;
      const digest: IDigest); virtual;

    function GenerateSignature(const &message: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    function VerifySignature(const &message: TCryptoLibByteArray;
      const RSig, SSig: TBigInteger): Boolean; virtual;

  end;

implementation

{ TECSchnorrSipaSigner }

function TECSchnorrSipaSigner.GenerateSignature(const &message
  : TCryptoLibByteArray): TCryptoLibGenericArray<TBigInteger>;
var
  N, k, s, Xr, Yr, e, PrivateKey: TBigInteger;
  input, keyPrefixedM: TCryptoLibByteArray;
  P, r: IECPoint;
  numBytes: Int32;
begin
  if (not FForSigning) then
  begin
    // not properly initialized... deal with it
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForSigning);
  end;

  N := Order;
  numBytes := TBigIntegers.GetUnsignedByteLength(N);

  PrivateKey := (FKey as IECPrivateKeyParameters).D;

  input := TArrayUtils.Concatenate(TBigIntegers.BigIntegerToBytes(PrivateKey,
    numBytes), &message);

  k := TBigInteger.Create(1, TDigestUtilities.DoFinal(FDigest, input)).&Mod(N);

  if k.CompareTo(TBigInteger.Zero) = 0 then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SSignatureGenerationError);
  end;

  r := G.Multiply(k).Normalize();
  Xr := r.XCoord.ToBigInteger();
  Yr := r.YCoord.ToBigInteger();
  if (TBigInteger.Jacobi(Yr, PP) <> 1) then
  begin
    k := N.Subtract(k);
  end;

  P := G.Multiply(PrivateKey);
  keyPrefixedM := TArrayUtils.Concatenate(TBigIntegers.BigIntegerToBytes(Xr,
    numBytes), TCryptoLibMatrixByteArray.Create(P.GetEncoded(true), &message));

  e := TBigInteger.Create(1, TDigestUtilities.DoFinal(FDigest,
    keyPrefixedM)).&Mod(N);

  s := k.Add(e.Multiply(PrivateKey)).&Mod(N);

  result := TCryptoLibGenericArray<TBigInteger>.Create(Xr, s);
end;

function TECSchnorrSipaSigner.GetAlgorithmName: String;
begin
  result := 'ECSCHNORRSIPA';
end;

function TECSchnorrSipaSigner.GetCurve: IECCurve;
begin
  result := FKey.parameters.Curve;
end;

function TECSchnorrSipaSigner.GetG: IECPoint;
begin
  result := FKey.parameters.G;
end;

function TECSchnorrSipaSigner.GetOrder: TBigInteger;
begin
  result := FKey.parameters.N;
end;

function TECSchnorrSipaSigner.GetPP: TBigInteger;
begin
  result := Curve.Field.Characteristic;
end;

class procedure TECSchnorrSipaSigner.ValidateAllowedCurves
  (const ACurve: IECCurve);
begin
  if (not(TECAlgorithms.IsFpCurve(ACurve))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SOnlyFPCurvesAllowed);
  end;
end;

procedure TECSchnorrSipaSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters; const digest: IDigest);
var
  rParam: IParametersWithRandom;
  Lparameters: ICipherParameters;
begin
  FForSigning := forSigning;
  FDigest := digest;
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

  ValidateAllowedCurves(Curve);
  Reset();
end;

procedure TECSchnorrSipaSigner.Reset;
begin
  FDigest.Reset;
end;

function TECSchnorrSipaSigner.VerifySignature(const &message
  : TCryptoLibByteArray; const RSig, SSig: TBigInteger): Boolean;
var
  N, e: TBigInteger;
  PublicKeyBytes, input: TCryptoLibByteArray;
  PublicKey: IECPublicKeyParameters;
  P, Q, r: IECPoint;
  numBytes: Int32;
begin
  if (FForSigning) then
  begin
    // not properly initialized... deal with it
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SNotInitializedForVerifying);
  end;

  N := Order;
  numBytes := TBigIntegers.GetUnsignedByteLength(N);

  if ((RSig.CompareTo(PP) >= 0) or (SSig.CompareTo(N) >= 0)) then
  begin
    result := false;
    Exit;
  end;

  PublicKey := (FKey as IECPublicKeyParameters);
  PublicKeyBytes := PublicKey.Q.GetEncoded(true);

  input := TArrayUtils.Concatenate(TBigIntegers.BigIntegerToBytes(RSig,
    numBytes), TCryptoLibMatrixByteArray.Create(PublicKeyBytes, &message));

  e := TBigInteger.Create(1, TDigestUtilities.DoFinal(FDigest, input)).&Mod(N);
  Q := PublicKey.Q.Normalize();
  P := Curve.CreatePoint(Q.XCoord.ToBigInteger(), Q.YCoord.ToBigInteger());

  r := G.Multiply(SSig).Add(P.Multiply(N.Subtract(e))).Normalize();

  if ((r.IsInfinity) or (r.XCoord.ToBigInteger().CompareTo(RSig) <> 0) or
    (TBigInteger.Jacobi(r.YCoord.ToBigInteger(), PP) <> 1)) then
  begin
    result := false;
    Exit;
  end;

  result := true;
end;

end.
