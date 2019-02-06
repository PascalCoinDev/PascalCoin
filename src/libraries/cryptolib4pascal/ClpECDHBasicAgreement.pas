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

unit ClpECDHBasicAgreement;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpECAlgorithms,
  ClpICipherParameters,
  ClpIECDomainParameters,
  ClpIECC,
  ClpIBasicAgreement,
  ClpIECDHBasicAgreement,
  ClpIECPrivateKeyParameters,
  ClpIParametersWithRandom,
  ClpIECPublicKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SWrongDomainParameter = 'ECDH Public Key has Wrong Domain Parameters';
  SInvalidAgreementValue = 'Infinity is not a Valid Agreement Value for ECDH';
  SInfinityInvalidPublicKey = 'Infinity is not a Valid Public Key for ECDH';

type
  /// <summary>
  /// P1363 7.2.1 ECSVDP-DH <br />ECSVDP-DH is Elliptic Curve Secret Value
  /// Derivation Primitive, <br />Diffie-Hellman version. It is based on the
  /// work of [DH76], [Mil86], <br />and [Kob87]. This primitive derives a
  /// shared secret value from one <br />party's private key and another
  /// party's public key, where both have <br />the same set of EC domain
  /// parameters. If two parties correctly <br />execute this primitive, they
  /// will produce the same output. This <br />primitive can be invoked by a
  /// scheme to derive a shared secret key; <br />specifically, it may be
  /// used with the schemes ECKAS-DH1 and <br />DL/ECKAS-DH2. It assumes that
  /// the input keys are valid (see also <br />Section 7.2.2). <br />
  /// </summary>
  TECDHBasicAgreement = class(TInterfacedObject, IECDHBasicAgreement,
    IBasicAgreement)

  strict protected
  var
    FprivKey: IECPrivateKeyParameters;

  public
    /// <summary>
    /// initialise the agreement engine.
    /// </summary>
    procedure Init(const parameters: ICipherParameters); virtual;

    /// <summary>
    /// return the field size for the agreement algorithm in bytes.
    /// </summary>
    function GetFieldSize(): Int32; virtual;

    /// <summary>
    /// given a public key from a given party calculate the next message
    /// in the agreement sequence.
    /// </summary>
    function CalculateAgreement(const pubKey: ICipherParameters)
      : TBigInteger; virtual;

  end;

implementation

{ TECDHBasicAgreement }

function TECDHBasicAgreement.CalculateAgreement(const pubKey: ICipherParameters)
  : TBigInteger;
var
  pub: IECPublicKeyParameters;
  params: IECDomainParameters;
  P, Q: IECPoint;
  d, h: TBigInteger;
begin
  pub := pubKey as IECPublicKeyParameters;
  params := FprivKey.parameters;
  if (not(params.Equals(pub.parameters))) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SWrongDomainParameter);

  end;

  d := FprivKey.d;
  // Always perform calculations on the exact curve specified by our private key's parameters

  Q := TECAlgorithms.CleanPoint(params.Curve, pub.Q);

  if (Q.IsInfinity) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SInfinityInvalidPublicKey);
  end;

  h := params.h;

  if (not(h.Equals(TBigInteger.One))) then
  begin
    d := params.HInv.Multiply(d).&Mod(params.N);
    Q := TECAlgorithms.ReferenceMultiply(Q, h);
  end;

  P := Q.Multiply(d).Normalize();

  if (P.IsInfinity) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SInvalidAgreementValue);

  end;

  result := P.AffineXCoord.ToBigInteger();
end;

function TECDHBasicAgreement.GetFieldSize: Int32;
begin
  result := (FprivKey.parameters.Curve.FieldSize + 7) div 8;
end;

procedure TECDHBasicAgreement.Init(const parameters: ICipherParameters);
var
  Lparameters: ICipherParameters;
begin
  Lparameters := parameters;
  if Supports(Lparameters, IParametersWithRandom) then
  begin
    Lparameters := (Lparameters as IParametersWithRandom).parameters;
  end;

  FprivKey := Lparameters as IECPrivateKeyParameters;
end;

end.
