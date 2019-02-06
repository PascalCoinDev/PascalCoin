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

unit ClpECDHCBasicAgreement;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpECAlgorithms,
  ClpICipherParameters,
  ClpIECC,
  ClpIBasicAgreement,
  ClpIECDomainParameters,
  ClpIECDHCBasicAgreement,
  ClpIECPrivateKeyParameters,
  ClpIParametersWithRandom,
  ClpIECPublicKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SWrongDomainParameter = 'ECDHC Public Key has Wrong Domain Parameters';
  SInvalidAgreementValue = 'Infinity is not a Valid Agreement Value for ECDHC';
  SInfinityInvalidPublicKey = 'Infinity is not a Valid Public Key for ECDHC';

type
  /// <summary>
  /// P1363 7.2.2 ECSVDP-DHC <br /><br />ECSVDP-DHC is Elliptic Curve Secret
  /// Value Derivation Primitive, <br />Diffie-Hellman version with cofactor
  /// multiplication. It is based on <br />the work of [DH76], [Mil86],
  /// [Kob87], [LMQ98] and [Kal98a]. This <br />primitive derives a shared
  /// secret value from one party's private key <br />and another party's
  /// public key, where both have the same set of EC <br />domain parameters.
  /// If two parties correctly execute this primitive, <br />they will
  /// produce the same output. This primitive can be invoked by a <br />
  /// scheme to derive a shared secret key; specifically, it may be used <br />
  /// with the schemes ECKAS-DH1 and DL/ECKAS-DH2. It does not assume the <br />
  /// validity of the input public key (see also Section 7.2.1). <br /><br />
  /// Note: As stated P1363 compatibility mode with ECDH can be preset, and <br />
  /// in this case the implementation doesn't have a ECDH compatibility mode <br />
  /// (if you want that just use ECDHBasicAgreement and note they both
  /// implement <br />BasicAgreement!). <br />
  /// </summary>
  TECDHCBasicAgreement = class(TInterfacedObject, IECDHCBasicAgreement,
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

{ TECDHCBasicAgreement }

function TECDHCBasicAgreement.CalculateAgreement(const pubKey
  : ICipherParameters): TBigInteger;
var
  pub: IECPublicKeyParameters;
  params: IECDomainParameters;
  hd: TBigInteger;
  P, pubPoint: IECPoint;
begin
  pub := pubKey as IECPublicKeyParameters;
  params := FprivKey.parameters;
  if (not(params.Equals(pub.parameters))) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SWrongDomainParameter);

  end;

  hd := params.H.Multiply(FprivKey.D).&Mod(params.N);

  // Always perform calculations on the exact curve specified by our private key's parameters
  pubPoint := TECAlgorithms.CleanPoint(params.Curve, pub.Q);
  if (pubPoint.IsInfinity) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SInfinityInvalidPublicKey);
  end;

  P := pubPoint.Multiply(hd).Normalize();

  if (P.IsInfinity) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SInvalidAgreementValue);

  end;

  result := P.AffineXCoord.ToBigInteger();
end;

function TECDHCBasicAgreement.GetFieldSize: Int32;
begin
  result := (FprivKey.parameters.Curve.FieldSize + 7) div 8;
end;

procedure TECDHCBasicAgreement.Init(const parameters: ICipherParameters);
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
