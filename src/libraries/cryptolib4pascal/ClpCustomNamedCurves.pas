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

unit ClpCustomNamedCurves;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  ClpEncoders,
  ClpGlvTypeBParameters,
  ClpIGlvTypeBEndomorphism,
  ClpSecObjectIdentifiers,
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpECC,
  ClpSecP256K1Custom,
  ClpISecP256K1Custom,
  ClpSecP256R1Custom,
  ClpISecP256R1Custom,
  ClpSecP384R1Custom,
  ClpISecP384R1Custom,
  ClpSecP521R1Custom,
  ClpISecP521R1Custom,
  ClpSecT283Custom,
  ClpISecT283Custom,
  ClpIECC,
  ClpX9ECC,
  ClpIX9ECC,
  ClpIAsn1Objects,
  ClpGlvTypeBEndomorphism,
  ClpX9ECParameters,
  ClpIX9ECParameters,
  ClpX9ECParametersHolder,
  ClpIX9ECParametersHolder,
  ClpIGlvTypeBParameters;

type
  TCustomNamedCurves = class sealed(TObject)

  strict private

  class var
    FnameToCurve: TDictionary<String, IX9ECParametersHolder>;
    FnameToOid: TDictionary<String, IDerObjectIdentifier>;
    FoidToCurve: TDictionary<IDerObjectIdentifier, IX9ECParametersHolder>;
    FoidToName: TDictionary<IDerObjectIdentifier, String>;

    Fnames: TList<String>;

    class function GetNames: TCryptoLibStringArray; static; inline;

    // class procedure DefineCurve(const name: String;
    // const holder: IX9ECParametersHolder); static; inline;

    class procedure DefineCurveWithOid(const name: String;
      const oid: IDerObjectIdentifier; const holder: IX9ECParametersHolder);
      static; inline;

    class procedure DefineCurveAlias(const name: String;
      const oid: IDerObjectIdentifier); static; inline;

    class function ConfigureCurve(const curve: IECCurve): IECCurve;
      static; inline;
    class function ConfigureCurveGlv(const c: IECCurve;
      const p: IGlvTypeBParameters): IECCurve; static; inline;

    class procedure Boot(); static;
    class constructor CreateCustomNamedCurves();
    class destructor DestroyCustomNamedCurves();

  public
    class function GetByName(const name: String): IX9ECParameters;
      static; inline;
    // /**
    // * return the X9ECParameters object for the named curve represented by
    // * the passed in object identifier. Null if the curve isn't present.
    // *
    // * @param oid an object identifier representing a named curve, if present.
    // */
    class function GetByOid(const oid: IDerObjectIdentifier): IX9ECParameters;
      static; inline;
    // /**
    // * return the object identifier signified by the passed in name. Null
    // * if there is no object identifier associated with name.
    // *
    // * @return the object identifier associated with name, if present.
    // */
    class function GetOid(const name: String): IDerObjectIdentifier;
      static; inline;
    // /**
    // * return the named curve name represented by the given object identifier.
    // */
    class function GetName(const oid: IDerObjectIdentifier): String;
      static; inline;
    // /**
    // * returns an enumeration containing the name strings for curves
    // * contained in this structure.
    // */
    class property Names: TCryptoLibStringArray read GetNames;

  type

    /// <summary>
    /// secp256k1
    /// </summary>
    TSecP256K1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp256r1
    /// </summary>
    TSecP256R1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp384r1
    /// </summary>
    TSecP384R1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp521r1
    /// </summary>
    TSecP521R1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect283k1
    /// </summary>
    TSecT283K1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  end;

implementation

{ TCustomNamedCurves }

// class procedure TCustomNamedCurves.DefineCurve(const name: String;
// const holder: IX9ECParametersHolder);
// var
// LName: string;
// begin
// LName := name;
// Fnames.Add(LName);
// LName := UpperCase(LName);
// FnameToCurve.Add(LName, holder);
// end;

class procedure TCustomNamedCurves.DefineCurveWithOid(const name: String;
  const oid: IDerObjectIdentifier; const holder: IX9ECParametersHolder);
var
  LName: string;
begin
  LName := name;
  Fnames.Add(LName);
  FoidToName.Add(oid, LName);
  FoidToCurve.Add(oid, holder);
  LName := UpperCase(LName);
  FnameToOid.Add(LName, oid);
  FnameToCurve.Add(LName, holder);
end;

class procedure TCustomNamedCurves.DefineCurveAlias(const name: String;
  const oid: IDerObjectIdentifier);
var
  curve: IX9ECParametersHolder;
  LName: string;
begin
  LName := name;
  if not(FoidToCurve.TryGetValue(oid, curve)) then
  begin
    raise EInvalidOperationCryptoLibException.Create('');
  end;
  LName := UpperCase(LName);
  FnameToOid.Add(LName, oid);
  FnameToCurve.Add(LName, curve);
end;

class function TCustomNamedCurves.ConfigureCurve(const curve: IECCurve)
  : IECCurve;
begin
  result := curve;
end;

class function TCustomNamedCurves.ConfigureCurveGlv(const c: IECCurve;
  const p: IGlvTypeBParameters): IECCurve;
var
  glv: IGlvTypeBEndomorphism;
begin
  glv := TGlvTypeBEndomorphism.Create(c, p);
  result := c.Configure().SetEndomorphism(glv).CreateCurve();
end;

class function TCustomNamedCurves.GetByOid(const oid: IDerObjectIdentifier)
  : IX9ECParameters;
var
  holder: IX9ECParametersHolder;
begin
  if FoidToCurve.TryGetValue(oid, holder) then
  begin
    result := holder.Parameters
  end
  else
  begin
    result := Nil;
  end;
end;

class function TCustomNamedCurves.GetOid(const name: String)
  : IDerObjectIdentifier;
begin
  if not(FnameToOid.TryGetValue(UpperCase(name), result)) then
  begin
    result := Nil;
  end;
end;

class function TCustomNamedCurves.GetByName(const name: String)
  : IX9ECParameters;
var
  holder: IX9ECParametersHolder;
begin
  if FnameToCurve.TryGetValue(UpperCase(name), holder) then
  begin
    result := holder.Parameters
  end
  else
  begin
    result := Nil;
  end;
end;

class function TCustomNamedCurves.GetName
  (const oid: IDerObjectIdentifier): String;
begin
  if not(FoidToName.TryGetValue(oid, result)) then
  begin
    result := '';
  end;
end;

class function TCustomNamedCurves.GetNames: TCryptoLibStringArray;
begin
  result := Fnames.ToArray();
end;

class constructor TCustomNamedCurves.CreateCustomNamedCurves;
begin
  TCustomNamedCurves.Boot;
end;

class destructor TCustomNamedCurves.DestroyCustomNamedCurves;
begin
  FnameToCurve.Free;
  FnameToOid.Free;
  FoidToCurve.Free;
  FoidToName.Free;
  Fnames.Free;
end;

class procedure TCustomNamedCurves.Boot;
begin
  FnameToCurve := TDictionary<String, IX9ECParametersHolder>.Create();
  FnameToOid := TDictionary<String, IDerObjectIdentifier>.Create();
  FoidToCurve := TDictionary<IDerObjectIdentifier,
    IX9ECParametersHolder>.Create();
  FoidToName := TDictionary<IDerObjectIdentifier, String>.Create();

  Fnames := TList<String>.Create();

  DefineCurveWithOid('secp256k1', TSecObjectIdentifiers.SecP256k1,
    TSecP256K1Holder.Instance);

  DefineCurveWithOid('secp256r1', TSecObjectIdentifiers.SecP256r1,
    TSecP256R1Holder.Instance);

  DefineCurveWithOid('secp384r1', TSecObjectIdentifiers.SecP384r1,
    TSecP384R1Holder.Instance);

  DefineCurveWithOid('secp521r1', TSecObjectIdentifiers.SecP521r1,
    TSecP521R1Holder.Instance);

  DefineCurveWithOid('sect283k1', TSecObjectIdentifiers.SecT283k1,
    TSecT283K1Holder.Instance);

  DefineCurveAlias('K-283', TSecObjectIdentifiers.SecT283k1);

  DefineCurveAlias('P-256', TSecObjectIdentifiers.SecP256r1);
  DefineCurveAlias('P-384', TSecObjectIdentifiers.SecP384r1);
  DefineCurveAlias('P-521', TSecObjectIdentifiers.SecP521r1);
end;

{ TCustomNamedCurves.TSecP256K1Holder }

function TCustomNamedCurves.TSecP256K1Holder.CreateParameters: IX9ECParameters;
var
  curve: IECCurve;
  G: IX9ECPoint;
  S: TCryptoLibByteArray;
  glv: IGlvTypeBParameters;
begin
  S := Nil;
  glv := TGlvTypeBParameters.Create
    (TBigInteger.Create
    ('7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee', 16),
    TBigInteger.Create
    ('5363ad4cc05c30e0a5261c028812645a122e22ea20816678df02967c1b23bd72', 16),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('3086d221a7d46bcde86c90e49284eb15', 16),
    TBigInteger.Create('-e4437ed6010e88286f547fa90abfe4c3', 16)),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('114ca50f7a8e2f3f657c1108d9d44cfd8', 16),
    TBigInteger.Create('3086d221a7d46bcde86c90e49284eb15', 16)),
    TBigInteger.Create('3086d221a7d46bcde86c90e49284eb153dab', 16),
    TBigInteger.Create('e4437ed6010e88286f547fa90abfe4c42212', 16), 272);
  curve := ConfigureCurveGlv(TSecP256K1Curve.Create() as ISecP256K1Curve, glv);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798' +
    '483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8'));
  result := TX9ECParameters.Create(curve, G, curve.Order, curve.Cofactor, S);
end;

class function TCustomNamedCurves.TSecP256K1Holder.Instance
  : IX9ECParametersHolder;
begin
  result := TSecP256K1Holder.Create();
end;

{ TCustomNamedCurves.TSecP384R1Holder }

function TCustomNamedCurves.TSecP384R1Holder.CreateParameters: IX9ECParameters;
var
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  S := THex.Decode('A335926AA319A27A1D00896A6773A4827ACDAC73');
  curve := ConfigureCurve(TSecP384R1Curve.Create() as ISecP384R1Curve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    'AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7'
    + '3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F')
    );
  result := TX9ECParameters.Create(curve, G, curve.Order, curve.Cofactor, S);
end;

class function TCustomNamedCurves.TSecP384R1Holder.Instance
  : IX9ECParametersHolder;
begin
  result := TSecP384R1Holder.Create();
end;

{ TCustomNamedCurves.TSecP521R1Holder }

function TCustomNamedCurves.TSecP521R1Holder.CreateParameters: IX9ECParameters;
var
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  S := THex.Decode('D09E8800291CB85396CC6717393284AAA0DA64BA');
  curve := ConfigureCurve(TSecP521R1Curve.Create() as ISecP521R1Curve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66'
    + '011839296A789A3BC0045C8A5FB42C7D1BD998F54449579B446817AFBD17273E662C97EE72995EF42640C550B9013FAD0761353C7086A272C24088BE94769FD16650')
    );
  result := TX9ECParameters.Create(curve, G, curve.Order, curve.Cofactor, S);
end;

class function TCustomNamedCurves.TSecP521R1Holder.Instance
  : IX9ECParametersHolder;
begin
  result := TSecP521R1Holder.Create();
end;

{ TCustomNamedCurves.TSecT283K1Holder }

function TCustomNamedCurves.TSecT283K1Holder.CreateParameters: IX9ECParameters;
var
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  S := Nil;
  curve := ConfigureCurve(TSecT283K1Curve.Create() as ISecT283K1Curve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '0503213F78CA44883F1A3B8162F188E553CD265F23C1567A16876913B0C2AC2458492836' +
    '01CCDA380F1C9E318D90F95D07E5426FE87E45C0E8184698E45962364E34116177DD2259')
    );
  result := TX9ECParameters.Create(curve, G, curve.Order, curve.Cofactor, S);
end;

class function TCustomNamedCurves.TSecT283K1Holder.Instance
  : IX9ECParametersHolder;
begin
  result := TSecT283K1Holder.Create();
end;

{ TCustomNamedCurves.TSecP256R1Holder }

function TCustomNamedCurves.TSecP256R1Holder.CreateParameters: IX9ECParameters;
var
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  S := THex.Decode('C49D360886E704936A6678E1139D26B7819F7E90');
  curve := ConfigureCurve(TSecP256R1Curve.Create() as ISecP256R1Curve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296' +
    '4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5'));
  result := TX9ECParameters.Create(curve, G, curve.Order, curve.Cofactor, S);
end;

class function TCustomNamedCurves.TSecP256R1Holder.Instance
  : IX9ECParametersHolder;
begin
  result := TSecP256R1Holder.Create();
end;

end.
