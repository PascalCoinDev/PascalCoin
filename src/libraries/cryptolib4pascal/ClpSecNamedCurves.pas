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

unit ClpSecNamedCurves;

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
  TSecNamedCurves = class sealed(TObject)

  strict private

  class var
    FobjIds: TDictionary<String, IDerObjectIdentifier>;
    Fnames: TDictionary<IDerObjectIdentifier, String>;
    Fcurves: TDictionary<IDerObjectIdentifier, IX9ECParametersHolder>;

    class function GetNames: TCryptoLibStringArray; static; inline;
    class procedure DefineCurve(const name: String;
      const oid: IDerObjectIdentifier; const holder: IX9ECParametersHolder);
      static; inline;

    class function ConfigureCurve(const curve: IECCurve): IECCurve;
      static; inline;
    class function ConfigureCurveGlv(const c: IECCurve;
      const p: IGlvTypeBParameters): IECCurve; static; inline;
    class function FromHex(const Hex: String): TBigInteger; static; inline;

    class procedure Boot(); static;
    class constructor CreateSecNamedCurves();
    class destructor DestroySecNamedCurves();

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
    /// secp112r1
    /// </summary>
    TSecp112r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp112r2
    /// </summary>
    TSecp112r2Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp128r1
    /// </summary>
    TSecp128r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp128r2
    /// </summary>
    TSecp128r2Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp160k1
    /// </summary>
    TSecp160k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp160r1
    /// </summary>
    TSecp160r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp160r2
    /// </summary>
    TSecp160r2Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp192k1
    /// </summary>
    TSecp192k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp192r1
    /// </summary>
    TSecp192r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp224k1
    /// </summary>
    TSecp224k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp224r1
    /// </summary>
    TSecp224r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// secp256k1
    /// </summary>
    TSecp256k1Holder = class sealed(TX9ECParametersHolder,
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
    TSecp256r1Holder = class sealed(TX9ECParametersHolder,
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
    TSecp384r1Holder = class sealed(TX9ECParametersHolder,
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
    TSecp521r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect113r1
    /// </summary>
    TSect113r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(113);
      Fk = Int32(9);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect113r2
    /// </summary>
    TSect113r2Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(113);
      Fk = Int32(9);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect131r1
    /// </summary>
    TSect131r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(131);
      Fk1 = Int32(2);
      Fk2 = Int32(3);
      Fk3 = Int32(8);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect131r2
    /// </summary>
    TSect131r2Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(131);
      Fk1 = Int32(2);
      Fk2 = Int32(3);
      Fk3 = Int32(8);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect163k1
    /// </summary>
    TSect163k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(163);
      Fk1 = Int32(3);
      Fk2 = Int32(6);
      Fk3 = Int32(7);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect163r1
    /// </summary>
    TSect163r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(163);
      Fk1 = Int32(3);
      Fk2 = Int32(6);
      Fk3 = Int32(7);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect163r2
    /// </summary>
    TSect163r2Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(163);
      Fk1 = Int32(3);
      Fk2 = Int32(6);
      Fk3 = Int32(7);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect193r1
    /// </summary>
    TSect193r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(193);
      Fk = Int32(15);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect193r2
    /// </summary>
    TSect193r2Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(193);
      Fk = Int32(15);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect233k1
    /// </summary>
    TSect233k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(233);
      Fk = Int32(74);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect233r1
    /// </summary>
    TSect233r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(233);
      Fk = Int32(74);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect239k1
    /// </summary>
    TSect239k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(239);
      Fk = Int32(158);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect283k1
    /// </summary>
    TSect283k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(283);
      Fk1 = Int32(5);
      Fk2 = Int32(7);
      Fk3 = Int32(12);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect283r1
    /// </summary>
    TSect283r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(283);
      Fk1 = Int32(5);
      Fk2 = Int32(7);
      Fk3 = Int32(12);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect409k1
    /// </summary>
    TSect409k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(409);
      Fk = Int32(87);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect409r1
    /// </summary>
    TSect409r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(409);
      Fk = Int32(87);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect571k1
    /// </summary>
    TSect571k1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(571);
      Fk1 = Int32(2);
      Fk2 = Int32(5);
      Fk3 = Int32(10);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  type

    /// <summary>
    /// sect571r1
    /// </summary>
    TSect571r1Holder = class sealed(TX9ECParametersHolder,
      IX9ECParametersHolder)

    strict private
    const
      Fm = Int32(571);
      Fk1 = Int32(2);
      Fk2 = Int32(5);
      Fk3 = Int32(10);

    strict protected
      function CreateParameters(): IX9ECParameters; override;

    public
      class function Instance(): IX9ECParametersHolder; static;

    end;

  end;

implementation

{ TSecNamedCurves }

class procedure TSecNamedCurves.DefineCurve(const name: String;
  const oid: IDerObjectIdentifier; const holder: IX9ECParametersHolder);
begin
  FobjIds.Add(UpperCase(name), oid);
  Fnames.Add(oid, name);
  Fcurves.Add(oid, holder);
end;

class function TSecNamedCurves.ConfigureCurve(const curve: IECCurve): IECCurve;
begin
  result := curve;
end;

class function TSecNamedCurves.ConfigureCurveGlv(const c: IECCurve;
  const p: IGlvTypeBParameters): IECCurve;
var
  glv: IGlvTypeBEndomorphism;
begin
  glv := TGlvTypeBEndomorphism.Create(c, p);
  result := c.Configure().SetEndomorphism(glv).CreateCurve();
end;

class function TSecNamedCurves.FromHex(const Hex: String): TBigInteger;
begin
  result := TBigInteger.Create(1, THex.Decode(Hex));
end;

class function TSecNamedCurves.GetByOid(const oid: IDerObjectIdentifier)
  : IX9ECParameters;
var
  holder: IX9ECParametersHolder;
begin
  if Fcurves.TryGetValue(oid, holder) then
  begin
    result := holder.Parameters
  end
  else
  begin
    result := Nil;
  end;
end;

class function TSecNamedCurves.GetOid(const name: String): IDerObjectIdentifier;
begin
  if not(FobjIds.TryGetValue(UpperCase(name), result)) then
  begin
    result := Nil;
  end;
end;

class function TSecNamedCurves.GetByName(const name: String): IX9ECParameters;
var
  oid: IDerObjectIdentifier;
begin
  oid := GetOid(name);
  if oid = Nil then
  begin
    result := Nil;
  end
  else
  begin
    result := GetByOid(oid);
  end;

end;

class function TSecNamedCurves.GetName(const oid: IDerObjectIdentifier): String;
begin
  if not(Fnames.TryGetValue(oid, result)) then
  begin
    result := '';
  end;
end;

class function TSecNamedCurves.GetNames: TCryptoLibStringArray;
begin
  result := Fnames.Values.ToArray();
end;

class constructor TSecNamedCurves.CreateSecNamedCurves;
begin
  TSecNamedCurves.Boot;
end;

class destructor TSecNamedCurves.DestroySecNamedCurves;
begin
  FobjIds.Free;
  Fnames.Free;
  Fcurves.Free;
end;

class procedure TSecNamedCurves.Boot;
begin
  FobjIds := TDictionary<String, IDerObjectIdentifier>.Create();
  Fnames := TDictionary<IDerObjectIdentifier, String>.Create();
  Fcurves := TDictionary<IDerObjectIdentifier, IX9ECParametersHolder>.Create();

  DefineCurve('secp112r1', TSecObjectIdentifiers.SecP112r1,
    TSecp112r1Holder.Instance);
  DefineCurve('secp112r2', TSecObjectIdentifiers.SecP112r2,
    TSecp112r2Holder.Instance);
  DefineCurve('secp128r1', TSecObjectIdentifiers.SecP128r1,
    TSecp128r1Holder.Instance);
  DefineCurve('secp128r2', TSecObjectIdentifiers.SecP128r2,
    TSecp128r2Holder.Instance);
  DefineCurve('secp160k1', TSecObjectIdentifiers.SecP160k1,
    TSecp160k1Holder.Instance);
  DefineCurve('secp160r1', TSecObjectIdentifiers.SecP160r1,
    TSecp160r1Holder.Instance);
  DefineCurve('secp160r2', TSecObjectIdentifiers.SecP160r2,
    TSecp160r2Holder.Instance);
  DefineCurve('secp192k1', TSecObjectIdentifiers.SecP192k1,
    TSecp192k1Holder.Instance);
  DefineCurve('secp192r1', TSecObjectIdentifiers.SecP192r1,
    TSecp192r1Holder.Instance);
  DefineCurve('secp224k1', TSecObjectIdentifiers.SecP224k1,
    TSecp224k1Holder.Instance);
  DefineCurve('secp224r1', TSecObjectIdentifiers.SecP224r1,
    TSecp224r1Holder.Instance);
  DefineCurve('secp256k1', TSecObjectIdentifiers.SecP256k1,
    TSecp256k1Holder.Instance);
  DefineCurve('secp256r1', TSecObjectIdentifiers.SecP256r1,
    TSecp256r1Holder.Instance);
  DefineCurve('secp384r1', TSecObjectIdentifiers.SecP384r1,
    TSecp384r1Holder.Instance);
  DefineCurve('secp521r1', TSecObjectIdentifiers.SecP521r1,
    TSecp521r1Holder.Instance);

  DefineCurve('sect113r1', TSecObjectIdentifiers.SecT113r1,
    TSect113r1Holder.Instance);
  DefineCurve('sect113r2', TSecObjectIdentifiers.SecT113r2,
    TSect113r2Holder.Instance);
  DefineCurve('sect131r1', TSecObjectIdentifiers.SecT131r1,
    TSect131r1Holder.Instance);
  DefineCurve('sect131r2', TSecObjectIdentifiers.SecT131r2,
    TSect131r2Holder.Instance);
  DefineCurve('sect163k1', TSecObjectIdentifiers.SecT163k1,
    TSect163k1Holder.Instance);
  DefineCurve('sect163r1', TSecObjectIdentifiers.SecT163r1,
    TSect163r1Holder.Instance);
  DefineCurve('sect163r2', TSecObjectIdentifiers.SecT163r2,
    TSect163r2Holder.Instance);
  DefineCurve('sect193r1', TSecObjectIdentifiers.SecT193r1,
    TSect193r1Holder.Instance);
  DefineCurve('sect193r2', TSecObjectIdentifiers.SecT193r2,
    TSect193r2Holder.Instance);
  DefineCurve('sect233k1', TSecObjectIdentifiers.SecT233k1,
    TSect233k1Holder.Instance);
  DefineCurve('sect233r1', TSecObjectIdentifiers.SecT233r1,
    TSect233r1Holder.Instance);
  DefineCurve('sect239k1', TSecObjectIdentifiers.SecT239k1,
    TSect239k1Holder.Instance);
  DefineCurve('sect283k1', TSecObjectIdentifiers.SecT283k1,
    TSect283k1Holder.Instance);
  DefineCurve('sect283r1', TSecObjectIdentifiers.SecT283r1,
    TSect283r1Holder.Instance);
  DefineCurve('sect409k1', TSecObjectIdentifiers.SecT409k1,
    TSect409k1Holder.Instance);
  DefineCurve('sect409r1', TSecObjectIdentifiers.SecT409r1,
    TSect409r1Holder.Instance);
  DefineCurve('sect571k1', TSecObjectIdentifiers.SecT571k1,
    TSect571k1Holder.Instance);
  DefineCurve('sect571r1', TSecObjectIdentifiers.SecT571r1,
    TSect571r1Holder.Instance);
end;

{ TSecNamedCurves.TSecp112r1Holder }

function TSecNamedCurves.TSecp112r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = (2^128 - 3) / 76439
  p := FromHex('DB7C2ABF62E35E668076BEAD208B');
  a := FromHex('DB7C2ABF62E35E668076BEAD2088');
  b := FromHex('659EF8BA043916EEDE8911702B22');
  S := THex.Decode('00F50B028E4D696E676875615175290472783FB1');
  n := FromHex('DB7C2ABF62E35E7628DFAC6561C5');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '09487239995A5EE76B55F9C2F098' +
    'A89CE5AF8724C0A23E0E0FF77500'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp112r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp112r1Holder.Create();
end;

{ TSecNamedCurves.TSecp112r2Holder }

function TSecNamedCurves.TSecp112r2Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = (2^128 - 3) / 76439
  p := FromHex('DB7C2ABF62E35E668076BEAD208B');
  a := FromHex('6127C24C05F38A0AAAF65C0EF02C');
  b := FromHex('51DEF1815DB5ED74FCC34C85D709');
  S := THex.Decode('002757A1114D696E6768756151755316C05E0BD4');
  n := FromHex('36DF0AAFD8B8D7597CA10520D04B');
  h := TBigInteger.ValueOf(4);

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '4BA30AB5E892B4E1649DD0928643' +
    'ADCD46F5882E3747DEF36E956E97'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp112r2Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp112r2Holder.Create();
end;

{ TSecNamedCurves.TSecp128r1Holder }

function TSecNamedCurves.TSecp128r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^128 - 2^97 - 1
  p := FromHex('FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF');
  a := FromHex('FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFC');
  b := FromHex('E87579C11079F43DD824993C2CEE5ED3');
  S := THex.Decode('000E0D4D696E6768756151750CC03A4473D03679');
  n := FromHex('FFFFFFFE0000000075A30D1B9038A115');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '161FF7528B899B2D0C28607CA52C5B86' +
    'CF5AC8395BAFEB13C02DA292DDED7A83'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp128r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp128r1Holder.Create();
end;

{ TSecNamedCurves.TSecp128r2Holder }

function TSecNamedCurves.TSecp128r2Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^128 - 2^97 - 1
  p := FromHex('FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF');
  a := FromHex('D6031998D1B3BBFEBF59CC9BBFF9AEE1');
  b := FromHex('5EEEFCA380D02919DC2C6558BB6D8A5D');
  S := THex.Decode('004D696E67687561517512D8F03431FCE63B88F4');
  n := FromHex('3FFFFFFF7FFFFFFFBE0024720613B5A3');
  h := TBigInteger.ValueOf(4);

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '7B6AA5D85E572983E6FB32A7CDEBC140' +
    '27B6916A894D3AEE7106FE805FC34B44'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp128r2Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp128r2Holder.Create();
end;

{ TSecNamedCurves.TSecp160k1Holder }

function TSecNamedCurves.TSecp160k1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  glv: IGlvTypeBParameters;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^160 - 2^32 - 2^14 - 2^12 - 2^9 - 2^8 - 2^7 - 2^3 - 2^2 - 1
  p := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73');
  a := TBigInteger.Zero;
  b := TBigInteger.ValueOf(7);
  S := Nil;
  n := FromHex('0100000000000000000001B8FA16DFAB9ACA16B6B3');
  h := TBigInteger.One;

  glv := TGlvTypeBParameters.Create
    (TBigInteger.Create('9ba48cba5ebcb9b6bd33b92830b2a2e0e192f10a', 16),
    TBigInteger.Create('c39c6c3b3a36d7701b9c71a1f5804ae5d0003f4', 16),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('9162fbe73984472a0a9e', 16),
    TBigInteger.Create('-96341f1138933bc2f505', 16)),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('127971af8721782ecffa3', 16),
    TBigInteger.Create('9162fbe73984472a0a9e', 16)),
    TBigInteger.Create('9162fbe73984472a0a9d0590', 16),
    TBigInteger.Create('96341f1138933bc2f503fd44', 16), 176);

  curve := ConfigureCurveGlv(TFpCurve.Create(p, a, b, n, h) as IFpCurve, glv);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '3B4C382CE37AA192A4019E763036F4F5DD4D7EBB' +
    '938CF935318FDCED6BC28286531733C3F03C4FEE'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp160k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp160k1Holder.Create();
end;

{ TSecNamedCurves.TSecp160r1Holder }

function TSecNamedCurves.TSecp160r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^160 - 2^31 - 1
  p := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF');
  a := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFC');
  b := FromHex('1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45');
  S := THex.Decode('1053CDE42C14D696E67687561517533BF3F83345');
  n := FromHex('0100000000000000000001F4C8F927AED3CA752257');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '4A96B5688EF573284664698968C38BB913CBFC82' +
    '23A628553168947D59DCC912042351377AC5FB32'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp160r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp160r1Holder.Create();
end;

{ TSecNamedCurves.TSecp160r2Holder }

function TSecNamedCurves.TSecp160r2Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^160 - 2^32 - 2^14 - 2^12 - 2^9 - 2^8 - 2^7 - 2^3 - 2^2 - 1
  p := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73');
  a := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC70');
  b := FromHex('B4E134D3FB59EB8BAB57274904664D5AF50388BA');
  S := THex.Decode('B99B99B099B323E02709A4D696E6768756151751');
  n := FromHex('0100000000000000000000351EE786A818F3A1A16B');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '52DCB034293A117E1F4FF11B30F7199D3144CE6D' +
    'FEAFFEF2E331F296E071FA0DF9982CFEA7D43F2E'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp160r2Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp160r2Holder.Create();
end;

{ TSecNamedCurves.TSecp192k1Holder }

function TSecNamedCurves.TSecp192k1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  glv: IGlvTypeBParameters;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^192 - 2^32 - 2^12 - 2^8 - 2^7 - 2^6 - 2^3 - 1
  p := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37');
  a := TBigInteger.Zero;
  b := TBigInteger.ValueOf(3);
  S := Nil;
  n := FromHex('FFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8D');
  h := TBigInteger.One;

  glv := TGlvTypeBParameters.Create
    (TBigInteger.Create('bb85691939b869c1d087f601554b96b80cb4f55b35f433c2', 16),
    TBigInteger.Create('3d84f26c12238d7b4f3d516613c1759033b1a5800175d0b1', 16),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('71169be7330b3038edb025f1', 16),
    TBigInteger.Create('-b3fb3400dec5c4adceb8655c', 16)),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('12511cfe811d0f4e6bc688b4d', 16),
    TBigInteger.Create('71169be7330b3038edb025f1', 16)),
    TBigInteger.Create('71169be7330b3038edb025f1d0f9', 16),
    TBigInteger.Create('b3fb3400dec5c4adceb8655d4c94', 16), 208);

  curve := ConfigureCurveGlv(TFpCurve.Create(p, a, b, n, h) as IFpCurve, glv);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + 'DB4FF10EC057E9AE26B07D0280B7F4341DA5D1B1EAE06C7D' +
    '9B2F2F6D9C5628A7844163D015BE86344082AA88D95E2F9D'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp192k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp192k1Holder.Create();
end;

{ TSecNamedCurves.TSecp192r1Holder }

function TSecNamedCurves.TSecp192r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^192 - 2^64 - 1
  p := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF');
  a := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC');
  b := FromHex('64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1');
  S := THex.Decode('3045AE6FC8422F64ED579528D38120EAE12196D5');
  n := FromHex('FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012' +
    '07192B95FFC8DA78631011ED6B24CDD573F977A11E794811'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp192r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp192r1Holder.Create();
end;

{ TSecNamedCurves.TSecp224k1Holder }

function TSecNamedCurves.TSecp224k1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  glv: IGlvTypeBParameters;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^224 - 2^32 - 2^12 - 2^11 - 2^9 - 2^7 - 2^4 - 2 - 1
  p := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D');
  a := TBigInteger.Zero;
  b := TBigInteger.ValueOf(5);
  S := Nil;
  n := FromHex('010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7');
  h := TBigInteger.One;

  glv := TGlvTypeBParameters.Create
    (TBigInteger.Create
    ('fe0e87005b4e83761908c5131d552a850b3f58b749c37cf5b84d6768', 16),
    TBigInteger.Create
    ('60dcd2104c4cbc0be6eeefc2bdd610739ec34e317f9b33046c9e4788', 16),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('6b8cf07d4ca75c88957d9d670591', 16),
    TBigInteger.Create('-b8adf1378a6eb73409fa6c9c637d', 16)),
    TCryptoLibGenericArray<TBigInteger>.Create
    (TBigInteger.Create('1243ae1b4d71613bc9f780a03690e', 16),
    TBigInteger.Create('6b8cf07d4ca75c88957d9d670591', 16)),
    TBigInteger.Create('6b8cf07d4ca75c88957d9d67059037a4', 16),
    TBigInteger.Create('b8adf1378a6eb73409fa6c9c637ba7f5', 16), 240);

  curve := ConfigureCurveGlv(TFpCurve.Create(p, a, b, n, h) as IFpCurve, glv);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    'A1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C' +
    '7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp224k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp224k1Holder.Create();
end;

{ TSecNamedCurves.TSecp224r1Holder }

function TSecNamedCurves.TSecp224r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^224 - 2^96 + 1
  p := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001');
  a := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE');
  b := FromHex('B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4');
  S := THex.Decode('BD71344799D5C7FCDC45B59FA3B9AB8F6A948BC5');
  n := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    'B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21' +
    'BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp224r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp224r1Holder.Create();
end;

{ TSecNamedCurves.TSecp256k1Holder }

function TSecNamedCurves.TSecp256k1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  curve: IECCurve;
  G: IX9ECPoint;
  S: TCryptoLibByteArray;
  glv: IGlvTypeBParameters;
begin
  // p := 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1
  p := FromHex
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F');
  a := TBigInteger.Zero;
  b := TBigInteger.ValueOf(7);
  S := Nil;
  n := FromHex
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141');
  h := TBigInteger.One;

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

  curve := ConfigureCurveGlv(TFpCurve.Create(p, a, b, n, h) as IFpCurve, glv);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798' +
    '483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp256k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp256k1Holder.Create();
end;

{ TSecNamedCurves.TSecp256r1Holder }

function TSecNamedCurves.TSecp256r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  // p = 2^224 (2^32 - 1) + 2^192 + 2^96 - 1
  p := FromHex
    ('FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF');
  a := FromHex
    ('FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC');
  b := FromHex
    ('5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B');
  S := THex.Decode('C49D360886E704936A6678E1139D26B7819F7E90');
  n := FromHex
    ('FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296' +
    '4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp256r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp256r1Holder.Create();
end;

{ TSecNamedCurves.TSecp384r1Holder }

function TSecNamedCurves.TSecp384r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  curve: IECCurve;
  G: IX9ECPoint;
  S: TCryptoLibByteArray;
begin
  // p := 2^384 - 2^128 - 2^96 + 2^32 - 1
  p := FromHex
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF');
  a := FromHex
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC');
  b := FromHex
    ('B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF');
  S := THex.Decode('A335926AA319A27A1D00896A6773A4827ACDAC73');
  n := FromHex
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    'AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7'
    + '3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp384r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp384r1Holder.Create();
end;

{ TSecNamedCurves.TSecp521r1Holder }

function TSecNamedCurves.TSecp521r1Holder.CreateParameters: IX9ECParameters;
var
  p, a, b, n, h: TBigInteger;
  curve: IECCurve;
  G: IX9ECPoint;
  S: TCryptoLibByteArray;
begin
  // p := 2^521 - 1
  p := FromHex
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  a := FromHex
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC');
  b := FromHex
    ('0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00');
  S := THex.Decode('D09E8800291CB85396CC6717393284AAA0DA64BA');
  n := FromHex
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409');
  h := TBigInteger.One;

  curve := ConfigureCurve(TFpCurve.Create(p, a, b, n, h) as IFpCurve);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66'
    + '011839296A789A3BC0045C8A5FB42C7D1BD998F54449579B446817AFBD17273E662C97EE72995EF42640C550B9013FAD0761353C7086A272C24088BE94769FD16650')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSecp521r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSecp521r1Holder.Create();
end;

{ TSecNamedCurves.TSect113r1Holder }

function TSecNamedCurves.TSect113r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := FromHex('003088250CA6E7C7FE649CE85820F7');
  b := FromHex('00E8BEE4D3E2260744188BE0E9C723');
  S := THex.Decode('10E723AB14D696E6768756151756FEBF8FCB49A9');
  n := FromHex('0100000000000000D9CCEC8A39E56F');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '009D73616F35F4AB1407D73562C10F' +
    '00A52830277958EE84D1315ED31886'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect113r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect113r1Holder.Create();
end;

{ TSecNamedCurves.TSect113r2Holder }

function TSecNamedCurves.TSect113r2Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := FromHex('00689918DBEC7E5A0DD6DFC0AA55C7');
  b := FromHex('0095E9A9EC9B297BD4BF36E059184F');
  S := THex.Decode('10C0FB15760860DEF1EEF4D696E676875615175D');
  n := FromHex('010000000000000108789B2496AF93');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '01A57A6A7B26CA5EF52FCDB8164797' +
    '00B3ADC94ED1FE674C06E695BABA1D'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect113r2Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect113r2Holder.Create();
end;

{ TSecNamedCurves.TSect131r1Holder }

function TSecNamedCurves.TSect131r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := FromHex('07A11B09A76B562144418FF3FF8C2570B8');
  b := FromHex('0217C05610884B63B9C6C7291678F9D341');
  S := THex.Decode('4D696E676875615175985BD3ADBADA21B43A97E2');
  n := FromHex('0400000000000000023123953A9464B54D');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '0081BAF91FDF9833C40F9C181343638399' +
    '078C6E7EA38C001F73C8134B1B4EF9E150'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect131r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect131r1Holder.Create();
end;

{ TSecNamedCurves.TSect131r2Holder }

function TSecNamedCurves.TSect131r2Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := FromHex('03E5A88919D7CAFCBF415F07C2176573B2');
  b := FromHex('04B8266A46C55657AC734CE38F018F2192');
  S := THex.Decode('985BD3ADBAD4D696E676875615175A21B43A97E3');
  n := FromHex('0400000000000000016954A233049BA98F');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '0356DCD8F2F95031AD652D23951BB366A8' +
    '0648F06D867940A5366D9E265DE9EB240F'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect131r2Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect131r2Holder.Create();
end;

{ TSecNamedCurves.TSect163k1Holder }

function TSecNamedCurves.TSect163k1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.One;
  b := TBigInteger.One;
  S := Nil;
  n := FromHex('04000000000000000000020108A2E0CC0D99F8A5EF');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '02FE13C0537BBC11ACAA07D793DE4E6D5E5C94EEE8' +
    '0289070FB05D38FF58321F2E800536D538CCDAA3D9'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect163k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect163k1Holder.Create();
end;

{ TSecNamedCurves.TSect163r1Holder }

function TSecNamedCurves.TSect163r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := FromHex('07B6882CAAEFA84F9554FF8428BD88E246D2782AE2');
  b := FromHex('0713612DCDDCB40AAB946BDA29CA91F73AF958AFD9');
  S := THex.Decode('24B7B137C8A14D696E6768756151756FD0DA2E5C');
  n := FromHex('03FFFFFFFFFFFFFFFFFFFF48AAB689C29CA710279B');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '0369979697AB43897789566789567F787A7876A654' +
    '00435EDB42EFAFB2989D51FEFCE3C80988F41FF883'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect163r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect163r1Holder.Create();
end;

{ TSecNamedCurves.TSect163r2Holder }

function TSecNamedCurves.TSect163r2Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.One;
  b := FromHex('020A601907B8C953CA1481EB10512F78744A3205FD');
  S := THex.Decode('85E25BFE5C86226CDB12016F7553F9D0E693A268');
  n := FromHex('040000000000000000000292FE77E70C12A4234C33');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '03F0EBA16286A2D57EA0991168D4994637E8343E36' +
    '00D51FBC6C71A0094FA2CDD545B11C5C0C797324F1'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect163r2Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect163r2Holder.Create();
end;

{ TSecNamedCurves.TSect193r1Holder }

function TSecNamedCurves.TSect193r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := FromHex('0017858FEB7A98975169E171F77B4087DE098AC8A911DF7B01');
  b := FromHex('00FDFB49BFE6C3A89FACADAA7A1E5BBC7CC1C2E5D831478814');
  S := THex.Decode('103FAEC74D696E676875615175777FC5B191EF30');
  n := FromHex('01000000000000000000000000C7F34A778F443ACC920EBA49');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '01F481BC5F0FF84A74AD6CDF6FDEF4BF6179625372D8C0C5E1' +
    '0025E399F2903712CCF3EA9E3A1AD17FB0B3201B6AF7CE1B05'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect193r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect193r1Holder.Create();
end;

{ TSecNamedCurves.TSect193r2Holder }

function TSecNamedCurves.TSect193r2Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := FromHex('0163F35A5137C2CE3EA6ED8667190B0BC43ECD69977702709B');
  b := FromHex('00C9BB9E8927D4D64C377E2AB2856A5B16E3EFB7F61D4316AE');
  S := THex.Decode('10B7B4D696E676875615175137C8A16FD0DA2211');
  n := FromHex('010000000000000000000000015AAB561B005413CCD4EE99D5');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' + '00D9B67D192E0367C803F39E1A7E82CA14A651350AAE617E8F' +
    '01CE94335607C304AC29E7DEFBD9CA01F596F927224CDECF6C'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect193r2Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect193r2Holder.Create();
end;

{ TSecNamedCurves.TSect233k1Holder }

function TSecNamedCurves.TSect233k1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.Zero;
  b := TBigInteger.One;
  S := Nil;
  n := FromHex('8000000000000000000000000000069D5BB915BCD46EFB1AD5F173ABDF');
  h := TBigInteger.ValueOf(4);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '017232BA853A7E731AF129F22FF4149563A419C26BF50A4C9D6EEFAD6126' +
    '01DB537DECE819B7F70F555A67C427A8CD9BF18AEB9B56E0C11056FAE6A3'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect233k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect233k1Holder.Create();
end;

{ TSecNamedCurves.TSect233r1Holder }

function TSecNamedCurves.TSect233r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.One;
  b := FromHex('0066647EDE6C332C7F8C0923BB58213B333B20E9CE4281FE115F7D8F90AD');
  S := THex.Decode('74D59FF07F6B413D0EA14B344B20A2DB049B50C3');
  n := FromHex('01000000000000000000000000000013E974E72F8A6922031D2603CFE0D7');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '00FAC9DFCBAC8313BB2139F1BB755FEF65BC391F8B36F8F8EB7371FD558B' +
    '01006A08A41903350678E58528BEBF8A0BEFF867A7CA36716F7E01F81052'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect233r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect233r1Holder.Create();
end;

{ TSecNamedCurves.TSect239k1Holder }

function TSecNamedCurves.TSect239k1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.Zero;
  b := TBigInteger.One;
  S := Nil;
  n := FromHex('2000000000000000000000000000005A79FEC67CB6E91F1C1DA800E478A5');
  h := TBigInteger.ValueOf(4);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '29A0B6A887A983E9730988A68727A8B2D126C44CC2CC7B2A6555193035DC' +
    '76310804F12E549BDB011C103089E73510ACB275FC312A5DC6B76553F0CA'));

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect239k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect239k1Holder.Create();
end;

{ TSecNamedCurves.TSect283k1Holder }

function TSecNamedCurves.TSect283k1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.Zero;
  b := TBigInteger.One;
  S := Nil;
  n := FromHex
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9AE2ED07577265DFF7F94451E061E163C61');
  h := TBigInteger.ValueOf(4);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '0503213F78CA44883F1A3B8162F188E553CD265F23C1567A16876913B0C2AC2458492836' +
    '01CCDA380F1C9E318D90F95D07E5426FE87E45C0E8184698E45962364E34116177DD2259')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect283k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect283k1Holder.Create();
end;

{ TSecNamedCurves.TSect283r1Holder }

function TSecNamedCurves.TSect283r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.One;
  b := FromHex
    ('027B680AC8B8596DA5A4AF8A19A0303FCA97FD7645309FA2A581485AF6263E313B79A2F5');
  S := THex.Decode('77E2B07370EB0F832A6DD5B62DFC88CD06BB84BE');
  n := FromHex
    ('03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF90399660FC938A90165B042A7CEFADB307');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '05F939258DB7DD90E1934F8C70B0DFEC2EED25B8557EAC9C80E2E198F8CDBECD86B12053' +
    '03676854FE24141CB98FE6D4B20D02B4516FF702350EDDB0826779C813F0DF45BE8112F4')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect283r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect283r1Holder.Create();
end;

{ TSecNamedCurves.TSect409k1Holder }

function TSecNamedCurves.TSect409k1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.Zero;
  b := TBigInteger.One;
  S := Nil;
  n := FromHex
    ('7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5F83B2D4EA20400EC4557D5ED3E3E7CA5B4B5C83B8E01E5FCF');
  h := TBigInteger.ValueOf(4);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '0060F05F658F49C1AD3AB1890F7184210EFD0987E307C84C27ACCFB8F9F67CC2C460189EB5AAAA62EE222EB1B35540CFE9023746'
    + '01E369050B7C4E42ACBA1DACBF04299C3460782F918EA427E6325165E9EA10E3DA5F6C42E9C55215AA9CA27A5863EC48D8E0286B')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect409k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect409k1Holder.Create();
end;

{ TSecNamedCurves.TSect409r1Holder }

function TSecNamedCurves.TSect409r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.One;
  b := FromHex
    ('0021A5C2C8EE9FEB5C4B9A753B7B476B7FD6422EF1F3DD674761FA99D6AC27C8A9A197B272822F6CD57A55AA4F50AE317B13545F');
  S := THex.Decode('4099B5A457F9D69F79213D094C4BCD4D4262210B');
  n := FromHex
    ('010000000000000000000000000000000000000000000000000001E2AAD6A612F33307BE5FA47C3C9E052F838164CD37D9A21173');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '015D4860D088DDB3496B0C6064756260441CDE4AF1771D4DB01FFE5B34E59703DC255A868A1180515603AEAB60794E54BB7996A7'
    + '0061B1CFAB6BE5F32BBFA78324ED106A7636B9C5A7BD198D0158AA4F5488D08F38514F1FDF4B4F40D2181B3681C364BA0273C706')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect409r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect409r1Holder.Create();
end;

{ TSecNamedCurves.TSect571k1Holder }

function TSecNamedCurves.TSect571k1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.Zero;
  b := TBigInteger.One;
  S := Nil;
  n := FromHex
    ('020000000000000000000000000000000000000000000000000000000000000000000000131850E1F19A63E4B391A8DB917F4138B630D84BE5D639381E91DEB45CFE778F637C1001');
  h := TBigInteger.ValueOf(4);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '026EB7A859923FBC82189631F8103FE4AC9CA2970012D5D46024804801841CA44370958493B205E647DA304DB4CEB08CBBD1BA39494776FB988B47174DCA88C7E2945283A01C8972'
    + '0349DC807F4FBF374F4AEADE3BCA95314DD58CEC9F307A54FFC61EFC006D8A2C9D4979C0AC44AEA74FBEBBB9F772AEDCB620B01A7BA7AF1B320430C8591984F601CD4C143EF1C7A3')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect571k1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect571k1Holder.Create();
end;

{ TSecNamedCurves.TSect571r1Holder }

function TSecNamedCurves.TSect571r1Holder.CreateParameters: IX9ECParameters;
var
  a, b, n, h: TBigInteger;
  S: TCryptoLibByteArray;
  curve: IECCurve;
  G: IX9ECPoint;
begin
  a := TBigInteger.One;
  b := FromHex
    ('02F40E7E2221F295DE297117B7F3D62F5C6A97FFCB8CEFF1CD6BA8CE4A9A18AD84FFABBD8EFA59332BE7AD6756A66E294AFD185A78FF12AA520E4DE739BACA0C7FFEFF7F2955727A');
  S := THex.Decode('2AA058F73A0E33AB486B0F610410C53A7F132310');
  n := FromHex
    ('03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE661CE18FF55987308059B186823851EC7DD9CA1161DE93D5174D66E8382E9BB2FE84E47');
  h := TBigInteger.ValueOf(2);

  curve := TF2mCurve.Create(Fm, Fk1, Fk2, Fk3, a, b, n, h);
  G := TX9ECPoint.Create(curve,
    THex.Decode('04' +
    '0303001D34B856296C16C0D40D3CD7750A93D1D2955FA80AA5F40FC8DB7B2ABDBDE53950F4C0D293CDD711A35B67FB1499AE60038614F1394ABFA3B4C850D927E1E7769C8EEC2D19'
    + '037BF27342DA639B6DCCFFFEB73D69D78C6C27A6009CBBCA1980F8533921E8A684423E43BAB08A576291AF8F461BB2A8B3531D2F0485C19B16E2F1516E23DD3C1A4827AF1B8AC15B')
    );

  result := TX9ECParameters.Create(curve, G, n, h, S);
end;

class function TSecNamedCurves.TSect571r1Holder.Instance: IX9ECParametersHolder;
begin
  result := TSect571r1Holder.Create();
end;

end.
