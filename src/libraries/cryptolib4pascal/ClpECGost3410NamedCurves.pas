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

unit ClpECGost3410NamedCurves;

{$I CryptoLib.inc}

interface

uses
  Generics.Collections,
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpCryptoProObjectIdentifiers,
  ClpRosstandartObjectIdentifiers,
  ClpECC,
  ClpIECC,
  ClpECDomainParameters,
  ClpIECDomainParameters,
  ClpIAsn1Objects;

type
  /// <summary>
  /// Table of the available named parameters for GOST 3410-2001 / 2012.
  /// </summary>
  TECGost3410NamedCurves = class sealed(TObject)

  strict private
  class var
    Fparameters: TDictionary<IDerObjectIdentifier, IECDomainParameters>;
    FobjIds: TDictionary<String, IDerObjectIdentifier>;
    Fnames: TDictionary<IDerObjectIdentifier, String>;

    class procedure Boot(); static;
    class constructor CreateECGost3410NamedCurves();
    class destructor DestroyECGost3410NamedCurves();

    class function GetNames: TCryptoLibStringArray; static; inline;

  public
    // /**
    // * return the ECDomainParameters object for the given OID, null if it
    // * isn't present.
    // *
    // * @param oid an object identifier representing a named parameters, if present.
    // */
    class function GetByOid(const oid: IDerObjectIdentifier)
      : IECDomainParameters; static; inline;
    // /**
    // * return the ECDomainParameters object for the given OID, null if it
    // * isn't present.
    // *
    // * @param oid an object identifier representing a named parameters, if present.
    // */
    class function GetByName(const name: String): IECDomainParameters;
      static; inline;
    // /**
    // * return the named curve name represented by the given object identifier.
    // */
    class function GetName(const oid: IDerObjectIdentifier): String;
      static; inline;

    class function GetOid(const name: String): IDerObjectIdentifier;
      static; inline;

    // /**
    // * returns an enumeration containing the name strings for curves
    // * contained in this structure.
    // */
    class property Names: TCryptoLibStringArray read GetNames;

  end;

implementation

{ TECGost3410NamedCurves }

class procedure TECGost3410NamedCurves.Boot;
var
  mod_p, mod_q: TBigInteger;
  curve: IFPCurve;
  ecParams: IECDomainParameters;
begin

  Fparameters := TDictionary<IDerObjectIdentifier,
    IECDomainParameters>.Create();
  FobjIds := TDictionary<String, IDerObjectIdentifier>.Create();
  Fnames := TDictionary<IDerObjectIdentifier, String>.Create();

  mod_p := TBigInteger.Create
    ('115792089237316195423570985008687907853269984665640564039457584007913129639319');
  mod_q := TBigInteger.Create
    ('115792089237316195423570985008687907853073762908499243225378155805079068850323');

  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('115792089237316195423570985008687907853269984665640564039457584007913129639316'),
    // a
    TBigInteger.Create('166'), // b
    mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create('1'), // x
    TBigInteger.Create
    ('64033881142927202683649881450433473985931760268884941288852745803908878638612')
    ), // y
    mod_q);

  Fparameters.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProA,
    ecParams);

  mod_p := TBigInteger.Create
    ('115792089237316195423570985008687907853269984665640564039457584007913129639319');
  mod_q := TBigInteger.Create
    ('115792089237316195423570985008687907853073762908499243225378155805079068850323');

  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('115792089237316195423570985008687907853269984665640564039457584007913129639316'),
    TBigInteger.Create('166'), mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create('1'), // x
    TBigInteger.Create
    ('64033881142927202683649881450433473985931760268884941288852745803908878638612')
    ), // y
    mod_q);

  Fparameters.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProXchA,
    ecParams);

  mod_p := TBigInteger.Create
    ('57896044618658097711785492504343953926634992332820282019728792003956564823193');
  // p
  mod_q := TBigInteger.Create
    ('57896044618658097711785492504343953927102133160255826820068844496087732066703');
  // q

  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('57896044618658097711785492504343953926634992332820282019728792003956564823190'),
    // a
    TBigInteger.Create
    ('28091019353058090096996979000309560759124368558014865957655842872397301267595'),
    // b
    mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create('1'), // x
    TBigInteger.Create
    ('28792665814854611296992347458380284135028636778229113005756334730996303888124')
    ), // y
    mod_q); // q

  Fparameters.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProB,
    ecParams);

  mod_p := TBigInteger.Create
    ('70390085352083305199547718019018437841079516630045180471284346843705633502619');
  mod_q := TBigInteger.Create
    ('70390085352083305199547718019018437840920882647164081035322601458352298396601');

  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('70390085352083305199547718019018437841079516630045180471284346843705633502616'),
    TBigInteger.Create('32858'), mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create('0'),
    TBigInteger.Create
    ('29818893917731240733471273240314769927240550812383695689146495261604565990247')
    ), mod_q);

  Fparameters.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProXchB,
    ecParams);

  mod_p := TBigInteger.Create
    ('70390085352083305199547718019018437841079516630045180471284346843705633502619');
  // p
  mod_q := TBigInteger.Create
    ('70390085352083305199547718019018437840920882647164081035322601458352298396601');
  // q
  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('70390085352083305199547718019018437841079516630045180471284346843705633502616'),
    // a
    TBigInteger.Create('32858'), // b
    mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create('0'), // x
    TBigInteger.Create
    ('29818893917731240733471273240314769927240550812383695689146495261604565990247')
    ), // y
    mod_q); // q

  Fparameters.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProC,
    ecParams);

  // GOST34.10 2012
  mod_p := TBigInteger.Create
    ('115792089237316195423570985008687907853269984665640564039457584007913129639319');
  // p
  mod_q := TBigInteger.Create
    ('115792089237316195423570985008687907853073762908499243225378155805079068850323');
  // q
  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('115792089237316195423570985008687907853269984665640564039457584007913129639316'),
    // a
    TBigInteger.Create('166'), // b
    mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create('1'), // x
    TBigInteger.Create
    ('64033881142927202683649881450433473985931760268884941288852745803908878638612')
    ), // y
    mod_q); // q

  Fparameters.Add(TRosstandartObjectIdentifiers.
    id_tc26_gost_3410_12_256_paramSetA, ecParams);

  mod_p := TBigInteger.Create
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDC7',
    16); // p
  mod_q := TBigInteger.Create
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF27E69532F48D89116FF22B8D4E0560609B4B38ABFAD2B85DCACDB1411F10B275',
    16); // q
  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDC4',
    16), // a
    TBigInteger.Create
    ('E8C2505DEDFC86DDC1BD0B2B6667F1DA34B82574761CB0E879BD081CFD0B6265EE3CB090F30D27614CB4574010DA90DD862EF9D4EBEE4761503190785A71C760',
    16), // b
    mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create
    ('00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003'),
    // x
    TBigInteger.Create
    ('7503CFE87A836AE3A61B8816E25450E6CE5E1C93ACF1ABC1778064FDCBEFA921DF1626BE4FD036E93D75E6A50E3A41E98028FE5FC235F5B889A589CB5215F2A4',
    16)), // y
    mod_q); // q

  Fparameters.Add(TRosstandartObjectIdentifiers.
    id_tc26_gost_3410_12_512_paramSetA, ecParams);

  mod_p := TBigInteger.Create
    ('8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006F',
    16); // p
  mod_q := TBigInteger.Create
    ('800000000000000000000000000000000000000000000000000000000000000149A1EC142565A545ACFDB77BD9D40CFA8B996712101BEA0EC6346C54374F25BD',
    16); // q
  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006C',
    16), // a
    TBigInteger.Create
    ('687D1B459DC841457E3E06CF6F5E2517B97C7D614AF138BCBF85DC806C4B289F3E965D2DB1416D217F8B276FAD1AB69C50F78BEE1FA3106EFB8CCBC7C5140116',
    16), // b
    mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create
    ('00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002'),
    // x
    TBigInteger.Create
    ('1A8F7EDA389B094C2C071E3647A8940F3C123B697578C213BE6DD9E6C8EC7335DCB228FD1EDF4A39152CBCAAF8C0398828041055F94CEEEC7E21340780FE41BD',
    16)), // y
    mod_q); // q

  Fparameters.Add(TRosstandartObjectIdentifiers.
    id_tc26_gost_3410_12_512_paramSetB, ecParams);

  mod_p := TBigInteger.Create
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDC7',
    16); // p
  mod_q := TBigInteger.Create
    ('3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC98CDBA46506AB004C33A9FF5147502CC8EDA9E7A769A12694623CEF47F023ED',
    16); // q
  curve := TFpCurve.Create(mod_p, // p
    TBigInteger.Create
    ('DC9203E514A721875485A529D2C722FB187BC8980EB866644DE41C68E143064546E861C0E2C9EDD92ADE71F46FCF50FF2AD97F951FDA9F2A2EB6546F39689BD3',
    16), // a
    TBigInteger.Create
    ('B4C4EE28CEBC6C2C8AC12952CF37F16AC7EFB6A9F69F4B57FFDA2E4F0DE5ADE038CBC2FFF719D2C18DE0284B8BFEF3B52B8CC7A5F5BF0A3C8D2319A5312557E1',
    16), // b
    mod_q, TBigInteger.One);

  ecParams := TECDomainParameters.Create(curve,
    curve.CreatePoint(TBigInteger.Create
    ('E2E31EDFC23DE7BDEBE241CE593EF5DE2295B7A9CBAEF021D385F7074CEA043AA27272A7AE602BF2A7B9033DB9ED3610C6FB85487EAE97AAC5BC7928C1950148',
    16), // x
    TBigInteger.Create
    ('F5CE40D95B5EB899ABBCCFF5911CB8577939804D6527378B8C108C3D2090FF9BE18E2D33E3021ED2EF32D85822423B6304F726AA854BAE07D0396E9A9ADDC40F',
    16)), // y
    mod_q); // q

  Fparameters.Add(TRosstandartObjectIdentifiers.
    id_tc26_gost_3410_12_512_paramSetC, ecParams);

  FobjIds.Add('GostR3410-2001-CryptoPro-A',
    TCryptoProObjectIdentifiers.GostR3410x2001CryptoProA);
  FobjIds.Add('GostR3410-2001-CryptoPro-B',
    TCryptoProObjectIdentifiers.GostR3410x2001CryptoProB);
  FobjIds.Add('GostR3410-2001-CryptoPro-C',
    TCryptoProObjectIdentifiers.GostR3410x2001CryptoProC);
  FobjIds.Add('GostR3410-2001-CryptoPro-XchA',
    TCryptoProObjectIdentifiers.GostR3410x2001CryptoProXchA);
  FobjIds.Add('GostR3410-2001-CryptoPro-XchB',
    TCryptoProObjectIdentifiers.GostR3410x2001CryptoProXchB);
  FobjIds.Add('Tc26-Gost-3410-12-256-paramSetA',
    TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_256_paramSetA);
  FobjIds.Add('Tc26-Gost-3410-12-512-paramSetA',
    TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_512_paramSetA);
  FobjIds.Add('Tc26-Gost-3410-12-512-paramSetB',
    TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_512_paramSetB);
  FobjIds.Add('Tc26-Gost-3410-12-512-paramSetC',
    TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_512_paramSetC);

  Fnames.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProA,
    'GostR3410-2001-CryptoPro-A');
  Fnames.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProB,
    'GostR3410-2001-CryptoPro-B');
  Fnames.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProC,
    'GostR3410-2001-CryptoPro-C');
  Fnames.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProXchA,
    'GostR3410-2001-CryptoPro-XchA');
  Fnames.Add(TCryptoProObjectIdentifiers.GostR3410x2001CryptoProXchB,
    'GostR3410-2001-CryptoPro-XchB');
  Fnames.Add(TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_256_paramSetA,
    'Tc26-Gost-3410-12-256-paramSetA');
  Fnames.Add(TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_512_paramSetA,
    'Tc26-Gost-3410-12-512-paramSetA');
  Fnames.Add(TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_512_paramSetB,
    'Tc26-Gost-3410-12-512-paramSetB');
  Fnames.Add(TRosstandartObjectIdentifiers.id_tc26_gost_3410_12_512_paramSetC,
    'Tc26-Gost-3410-12-512-paramSetC');
end;

class constructor TECGost3410NamedCurves.CreateECGost3410NamedCurves;
begin
  TECGost3410NamedCurves.Boot;
end;

class destructor TECGost3410NamedCurves.DestroyECGost3410NamedCurves;
begin
  Fparameters.Free;
  FobjIds.Free;
  Fnames.Free;
end;

class function TECGost3410NamedCurves.GetByName(const name: String)
  : IECDomainParameters;
var
  oid: IDerObjectIdentifier;
begin
  if (FobjIds.TryGetValue(name, oid)) then
  begin
    Fparameters.TryGetValue(oid, Result);
    Exit;
  end;
  Result := Nil;
end;

class function TECGost3410NamedCurves.GetByOid(const oid: IDerObjectIdentifier)
  : IECDomainParameters;
begin
  Fparameters.TryGetValue(oid, Result);
end;

class function TECGost3410NamedCurves.GetName
  (const oid: IDerObjectIdentifier): String;
begin
  Fnames.TryGetValue(oid, Result);
end;

class function TECGost3410NamedCurves.GetNames: TCryptoLibStringArray;
begin
  Result := Fnames.Values.ToArray();
end;

class function TECGost3410NamedCurves.GetOid(const name: String)
  : IDerObjectIdentifier;
begin
  if (not(FobjIds.TryGetValue(name, Result))) then
  begin
    Result := Nil;
  end;
end;

end.
