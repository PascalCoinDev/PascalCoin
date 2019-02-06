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

unit ClpNistNamedCurves;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  ClpIX9ECParameters,
  ClpSecNamedCurves,
  ClpCryptoLibTypes,
  ClpIAsn1Objects,
  ClpSecObjectIdentifiers;

type

  /// <summary>
  /// Utility class for fetching curves using their NIST names as published
  /// in FIPS-PUB 186-3
  /// </summary>
  TNistNamedCurves = class sealed(TObject)

  strict private

  class var
    FobjIds: TDictionary<String, IDerObjectIdentifier>;
    Fnames: TDictionary<IDerObjectIdentifier, String>;

    class function GetNames: TCryptoLibStringArray; static; inline;

    class procedure DefineCurveAlias(const name: String;
      const oid: IDerObjectIdentifier); static; inline;

    class procedure Boot; static;
    class constructor CreateNistNamedCurves();
    class destructor DestroyNistNamedCurves();

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

  end;

implementation

{ TNistNamedCurves }

class procedure TNistNamedCurves.DefineCurveAlias(const name: String;
  const oid: IDerObjectIdentifier);
begin
  FobjIds.Add(UpperCase(name), oid);
  Fnames.Add(oid, name);
end;

class function TNistNamedCurves.GetByOid(const oid: IDerObjectIdentifier)
  : IX9ECParameters;
begin
  result := TSecNamedCurves.GetByOid(oid);
end;

class function TNistNamedCurves.GetOid(const name: String)
  : IDerObjectIdentifier;
begin
  if not(FobjIds.TryGetValue(UpperCase(name), result)) then
  begin
    result := Nil;
  end;
end;

class function TNistNamedCurves.GetByName(const name: String): IX9ECParameters;
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

class function TNistNamedCurves.GetName(const oid
  : IDerObjectIdentifier): String;
begin
  if not(Fnames.TryGetValue(oid, result)) then
  begin
    result := '';
  end;
end;

class function TNistNamedCurves.GetNames: TCryptoLibStringArray;
begin
  result := Fnames.Values.ToArray();
end;

class procedure TNistNamedCurves.Boot;
begin
  FobjIds := TDictionary<String, IDerObjectIdentifier>.Create();
  Fnames := TDictionary<IDerObjectIdentifier, String>.Create();

  DefineCurveAlias('B-163', TSecObjectIdentifiers.SecT163r2);
  DefineCurveAlias('B-233', TSecObjectIdentifiers.SecT233r1);
  DefineCurveAlias('B-283', TSecObjectIdentifiers.SecT283r1);
  DefineCurveAlias('B-409', TSecObjectIdentifiers.SecT409r1);
  DefineCurveAlias('B-571', TSecObjectIdentifiers.SecT571r1);

  DefineCurveAlias('K-163', TSecObjectIdentifiers.SecT163k1);
  DefineCurveAlias('K-233', TSecObjectIdentifiers.SecT233k1);
  DefineCurveAlias('K-283', TSecObjectIdentifiers.SecT283k1);
  DefineCurveAlias('K-409', TSecObjectIdentifiers.SecT409k1);
  DefineCurveAlias('K-571', TSecObjectIdentifiers.SecT571k1);

  DefineCurveAlias('P-192', TSecObjectIdentifiers.SecP192r1);
  DefineCurveAlias('P-224', TSecObjectIdentifiers.SecP224r1);
  DefineCurveAlias('P-256', TSecObjectIdentifiers.SecP256r1);
  DefineCurveAlias('P-384', TSecObjectIdentifiers.SecP384r1);
  DefineCurveAlias('P-521', TSecObjectIdentifiers.SecP521r1);
end;

class constructor TNistNamedCurves.CreateNistNamedCurves;
begin
  TNistNamedCurves.Boot;
end;

class destructor TNistNamedCurves.DestroyNistNamedCurves;
begin
  FobjIds.Free;
  Fnames.Free;
end;

end.
