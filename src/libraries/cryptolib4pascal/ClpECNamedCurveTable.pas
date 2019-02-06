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

unit ClpECNamedCurveTable;

{$I CryptoLib.inc}

interface

uses
  Generics.Collections,
  ClpCryptoLibTypes,
  // ClpX962NamedCurves,
  // ClpECGost3410NamedCurves,
  // ClpX9ECParameters,
  ClpSecNamedCurves,
  ClpNistNamedCurves,
  // ClpIECDomainParameters,
  ClpIAsn1Objects,
  ClpIX9ECParameters;

type
  /// <summary>
  /// A general class that reads all X9.62 style EC curve tables.
  /// </summary>
  TECNamedCurveTable = class sealed(TObject)

  strict private

    class function GetNames: TCryptoLibStringArray; static;
    // class function FromDomainParameters(const dp: IECDomainParameters)
    // : IX9ECParameters; static; inline;

  public
    // /**
    // * return a X9ECParameters object representing the passed in named
    // * curve. The routine returns null if the curve is not present.
    // *
    // * @param name the name of the curve requested
    // * @return an X9ECParameters object or null if the curve is not available.
    // */
    class function GetByName(const name: String): IX9ECParameters; static;

    class function GetName(const oid: IDerObjectIdentifier): String; static;
    // /**
    // * return the object identifier signified by the passed in name. Null
    // * if there is no object identifier associated with name.
    // *
    // * @return the object identifier associated with name, if present.
    // */
    class function GetOid(const name: String): IDerObjectIdentifier; static;

    // /**
    // * return a X9ECParameters object representing the passed in named
    // * curve.
    // *
    // * @param oid the object id of the curve requested
    // * @return an X9ECParameters object or null if the curve is not available.
    // */

    class function GetByOid(const oid: IDerObjectIdentifier)
      : IX9ECParameters; static;

    // /**
    // * return an enumeration of the names of the available curves.
    // *
    // * @return an enumeration of the names of the available curves.
    // */
    class property Names: TCryptoLibStringArray read GetNames;

  end;

implementation

{ TECNamedCurveTable }

// class function TECNamedCurveTable.FromDomainParameters
// (const dp: IECDomainParameters): IX9ECParameters;
// begin
// if dp = Nil then
// begin
// result := Nil;
// end
// else
// begin
// result := TX9ECParameters.Create(dp.Curve, dp.G, dp.N, dp.H, dp.GetSeed())
// end;
//
// end;

class function TECNamedCurveTable.GetByName(const name: String)
  : IX9ECParameters;
var
  ecP: IX9ECParameters;
begin
  // ecP := TX962NamedCurves.GetByName(name);
  // if (ecP = Nil) then
  // begin
  ecP := TSecNamedCurves.GetByName(name);
  // end;

  if (ecP = Nil) then
  begin
    ecP := TNistNamedCurves.GetByName(name);
  end;

  result := ecP;
end;

class function TECNamedCurveTable.GetByOid(const oid: IDerObjectIdentifier)
  : IX9ECParameters;
var
  ecP: IX9ECParameters;
begin
  // ecP := TX962NamedCurves.GetByOid(oid);
  // if (ecP = Nil) then
  // begin
  ecP := TSecNamedCurves.GetByOid(oid);
  // end;
  // NOTE: All the NIST curves are currently from SEC, so no point in redundant OID lookup
  result := ecP;
end;

class function TECNamedCurveTable.GetName
  (const oid: IDerObjectIdentifier): String;
var
  name: String;
begin
  // name := TX962NamedCurves.GetName(oid);
  // if (name = '') then
  // begin
  name := TSecNamedCurves.GetName(oid);
  // end;

  if (name = '') then
  begin
    name := TNistNamedCurves.GetName(oid);
  end;
  result := name;
end;

class function TECNamedCurveTable.GetOid(const name: String)
  : IDerObjectIdentifier;
var
  oid: IDerObjectIdentifier;
begin
  // oid := TX962NamedCurves.GetOid(name);
  // if (oid = Nil) then
  // begin
  oid := TSecNamedCurves.GetOid(name);
  // end;

  if (oid = Nil) then
  begin
    oid := TNistNamedCurves.GetOid(name);
  end;

  result := oid;
end;

class function TECNamedCurveTable.GetNames: TCryptoLibStringArray;
var
  temp: TList<String>;
begin
  temp := TList<String>.Create();
  try
    temp.AddRange(TSecNamedCurves.Names);
    temp.AddRange(TNistNamedCurves.Names);
    result := temp.ToArray;
  finally
    temp.Free;
  end;

end;

end.
