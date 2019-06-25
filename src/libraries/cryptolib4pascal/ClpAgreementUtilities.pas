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

unit ClpAgreementUtilities;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  ClpIBasicAgreement,
  ClpDHBasicAgreement,
  ClpIDHBasicAgreement,
  ClpECDHBasicAgreement,
  ClpIECDHBasicAgreement,
  ClpECDHCBasicAgreement,
  ClpIECDHCBasicAgreement,
  ClpX25519Agreement,
  ClpIX25519Agreement,
  ClpIRawAgreement,
  ClpEdECObjectIdentifiers,
  ClpIAsn1Objects,
  ClpCryptoLibTypes;

resourcestring
  SUnRecognizedRawAgreementAlgorithm =
    'Raw Agreement Algorithm "%s" Not Recognised.';
  SUnRecognizedBasicAgreementAlgorithm =
    'Basic Agreement Algorithm "%s" Not Recognised.';

type

  /// <summary>
  /// Utility class for creating IBasicAgreement objects from their
  /// names/Oids
  /// </summary>
  TAgreementUtilities = class sealed(TObject)

  strict private
    class var

      Falgorithms: TDictionary<String, String>;

    class function GetMechanism(const algorithm: String): String;
      static; inline;

    class procedure Boot(); static;
    class constructor CreateAgreementUtilities();
    class destructor DestroyAgreementUtilities();

  public
    class function GetBasicAgreement(const algorithm: String)
      : IBasicAgreement; static;

    class function GetRawAgreement(const oid: IDerObjectIdentifier)
      : IRawAgreement; overload; static; inline;

    class function GetRawAgreement(const algorithm: String): IRawAgreement;
      overload; static;

    class function GetAlgorithmName(const oid: IDerObjectIdentifier): String;
      static; inline;

  end;

implementation

{ TAgreementUtilities }

class procedure TAgreementUtilities.Boot;
begin
  Falgorithms := TDictionary<string, string>.Create();
  Falgorithms.Add(TEdECObjectIdentifiers.id_X25519.Id, 'X25519');
end;

class constructor TAgreementUtilities.CreateAgreementUtilities;
begin
  TAgreementUtilities.Boot;
end;

class destructor TAgreementUtilities.DestroyAgreementUtilities;
begin
  Falgorithms.Free;
end;

class function TAgreementUtilities.GetMechanism(const algorithm
  : String): String;
var
  upper, mechanism: String;
begin
  upper := UpperCase(algorithm);
  if Falgorithms.TryGetValue(upper, mechanism) then
  begin
    result := mechanism
  end
  else
  begin
    result := upper;
  end;
end;

class function TAgreementUtilities.GetAlgorithmName
  (const oid: IDerObjectIdentifier): String;
begin
  if not(Falgorithms.TryGetValue(oid.Id, result)) then
  begin
    result := '';
  end;
end;

class function TAgreementUtilities.GetBasicAgreement(const algorithm: String)
  : IBasicAgreement;
var
  mechanism: String;
begin
  mechanism := GetMechanism(algorithm);

  if ((mechanism = 'DH') or (mechanism = 'DIFFIEHELLMAN')) then
  begin
    result := TDHBasicAgreement.Create() as IDHBasicAgreement;
    Exit;
  end;

  if (mechanism = 'ECDH') then
  begin
    result := TECDHBasicAgreement.Create() as IECDHBasicAgreement;
    Exit;
  end;

  if ((mechanism = 'ECDHC') or (mechanism = 'ECCDH')) then
  begin
    result := TECDHCBasicAgreement.Create() as IECDHCBasicAgreement;
    Exit;
  end;

  raise ESecurityUtilityCryptoLibException.CreateResFmt
    (@SUnRecognizedBasicAgreementAlgorithm, [algorithm]);
end;

class function TAgreementUtilities.GetRawAgreement(const algorithm: String)
  : IRawAgreement;
var
  mechanism: String;
begin
  mechanism := GetMechanism(algorithm);

  if (mechanism = 'X25519') then
  begin
    result := TX25519Agreement.Create() as IX25519Agreement;
    Exit;
  end;

  raise ESecurityUtilityCryptoLibException.CreateResFmt
    (@SUnRecognizedRawAgreementAlgorithm, [algorithm]);
end;

class function TAgreementUtilities.GetRawAgreement
  (const oid: IDerObjectIdentifier): IRawAgreement;
begin
  result := GetRawAgreement(oid.Id);
end;

end.
