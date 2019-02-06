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

unit ClpMacUtilities;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  ClpIAsn1Objects,
  ClpIMac,
  ClpHMac,
  ClpICipherParameters,
  ClpIanaObjectIdentifiers,
  ClpPkcsObjectIdentifiers,
  ClpNistObjectIdentifiers,
  ClpRosstandartObjectIdentifiers,
  ClpDigestUtilities,
  ClpStringUtils,
  ClpCryptoLibTypes;

resourcestring
  SUnRecognizedMac = '"Mac " %s not recognised.';

type
  TMacUtilities = class sealed(TObject)

  strict private

    class var

      Falgorithms: TDictionary<String, String>;

    class procedure Boot(); static;
    class constructor CreateMacUtilities();
    class destructor DestroyMacUtilities();

  public

    class function GetMac(const id: IDerObjectIdentifier): IMac; overload;
      static; inline;
    class function GetMac(const algorithm: string): IMac; overload; static;

    class function GetAlgorithmName(const oid: IDerObjectIdentifier): string;
      static; inline;

    class function CalculateMac(const algorithm: String;
      const cp: ICipherParameters; const input: TCryptoLibByteArray)
      : TCryptoLibByteArray; static; inline;

    class function DoFinal(const mac: IMac): TCryptoLibByteArray; overload;
      static; inline;

    class function DoFinal(const mac: IMac; const input: TCryptoLibByteArray)
      : TCryptoLibByteArray; overload; static; inline;

  end;

implementation

{ TMacUtilities }

class procedure TMacUtilities.Boot;
begin
  Falgorithms := TDictionary<string, string>.Create();

  TIanaObjectIdentifiers.Boot;

  Falgorithms.Add(TIanaObjectIdentifiers.HmacMD5.id, 'HMAC-MD5');
  Falgorithms.Add(TIanaObjectIdentifiers.HmacRipeMD160.id, 'HMAC-RIPEMD160');
  Falgorithms.Add(TIanaObjectIdentifiers.HmacSha1.id, 'HMAC-SHA1');
  Falgorithms.Add(TIanaObjectIdentifiers.HmacTiger.id, 'HMAC-TIGER');

  TPkcsObjectIdentifiers.Boot;

  Falgorithms.Add(TPkcsObjectIdentifiers.IdHmacWithSha1.id, 'HMAC-SHA1');
  Falgorithms.Add(TPkcsObjectIdentifiers.IdHmacWithSha224.id, 'HMAC-SHA224');
  Falgorithms.Add(TPkcsObjectIdentifiers.IdHmacWithSha256.id, 'HMAC-SHA256');
  Falgorithms.Add(TPkcsObjectIdentifiers.IdHmacWithSha384.id, 'HMAC-SHA384');
  Falgorithms.Add(TPkcsObjectIdentifiers.IdHmacWithSha512.id, 'HMAC-SHA512');

  TNistObjectIdentifiers.Boot;

  Falgorithms.Add(TNistObjectIdentifiers.IdHMacWithSha3_224.id,
    'HMAC-SHA3-224');
  Falgorithms.Add(TNistObjectIdentifiers.IdHMacWithSha3_256.id,
    'HMAC-SHA3-256');
  Falgorithms.Add(TNistObjectIdentifiers.IdHMacWithSha3_384.id,
    'HMAC-SHA3-384');
  Falgorithms.Add(TNistObjectIdentifiers.IdHMacWithSha3_512.id,
    'HMAC-SHA3-512');

  TRosstandartObjectIdentifiers.Boot;

  Falgorithms.Add(TRosstandartObjectIdentifiers.id_tc26_hmac_gost_3411_12_256.
    id, 'HMAC-GOST3411-2012-256');
  Falgorithms.Add(TRosstandartObjectIdentifiers.id_tc26_hmac_gost_3411_12_512.
    id, 'HMAC-GOST3411-2012-512');
end;

class function TMacUtilities.GetMac(const algorithm: string): IMac;
var
  upper, mechanism, digestName: string;
  HighPoint: Int32;
begin
  upper := UpperCase(algorithm);

  if not Falgorithms.TryGetValue(upper, mechanism) then
  begin
    mechanism := upper;
  end;

  if TStringUtils.BeginsWith(mechanism, 'HMAC', True) then
  begin
{$IFDEF DELPHIXE3_UP}
    HighPoint := System.High(mechanism);
{$ELSE}
    HighPoint := System.Length(mechanism);
{$ENDIF DELPHIXE3_UP}
    if ((TStringUtils.BeginsWith(mechanism, 'HMAC-', True)) or
      (TStringUtils.BeginsWith(mechanism, 'HMAC/', True))) then
    begin
      digestName := System.Copy(mechanism, 6, HighPoint - 5);
    end
    else
    begin
      digestName := System.Copy(mechanism, 5, HighPoint - 4);
    end;

    result := THMac.Create(TDigestUtilities.GetDigest(digestName));
    Exit;
  end;

  raise ESecurityUtilityCryptoLibException.CreateResFmt(@SUnRecognizedMac,
    [mechanism]);
end;

class function TMacUtilities.DoFinal(const mac: IMac): TCryptoLibByteArray;
begin
  System.SetLength(result, mac.GetMacSize());
  mac.DoFinal(result, 0);
end;

class function TMacUtilities.DoFinal(const mac: IMac;
  const input: TCryptoLibByteArray): TCryptoLibByteArray;
begin
  mac.BlockUpdate(input, 0, System.Length(input));
  result := DoFinal(mac);
end;

class function TMacUtilities.CalculateMac(const algorithm: String;
  const cp: ICipherParameters; const input: TCryptoLibByteArray)
  : TCryptoLibByteArray;
var
  mac: IMac;
begin
  mac := GetMac(algorithm);
  mac.Init(cp);
  mac.BlockUpdate(input, 0, System.Length(input));
  result := DoFinal(mac);
end;

class constructor TMacUtilities.CreateMacUtilities;
begin
  TMacUtilities.Boot;
end;

class destructor TMacUtilities.DestroyMacUtilities;
begin
  Falgorithms.Free;
end;

class function TMacUtilities.GetMac(const id: IDerObjectIdentifier): IMac;
begin
  result := GetMac(id.id);
end;

class function TMacUtilities.GetAlgorithmName
  (const oid: IDerObjectIdentifier): string;
begin
  Falgorithms.TryGetValue(oid.id, result);
end;

end.
