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

unit ClpX9ObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TX9ObjectIdentifiers = class abstract(TObject)

  strict private
    //
    // X9.62
    //
    // ansi-X9-62 OBJECT IDENTIFIER ::= { iso(1) member-body(2)
    // us(840) ansi-x962(10045) }
    //
  const

    AnsiX962: String = '1.2.840.10045';

  class var

    FIsBooted: Boolean;
    Fansi_X9_62, FIdFieldType, FPrimeField, FCharacteristicTwoField, FGNBasis,
      FTPBasis, FPPBasis, Fid_ecSigType, FECDsaWithSha1, Fid_publicKeyType,
      FIdECPublicKey, FECDsaWithSha2, FECDsaWithSha224, FECDsaWithSha256,
      FECDsaWithSha384, FECDsaWithSha512, FEllipticCurve, FPrimeCurve,
      FPrime256v1, FPrime239v2, FPrime239v3, FPrime192v2, FPrime239v1,
      FPrime192v3, FPrime192v1, FIdDsa, FIdDsaWithSha1: IDerObjectIdentifier;

    class function Getansi_X9_62: IDerObjectIdentifier; static; inline;
    class function GetIdFieldType: IDerObjectIdentifier; static; inline;
    class function GetPrimeField: IDerObjectIdentifier; static; inline;
    class function GetCharacteristicTwoField: IDerObjectIdentifier;
      static; inline;
    class function GetEllipticCurve: IDerObjectIdentifier; static; inline;
    class function GetPrimeCurve: IDerObjectIdentifier; static; inline;
    class function GetPrime192v1: IDerObjectIdentifier; static; inline;
    class function GetPrime192v2: IDerObjectIdentifier; static; inline;
    class function GetPrime192v3: IDerObjectIdentifier; static; inline;
    class function GetPrime239v1: IDerObjectIdentifier; static; inline;
    class function GetPrime239v2: IDerObjectIdentifier; static; inline;
    class function GetPrime239v3: IDerObjectIdentifier; static; inline;
    class function GetPrime256v1: IDerObjectIdentifier; static; inline;
    class function GetGNBasis: IDerObjectIdentifier; static; inline;
    class function GetPPBasis: IDerObjectIdentifier; static; inline;
    class function GetTPBasis: IDerObjectIdentifier; static; inline;
    class function Getid_ecSigType: IDerObjectIdentifier; static; inline;

    class function GetECDsaWithSha1: IDerObjectIdentifier; static; inline;
    class function GetECDsaWithSha2: IDerObjectIdentifier; static; inline;
    class function GetECDsaWithSha224: IDerObjectIdentifier; static; inline;
    class function GetECDsaWithSha256: IDerObjectIdentifier; static; inline;
    class function GetECDsaWithSha384: IDerObjectIdentifier; static; inline;
    class function GetECDsaWithSha512: IDerObjectIdentifier; static; inline;
    class function Getid_publicKeyType: IDerObjectIdentifier; static; inline;
    class function GetIdECPublicKey: IDerObjectIdentifier; static; inline;

    class function GetIdDsa: IDerObjectIdentifier; static; inline;
    class function GetIdDsaWithSha1: IDerObjectIdentifier; static; inline;

    class constructor X9ObjectIdentifiers();

  public

    class property ansi_X9_62: IDerObjectIdentifier read Getansi_X9_62;
    class property IdFieldType: IDerObjectIdentifier read GetIdFieldType;
    class property PrimeField: IDerObjectIdentifier read GetPrimeField;
    class property CharacteristicTwoField: IDerObjectIdentifier
      read GetCharacteristicTwoField;
    class property GNBasis: IDerObjectIdentifier read GetGNBasis;
    class property TPBasis: IDerObjectIdentifier read GetTPBasis;
    class property PPBasis: IDerObjectIdentifier read GetPPBasis;

    class property id_ecSigType: IDerObjectIdentifier read Getid_ecSigType;

    class property ECDsaWithSha1: IDerObjectIdentifier read GetECDsaWithSha1;

    class property id_publicKeyType: IDerObjectIdentifier
      read Getid_publicKeyType;

    class property IdECPublicKey: IDerObjectIdentifier read GetIdECPublicKey;

    class property ECDsaWithSha2: IDerObjectIdentifier read GetECDsaWithSha2;
    class property ECDsaWithSha224: IDerObjectIdentifier
      read GetECDsaWithSha224;
    class property ECDsaWithSha256: IDerObjectIdentifier
      read GetECDsaWithSha256;
    class property ECDsaWithSha384: IDerObjectIdentifier
      read GetECDsaWithSha384;
    class property ECDsaWithSha512: IDerObjectIdentifier
      read GetECDsaWithSha512;

    class property EllipticCurve: IDerObjectIdentifier read GetEllipticCurve;
    class property PrimeCurve: IDerObjectIdentifier read GetPrimeCurve;
    class property Prime192v1: IDerObjectIdentifier read GetPrime192v1;
    class property Prime192v2: IDerObjectIdentifier read GetPrime192v2;
    class property Prime192v3: IDerObjectIdentifier read GetPrime192v3;
    class property Prime239v1: IDerObjectIdentifier read GetPrime239v1;
    class property Prime239v2: IDerObjectIdentifier read GetPrime239v2;
    class property Prime239v3: IDerObjectIdentifier read GetPrime239v3;
    class property Prime256v1: IDerObjectIdentifier read GetPrime256v1;

    class property IdDsa: IDerObjectIdentifier read GetIdDsa;
    class property IdDsaWithSha1: IDerObjectIdentifier read GetIdDsaWithSha1;

    class procedure Boot(); static;

  end;

implementation

{ TX9ObjectIdentifiers }

class function TX9ObjectIdentifiers.Getansi_X9_62: IDerObjectIdentifier;
begin
  result := Fansi_X9_62;
end;

class function TX9ObjectIdentifiers.GetIdDsa: IDerObjectIdentifier;
begin
  result := FIdDsa;
end;

class function TX9ObjectIdentifiers.GetIdDsaWithSha1: IDerObjectIdentifier;
begin
  result := FIdDsaWithSha1;
end;

class function TX9ObjectIdentifiers.GetIdECPublicKey: IDerObjectIdentifier;
begin
  result := FIdECPublicKey;
end;

class function TX9ObjectIdentifiers.GetIdFieldType: IDerObjectIdentifier;
begin
  result := FIdFieldType;
end;

class function TX9ObjectIdentifiers.Getid_ecSigType: IDerObjectIdentifier;
begin
  result := Fid_ecSigType;
end;

class function TX9ObjectIdentifiers.Getid_publicKeyType: IDerObjectIdentifier;
begin
  result := Fid_publicKeyType;
end;

class function TX9ObjectIdentifiers.GetPrimeField: IDerObjectIdentifier;
begin
  result := FPrimeField;
end;

class function TX9ObjectIdentifiers.GetCharacteristicTwoField
  : IDerObjectIdentifier;
begin
  result := FCharacteristicTwoField;
end;

class function TX9ObjectIdentifiers.GetGNBasis: IDerObjectIdentifier;
begin
  result := FGNBasis;
end;

class function TX9ObjectIdentifiers.GetTPBasis: IDerObjectIdentifier;
begin
  result := FTPBasis;
end;

class function TX9ObjectIdentifiers.GetPPBasis: IDerObjectIdentifier;
begin
  result := FPPBasis;
end;

class function TX9ObjectIdentifiers.GetECDsaWithSha1: IDerObjectIdentifier;
begin
  result := FECDsaWithSha1;
end;

class function TX9ObjectIdentifiers.GetECDsaWithSha2: IDerObjectIdentifier;
begin
  result := FECDsaWithSha2;
end;

class function TX9ObjectIdentifiers.GetECDsaWithSha224: IDerObjectIdentifier;
begin
  result := FECDsaWithSha224;
end;

class function TX9ObjectIdentifiers.GetECDsaWithSha256: IDerObjectIdentifier;
begin
  result := FECDsaWithSha256;
end;

class function TX9ObjectIdentifiers.GetECDsaWithSha384: IDerObjectIdentifier;
begin
  result := FECDsaWithSha384;
end;

class function TX9ObjectIdentifiers.GetECDsaWithSha512: IDerObjectIdentifier;
begin
  result := FECDsaWithSha512;
end;

class function TX9ObjectIdentifiers.GetEllipticCurve: IDerObjectIdentifier;
begin
  result := FEllipticCurve;
end;

class function TX9ObjectIdentifiers.GetPrime192v1: IDerObjectIdentifier;
begin
  result := FPrime192v1;
end;

class function TX9ObjectIdentifiers.GetPrime192v2: IDerObjectIdentifier;
begin
  result := FPrime192v2;
end;

class function TX9ObjectIdentifiers.GetPrime192v3: IDerObjectIdentifier;
begin
  result := FPrime192v3;
end;

class function TX9ObjectIdentifiers.GetPrime239v1: IDerObjectIdentifier;
begin
  result := FPrime239v1;
end;

class function TX9ObjectIdentifiers.GetPrime239v2: IDerObjectIdentifier;
begin
  result := FPrime239v2;
end;

class function TX9ObjectIdentifiers.GetPrime239v3: IDerObjectIdentifier;
begin
  result := FPrime239v3;
end;

class function TX9ObjectIdentifiers.GetPrime256v1: IDerObjectIdentifier;
begin
  result := FPrime256v1;
end;

class function TX9ObjectIdentifiers.GetPrimeCurve: IDerObjectIdentifier;
begin
  result := FPrimeCurve;
end;

class procedure TX9ObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    Fansi_X9_62 := TDerObjectIdentifier.Create(AnsiX962);

    FIdFieldType := ansi_X9_62.Branch('1');

    FPrimeField := IdFieldType.Branch('1');

    FCharacteristicTwoField := IdFieldType.Branch('2');

    FGNBasis := CharacteristicTwoField.Branch('3.1');
    FTPBasis := CharacteristicTwoField.Branch('3.2');
    FPPBasis := CharacteristicTwoField.Branch('3.3');

    Fid_ecSigType := ansi_X9_62.Branch('4');
    //
    FECDsaWithSha1 := id_ecSigType.Branch('1');

    Fid_publicKeyType := ansi_X9_62.Branch('2');

    FIdECPublicKey := id_publicKeyType.Branch('1');

    FECDsaWithSha2 := id_ecSigType.Branch('3');

    FECDsaWithSha224 := ECDsaWithSha2.Branch('1');
    FECDsaWithSha256 := ECDsaWithSha2.Branch('2');
    FECDsaWithSha384 := ECDsaWithSha2.Branch('3');
    FECDsaWithSha512 := ECDsaWithSha2.Branch('4');
    //
    //
    // named curves
    //
    FEllipticCurve := ansi_X9_62.Branch('3');

    //
    // Prime
    //
    FPrimeCurve := EllipticCurve.Branch('1');

    FPrime192v1 := PrimeCurve.Branch('1');
    FPrime192v2 := PrimeCurve.Branch('2');
    FPrime192v3 := PrimeCurve.Branch('3');
    FPrime239v1 := PrimeCurve.Branch('4');
    FPrime239v2 := PrimeCurve.Branch('5');
    FPrime239v3 := PrimeCurve.Branch('6');
    FPrime256v1 := PrimeCurve.Branch('7');

    //
    // DSA
    //
    // dsapublicnumber OBJECT IDENTIFIER ::= { iso(1) member-body(2)
    // us(840) ansi-x957(10040) number-type(4) 1 }
    FIdDsa := TDerObjectIdentifier.Create('1.2.840.10040.4.1');

    // /**
    // *   id-dsa-with-sha1 OBJECT IDENTIFIER ::=  { iso(1) member-body(2)
    // *         us(840) x9-57 (10040) x9cm(4) 3 }
    // */
    FIdDsaWithSha1 := TDerObjectIdentifier.Create('1.2.840.10040.4.3');

    FIsBooted := True;
  end;

end;

class constructor TX9ObjectIdentifiers.X9ObjectIdentifiers;
begin
  TX9ObjectIdentifiers.Boot;
end;

end.
