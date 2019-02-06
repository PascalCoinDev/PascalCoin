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

unit ClpNistObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TNistObjectIdentifiers = class sealed(TObject)

  strict private

  class var

    FIsBooted: Boolean;
    FNistAlgorithm, FHashAlgs, FSigAlgs, FIdSha256, FIdSha384, FIdSha512,
      FIdSha224, FIdSha512_224, FIdSha512_256, FIdSha3_224, FIdSha3_256,
      FIdSha3_384, FIdSha3_512, FIdHMacWithSha3_224, FIdHMacWithSha3_256,
      FIdHMacWithSha3_384, FIdHMacWithSha3_512, FAES, FIdAES128Ecb,
      FIdAes128Cbc, FIdAes128Ofb, FIdAes128Cfb, FidAes192Ecb, FIdAes192Cbc,
      FIdAes192Ofb, FIdAes192Cfb, FIdAes256Ecb, FIdAes256Cbc, FIdAes256Ofb,
      FIdAes256Cfb, FIdDsaWithSha2, FDsaWithSha224, FDsaWithSha256,
      FDsaWithSha384, FDsaWithSha512, FIdDsaWithSha3_224, FIdDsaWithSha3_256,
      FIdDsaWithSha3_384, FIdDsaWithSha3_512, FIdECDsaWithSha3_224,
      FIdECDsaWithSha3_256, FIdECDsaWithSha3_384, FIdECDsaWithSha3_512
      : IDerObjectIdentifier;

    class function GetNistAlgorithm: IDerObjectIdentifier; static; inline;
    class function GetHashAlgs: IDerObjectIdentifier; static; inline;
    class function GetSigAlgs: IDerObjectIdentifier; static; inline;

    class function GetIdSha224: IDerObjectIdentifier; static; inline;
    class function GetIdSha256: IDerObjectIdentifier; static; inline;
    class function GetIdSha3_224: IDerObjectIdentifier; static; inline;
    class function GetIdSha3_256: IDerObjectIdentifier; static; inline;
    class function GetIdSha3_384: IDerObjectIdentifier; static; inline;
    class function GetIdSha3_512: IDerObjectIdentifier; static; inline;
    class function GetIdSha384: IDerObjectIdentifier; static; inline;
    class function GetIdSha512: IDerObjectIdentifier; static; inline;
    class function GetIdSha512_224: IDerObjectIdentifier; static; inline;
    class function GetIdSha512_256: IDerObjectIdentifier; static; inline;

    class function GetIdHMacWithSha3_224: IDerObjectIdentifier; static; inline;
    class function GetIdHMacWithSha3_256: IDerObjectIdentifier; static; inline;
    class function GetIdHMacWithSha3_384: IDerObjectIdentifier; static; inline;
    class function GetIdHMacWithSha3_512: IDerObjectIdentifier; static; inline;

    class function GetAES: IDerObjectIdentifier; static; inline;

    class function GetIdAes128Ecb: IDerObjectIdentifier; static; inline;
    class function GetIdAes128Cbc: IDerObjectIdentifier; static; inline;
    class function GetIdAes128Ofb: IDerObjectIdentifier; static; inline;
    class function GetIdAes128Cfb: IDerObjectIdentifier; static; inline;
    class function GetIdAes192Ecb: IDerObjectIdentifier; static; inline;
    class function GetIdAes192Cbc: IDerObjectIdentifier; static; inline;
    class function GetIdAes192Ofb: IDerObjectIdentifier; static; inline;
    class function GetIdAes192Cfb: IDerObjectIdentifier; static; inline;
    class function GetIdAes256Ecb: IDerObjectIdentifier; static; inline;
    class function GetIdAes256Cbc: IDerObjectIdentifier; static; inline;
    class function GetIdAes256Ofb: IDerObjectIdentifier; static; inline;
    class function GetIdAes256Cfb: IDerObjectIdentifier; static; inline;

    class function GetIdDsaWithSha2: IDerObjectIdentifier; static; inline;
    class function GetDsaWithSha224: IDerObjectIdentifier; static; inline;
    class function GetDsaWithSha256: IDerObjectIdentifier; static; inline;
    class function GetDsaWithSha384: IDerObjectIdentifier; static; inline;
    class function GetDsaWithSha512: IDerObjectIdentifier; static; inline;

    class function GetIdDsaWithSha3_224: IDerObjectIdentifier; static; inline;
    class function GetIdDsaWithSha3_256: IDerObjectIdentifier; static; inline;
    class function GetIdDsaWithSha3_384: IDerObjectIdentifier; static; inline;
    class function GetIdDsaWithSha3_512: IDerObjectIdentifier; static; inline;

    class function GetIdECDsaWithSha3_224: IDerObjectIdentifier; static; inline;
    class function GetIdECDsaWithSha3_256: IDerObjectIdentifier; static; inline;
    class function GetIdECDsaWithSha3_384: IDerObjectIdentifier; static; inline;
    class function GetIdECDsaWithSha3_512: IDerObjectIdentifier; static; inline;

    class constructor NistObjectIdentifiers();

  public

    //
    // NIST
    // iso/itu(2) joint-assign(16) us(840) organization(1) gov(101) csor(3)

    //
    // nistalgorithms(4)
    //
    class property NistAlgorithm: IDerObjectIdentifier read GetNistAlgorithm;
    class property HashAlgs: IDerObjectIdentifier read GetHashAlgs;
    class property SigAlgs: IDerObjectIdentifier read GetSigAlgs;

    class property IdSha256: IDerObjectIdentifier read GetIdSha256;
    class property IdSha384: IDerObjectIdentifier read GetIdSha384;
    class property IdSha512: IDerObjectIdentifier read GetIdSha512;
    class property IdSha224: IDerObjectIdentifier read GetIdSha224;
    class property IdSha512_224: IDerObjectIdentifier read GetIdSha512_224;
    class property IdSha512_256: IDerObjectIdentifier read GetIdSha512_256;
    class property IdSha3_224: IDerObjectIdentifier read GetIdSha3_224;
    class property IdSha3_256: IDerObjectIdentifier read GetIdSha3_256;
    class property IdSha3_384: IDerObjectIdentifier read GetIdSha3_384;
    class property IdSha3_512: IDerObjectIdentifier read GetIdSha3_512;

    class property IdHMacWithSha3_224: IDerObjectIdentifier
      read GetIdHMacWithSha3_224;
    class property IdHMacWithSha3_256: IDerObjectIdentifier
      read GetIdHMacWithSha3_256;
    class property IdHMacWithSha3_384: IDerObjectIdentifier
      read GetIdHMacWithSha3_384;
    class property IdHMacWithSha3_512: IDerObjectIdentifier
      read GetIdHMacWithSha3_512;

    class property AES: IDerObjectIdentifier read GetAES;

    class property IdAes128Ecb: IDerObjectIdentifier read GetIdAes128Ecb;
    class property IdAes128Cbc: IDerObjectIdentifier read GetIdAes128Cbc;
    class property IdAes128Ofb: IDerObjectIdentifier read GetIdAes128Ofb;
    class property IdAes128Cfb: IDerObjectIdentifier read GetIdAes128Cfb;
    class property IdAes192Ecb: IDerObjectIdentifier read GetIdAes192Ecb;
    class property IdAes192Cbc: IDerObjectIdentifier read GetIdAes192Cbc;
    class property IdAes192Ofb: IDerObjectIdentifier read GetIdAes192Ofb;
    class property IdAes192Cfb: IDerObjectIdentifier read GetIdAes192Cfb;
    class property IdAes256Ecb: IDerObjectIdentifier read GetIdAes256Ecb;
    class property IdAes256Cbc: IDerObjectIdentifier read GetIdAes256Cbc;
    class property IdAes256Ofb: IDerObjectIdentifier read GetIdAes256Ofb;
    class property IdAes256Cfb: IDerObjectIdentifier read GetIdAes256Cfb;

    class property IdDsaWithSha2: IDerObjectIdentifier read GetIdDsaWithSha2;
    class property DsaWithSha224: IDerObjectIdentifier read GetDsaWithSha224;
    class property DsaWithSha256: IDerObjectIdentifier read GetDsaWithSha256;
    class property DsaWithSha384: IDerObjectIdentifier read GetDsaWithSha384;
    class property DsaWithSha512: IDerObjectIdentifier read GetDsaWithSha512;

    class property IdDsaWithSha3_224: IDerObjectIdentifier
      read GetIdDsaWithSha3_224;
    class property IdDsaWithSha3_256: IDerObjectIdentifier
      read GetIdDsaWithSha3_256;
    class property IdDsaWithSha3_384: IDerObjectIdentifier
      read GetIdDsaWithSha3_384;
    class property IdDsaWithSha3_512: IDerObjectIdentifier
      read GetIdDsaWithSha3_512;

    class property IdECDsaWithSha3_224: IDerObjectIdentifier
      read GetIdECDsaWithSha3_224;
    class property IdECDsaWithSha3_256: IDerObjectIdentifier
      read GetIdECDsaWithSha3_256;
    class property IdECDsaWithSha3_384: IDerObjectIdentifier
      read GetIdECDsaWithSha3_384;
    class property IdECDsaWithSha3_512: IDerObjectIdentifier
      read GetIdECDsaWithSha3_512;

    class procedure Boot(); static;

  end;

implementation

{ TNistObjectIdentifiers }

class function TNistObjectIdentifiers.GetAES: IDerObjectIdentifier;
begin
  result := FAES;
end;

class function TNistObjectIdentifiers.GetDsaWithSha224: IDerObjectIdentifier;
begin
  result := FDsaWithSha224;
end;

class function TNistObjectIdentifiers.GetDsaWithSha256: IDerObjectIdentifier;
begin
  result := FDsaWithSha256;
end;

class function TNistObjectIdentifiers.GetDsaWithSha384: IDerObjectIdentifier;
begin
  result := FDsaWithSha384;
end;

class function TNistObjectIdentifiers.GetDsaWithSha512: IDerObjectIdentifier;
begin
  result := FDsaWithSha512;
end;

class function TNistObjectIdentifiers.GetHashAlgs: IDerObjectIdentifier;
begin
  result := FHashAlgs;
end;

class function TNistObjectIdentifiers.GetIdDsaWithSha2: IDerObjectIdentifier;
begin
  result := FIdDsaWithSha2;
end;

class function TNistObjectIdentifiers.GetIdAes128Cbc: IDerObjectIdentifier;
begin
  result := FIdAes128Cbc;
end;

class function TNistObjectIdentifiers.GetIdAes128Cfb: IDerObjectIdentifier;
begin
  result := FIdAes128Cfb;
end;

class function TNistObjectIdentifiers.GetIdAes128Ecb: IDerObjectIdentifier;
begin
  result := FIdAES128Ecb;
end;

class function TNistObjectIdentifiers.GetIdAes128Ofb: IDerObjectIdentifier;
begin
  result := FIdAes128Ofb;
end;

class function TNistObjectIdentifiers.GetIdAes192Cbc: IDerObjectIdentifier;
begin
  result := FIdAes192Cbc;
end;

class function TNistObjectIdentifiers.GetIdAes192Cfb: IDerObjectIdentifier;
begin
  result := FIdAes192Cfb;
end;

class function TNistObjectIdentifiers.GetIdAes192Ecb: IDerObjectIdentifier;
begin
  result := FidAes192Ecb;
end;

class function TNistObjectIdentifiers.GetIdAes192Ofb: IDerObjectIdentifier;
begin
  result := FIdAes192Ofb;
end;

class function TNistObjectIdentifiers.GetIdAes256Cbc: IDerObjectIdentifier;
begin
  result := FIdAes256Cbc;
end;

class function TNistObjectIdentifiers.GetIdAes256Cfb: IDerObjectIdentifier;
begin
  result := FIdAes256Cfb;
end;

class function TNistObjectIdentifiers.GetIdAes256Ecb: IDerObjectIdentifier;
begin
  result := FIdAes256Ecb;
end;

class function TNistObjectIdentifiers.GetIdAes256Ofb: IDerObjectIdentifier;
begin
  result := FIdAes256Ofb;
end;

class function TNistObjectIdentifiers.GetIdHMacWithSha3_224
  : IDerObjectIdentifier;
begin
  result := FIdHMacWithSha3_224;
end;

class function TNistObjectIdentifiers.GetIdHMacWithSha3_256
  : IDerObjectIdentifier;
begin
  result := FIdHMacWithSha3_256;
end;

class function TNistObjectIdentifiers.GetIdHMacWithSha3_384
  : IDerObjectIdentifier;
begin
  result := FIdHMacWithSha3_384;
end;

class function TNistObjectIdentifiers.GetIdHMacWithSha3_512
  : IDerObjectIdentifier;
begin
  result := FIdHMacWithSha3_512;
end;

class function TNistObjectIdentifiers.GetIdSha224: IDerObjectIdentifier;
begin
  result := FIdSha224;
end;

class function TNistObjectIdentifiers.GetIdSha256: IDerObjectIdentifier;
begin
  result := FIdSha256;
end;

class function TNistObjectIdentifiers.GetIdSha384: IDerObjectIdentifier;
begin
  result := FIdSha384;
end;

class function TNistObjectIdentifiers.GetIdSha3_224: IDerObjectIdentifier;
begin
  result := FIdSha3_224;
end;

class function TNistObjectIdentifiers.GetIdSha3_256: IDerObjectIdentifier;
begin
  result := FIdSha3_256;
end;

class function TNistObjectIdentifiers.GetIdSha3_384: IDerObjectIdentifier;
begin
  result := FIdSha3_384;
end;

class function TNistObjectIdentifiers.GetIdSha3_512: IDerObjectIdentifier;
begin
  result := FIdSha3_512;
end;

class function TNistObjectIdentifiers.GetIdSha512: IDerObjectIdentifier;
begin
  result := FIdSha512;
end;

class function TNistObjectIdentifiers.GetIdSha512_224: IDerObjectIdentifier;
begin
  result := FIdSha512_224;
end;

class function TNistObjectIdentifiers.GetIdSha512_256: IDerObjectIdentifier;
begin
  result := FIdSha512_256;
end;

class function TNistObjectIdentifiers.GetIdDsaWithSha3_224
  : IDerObjectIdentifier;
begin
  result := FIdDsaWithSha3_224;
end;

class function TNistObjectIdentifiers.GetIdDsaWithSha3_256
  : IDerObjectIdentifier;
begin
  result := FIdDsaWithSha3_256;
end;

class function TNistObjectIdentifiers.GetIdDsaWithSha3_384
  : IDerObjectIdentifier;
begin
  result := FIdDsaWithSha3_384;
end;

class function TNistObjectIdentifiers.GetIdDsaWithSha3_512
  : IDerObjectIdentifier;
begin
  result := FIdDsaWithSha3_512;
end;

class function TNistObjectIdentifiers.GetIdECDsaWithSha3_224
  : IDerObjectIdentifier;
begin
  result := FIdECDsaWithSha3_224;
end;

class function TNistObjectIdentifiers.GetIdECDsaWithSha3_256
  : IDerObjectIdentifier;
begin
  result := FIdECDsaWithSha3_256;
end;

class function TNistObjectIdentifiers.GetIdECDsaWithSha3_384
  : IDerObjectIdentifier;
begin
  result := FIdECDsaWithSha3_384;
end;

class function TNistObjectIdentifiers.GetIdECDsaWithSha3_512
  : IDerObjectIdentifier;
begin
  result := FIdECDsaWithSha3_512;
end;

class function TNistObjectIdentifiers.GetNistAlgorithm: IDerObjectIdentifier;
begin
  result := FNistAlgorithm;
end;

class function TNistObjectIdentifiers.GetSigAlgs: IDerObjectIdentifier;
begin
  result := FSigAlgs;
end;

class constructor TNistObjectIdentifiers.NistObjectIdentifiers;
begin
  TNistObjectIdentifiers.Boot;
end;

class procedure TNistObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    FNistAlgorithm := TDerObjectIdentifier.Create('2.16.840.1.101.3.4');
    FHashAlgs := NistAlgorithm.Branch('2');

    FIdSha256 := HashAlgs.Branch('1');
    FIdSha384 := HashAlgs.Branch('2');
    FIdSha512 := HashAlgs.Branch('3');
    FIdSha224 := HashAlgs.Branch('4');
    FIdSha512_224 := HashAlgs.Branch('5');
    FIdSha512_256 := HashAlgs.Branch('6');
    FIdSha3_224 := HashAlgs.Branch('7');
    FIdSha3_256 := HashAlgs.Branch('8');
    FIdSha3_384 := HashAlgs.Branch('9');
    FIdSha3_512 := HashAlgs.Branch('10');

    FIdHMacWithSha3_224 := HashAlgs.Branch('13');
    FIdHMacWithSha3_256 := HashAlgs.Branch('14');
    FIdHMacWithSha3_384 := HashAlgs.Branch('15');
    FIdHMacWithSha3_512 := HashAlgs.Branch('16');

    FAES := TDerObjectIdentifier.Create(NistAlgorithm.id + '.1');

    FIdAES128Ecb := TDerObjectIdentifier.Create(AES.id + '.1');
    FIdAes128Cbc := TDerObjectIdentifier.Create(AES.id + '.2');
    FIdAes128Ofb := TDerObjectIdentifier.Create(AES.id + '.3');
    FIdAes128Cfb := TDerObjectIdentifier.Create(AES.id + '.4');
    FidAes192Ecb := TDerObjectIdentifier.Create(AES.id + '.21');
    FIdAes192Cbc := TDerObjectIdentifier.Create(AES.id + '.22');
    FIdAes192Ofb := TDerObjectIdentifier.Create(AES.id + '.23');
    FIdAes192Cfb := TDerObjectIdentifier.Create(AES.id + '.24');
    FIdAes256Ecb := TDerObjectIdentifier.Create(AES.id + '.41');
    FIdAes256Cbc := TDerObjectIdentifier.Create(AES.id + '.42');
    FIdAes256Ofb := TDerObjectIdentifier.Create(AES.id + '.43');
    FIdAes256Cfb := TDerObjectIdentifier.Create(AES.id + '.44');

    //
    // signatures
    //
    FSigAlgs := NistAlgorithm.Branch('3');
    FIdDsaWithSha2 := SigAlgs;

    FDsaWithSha224 := TDerObjectIdentifier.Create(SigAlgs.id + '.1');
    FDsaWithSha256 := TDerObjectIdentifier.Create(SigAlgs.id + '.2');
    FDsaWithSha384 := TDerObjectIdentifier.Create(SigAlgs.id + '.3');
    FDsaWithSha512 := TDerObjectIdentifier.Create(SigAlgs.id + '.4');

    FIdDsaWithSha3_224 := TDerObjectIdentifier.Create(SigAlgs.id + '.5');
    FIdDsaWithSha3_256 := TDerObjectIdentifier.Create(SigAlgs.id + '.6');
    FIdDsaWithSha3_384 := TDerObjectIdentifier.Create(SigAlgs.id + '.7');
    FIdDsaWithSha3_512 := TDerObjectIdentifier.Create(SigAlgs.id + '.8');

    // ECDSA with SHA-3
    FIdECDsaWithSha3_224 := TDerObjectIdentifier.Create(SigAlgs.id + '.9');
    FIdECDsaWithSha3_256 := TDerObjectIdentifier.Create(SigAlgs.id + '.10');
    FIdECDsaWithSha3_384 := TDerObjectIdentifier.Create(SigAlgs.id + '.11');
    FIdECDsaWithSha3_512 := TDerObjectIdentifier.Create(SigAlgs.id + '.12');

    FIsBooted := True;
  end;

end;

end.
