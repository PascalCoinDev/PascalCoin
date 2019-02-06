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

unit ClpEacObjectIdentifiers;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TEacObjectIdentifiers = class abstract(TObject)

  strict private

  class var

    FIsBooted: Boolean;
    Fbsi_de, Fid_TA, Fid_TA_ECDSA, Fid_TA_ECDSA_SHA_1, Fid_TA_ECDSA_SHA_224,
      Fid_TA_ECDSA_SHA_256, Fid_TA_ECDSA_SHA_384, Fid_TA_ECDSA_SHA_512
      : IDerObjectIdentifier;

    class function Getbsi_de: IDerObjectIdentifier; static; inline;
    class function Getid_TA: IDerObjectIdentifier; static; inline;
    class function Getid_TA_ECDSA: IDerObjectIdentifier; static; inline;
    class function Getid_TA_ECDSA_SHA_1: IDerObjectIdentifier; static; inline;
    class function Getid_TA_ECDSA_SHA_224: IDerObjectIdentifier; static; inline;
    class function Getid_TA_ECDSA_SHA_256: IDerObjectIdentifier; static; inline;
    class function Getid_TA_ECDSA_SHA_384: IDerObjectIdentifier; static; inline;
    class function Getid_TA_ECDSA_SHA_512: IDerObjectIdentifier; static; inline;

    class constructor EacObjectIdentifiers();

  public
    // bsi-de OBJECT IDENTIFIER ::= {
    // itu-t(0) identified-organization(4) etsi(0)
    // reserved(127) etsi-identified-organization(0) 7
    // }
    class property bsi_de: IDerObjectIdentifier read Getbsi_de;
    //
    // id-TA OBJECT IDENTIFIER ::= {
    // bsi-de protocols(2) smartcard(2) 2
    // }
    class property id_TA: IDerObjectIdentifier read Getid_TA;
    class property id_TA_ECDSA: IDerObjectIdentifier read Getid_TA_ECDSA;
    class property id_TA_ECDSA_SHA_1: IDerObjectIdentifier
      read Getid_TA_ECDSA_SHA_1;
    class property id_TA_ECDSA_SHA_224: IDerObjectIdentifier
      read Getid_TA_ECDSA_SHA_224;
    class property id_TA_ECDSA_SHA_256: IDerObjectIdentifier
      read Getid_TA_ECDSA_SHA_256;
    class property id_TA_ECDSA_SHA_384: IDerObjectIdentifier
      read Getid_TA_ECDSA_SHA_384;
    class property id_TA_ECDSA_SHA_512: IDerObjectIdentifier
      read Getid_TA_ECDSA_SHA_512;
    class procedure Boot(); static;

  end;

implementation

{ TEacObjectIdentifiers }

class procedure TEacObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    Fbsi_de := TDerObjectIdentifier.Create('0.4.0.127.0.7');
    Fid_TA := TDerObjectIdentifier.Create(Fbsi_de.ID + '.2.2.2');
    Fid_TA_ECDSA := TDerObjectIdentifier.Create(Fid_TA.ID + '.2');
    Fid_TA_ECDSA_SHA_1 := TDerObjectIdentifier.Create(Fid_TA_ECDSA.ID + '.1');
    Fid_TA_ECDSA_SHA_224 := TDerObjectIdentifier.Create(Fid_TA_ECDSA.ID + '.2');
    Fid_TA_ECDSA_SHA_256 := TDerObjectIdentifier.Create(Fid_TA_ECDSA.ID + '.3');
    Fid_TA_ECDSA_SHA_384 := TDerObjectIdentifier.Create(Fid_TA_ECDSA.ID + '.4');
    Fid_TA_ECDSA_SHA_512 := TDerObjectIdentifier.Create(Fid_TA_ECDSA.ID + '.5');

    FIsBooted := True;
  end;
end;

class constructor TEacObjectIdentifiers.EacObjectIdentifiers;
begin
  TEacObjectIdentifiers.Boot;
end;

class function TEacObjectIdentifiers.Getbsi_de: IDerObjectIdentifier;
begin
  result := Fbsi_de;
end;

class function TEacObjectIdentifiers.Getid_TA: IDerObjectIdentifier;
begin
  result := Fid_TA;
end;

class function TEacObjectIdentifiers.Getid_TA_ECDSA: IDerObjectIdentifier;
begin
  result := Fid_TA_ECDSA;
end;

class function TEacObjectIdentifiers.Getid_TA_ECDSA_SHA_1: IDerObjectIdentifier;
begin
  result := Fid_TA_ECDSA_SHA_1;
end;

class function TEacObjectIdentifiers.Getid_TA_ECDSA_SHA_224
  : IDerObjectIdentifier;
begin
  result := Fid_TA_ECDSA_SHA_224;
end;

class function TEacObjectIdentifiers.Getid_TA_ECDSA_SHA_256
  : IDerObjectIdentifier;
begin
  result := Fid_TA_ECDSA_SHA_256;
end;

class function TEacObjectIdentifiers.Getid_TA_ECDSA_SHA_384
  : IDerObjectIdentifier;
begin
  result := Fid_TA_ECDSA_SHA_384;
end;

class function TEacObjectIdentifiers.Getid_TA_ECDSA_SHA_512
  : IDerObjectIdentifier;
begin
  result := Fid_TA_ECDSA_SHA_512;
end;

end.
