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

unit ClpBsiObjectIdentifiers;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TBsiObjectIdentifiers = class abstract(TObject)

  strict private

    /// <remarks>See https://www.bsi.bund.de/cae/servlet/contentblob/471398/publicationFile/30615/BSI-TR-03111_pdf.pdf</remarks>
  class var

    FIsBooted: Boolean;
    Fbsi_de, Fid_ecc, Fecdsa_plain_signatures, Fecdsa_plain_SHA1,
      Fecdsa_plain_SHA224, Fecdsa_plain_SHA256, Fecdsa_plain_SHA384,
      Fecdsa_plain_SHA512, Fecdsa_plain_RIPEMD160: IDerObjectIdentifier;

    class function Getbsi_de: IDerObjectIdentifier; static; inline;
    class function Getecdsa_plain_RIPEMD160: IDerObjectIdentifier;
      static; inline;
    class function Getecdsa_plain_SHA1: IDerObjectIdentifier; static; inline;
    class function Getecdsa_plain_SHA224: IDerObjectIdentifier; static; inline;
    class function Getecdsa_plain_SHA256: IDerObjectIdentifier; static; inline;
    class function Getecdsa_plain_SHA384: IDerObjectIdentifier; static; inline;
    class function Getecdsa_plain_SHA512: IDerObjectIdentifier; static; inline;
    class function Getecdsa_plain_signatures: IDerObjectIdentifier;
      static; inline;
    class function Getid_ecc: IDerObjectIdentifier; static; inline;

    class constructor BsiObjectIdentifiers();

  public

    class property bsi_de: IDerObjectIdentifier read Getbsi_de;
    class property id_ecc: IDerObjectIdentifier read Getid_ecc;
    class property ecdsa_plain_signatures: IDerObjectIdentifier
      read Getecdsa_plain_signatures;
    class property ecdsa_plain_SHA1: IDerObjectIdentifier
      read Getecdsa_plain_SHA1;
    class property ecdsa_plain_SHA224: IDerObjectIdentifier
      read Getecdsa_plain_SHA224;
    class property ecdsa_plain_SHA256: IDerObjectIdentifier
      read Getecdsa_plain_SHA256;
    class property ecdsa_plain_SHA384: IDerObjectIdentifier
      read Getecdsa_plain_SHA384;
    class property ecdsa_plain_SHA512: IDerObjectIdentifier
      read Getecdsa_plain_SHA512;
    class property ecdsa_plain_RIPEMD160: IDerObjectIdentifier
      read Getecdsa_plain_RIPEMD160;

    class procedure Boot(); static;

  end;

implementation

{ TBsiObjectIdentifiers }

class procedure TBsiObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    Fbsi_de := TDerObjectIdentifier.Create('0.4.0.127.0.7');

    // /* 0.4.0.127.0.7.1.1 */
    Fid_ecc := Fbsi_de.Branch('1.1');

    // /* 0.4.0.127.0.7.1.1.4.1 */
    Fecdsa_plain_signatures := Fid_ecc.Branch('4.1');

    // /* 0.4.0.127.0.7.1.1.4.1.1 */
    Fecdsa_plain_SHA1 := Fecdsa_plain_signatures.Branch('1');

    // /* 0.4.0.127.0.7.1.1.4.1.2 */
    Fecdsa_plain_SHA224 := Fecdsa_plain_signatures.Branch('2');

    // /* 0.4.0.127.0.7.1.1.4.1.3 */
    Fecdsa_plain_SHA256 := Fecdsa_plain_signatures.Branch('3');

    // /* 0.4.0.127.0.7.1.1.4.1.4 */
    Fecdsa_plain_SHA384 := Fecdsa_plain_signatures.Branch('4');

    // /* 0.4.0.127.0.7.1.1.4.1.5 */
    Fecdsa_plain_SHA512 := Fecdsa_plain_signatures.Branch('5');

    // /* 0.4.0.127.0.7.1.1.4.1.6 */
    Fecdsa_plain_RIPEMD160 := Fecdsa_plain_signatures.Branch('6');

    FIsBooted := True;
  end;
end;

class constructor TBsiObjectIdentifiers.BsiObjectIdentifiers;
begin
  TBsiObjectIdentifiers.Boot;
end;

class function TBsiObjectIdentifiers.Getbsi_de: IDerObjectIdentifier;
begin
  result := Fbsi_de;
end;

class function TBsiObjectIdentifiers.Getecdsa_plain_RIPEMD160
  : IDerObjectIdentifier;
begin
  result := Fecdsa_plain_RIPEMD160;
end;

class function TBsiObjectIdentifiers.Getecdsa_plain_SHA1: IDerObjectIdentifier;
begin
  result := Fecdsa_plain_SHA1;
end;

class function TBsiObjectIdentifiers.Getecdsa_plain_SHA224
  : IDerObjectIdentifier;
begin
  result := Fecdsa_plain_SHA224;
end;

class function TBsiObjectIdentifiers.Getecdsa_plain_SHA256
  : IDerObjectIdentifier;
begin
  result := Fecdsa_plain_SHA256;
end;

class function TBsiObjectIdentifiers.Getecdsa_plain_SHA384
  : IDerObjectIdentifier;
begin
  result := Fecdsa_plain_SHA384;
end;

class function TBsiObjectIdentifiers.Getecdsa_plain_SHA512
  : IDerObjectIdentifier;
begin
  result := Fecdsa_plain_SHA512;
end;

class function TBsiObjectIdentifiers.Getecdsa_plain_signatures
  : IDerObjectIdentifier;
begin
  result := Fecdsa_plain_signatures;
end;

class function TBsiObjectIdentifiers.Getid_ecc: IDerObjectIdentifier;
begin
  result := Fid_ecc;
end;

end.
