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

unit ClpMiscObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TMiscObjectIdentifiers = class abstract(TObject)

  strict private

  class var

    FIsBooted: Boolean;
    Fcryptlib, Fcryptlib_algorithm, Fcryptlib_algorithm_blowfish_ECB,
      Fcryptlib_algorithm_blowfish_CBC, Fcryptlib_algorithm_blowfish_CFB,
      Fcryptlib_algorithm_blowfish_OFB, Fblake2, Fid_blake2b160, Fid_blake2b256,
      Fid_blake2b384, Fid_blake2b512, Fid_blake2s128, Fid_blake2s160,
      Fid_blake2s224, Fid_blake2s256: IDerObjectIdentifier;

    class function Getcryptlib: IDerObjectIdentifier; static; inline;

    class function Getcryptlib_algorithm: IDerObjectIdentifier; static; inline;
    class function Getcryptlib_algorithm_blowfish_ECB: IDerObjectIdentifier;
      static; inline;
    class function Getcryptlib_algorithm_blowfish_CBC: IDerObjectIdentifier;
      static; inline;
    class function Getcryptlib_algorithm_blowfish_CFB: IDerObjectIdentifier;
      static; inline;
    class function Getcryptlib_algorithm_blowfish_OFB: IDerObjectIdentifier;
      static; inline;

    class function Getblake2: IDerObjectIdentifier; static; inline;

    class function Getid_blake2b160: IDerObjectIdentifier; static; inline;
    class function Getid_blake2b256: IDerObjectIdentifier; static; inline;
    class function Getid_blake2b384: IDerObjectIdentifier; static; inline;
    class function Getid_blake2b512: IDerObjectIdentifier; static; inline;

    class function Getid_blake2s128: IDerObjectIdentifier; static; inline;
    class function Getid_blake2s160: IDerObjectIdentifier; static; inline;
    class function Getid_blake2s224: IDerObjectIdentifier; static; inline;
    class function Getid_blake2s256: IDerObjectIdentifier; static; inline;

    class constructor MiscObjectIdentifiers();

  public

    class property cryptlib: IDerObjectIdentifier read Getcryptlib;

    class property cryptlib_algorithm: IDerObjectIdentifier
      read Getcryptlib_algorithm;
    class property cryptlib_algorithm_blowfish_ECB: IDerObjectIdentifier
      read Getcryptlib_algorithm_blowfish_ECB;
    class property cryptlib_algorithm_blowfish_CBC: IDerObjectIdentifier
      read Getcryptlib_algorithm_blowfish_CBC;
    class property cryptlib_algorithm_blowfish_CFB: IDerObjectIdentifier
      read Getcryptlib_algorithm_blowfish_CFB;
    class property cryptlib_algorithm_blowfish_OFB: IDerObjectIdentifier
      read Getcryptlib_algorithm_blowfish_OFB;

    class property blake2: IDerObjectIdentifier read Getblake2;

    class property id_blake2b160: IDerObjectIdentifier read Getid_blake2b160;
    class property id_blake2b256: IDerObjectIdentifier read Getid_blake2b256;
    class property id_blake2b384: IDerObjectIdentifier read Getid_blake2b384;
    class property id_blake2b512: IDerObjectIdentifier read Getid_blake2b512;

    class property id_blake2s128: IDerObjectIdentifier read Getid_blake2s128;
    class property id_blake2s160: IDerObjectIdentifier read Getid_blake2s160;
    class property id_blake2s224: IDerObjectIdentifier read Getid_blake2s224;
    class property id_blake2s256: IDerObjectIdentifier read Getid_blake2s256;

    class procedure Boot(); static;

  end;

implementation

{ TMiscObjectIdentifiers }

class function TMiscObjectIdentifiers.Getblake2: IDerObjectIdentifier;
begin
  result := Fblake2;
end;

class function TMiscObjectIdentifiers.Getcryptlib: IDerObjectIdentifier;
begin
  result := Fcryptlib;
end;

class function TMiscObjectIdentifiers.Getcryptlib_algorithm
  : IDerObjectIdentifier;
begin
  result := Fcryptlib_algorithm;
end;

class function TMiscObjectIdentifiers.Getcryptlib_algorithm_blowfish_CBC
  : IDerObjectIdentifier;
begin
  result := Fcryptlib_algorithm_blowfish_CBC;
end;

class function TMiscObjectIdentifiers.Getcryptlib_algorithm_blowfish_CFB
  : IDerObjectIdentifier;
begin
  result := Fcryptlib_algorithm_blowfish_CFB;
end;

class function TMiscObjectIdentifiers.Getcryptlib_algorithm_blowfish_ECB
  : IDerObjectIdentifier;
begin
  result := Fcryptlib_algorithm_blowfish_ECB;
end;

class function TMiscObjectIdentifiers.Getcryptlib_algorithm_blowfish_OFB
  : IDerObjectIdentifier;
begin
  result := Fcryptlib_algorithm_blowfish_OFB;
end;

class function TMiscObjectIdentifiers.Getid_blake2b160: IDerObjectIdentifier;
begin
  result := Fid_blake2b160;
end;

class function TMiscObjectIdentifiers.Getid_blake2b256: IDerObjectIdentifier;
begin
  result := Fid_blake2b256;
end;

class function TMiscObjectIdentifiers.Getid_blake2b384: IDerObjectIdentifier;
begin
  result := Fid_blake2b384;
end;

class function TMiscObjectIdentifiers.Getid_blake2b512: IDerObjectIdentifier;
begin
  result := Fid_blake2b512;
end;

class function TMiscObjectIdentifiers.Getid_blake2s128: IDerObjectIdentifier;
begin
  result := Fid_blake2s128;
end;

class function TMiscObjectIdentifiers.Getid_blake2s160: IDerObjectIdentifier;
begin
  result := Fid_blake2s160;
end;

class function TMiscObjectIdentifiers.Getid_blake2s224: IDerObjectIdentifier;
begin
  result := Fid_blake2s224;
end;

class function TMiscObjectIdentifiers.Getid_blake2s256: IDerObjectIdentifier;
begin
  result := Fid_blake2s256;
end;

class procedure TMiscObjectIdentifiers.Boot;
begin

  if not FIsBooted then
  begin

    //
    // Peter Gutmann's Cryptlib
    //

    Fcryptlib := TDerObjectIdentifier.Create('1.3.6.1.4.1.3029');

    Fcryptlib_algorithm := cryptlib.Branch('1');
    Fcryptlib_algorithm_blowfish_ECB := cryptlib_algorithm.Branch('1.1');
    Fcryptlib_algorithm_blowfish_CBC := cryptlib_algorithm.Branch('1.2');
    Fcryptlib_algorithm_blowfish_CFB := cryptlib_algorithm.Branch('1.3');
    Fcryptlib_algorithm_blowfish_OFB := cryptlib_algorithm.Branch('1.4');

    //
    // Blake2b and Blake2s
    //
    Fblake2 := TDerObjectIdentifier.Create('1.3.6.1.4.1.1722.12.2');

    Fid_blake2b160 := blake2.Branch('1.5');
    Fid_blake2b256 := blake2.Branch('1.8');
    Fid_blake2b384 := blake2.Branch('1.12');
    Fid_blake2b512 := blake2.Branch('1.16');

    Fid_blake2s128 := blake2.Branch('2.4');
    Fid_blake2s160 := blake2.Branch('2.5');
    Fid_blake2s224 := blake2.Branch('2.7');
    Fid_blake2s256 := blake2.Branch('2.8');

    FIsBooted := True;
  end;
end;

class constructor TMiscObjectIdentifiers.MiscObjectIdentifiers;
begin
  TMiscObjectIdentifiers.Boot;
end;

end.
