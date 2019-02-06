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

unit ClpIanaObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TIanaObjectIdentifiers = class abstract(TObject)

  strict private

    // id-SHA1 OBJECT IDENTIFIER ::=
    // {iso(1) identified-organization(3) dod(6) internet(1) security(5) mechanisms(5) ipsec(8) isakmpOakley(1)}
    //

  class var
    FIsBooted: Boolean;
    FIsakmpOakley, FHmacMD5, FHmacSha1, FHmacTiger, FHmacRipeMD160
      : IDerObjectIdentifier;

    class function GetIsakmpOakley: IDerObjectIdentifier; static; inline;
    class function GetHmacMD5: IDerObjectIdentifier; static; inline;
    class function GetHmacSha1: IDerObjectIdentifier; static; inline;

    class function GetHmacTiger: IDerObjectIdentifier; static; inline;
    class function GetHmacRipeMD160: IDerObjectIdentifier; static; inline;

    class constructor IanaObjectIdentifiers();

  public

    class property IsakmpOakley: IDerObjectIdentifier read GetIsakmpOakley;

    class property HmacMD5: IDerObjectIdentifier read GetHmacMD5;

    class property HmacSha1: IDerObjectIdentifier read GetHmacSha1;

    class property HmacTiger: IDerObjectIdentifier read GetHmacTiger;

    class property HmacRipeMD160: IDerObjectIdentifier read GetHmacRipeMD160;

    class procedure Boot(); static;

  end;

implementation

{ TIanaObjectIdentifiers }

class function TIanaObjectIdentifiers.GetHmacMD5: IDerObjectIdentifier;
begin
  result := FHmacMD5;
end;

class function TIanaObjectIdentifiers.GetHmacRipeMD160: IDerObjectIdentifier;
begin
  result := FHmacRipeMD160;
end;

class function TIanaObjectIdentifiers.GetHmacSha1: IDerObjectIdentifier;
begin
  result := FHmacSha1;
end;

class function TIanaObjectIdentifiers.GetHmacTiger: IDerObjectIdentifier;
begin
  result := FHmacTiger;
end;

class function TIanaObjectIdentifiers.GetIsakmpOakley: IDerObjectIdentifier;
begin
  result := FIsakmpOakley;
end;

class procedure TIanaObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin

    FIsakmpOakley := TDerObjectIdentifier.Create('1.3.6.1.5.5.8.1');

    FHmacMD5 := TDerObjectIdentifier.Create(IsakmpOakley.ID + '.1');
    FHmacSha1 := TDerObjectIdentifier.Create(IsakmpOakley.ID + '.2');

    FHmacTiger := TDerObjectIdentifier.Create(IsakmpOakley.ID + '.3');

    FHmacRipeMD160 := TDerObjectIdentifier.Create(IsakmpOakley.ID + '.4');

    FIsBooted := True;
  end;
end;

class constructor TIanaObjectIdentifiers.IanaObjectIdentifiers;
begin
  TIanaObjectIdentifiers.Boot;
end;

end.
