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

unit ClpEdECObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type

  /// <summary>
  /// Edwards Elliptic Curve Object Identifiers (RFC 8410)
  /// </summary>
  TEdECObjectIdentifiers = class abstract(TObject)

  strict private

  class var

    FIsBooted: Boolean;
    Fid_edwards_curve_algs, Fid_X25519, Fid_X448, Fid_Ed25519,
      Fid_Ed448: IDerObjectIdentifier;

    class constructor EdECObjectIdentifiers();
  private
    class function Getid_Ed25519: IDerObjectIdentifier; static; inline;
    class function Getid_Ed448: IDerObjectIdentifier; static; inline;
    class function Getid_edwards_curve_algs: IDerObjectIdentifier;
      static; inline;
    class function Getid_X25519: IDerObjectIdentifier; static; inline;
    class function Getid_X448: IDerObjectIdentifier; static; inline;

  public

    class property id_edwards_curve_algs: IDerObjectIdentifier
      read Getid_edwards_curve_algs;
    class property id_X25519: IDerObjectIdentifier read Getid_X25519;
    class property id_X448: IDerObjectIdentifier read Getid_X448;
    class property id_Ed25519: IDerObjectIdentifier read Getid_Ed25519;
    class property id_Ed448: IDerObjectIdentifier read Getid_Ed448;

    class procedure Boot(); static;
  end;

implementation

{ TEdECObjectIdentifiers }

class procedure TEdECObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    Fid_edwards_curve_algs := TDerObjectIdentifier.Create('1.3.101');

    Fid_X25519 := id_edwards_curve_algs.Branch('110');
    Fid_X448 := id_edwards_curve_algs.Branch('111');
    Fid_Ed25519 := id_edwards_curve_algs.Branch('112');
    Fid_Ed448 := id_edwards_curve_algs.Branch('113');

    FIsBooted := True;
  end;
end;

class constructor TEdECObjectIdentifiers.EdECObjectIdentifiers;
begin
  TEdECObjectIdentifiers.Boot();
end;

class function TEdECObjectIdentifiers.Getid_Ed25519: IDerObjectIdentifier;
begin
  result := Fid_Ed25519;
end;

class function TEdECObjectIdentifiers.Getid_Ed448: IDerObjectIdentifier;
begin
  result := Fid_Ed448;
end;

class function TEdECObjectIdentifiers.Getid_edwards_curve_algs
  : IDerObjectIdentifier;
begin
  result := Fid_edwards_curve_algs;
end;

class function TEdECObjectIdentifiers.Getid_X25519: IDerObjectIdentifier;
begin
  result := Fid_X25519;
end;

class function TEdECObjectIdentifiers.Getid_X448: IDerObjectIdentifier;
begin
  result := Fid_X448;
end;

end.
