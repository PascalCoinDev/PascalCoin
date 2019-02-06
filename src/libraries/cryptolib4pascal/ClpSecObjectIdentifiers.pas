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

unit ClpSecObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects,
  ClpX9ObjectIdentifiers;

type
  // /**
  // *  EllipticCurve OBJECT IDENTIFIER ::= {
  // *        iso(1) identified-organization(3) certicom(132) curve(0)
  // *  }
  // */
  TSecObjectIdentifiers = class abstract(TObject)

  strict private

  class var

    FIsBooted: Boolean;
    FSecT193r1, FSecP112r2, FEllipticCurve, FSecP224r1, FSecP160r2, FSecT233r1,
      FSecP112r1, FSecT409r1, FSecP192k1, FSecT131r2, FSecT283k1, FSecT113r2,
      FSecT163r2, FSecP160r1, FSecT239k1, FSecP521r1, FSecP256r1, FSecT131r1,
      FSecT113r1, FSecT163r1, FSecP224k1, FSecT233k1, FSecT409k1, FSecP160k1,
      FSecT571r1, FSecP256k1, FSecT163k1, FSecP128r2, FSecT571k1, FSecP128r1,
      FSecP192r1, FSecT283r1, FSecT193r2, FSecP384r1: IDerObjectIdentifier;

    class constructor SecObjectIdentifiers();

    class function GetEllipticCurve: IDerObjectIdentifier; static; inline;
    class function GetSecP112r1: IDerObjectIdentifier; static; inline;
    class function GetSecP112r2: IDerObjectIdentifier; static; inline;
    class function GetSecP128r1: IDerObjectIdentifier; static; inline;
    class function GetSecP128r2: IDerObjectIdentifier; static; inline;
    class function GetSecP160k1: IDerObjectIdentifier; static; inline;
    class function GetSecP160r1: IDerObjectIdentifier; static; inline;
    class function GetSecP160r2: IDerObjectIdentifier; static; inline;
    class function GetSecP192k1: IDerObjectIdentifier; static; inline;
    class function GetSecP192r1: IDerObjectIdentifier; static; inline;
    class function GetSecP224k1: IDerObjectIdentifier; static; inline;
    class function GetSecP224r1: IDerObjectIdentifier; static; inline;
    class function GetSecP256k1: IDerObjectIdentifier; static; inline;
    class function GetSecP256r1: IDerObjectIdentifier; static; inline;
    class function GetSecP384r1: IDerObjectIdentifier; static; inline;
    class function GetSecP521r1: IDerObjectIdentifier; static; inline;
    class function GetSecT113r1: IDerObjectIdentifier; static; inline;
    class function GetSecT113r2: IDerObjectIdentifier; static; inline;
    class function GetSecT131r1: IDerObjectIdentifier; static; inline;
    class function GetSecT131r2: IDerObjectIdentifier; static; inline;
    class function GetSecT163k1: IDerObjectIdentifier; static; inline;
    class function GetSecT163r1: IDerObjectIdentifier; static; inline;
    class function GetSecT163r2: IDerObjectIdentifier; static; inline;
    class function GetSecT193r1: IDerObjectIdentifier; static; inline;
    class function GetSecT193r2: IDerObjectIdentifier; static; inline;
    class function GetSecT233k1: IDerObjectIdentifier; static; inline;
    class function GetSecT233r1: IDerObjectIdentifier; static; inline;
    class function GetSecT239k1: IDerObjectIdentifier; static; inline;
    class function GetSecT283k1: IDerObjectIdentifier; static; inline;
    class function GetSecT283r1: IDerObjectIdentifier; static; inline;
    class function GetSecT409k1: IDerObjectIdentifier; static; inline;
    class function GetSecT409r1: IDerObjectIdentifier; static; inline;
    class function GetSecT571k1: IDerObjectIdentifier; static; inline;
    class function GetSecT571r1: IDerObjectIdentifier; static; inline;

  public

    class property EllipticCurve: IDerObjectIdentifier read GetEllipticCurve;

    class property SecT163k1: IDerObjectIdentifier read GetSecT163k1;
    class property SecT163r1: IDerObjectIdentifier read GetSecT163r1;
    class property SecT239k1: IDerObjectIdentifier read GetSecT239k1;
    class property SecT113r1: IDerObjectIdentifier read GetSecT113r1;
    class property SecT113r2: IDerObjectIdentifier read GetSecT113r2;
    class property SecP112r1: IDerObjectIdentifier read GetSecP112r1;
    class property SecP112r2: IDerObjectIdentifier read GetSecP112r2;
    class property SecP160r1: IDerObjectIdentifier read GetSecP160r1;
    class property SecP160k1: IDerObjectIdentifier read GetSecP160k1;
    class property SecP256k1: IDerObjectIdentifier read GetSecP256k1;
    class property SecT163r2: IDerObjectIdentifier read GetSecT163r2;
    class property SecT283k1: IDerObjectIdentifier read GetSecT283k1;
    class property SecT283r1: IDerObjectIdentifier read GetSecT283r1;
    class property SecT131r1: IDerObjectIdentifier read GetSecT131r1;
    class property SecT131r2: IDerObjectIdentifier read GetSecT131r2;
    class property SecT193r1: IDerObjectIdentifier read GetSecT193r1;
    class property SecT193r2: IDerObjectIdentifier read GetSecT193r2;
    class property SecT233k1: IDerObjectIdentifier read GetSecT233k1;
    class property SecT233r1: IDerObjectIdentifier read GetSecT233r1;
    class property SecP128r1: IDerObjectIdentifier read GetSecP128r1;
    class property SecP128r2: IDerObjectIdentifier read GetSecP128r2;
    class property SecP160r2: IDerObjectIdentifier read GetSecP160r2;
    class property SecP192k1: IDerObjectIdentifier read GetSecP192k1;
    class property SecP224k1: IDerObjectIdentifier read GetSecP224k1;
    class property SecP224r1: IDerObjectIdentifier read GetSecP224r1;
    class property SecP384r1: IDerObjectIdentifier read GetSecP384r1;
    class property SecP521r1: IDerObjectIdentifier read GetSecP521r1;
    class property SecT409k1: IDerObjectIdentifier read GetSecT409k1;
    class property SecT409r1: IDerObjectIdentifier read GetSecT409r1;
    class property SecT571k1: IDerObjectIdentifier read GetSecT571k1;
    class property SecT571r1: IDerObjectIdentifier read GetSecT571r1;

    class property SecP192r1: IDerObjectIdentifier read GetSecP192r1;
    class property SecP256r1: IDerObjectIdentifier read GetSecP256r1;

    class procedure Boot(); static;

  end;

implementation

{ TClpSecObjectIdentifiers }

class function TSecObjectIdentifiers.GetEllipticCurve: IDerObjectIdentifier;
begin
  result := FEllipticCurve;
end;

class function TSecObjectIdentifiers.GetSecP112r1: IDerObjectIdentifier;
begin
  result := FSecP112r1;
end;

class function TSecObjectIdentifiers.GetSecP112r2: IDerObjectIdentifier;
begin
  result := FSecP112r2;
end;

class function TSecObjectIdentifiers.GetSecP128r1: IDerObjectIdentifier;
begin
  result := FSecP128r1;
end;

class function TSecObjectIdentifiers.GetSecP128r2: IDerObjectIdentifier;
begin
  result := FSecP128r2;
end;

class function TSecObjectIdentifiers.GetSecP160k1: IDerObjectIdentifier;
begin
  result := FSecP160k1;
end;

class function TSecObjectIdentifiers.GetSecP160r1: IDerObjectIdentifier;
begin
  result := FSecP160r1;
end;

class function TSecObjectIdentifiers.GetSecP160r2: IDerObjectIdentifier;
begin
  result := FSecP160r2;
end;

class function TSecObjectIdentifiers.GetSecP192k1: IDerObjectIdentifier;
begin
  result := FSecP192k1;
end;

class function TSecObjectIdentifiers.GetSecP192r1: IDerObjectIdentifier;
begin
  result := FSecP192r1;
end;

class function TSecObjectIdentifiers.GetSecP224k1: IDerObjectIdentifier;
begin
  result := FSecP224k1;
end;

class function TSecObjectIdentifiers.GetSecP224r1: IDerObjectIdentifier;
begin
  result := FSecP224r1;
end;

class function TSecObjectIdentifiers.GetSecP256k1: IDerObjectIdentifier;
begin
  result := FSecP256k1;
end;

class function TSecObjectIdentifiers.GetSecP256r1: IDerObjectIdentifier;
begin
  result := FSecP256r1;
end;

class function TSecObjectIdentifiers.GetSecP384r1: IDerObjectIdentifier;
begin
  result := FSecP384r1;
end;

class function TSecObjectIdentifiers.GetSecP521r1: IDerObjectIdentifier;
begin
  result := FSecP521r1;
end;

class function TSecObjectIdentifiers.GetSecT113r1: IDerObjectIdentifier;
begin
  result := FSecT113r1;
end;

class function TSecObjectIdentifiers.GetSecT113r2: IDerObjectIdentifier;
begin
  result := FSecT113r2;
end;

class function TSecObjectIdentifiers.GetSecT131r1: IDerObjectIdentifier;
begin
  result := FSecT131r1;
end;

class function TSecObjectIdentifiers.GetSecT131r2: IDerObjectIdentifier;
begin
  result := FSecT131r2;
end;

class function TSecObjectIdentifiers.GetSecT163k1: IDerObjectIdentifier;
begin
  result := FSecT163k1;
end;

class function TSecObjectIdentifiers.GetSecT163r1: IDerObjectIdentifier;
begin
  result := FSecT163r1;
end;

class function TSecObjectIdentifiers.GetSecT163r2: IDerObjectIdentifier;
begin
  result := FSecT163r2;
end;

class function TSecObjectIdentifiers.GetSecT193r1: IDerObjectIdentifier;
begin
  result := FSecT193r1;
end;

class function TSecObjectIdentifiers.GetSecT193r2: IDerObjectIdentifier;
begin
  result := FSecT193r2;
end;

class function TSecObjectIdentifiers.GetSecT233k1: IDerObjectIdentifier;
begin
  result := FSecT233k1;
end;

class function TSecObjectIdentifiers.GetSecT233r1: IDerObjectIdentifier;
begin
  result := FSecT233r1;
end;

class function TSecObjectIdentifiers.GetSecT239k1: IDerObjectIdentifier;
begin
  result := FSecT239k1;
end;

class function TSecObjectIdentifiers.GetSecT283k1: IDerObjectIdentifier;
begin
  result := FSecT283k1;
end;

class function TSecObjectIdentifiers.GetSecT283r1: IDerObjectIdentifier;
begin
  result := FSecT283r1;
end;

class function TSecObjectIdentifiers.GetSecT409k1: IDerObjectIdentifier;
begin
  result := FSecT409k1;
end;

class function TSecObjectIdentifiers.GetSecT409r1: IDerObjectIdentifier;
begin
  result := FSecT409r1;
end;

class function TSecObjectIdentifiers.GetSecT571k1: IDerObjectIdentifier;
begin
  result := FSecT571k1;
end;

class function TSecObjectIdentifiers.GetSecT571r1: IDerObjectIdentifier;
begin
  result := FSecT571r1;
end;

class procedure TSecObjectIdentifiers.Boot;
begin

  if not FIsBooted then
  begin

    FEllipticCurve := TDerObjectIdentifier.Create('1.3.132.0');

    FSecT163k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.1');
    FSecT163r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.2');
    FSecT239k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.3');
    FSecT113r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.4');
    FSecT113r2 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.5');
    FSecP112r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.6');
    FSecP112r2 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.7');
    FSecP160r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.8');
    FSecP160k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.9');
    FSecP256k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.10');
    FSecT163r2 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.15');
    FSecT283k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.16');
    FSecT283r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.17');
    FSecT131r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.22');
    FSecT131r2 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.23');
    FSecT193r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.24');
    FSecT193r2 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.25');
    FSecT233k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.26');
    FSecT233r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.27');
    FSecP128r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.28');
    FSecP128r2 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.29');
    FSecP160r2 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.30');
    FSecP192k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.31');
    FSecP224k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.32');
    FSecP224r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.33');
    FSecP384r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.34');
    FSecP521r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.35');
    FSecT409k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.36');
    FSecT409r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.37');
    FSecT571k1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.38');
    FSecT571r1 := TDerObjectIdentifier.Create(EllipticCurve.Id + '.39');

    TX9ObjectIdentifiers.Boot;

    FSecP192r1 := TX9ObjectIdentifiers.Prime192v1;

    FSecP256r1 := TX9ObjectIdentifiers.Prime256v1;

    FIsBooted := True;
  end;

end;

class constructor TSecObjectIdentifiers.SecObjectIdentifiers;
begin
  TSecObjectIdentifiers.Boot;
end;

end.
