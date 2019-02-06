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

unit ClpCryptoProObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TCryptoProObjectIdentifiers = class sealed(TObject)

  strict private

  const
    // GOST Algorithms OBJECT IDENTIFIERS :
    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2)}
    GostID: String = '1.2.643.2.2';

  class var

    FIsBooted: Boolean;
    FGostR3411, FGostR3410x2001, FGostR3410x94, FGostR28147Cbc,
      FID_Gost28147_89_CryptoPro_A_ParamSet, FGostR3411x94CryptoProParam,
      FGostR3411Hmac, FGostR3410x2001CryptoProA, FGostElSgDH3410Default,
      FGostR3410x94CryptoProXchB, FGostR3410x94CryptoProXchC,
      FGostR3411x94WithGostR3410x94, FGostR3410x94CryptoProXchA,
      FGostR3410x94CryptoProB, FGostR3410x94CryptoProC, FGostR3410x94CryptoProA,
      FGostR3410x94CryptoProD, FGostR3411x94CryptoProParamSet,
      FGostR3410x2001CryptoProXchB, FGostR3411x94WithGostR3410x2001,
      FGostR3410x2001CryptoProXchA, FGostR3410x2001CryptoProB,
      FGostR3410x2001CryptoProC, FGostElSgDH3410x1: IDerObjectIdentifier;

    class function GetGostR3411: IDerObjectIdentifier; static; inline;

    class function GetGostR28147Cbc: IDerObjectIdentifier; static; inline;
    class function GetGostR3410x94: IDerObjectIdentifier; static; inline;
    class function GetGostR3411Hmac: IDerObjectIdentifier; static; inline;
    class function GetID_Gost28147_89_CryptoPro_A_ParamSet
      : IDerObjectIdentifier; static; inline;
    class function GetGostR3410x2001: IDerObjectIdentifier; static; inline;

    class function GetGostElSgDH3410Default: IDerObjectIdentifier;
      static; inline;
    class function GetGostElSgDH3410x1: IDerObjectIdentifier; static; inline;
    class function GetGostR3410x2001CryptoProA: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x2001CryptoProB: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x2001CryptoProC: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x2001CryptoProXchA: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x2001CryptoProXchB: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x94CryptoProA: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x94CryptoProB: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x94CryptoProC: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x94CryptoProD: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x94CryptoProXchA: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x94CryptoProXchB: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3410x94CryptoProXchC: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3411x94CryptoProParam: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3411x94WithGostR3410x2001: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3411x94WithGostR3410x94: IDerObjectIdentifier;
      static; inline;
    class function GetGostR3411x94CryptoProParamSet: IDerObjectIdentifier;
      static; inline;

    class constructor CryptoProObjectIdentifiers();

  public

    class property GostR3411: IDerObjectIdentifier read GetGostR3411;
    class property GostR3411Hmac: IDerObjectIdentifier read GetGostR3411Hmac;
    class property GostR28147Cbc: IDerObjectIdentifier read GetGostR28147Cbc;
    class property ID_Gost28147_89_CryptoPro_A_ParamSet: IDerObjectIdentifier
      read GetID_Gost28147_89_CryptoPro_A_ParamSet;
    class property GostR3410x94: IDerObjectIdentifier read GetGostR3410x94;

    class property GostR3410x2001: IDerObjectIdentifier read GetGostR3410x2001;
    class property GostR3411x94WithGostR3410x94: IDerObjectIdentifier
      read GetGostR3411x94WithGostR3410x94;
    class property GostR3411x94WithGostR3410x2001: IDerObjectIdentifier
      read GetGostR3411x94WithGostR3410x2001;
    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) hashes(30) }
    class property GostR3411x94CryptoProParamSet: IDerObjectIdentifier
      read GetGostR3411x94CryptoProParamSet;
    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) signs(32) }
    class property GostR3410x94CryptoProA: IDerObjectIdentifier
      read GetGostR3410x94CryptoProA;

    class property GostR3410x94CryptoProB: IDerObjectIdentifier
      read GetGostR3410x94CryptoProB;
    class property GostR3410x94CryptoProC: IDerObjectIdentifier
      read GetGostR3410x94CryptoProC;
    class property GostR3410x94CryptoProD: IDerObjectIdentifier
      read GetGostR3410x94CryptoProD;
    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) exchanges(33) }
    class property GostR3410x94CryptoProXchA: IDerObjectIdentifier
      read GetGostR3410x94CryptoProXchA;
    class property GostR3410x94CryptoProXchB: IDerObjectIdentifier
      read GetGostR3410x94CryptoProXchB;
    class property GostR3410x94CryptoProXchC: IDerObjectIdentifier
      read GetGostR3410x94CryptoProXchC;
    // { iso(1) member-body(2)ru(643) rans(2) cryptopro(2) ecc-signs(35) }
    class property GostR3410x2001CryptoProA: IDerObjectIdentifier
      read GetGostR3410x2001CryptoProA;
    class property GostR3410x2001CryptoProB: IDerObjectIdentifier
      read GetGostR3410x2001CryptoProB;
    class property GostR3410x2001CryptoProC: IDerObjectIdentifier
      read GetGostR3410x2001CryptoProC;
    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) ecc-exchanges(36) }
    class property GostR3410x2001CryptoProXchA: IDerObjectIdentifier
      read GetGostR3410x2001CryptoProXchA;
    class property GostR3410x2001CryptoProXchB: IDerObjectIdentifier
      read GetGostR3410x2001CryptoProXchB;

    class property GostElSgDH3410Default: IDerObjectIdentifier
      read GetGostElSgDH3410Default;
    class property GostElSgDH3410x1: IDerObjectIdentifier
      read GetGostElSgDH3410x1;

    class property GostR3411x94CryptoProParam: IDerObjectIdentifier
      read GetGostR3411x94CryptoProParam;

    class procedure Boot(); static;

  end;

implementation

{ TCryptoProObjectIdentifiers }

class function TCryptoProObjectIdentifiers.GetGostR3411: IDerObjectIdentifier;
begin
  result := FGostR3411;
end;

class function TCryptoProObjectIdentifiers.GetGostElSgDH3410Default
  : IDerObjectIdentifier;
begin
  result := FGostElSgDH3410Default;
end;

class function TCryptoProObjectIdentifiers.GetGostElSgDH3410x1
  : IDerObjectIdentifier;
begin
  result := FGostElSgDH3410x1;
end;

class function TCryptoProObjectIdentifiers.GetGostR28147Cbc
  : IDerObjectIdentifier;
begin
  result := FGostR28147Cbc;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x2001
  : IDerObjectIdentifier;
begin
  result := FGostR3410x2001;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x2001CryptoProA
  : IDerObjectIdentifier;
begin
  result := FGostR3410x2001CryptoProA;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x2001CryptoProB
  : IDerObjectIdentifier;
begin
  result := FGostR3410x2001CryptoProB;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x2001CryptoProC
  : IDerObjectIdentifier;
begin
  result := FGostR3410x2001CryptoProC;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x2001CryptoProXchA
  : IDerObjectIdentifier;
begin
  result := FGostR3410x2001CryptoProXchA;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x2001CryptoProXchB
  : IDerObjectIdentifier;
begin
  result := FGostR3410x2001CryptoProXchB;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94CryptoProA
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94CryptoProA;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94CryptoProB
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94CryptoProB;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94CryptoProC
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94CryptoProC;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94CryptoProD
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94CryptoProD;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94CryptoProXchA
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94CryptoProXchA;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94CryptoProXchB
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94CryptoProXchB;
end;

class function TCryptoProObjectIdentifiers.GetGostR3410x94CryptoProXchC
  : IDerObjectIdentifier;
begin
  result := FGostR3410x94CryptoProXchC;
end;

class function TCryptoProObjectIdentifiers.GetGostR3411Hmac
  : IDerObjectIdentifier;
begin
  result := FGostR3411Hmac;
end;

class function TCryptoProObjectIdentifiers.GetGostR3411x94CryptoProParam
  : IDerObjectIdentifier;
begin
  result := FGostR3411x94CryptoProParam;
end;

class function TCryptoProObjectIdentifiers.GetGostR3411x94CryptoProParamSet
  : IDerObjectIdentifier;
begin
  result := FGostR3411x94CryptoProParamSet;
end;

class function TCryptoProObjectIdentifiers.GetGostR3411x94WithGostR3410x2001
  : IDerObjectIdentifier;
begin
  result := FGostR3411x94WithGostR3410x2001;
end;

class function TCryptoProObjectIdentifiers.GetGostR3411x94WithGostR3410x94
  : IDerObjectIdentifier;
begin
  result := FGostR3411x94WithGostR3410x94;
end;

class function TCryptoProObjectIdentifiers.
  GetID_Gost28147_89_CryptoPro_A_ParamSet: IDerObjectIdentifier;
begin
  result := FID_Gost28147_89_CryptoPro_A_ParamSet;
end;

class procedure TCryptoProObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    FGostR3411 := TDerObjectIdentifier.Create(GostID + '.9');
    FGostR3411Hmac := TDerObjectIdentifier.Create(GostID + '.10');

    FGostR28147Cbc := TDerObjectIdentifier.Create(GostID + '.21');

    FID_Gost28147_89_CryptoPro_A_ParamSet := TDerObjectIdentifier.Create
      (GostID + '.31.1');

    FGostR3410x94 := TDerObjectIdentifier.Create(GostID + '.20');
    FGostR3410x2001 := TDerObjectIdentifier.Create(GostID + '.19');
    FGostR3411x94WithGostR3410x94 := TDerObjectIdentifier.Create(GostID + '.4');
    FGostR3411x94WithGostR3410x2001 := TDerObjectIdentifier.Create
      (GostID + '.3');

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) hashes(30) }
    FGostR3411x94CryptoProParamSet := TDerObjectIdentifier.Create
      (GostID + '.30.1');

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) signs(32) }
    FGostR3410x94CryptoProA := TDerObjectIdentifier.Create(GostID + '.32.2');
    FGostR3410x94CryptoProB := TDerObjectIdentifier.Create(GostID + '.32.3');
    FGostR3410x94CryptoProC := TDerObjectIdentifier.Create(GostID + '.32.4');
    FGostR3410x94CryptoProD := TDerObjectIdentifier.Create(GostID + '.32.5');

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) exchanges(33) }
    FGostR3410x94CryptoProXchA := TDerObjectIdentifier.Create(GostID + '.33.1');
    FGostR3410x94CryptoProXchB := TDerObjectIdentifier.Create(GostID + '.33.2');
    FGostR3410x94CryptoProXchC := TDerObjectIdentifier.Create(GostID + '.33.3');

    // { iso(1) member-body(2)ru(643) rans(2) cryptopro(2) ecc-signs(35) }
    FGostR3410x2001CryptoProA := TDerObjectIdentifier.Create(GostID + '.35.1');
    FGostR3410x2001CryptoProB := TDerObjectIdentifier.Create(GostID + '.35.2');
    FGostR3410x2001CryptoProC := TDerObjectIdentifier.Create(GostID + '.35.3');

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) ecc-exchanges(36) }
    FGostR3410x2001CryptoProXchA := TDerObjectIdentifier.Create
      (GostID + '.36.0');
    FGostR3410x2001CryptoProXchB := TDerObjectIdentifier.Create
      (GostID + '.36.1');

    FGostElSgDH3410Default := TDerObjectIdentifier.Create(GostID + '.36.0');
    FGostElSgDH3410x1 := TDerObjectIdentifier.Create(GostID + '.36.1');

    FIsBooted := True;
  end;
end;

class constructor TCryptoProObjectIdentifiers.CryptoProObjectIdentifiers;
begin
  TCryptoProObjectIdentifiers.Boot;
end;

end.
