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

unit ClpRosstandartObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TRosstandartObjectIdentifiers = class abstract(TObject)
  strict private

  class var

    FIsBooted: Boolean;
    Frosstandart, Fid_tc26_gost_3410_12_512_paramSetC,
      Fid_tc26_gost_3410_12_512_paramSetA, Fid_tc26_gost_28147_param_Z,
      Fid_tc26_gost_3410_12_256_paramSetA, Fid_tc26_hmac_gost_3411_12_512,
      Fid_tc26_hmac_gost_3411_12_256, Fid_tc26_agreement_gost_3410_12_512,
      Fid_tc26_agreement_gost_3410_12_256, Fid_tc26_agreement,
      Fid_tc26_signwithdigest_gost_3410_12_512,
      Fid_tc26_signwithdigest_gost_3410_12_256, Fid_tc26_gost_3410_12_512,
      Fid_tc26_gost_3410_12_512_paramSet, Fid_tc26_gost_3410_12_256,
      Fid_tc26_gost_3411_12_512, Fid_tc26_gost_3410_12_256_paramSet,
      Fid_tc26_gost_3411_12_256, Fid_tc26, Fid_tc26_gost_3410_12_512_paramSetB
      : IDerObjectIdentifier;

    class function Getid_tc26: IDerObjectIdentifier; static; inline;
    class function Getid_tc26_agreement: IDerObjectIdentifier; static; inline;
    class function Getid_tc26_agreement_gost_3410_12_256: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_agreement_gost_3410_12_512: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_28147_param_Z: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_256: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_256_paramSet: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_256_paramSetA: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_512: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_512_paramSet: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_512_paramSetA: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_512_paramSetB: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3410_12_512_paramSetC: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3411_12_256: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_gost_3411_12_512: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_hmac_gost_3411_12_256: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_hmac_gost_3411_12_512: IDerObjectIdentifier;
      static; inline;
    class function Getid_tc26_signwithdigest_gost_3410_12_256
      : IDerObjectIdentifier; static; inline;
    class function Getid_tc26_signwithdigest_gost_3410_12_512
      : IDerObjectIdentifier; static; inline;
    class function Getrosstandart: IDerObjectIdentifier; static; inline;

    class constructor RosstandartObjectIdentifiers();

  public
    class property rosstandart: IDerObjectIdentifier read Getrosstandart;
    class property id_tc26: IDerObjectIdentifier read Getid_tc26;
    class property id_tc26_gost_3411_12_256: IDerObjectIdentifier
      read Getid_tc26_gost_3411_12_256;
    class property id_tc26_gost_3411_12_512: IDerObjectIdentifier
      read Getid_tc26_gost_3411_12_512;
    class property id_tc26_hmac_gost_3411_12_256: IDerObjectIdentifier
      read Getid_tc26_hmac_gost_3411_12_256;
    class property id_tc26_hmac_gost_3411_12_512: IDerObjectIdentifier
      read Getid_tc26_hmac_gost_3411_12_512;
    class property id_tc26_gost_3410_12_256: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_256;
    class property id_tc26_gost_3410_12_512: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_512;
    class property id_tc26_signwithdigest_gost_3410_12_256: IDerObjectIdentifier
      read Getid_tc26_signwithdigest_gost_3410_12_256;
    class property id_tc26_signwithdigest_gost_3410_12_512: IDerObjectIdentifier
      read Getid_tc26_signwithdigest_gost_3410_12_512;
    class property id_tc26_agreement: IDerObjectIdentifier
      read Getid_tc26_agreement;
    class property id_tc26_agreement_gost_3410_12_256: IDerObjectIdentifier
      read Getid_tc26_agreement_gost_3410_12_256;
    class property id_tc26_agreement_gost_3410_12_512: IDerObjectIdentifier
      read Getid_tc26_agreement_gost_3410_12_512;
    class property id_tc26_gost_3410_12_256_paramSet: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_256_paramSet;
    class property id_tc26_gost_3410_12_256_paramSetA: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_256_paramSetA;
    class property id_tc26_gost_3410_12_512_paramSet: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_512_paramSet;
    class property id_tc26_gost_3410_12_512_paramSetA: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_512_paramSetA;
    class property id_tc26_gost_3410_12_512_paramSetB: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_512_paramSetB;
    class property id_tc26_gost_3410_12_512_paramSetC: IDerObjectIdentifier
      read Getid_tc26_gost_3410_12_512_paramSetC;
    class property id_tc26_gost_28147_param_Z: IDerObjectIdentifier
      read Getid_tc26_gost_28147_param_Z;

    class procedure Boot(); static;

  end;

implementation

{ TRosstandartObjectIdentifiers }

class constructor TRosstandartObjectIdentifiers.RosstandartObjectIdentifiers;
begin
  TRosstandartObjectIdentifiers.Boot;
end;

class function TRosstandartObjectIdentifiers.Getrosstandart
  : IDerObjectIdentifier;
begin
  result := Frosstandart;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26: IDerObjectIdentifier;
begin
  result := Fid_tc26;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_agreement
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_agreement;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_agreement_gost_3410_12_256: IDerObjectIdentifier;
begin
  result := Fid_tc26_agreement_gost_3410_12_256;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_agreement_gost_3410_12_512: IDerObjectIdentifier;
begin
  result := Fid_tc26_agreement_gost_3410_12_512;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_gost_28147_param_Z
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_28147_param_Z;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_gost_3410_12_256
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_256;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_gost_3410_12_256_paramSet: IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_256_paramSet;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_gost_3410_12_256_paramSetA: IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_256_paramSetA;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_gost_3410_12_512
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_512;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_gost_3410_12_512_paramSet: IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_512_paramSet;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_gost_3410_12_512_paramSetA: IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_512_paramSetA;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_gost_3410_12_512_paramSetB: IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_512_paramSetB;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_gost_3410_12_512_paramSetC: IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3410_12_512_paramSetC;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_gost_3411_12_256
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3411_12_256;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_gost_3411_12_512
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_gost_3411_12_512;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_hmac_gost_3411_12_256
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_hmac_gost_3411_12_256;
end;

class function TRosstandartObjectIdentifiers.Getid_tc26_hmac_gost_3411_12_512
  : IDerObjectIdentifier;
begin
  result := Fid_tc26_hmac_gost_3411_12_512;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_signwithdigest_gost_3410_12_256: IDerObjectIdentifier;
begin
  result := Fid_tc26_signwithdigest_gost_3410_12_256;
end;

class function TRosstandartObjectIdentifiers.
  Getid_tc26_signwithdigest_gost_3410_12_512: IDerObjectIdentifier;
begin
  result := Fid_tc26_signwithdigest_gost_3410_12_512;
end;

class procedure TRosstandartObjectIdentifiers.Boot;
begin

  if not FIsBooted then
  begin

    Frosstandart := TDerObjectIdentifier.Create('1.2.643.7');

    Fid_tc26 := rosstandart.Branch('1');

    Fid_tc26_gost_3411_12_256 := id_tc26.Branch('1.2.2');

    Fid_tc26_gost_3411_12_512 := id_tc26.Branch('1.2.3');

    Fid_tc26_hmac_gost_3411_12_256 := id_tc26.Branch('1.4.1');

    Fid_tc26_hmac_gost_3411_12_512 := id_tc26.Branch('1.4.2');

    Fid_tc26_gost_3410_12_256 := id_tc26.Branch('1.1.1');

    Fid_tc26_gost_3410_12_512 := id_tc26.Branch('1.1.2');

    Fid_tc26_signwithdigest_gost_3410_12_256 := id_tc26.Branch('1.3.2');

    Fid_tc26_signwithdigest_gost_3410_12_512 := id_tc26.Branch('1.3.3');

    Fid_tc26_agreement := id_tc26.Branch('1.6');

    Fid_tc26_agreement_gost_3410_12_256 := id_tc26_agreement.Branch('1');

    Fid_tc26_agreement_gost_3410_12_512 := id_tc26_agreement.Branch('2');

    Fid_tc26_gost_3410_12_256_paramSet := id_tc26.Branch('2.1.1');

    Fid_tc26_gost_3410_12_256_paramSetA :=
      id_tc26_gost_3410_12_256_paramSet.Branch('1');

    Fid_tc26_gost_3410_12_512_paramSet := id_tc26.Branch('2.1.2');

    Fid_tc26_gost_3410_12_512_paramSetA :=
      id_tc26_gost_3410_12_512_paramSet.Branch('1');

    Fid_tc26_gost_3410_12_512_paramSetB :=
      id_tc26_gost_3410_12_512_paramSet.Branch('2');

    Fid_tc26_gost_3410_12_512_paramSetC :=
      id_tc26_gost_3410_12_512_paramSet.Branch('3');

    Fid_tc26_gost_28147_param_Z := id_tc26.Branch('2.5.1.1');

    FIsBooted := True;
  end;
end;

end.
