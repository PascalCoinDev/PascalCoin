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

unit ClpTeleTrusTObjectIdentifiers;

{$I CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TTeleTrusTObjectIdentifiers = class sealed(TObject)

  strict private

  class var

    FIsBooted: Boolean;
    FTeleTrusTAlgorithm, FRipeMD160, FRipeMD128, FRipeMD256, FECSign,
      FECSignWithSha1, FECSignWithRipeMD160: IDerObjectIdentifier;

    class function GetRipeMD128: IDerObjectIdentifier; static; inline;
    class function GetRipeMD160: IDerObjectIdentifier; static; inline;
    class function GetRipeMD256: IDerObjectIdentifier; static; inline;
    class function GetTeleTrusTAlgorithm: IDerObjectIdentifier; static; inline;
    class function GetECSign: IDerObjectIdentifier; static; inline;
    class function GetECSignWithRipeMD160: IDerObjectIdentifier; static; inline;
    class function GetECSignWithSha1: IDerObjectIdentifier; static; inline;

    class constructor TeleTrusTObjectIdentifiers();

  public

    class property RipeMD160: IDerObjectIdentifier read GetRipeMD160;
    class property RipeMD128: IDerObjectIdentifier read GetRipeMD128;
    class property RipeMD256: IDerObjectIdentifier read GetRipeMD256;
    class property TeleTrusTAlgorithm: IDerObjectIdentifier
      read GetTeleTrusTAlgorithm;
    class property ECSign: IDerObjectIdentifier read GetECSign;
    class property ECSignWithSha1: IDerObjectIdentifier read GetECSignWithSha1;
    class property ECSignWithRipeMD160: IDerObjectIdentifier
      read GetECSignWithRipeMD160;

    class procedure Boot(); static;

  end;

implementation

{ TTeleTrusTObjectIdentifiers }

class function TTeleTrusTObjectIdentifiers.GetTeleTrusTAlgorithm
  : IDerObjectIdentifier;
begin
  result := FTeleTrusTAlgorithm;
end;

class function TTeleTrusTObjectIdentifiers.GetECSign: IDerObjectIdentifier;
begin
  result := FECSign;
end;

class function TTeleTrusTObjectIdentifiers.GetECSignWithRipeMD160
  : IDerObjectIdentifier;
begin
  result := FECSignWithRipeMD160;
end;

class function TTeleTrusTObjectIdentifiers.GetECSignWithSha1
  : IDerObjectIdentifier;
begin
  result := FECSignWithSha1;
end;

class function TTeleTrusTObjectIdentifiers.GetRipeMD128: IDerObjectIdentifier;
begin
  result := FRipeMD128;
end;

class function TTeleTrusTObjectIdentifiers.GetRipeMD160: IDerObjectIdentifier;
begin
  result := FRipeMD160;
end;

class function TTeleTrusTObjectIdentifiers.GetRipeMD256: IDerObjectIdentifier;
begin
  result := FRipeMD256;
end;

class procedure TTeleTrusTObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    FTeleTrusTAlgorithm := TDerObjectIdentifier.Create('1.3.36.3');
    FRipeMD160 := TDerObjectIdentifier.Create(TeleTrusTAlgorithm.ID + '.2.1');
    FRipeMD128 := TDerObjectIdentifier.Create(TeleTrusTAlgorithm.ID + '.2.2');
    FRipeMD256 := TDerObjectIdentifier.Create(TeleTrusTAlgorithm.ID + '.2.3');

    FECSign := TDerObjectIdentifier.Create(TeleTrusTAlgorithm.ID + '.3.2');

    FECSignWithSha1 := TDerObjectIdentifier.Create(ECSign.ID + '.1');
    FECSignWithRipeMD160 := TDerObjectIdentifier.Create(ECSign.ID + '.2');

    FIsBooted := True;
  end;

end;

class constructor TTeleTrusTObjectIdentifiers.TeleTrusTObjectIdentifiers;
begin
  TTeleTrusTObjectIdentifiers.Boot;
end;

end.
