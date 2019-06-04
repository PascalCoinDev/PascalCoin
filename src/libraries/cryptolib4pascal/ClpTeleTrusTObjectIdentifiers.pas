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
      FECSignWithSha1, FECSignWithRipeMD160, FEccBrainpool, FEllipticCurve,
      FVersionOne, FBrainpoolP160R1, FBrainpoolP160T1, FBrainpoolP192R1,
      FBrainpoolP192T1, FBrainpoolP224R1, FBrainpoolP224T1, FBrainpoolP256R1,
      FBrainpoolP256T1, FBrainpoolP320R1, FBrainpoolP320T1, FBrainpoolP384R1,
      FBrainpoolP384T1, FBrainpoolP512R1, FBrainpoolP512T1
      : IDerObjectIdentifier;

    class function GetRipeMD128: IDerObjectIdentifier; static; inline;
    class function GetRipeMD160: IDerObjectIdentifier; static; inline;
    class function GetRipeMD256: IDerObjectIdentifier; static; inline;
    class function GetTeleTrusTAlgorithm: IDerObjectIdentifier; static; inline;
    class function GetECSign: IDerObjectIdentifier; static; inline;
    class function GetECSignWithRipeMD160: IDerObjectIdentifier; static; inline;
    class function GetECSignWithSha1: IDerObjectIdentifier; static; inline;

    class function GetBrainpoolP160R1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP160T1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP192R1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP192T1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP224R1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP224T1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP256R1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP256T1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP320R1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP320T1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP384R1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP384T1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP512R1: IDerObjectIdentifier; static; inline;
    class function GetBrainpoolP512T1: IDerObjectIdentifier; static; inline;
    class function GetEccBrainpool: IDerObjectIdentifier; static; inline;
    class function GetEllipticCurve: IDerObjectIdentifier; static; inline;
    class function GetVersionOne: IDerObjectIdentifier; static; inline;

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

    class property EccBrainpool: IDerObjectIdentifier read GetEccBrainpool;
    class property EllipticCurve: IDerObjectIdentifier read GetEllipticCurve;
    class property VersionOne: IDerObjectIdentifier read GetVersionOne;
    class property BrainpoolP160R1: IDerObjectIdentifier
      read GetBrainpoolP160R1;
    class property BrainpoolP160T1: IDerObjectIdentifier
      read GetBrainpoolP160T1;
    class property BrainpoolP192R1: IDerObjectIdentifier
      read GetBrainpoolP192R1;
    class property BrainpoolP192T1: IDerObjectIdentifier
      read GetBrainpoolP192T1;
    class property BrainpoolP224R1: IDerObjectIdentifier
      read GetBrainpoolP224R1;
    class property BrainpoolP224T1: IDerObjectIdentifier
      read GetBrainpoolP224T1;
    class property BrainpoolP256R1: IDerObjectIdentifier
      read GetBrainpoolP256R1;
    class property BrainpoolP256T1: IDerObjectIdentifier
      read GetBrainpoolP256T1;
    class property BrainpoolP320R1: IDerObjectIdentifier
      read GetBrainpoolP320R1;
    class property BrainpoolP320T1: IDerObjectIdentifier
      read GetBrainpoolP320T1;
    class property BrainpoolP384R1: IDerObjectIdentifier
      read GetBrainpoolP384R1;
    class property BrainpoolP384T1: IDerObjectIdentifier
      read GetBrainpoolP384T1;
    class property BrainpoolP512R1: IDerObjectIdentifier
      read GetBrainpoolP512R1;
    class property BrainpoolP512T1: IDerObjectIdentifier
      read GetBrainpoolP512T1;

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

class function TTeleTrusTObjectIdentifiers.GetEccBrainpool
  : IDerObjectIdentifier;
begin
  result := FEccBrainpool;
end;

class function TTeleTrusTObjectIdentifiers.GetVersionOne: IDerObjectIdentifier;
begin
  result := FVersionOne;
end;

class function TTeleTrusTObjectIdentifiers.GetEllipticCurve
  : IDerObjectIdentifier;
begin
  result := FEllipticCurve;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP160R1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP160R1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP160T1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP160T1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP192R1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP192R1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP192T1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP192T1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP224R1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP224R1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP224T1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP224T1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP256R1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP256R1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP256T1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP256T1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP320R1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP320R1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP320T1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP320T1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP384R1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP384R1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP384T1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP384T1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP512R1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP512R1;
end;

class function TTeleTrusTObjectIdentifiers.GetBrainpoolP512T1
  : IDerObjectIdentifier;
begin
  result := FBrainpoolP512T1;
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

    FEccBrainpool := TDerObjectIdentifier.Create(TeleTrusTAlgorithm.ID +
      '.3.2.8');
    FEllipticCurve := TDerObjectIdentifier.Create(EccBrainpool.ID + '.1');
    FVersionOne := TDerObjectIdentifier.Create(EllipticCurve.ID + '.1');

    FBrainpoolP160R1 := TDerObjectIdentifier.Create(VersionOne.ID + '.1');
    FBrainpoolP160T1 := TDerObjectIdentifier.Create(VersionOne.ID + '.2');
    FBrainpoolP192R1 := TDerObjectIdentifier.Create(VersionOne.ID + '.3');
    FBrainpoolP192T1 := TDerObjectIdentifier.Create(VersionOne.ID + '.4');
    FBrainpoolP224R1 := TDerObjectIdentifier.Create(VersionOne.ID + '.5');
    FBrainpoolP224T1 := TDerObjectIdentifier.Create(VersionOne.ID + '.6');
    FBrainpoolP256R1 := TDerObjectIdentifier.Create(VersionOne.ID + '.7');
    FBrainpoolP256T1 := TDerObjectIdentifier.Create(VersionOne.ID + '.8');
    FBrainpoolP320R1 := TDerObjectIdentifier.Create(VersionOne.ID + '.9');
    FBrainpoolP320T1 := TDerObjectIdentifier.Create(VersionOne.ID + '.10');
    FBrainpoolP384R1 := TDerObjectIdentifier.Create(VersionOne.ID + '.11');
    FBrainpoolP384T1 := TDerObjectIdentifier.Create(VersionOne.ID + '.12');
    FBrainpoolP512R1 := TDerObjectIdentifier.Create(VersionOne.ID + '.13');
    FBrainpoolP512T1 := TDerObjectIdentifier.Create(VersionOne.ID + '.14');

    FIsBooted := True;
  end;

end;

class constructor TTeleTrusTObjectIdentifiers.TeleTrusTObjectIdentifiers;
begin
  TTeleTrusTObjectIdentifiers.Boot;
end;

end.
