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

unit ClpECPrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpCryptoLibTypes,
  ClpECKeyParameters,
  ClpIECPrivateKeyParameters,
  ClpIAsn1Objects,
  ClpIECDomainParameters;

resourcestring
  SBigIntegerNotInitialized = 'BigInteger Not Initialized "%s"';

type
  TECPrivateKeyParameters = class sealed(TECKeyParameters,
    IECPrivateKeyParameters)

  strict private
  var
    Fd: TBigInteger;

    function GetD: TBigInteger; inline;

  public
    constructor Create(const d: TBigInteger;
      const parameters: IECDomainParameters); overload;

    constructor Create(const algorithm: String; const d: TBigInteger;
      const parameters: IECDomainParameters); overload;

    constructor Create(const algorithm: String; const d: TBigInteger;
      const publicKeyParamSet: IDerObjectIdentifier); overload;

    property d: TBigInteger read GetD;

    function Equals(const other: IECPrivateKeyParameters): Boolean; reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TECPrivateKeyParameters }

function TECPrivateKeyParameters.GetD: TBigInteger;
begin
  result := Fd;
end;

constructor TECPrivateKeyParameters.Create(const d: TBigInteger;
  const parameters: IECDomainParameters);
begin
  Create('EC', d, parameters);
end;

constructor TECPrivateKeyParameters.Create(const algorithm: String;
  const d: TBigInteger; const parameters: IECDomainParameters);
begin
  Inherited Create(algorithm, true, parameters);
  if (not(d.IsInitialized)) then
    raise EArgumentNilCryptoLibException.CreateResFmt
      (@SBigIntegerNotInitialized, ['d']);
  Fd := d;
end;

constructor TECPrivateKeyParameters.Create(const algorithm: String;
  const d: TBigInteger; const publicKeyParamSet: IDerObjectIdentifier);
begin
  Inherited Create(algorithm, true, publicKeyParamSet);
  if (not(d.IsInitialized)) then
    raise EArgumentNilCryptoLibException.CreateResFmt
      (@SBigIntegerNotInitialized, ['d']);
  Fd := d;
end;

function TECPrivateKeyParameters.Equals(const other
  : IECPrivateKeyParameters): Boolean;
begin
  if (other = Self as IECPrivateKeyParameters) then
  begin
    result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := d.Equals(other.d) and (inherited Equals(other));
end;

function TECPrivateKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := d.GetHashCode() xor (inherited GetHashCode());
end;

end.
