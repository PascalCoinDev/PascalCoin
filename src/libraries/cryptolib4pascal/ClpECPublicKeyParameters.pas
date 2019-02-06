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

unit ClpECPublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpECDomainParameters,
  ClpBigInteger,
  ClpIECC,
  ClpIECPublicKeyParameters,
  ClpIAsn1Objects,
  ClpIECDomainParameters,
  ClpECKeyParameters;

resourcestring
  SQNil = 'Q Cannot be Nil';

type
  TECPublicKeyParameters = class sealed(TECKeyParameters,
    IECPublicKeyParameters)

  strict private
  var
    Fq: IECPoint;

    function GetQ: IECPoint; inline;

  public
    constructor Create(const q: IECPoint;
      const parameters: IECDomainParameters); overload;

    constructor Create(const algorithm: String; const q: IECPoint;
      const parameters: IECDomainParameters); overload;

    constructor Create(const algorithm: String; const q: IECPoint;
      const publicKeyParamSet: IDerObjectIdentifier); overload;

    property q: IECPoint read GetQ;

    function Equals(const other: IECPublicKeyParameters): Boolean; reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TECPublicKeyParameters }

function TECPublicKeyParameters.GetQ: IECPoint;
begin
  result := Fq;
end;

constructor TECPublicKeyParameters.Create(const algorithm: String;
  const q: IECPoint; const parameters: IECDomainParameters);
begin
  Inherited Create(algorithm, false, parameters);

  if (q = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SQNil);
  end;

  Fq := TECDomainParameters.Validate(parameters.Curve, q);
end;

constructor TECPublicKeyParameters.Create(const q: IECPoint;
  const parameters: IECDomainParameters);
begin
  Create('EC', q, parameters);
end;

constructor TECPublicKeyParameters.Create(const algorithm: String;
  const q: IECPoint; const publicKeyParamSet: IDerObjectIdentifier);
begin
  Inherited Create(algorithm, false, publicKeyParamSet);

  if (q = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SQNil);
  end;

  Fq := TECDomainParameters.Validate(parameters.Curve, q);
end;

function TECPublicKeyParameters.Equals(const other
  : IECPublicKeyParameters): Boolean;
begin
  if (other = Self as IECPublicKeyParameters) then
  begin
    result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := q.Equals(other.q) and (inherited Equals(other));
end;

function TECPublicKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := q.GetHashCode() xor (inherited GetHashCode());
end;

end.
