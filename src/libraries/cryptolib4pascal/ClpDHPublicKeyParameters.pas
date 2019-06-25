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

unit ClpDHPublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHParameters,
  ClpIDHPublicKeyParameters,
  ClpIAsn1Objects,
  ClpDHKeyParameters,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SYUnInitialized = '"Y" Cannot Be Uninitialized';
  SInvalidDHPublicKey = 'Invalid DH public key "Y"';
  SInvalidYInCorrectGroup = '"Y" Value Does Not Appear To Be In Correct Group';

type
  TDHPublicKeyParameters = class sealed(TDHKeyParameters,
    IDHPublicKeyParameters)

  strict private
  var
    Fy: TBigInteger;

    class function Validate(const y: TBigInteger; const dhParams: IDHParameters)
      : TBigInteger; static; inline;

    function GetY: TBigInteger; inline;

  public
    constructor Create(const y: TBigInteger;
      const parameters: IDHParameters); overload;

    constructor Create(const y: TBigInteger; const parameters: IDHParameters;
      const algorithmOid: IDerObjectIdentifier); overload;

    function Equals(const other: IDHPublicKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property y: TBigInteger read GetY;
  end;

implementation

{ TDHPublicKeyParameters }

function TDHPublicKeyParameters.GetY: TBigInteger;
begin
  result := Fy;
end;

class function TDHPublicKeyParameters.Validate(const y: TBigInteger;
  const dhParams: IDHParameters): TBigInteger;
begin
  if (not(y.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SYUnInitialized);
  end;

  // TLS check
  if ((y.CompareTo(TBigInteger.Two) < 0) or
    (y.CompareTo(dhParams.P.Subtract(TBigInteger.Two)) > 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidDHPublicKey);
  end;

  // we can't validate without Q.
  if ((dhParams.Q.IsInitialized) and
    (not(y.ModPow(dhParams.Q, dhParams.P).Equals(TBigInteger.One)))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidYInCorrectGroup);
  end;

  result := y;
end;

constructor TDHPublicKeyParameters.Create(const y: TBigInteger;
  const parameters: IDHParameters);
begin
  Inherited Create(false, parameters);
  Fy := Validate(y, parameters);
end;

constructor TDHPublicKeyParameters.Create(const y: TBigInteger;
  const parameters: IDHParameters; const algorithmOid: IDerObjectIdentifier);
begin
  Inherited Create(false, parameters, algorithmOid);
  Fy := Validate(y, parameters);
end;

function TDHPublicKeyParameters.Equals(const other
  : IDHPublicKeyParameters): Boolean;
begin
  if other = Nil then
  begin
    result := false;
    Exit;
  end;
  if ((Self as IDHPublicKeyParameters) = other) then
  begin
    result := True;
    Exit;
  end;
  result := (y.Equals(other.y)) and (Inherited Equals(other));
end;

function TDHPublicKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := y.GetHashCode() xor (Inherited GetHashCode());
end;

end.
