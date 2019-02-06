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

unit ClpDsaPublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDsaParameters,
  ClpIDsaPublicKeyParameters,
  ClpDsaKeyParameters,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SYUnInitialized = '"Y" Cannot Be Uninitialized';
  SInvalidYInCorrectGroup = '"Y" Value Does Not Appear To Be In Correct Group';

type
  TDsaPublicKeyParameters = class sealed(TDsaKeyParameters,
    IDsaPublicKeyParameters)

  strict private
  var
    Fy: TBigInteger;

    class function Validate(const y: TBigInteger;
      const parameters: IDsaParameters): TBigInteger; static; inline;

    function GetY: TBigInteger; inline;

  public
    constructor Create(const y: TBigInteger; const parameters: IDsaParameters);

    function Equals(const other: IDsaPublicKeyParameters): Boolean; reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property y: TBigInteger read GetY;
  end;

implementation

{ TDsaPublicKeyParameters }

function TDsaPublicKeyParameters.GetY: TBigInteger;
begin
  result := Fy;
end;

class function TDsaPublicKeyParameters.Validate(const y: TBigInteger;
  const parameters: IDsaParameters): TBigInteger;
begin
  // we can't validate without params, fortunately we can't use the key either...
  if (parameters <> Nil) then
  begin
    if ((y.CompareTo(TBigInteger.Two) < 0) or
      (y.CompareTo(parameters.P.Subtract(TBigInteger.Two)) > 0) or
      (not(y.ModPow(parameters.Q, parameters.P).Equals(TBigInteger.One)))) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidYInCorrectGroup);
    end;
  end;

  result := y;
end;

constructor TDsaPublicKeyParameters.Create(const y: TBigInteger;
  const parameters: IDsaParameters);
begin
  Inherited Create(false, parameters);
  if (not(y.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SYUnInitialized);
  end;

  Fy := Validate(y, parameters);
end;

function TDsaPublicKeyParameters.Equals(const other
  : IDsaPublicKeyParameters): Boolean;
begin
  if other = Nil then
  begin
    result := false;
    Exit;
  end;
  if ((Self as IDsaPublicKeyParameters) = other) then
  begin
    result := True;
    Exit;
  end;
  result := (y.Equals(other.y)) and (Inherited Equals(other));
end;

function TDsaPublicKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := y.GetHashCode() xor (Inherited GetHashCode());
end;

end.
