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

unit ClpDHPrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHParameters,
  ClpIDHPrivateKeyParameters,
  ClpDHKeyParameters,
  ClpIAsn1Objects,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SXUnInitialized = '"X" Cannot Be Uninitialized';

type
  TDHPrivateKeyParameters = class sealed(TDHKeyParameters,
    IDHPrivateKeyParameters)

  strict private
  var
    Fx: TBigInteger;

    function GetX: TBigInteger; inline;

    class function Validate(const x: TBigInteger): TBigInteger; static; inline;

  public
    constructor Create(const x: TBigInteger;
      const parameters: IDHParameters); overload;

    constructor Create(const x: TBigInteger; const parameters: IDHParameters;
      const algorithmOid: IDerObjectIdentifier); overload;

    function Equals(const other: IDHPrivateKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property x: TBigInteger read GetX;
  end;

implementation

{ TDHPrivateKeyParameters }

function TDHPrivateKeyParameters.GetX: TBigInteger;
begin
  result := Fx;
end;

class function TDHPrivateKeyParameters.Validate(const x: TBigInteger)
  : TBigInteger;
begin
  if (not(x.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SXUnInitialized);
  end;
  result := x;
end;

constructor TDHPrivateKeyParameters.Create(const x: TBigInteger;
  const parameters: IDHParameters);
begin
  Inherited Create(true, parameters);
  Fx := Validate(x);
end;

constructor TDHPrivateKeyParameters.Create(const x: TBigInteger;
  const parameters: IDHParameters; const algorithmOid: IDerObjectIdentifier);
begin
  Inherited Create(true, parameters, algorithmOid);
  Fx := Validate(x);
end;

function TDHPrivateKeyParameters.Equals(const other
  : IDHPrivateKeyParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDHPrivateKeyParameters) = other) then
  begin
    result := true;
    Exit;
  end;
  result := (x.Equals(other.x)) and (Inherited Equals(other));
end;

function TDHPrivateKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := x.GetHashCode() xor (Inherited GetHashCode());
end;

end.
