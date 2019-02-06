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

unit ClpDsaPrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDsaParameters,
  ClpIDsaPrivateKeyParameters,
  ClpDsaKeyParameters,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SXUnInitialized = '"X" Cannot Be Uninitialized';

type
  TDsaPrivateKeyParameters = class sealed(TDsaKeyParameters,
    IDsaPrivateKeyParameters)

  strict private
  var
    Fx: TBigInteger;

    function GetX: TBigInteger; inline;

  public
    constructor Create(const x: TBigInteger; const parameters: IDsaParameters);

    function Equals(const other: IDsaPrivateKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property x: TBigInteger read GetX;
  end;

implementation

{ TDsaPrivateKeyParameters }

function TDsaPrivateKeyParameters.GetX: TBigInteger;
begin
  result := Fx;
end;

constructor TDsaPrivateKeyParameters.Create(const x: TBigInteger;
  const parameters: IDsaParameters);
begin
  Inherited Create(true, parameters);
  if (not(x.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SXUnInitialized);
  end;

  Fx := x;
end;

function TDsaPrivateKeyParameters.Equals(const other
  : IDsaPrivateKeyParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDsaPrivateKeyParameters) = other) then
  begin
    result := true;
    Exit;
  end;
  result := (x.Equals(other.x)) and (Inherited Equals(other));
end;

function TDsaPrivateKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := x.GetHashCode() xor (Inherited GetHashCode());
end;

end.
