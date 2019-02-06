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

unit ClpDsaKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDsaParameters,
  ClpIDsaKeyParameters,
  ClpAsymmetricKeyParameter;

type
  TDsaKeyParameters = class abstract(TAsymmetricKeyParameter, IDsaKeyParameters)

  strict private
  var
    Fparameters: IDsaParameters;
  strict protected
  function GetParameters: IDsaParameters;
    constructor Create(isPrivate: Boolean; parameters: IDsaParameters);

  public
    function Equals(const other: IDsaKeyParameters): Boolean; reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property parameters: IDsaParameters read GetParameters;

  end;

implementation

{ TDsaKeyParameters }

constructor TDsaKeyParameters.Create(isPrivate: Boolean;
  parameters: IDsaParameters);
begin
  Inherited Create(isPrivate);
  // Note: parameters may be Nil
  Fparameters := parameters;
end;

function TDsaKeyParameters.Equals(const other: IDsaKeyParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDsaKeyParameters) = other) then
  begin
    result := True;
    Exit;
  end;
  result := (parameters as TObject).Equals(other.parameters as TObject) and
    (Inherited Equals(other));
end;

function TDsaKeyParameters.GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Inherited GetHashCode();

  if (parameters <> Nil) then
  begin
    result := result xor parameters.GetHashCode();
  end;

end;

function TDsaKeyParameters.GetParameters: IDsaParameters;
begin
  result := Fparameters;
end;

end.
