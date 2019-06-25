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

unit ClpDHKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHParameters,
  ClpIDHKeyParameters,
  ClpPkcsObjectIdentifiers,
  ClpIAsn1Objects,
  ClpAsymmetricKeyParameter;

type
  TDHKeyParameters = class abstract(TAsymmetricKeyParameter, IDHKeyParameters)

  strict private
  var
    Fparameters: IDHParameters;
    FalgorithmOid: IDerObjectIdentifier;
  strict protected
    function GetParameters: IDHParameters;
    function GetAlgorithmOid: IDerObjectIdentifier;

    constructor Create(isPrivate: Boolean;
      const parameters: IDHParameters); overload;

    constructor Create(isPrivate: Boolean; const parameters: IDHParameters;
      const algorithmOid: IDerObjectIdentifier); overload;

  public
    function Equals(const other: IDHKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property parameters: IDHParameters read GetParameters;
    property algorithmOid: IDerObjectIdentifier read GetAlgorithmOid;

  end;

implementation

{ TDHKeyParameters }

function TDHKeyParameters.GetParameters: IDHParameters;
begin
  result := Fparameters;
end;

function TDHKeyParameters.GetAlgorithmOid: IDerObjectIdentifier;
begin
  result := FalgorithmOid;
end;

constructor TDHKeyParameters.Create(isPrivate: Boolean;
  const parameters: IDHParameters);
begin
  Create(isPrivate, parameters, TPkcsObjectIdentifiers.DhKeyAgreement);
end;

constructor TDHKeyParameters.Create(isPrivate: Boolean;
  const parameters: IDHParameters; const algorithmOid: IDerObjectIdentifier);
begin
  Inherited Create(isPrivate);
  // TODO Should we allow parameters to be null?
  Fparameters := parameters;
  FalgorithmOid := algorithmOid;
end;

function TDHKeyParameters.Equals(const other: IDHKeyParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDHKeyParameters) = other) then
  begin
    result := True;
    Exit;
  end;

  result := parameters.Equals(other.parameters) and (Inherited Equals(other));
end;

function TDHKeyParameters.GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Inherited GetHashCode();

  if (parameters <> Nil) then
  begin
    result := result xor parameters.GetHashCode();
  end;
end;

end.
