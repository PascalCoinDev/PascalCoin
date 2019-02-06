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

unit ClpAsymmetricKeyParameter;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpIAsymmetricKeyParameter;

type
  TAsymmetricKeyParameter = class abstract(TInterfacedObject,
    IAsymmetricKeyParameter, ICipherParameters)

  strict private
  var
    FprivateKey: Boolean;

  strict protected
    function GetPrivateKey: Boolean; inline;
    function GetIsPrivate: Boolean; inline;

    constructor Create(privateKey: Boolean);

  public
    property IsPrivate: Boolean read GetIsPrivate;
    property privateKey: Boolean read GetPrivateKey;
    function Equals(const other: IAsymmetricKeyParameter): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TAsymmetricKeyParameter }

constructor TAsymmetricKeyParameter.Create(privateKey: Boolean);
begin
  FprivateKey := privateKey;
end;

function TAsymmetricKeyParameter.Equals(const other
  : IAsymmetricKeyParameter): Boolean;
begin
  if (other = Nil) then
  begin
    Result := false;
    Exit;
  end;
  Result := FprivateKey = other.privateKey;
end;

function TAsymmetricKeyParameter.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  Result := Ord(FprivateKey);
end;

function TAsymmetricKeyParameter.GetIsPrivate: Boolean;
begin
  Result := FprivateKey;
end;

function TAsymmetricKeyParameter.GetPrivateKey: Boolean;
begin
  Result := FprivateKey;
end;

end.
