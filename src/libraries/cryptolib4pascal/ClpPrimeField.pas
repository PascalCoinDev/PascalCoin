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

unit ClpPrimeField;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIFiniteField,
  ClpIPrimeField;

type
  TPrimeField = class(TInterfacedObject, IFiniteField, IPrimeField)

  strict private
    function GetCharacteristic: TBigInteger; virtual;
    function GetDimension: Int32; virtual;

  strict protected
  var
    Fcharacteristic: TBigInteger;

  public
    constructor Create(const characteristic: TBigInteger);

    function Equals(other: TObject): Boolean; overload; override;
    function Equals(const other: IPrimeField): Boolean; reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property characteristic: TBigInteger read GetCharacteristic;
    property Dimension: Int32 read GetDimension;

  end;

implementation

{ TPrimeField }

constructor TPrimeField.Create(const characteristic: TBigInteger);
begin
  Fcharacteristic := characteristic;
end;

function TPrimeField.Equals(const other: IPrimeField): Boolean;
begin
  if ((Self as IPrimeField) = other) then
  begin
    Result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    Result := false;
    Exit;
  end;
  Result := Fcharacteristic.Equals(other.characteristic);
end;

function TPrimeField.Equals(other: TObject): Boolean;
begin
  Result := Self.Equals((other as TPrimeField) as IPrimeField);
end;

function TPrimeField.GetCharacteristic: TBigInteger;
begin
  Result := Fcharacteristic;
end;

function TPrimeField.GetDimension: Int32;
begin
  Result := 1;
end;

function TPrimeField.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  Result := Fcharacteristic.GetHashCode();
end;

end.
