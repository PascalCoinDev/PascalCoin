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

unit ClpGenericPolynomialExtensionField;

{$I CryptoLib.inc}

interface

uses
  ClpBits,
  ClpBigInteger,
  ClpIFiniteField,
  ClpIPolynomial,
  ClpIGenericPolynomialExtensionField,
  ClpIPolynomialExtensionField;

type
  TGenericPolynomialExtensionField = class(TInterfacedObject,
    IPolynomialExtensionField, IGenericPolynomialExtensionField)

  strict private
    function GetCharacteristic: TBigInteger; virtual;
    function GetDegree: Int32; virtual;
    function GetDimension: Int32; virtual;
    function GetMinimalPolynomial: IPolynomial; virtual;
    function GetSubField: IFiniteField; virtual;
  strict protected
  var
    Fsubfield: IFiniteField;
    FminimalPolynomial: IPolynomial;

    property Characteristic: TBigInteger read GetCharacteristic;
    property Dimension: Int32 read GetDimension;
    property subfield: IFiniteField read GetSubField;
    property Degree: Int32 read GetDegree;
    property MinimalPolynomial: IPolynomial read GetMinimalPolynomial;

  public
    constructor Create(const subfield: IFiniteField;
      const polynomial: IPolynomial);

    function Equals(other: TObject): Boolean; overload; override;
    function Equals(const other: IGenericPolynomialExtensionField): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TGenericPolynomialExtensionField }

constructor TGenericPolynomialExtensionField.Create(const subfield
  : IFiniteField; const polynomial: IPolynomial);
begin
  Fsubfield := subfield;
  FminimalPolynomial := polynomial;
end;

function TGenericPolynomialExtensionField.Equals(const other
  : IGenericPolynomialExtensionField): Boolean;
begin
  if ((Self as IGenericPolynomialExtensionField) = other) then
  begin
    Result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    Result := false;
    Exit;
  end;
  Result := (subfield as TObject).Equals(other.subfield as TObject) and
    (MinimalPolynomial as TObject).Equals(other.MinimalPolynomial as TObject);
end;

function TGenericPolynomialExtensionField.Equals(other: TObject): Boolean;
begin
  Result := Self.Equals((other as TGenericPolynomialExtensionField)
    as IGenericPolynomialExtensionField);
end;

function TGenericPolynomialExtensionField.GetCharacteristic: TBigInteger;
begin
  Result := Fsubfield.Characteristic;
end;

function TGenericPolynomialExtensionField.GetDegree: Int32;
begin
  Result := FminimalPolynomial.Degree;
end;

function TGenericPolynomialExtensionField.GetDimension: Int32;
begin
  Result := Fsubfield.Dimension * FminimalPolynomial.Degree;
end;

function TGenericPolynomialExtensionField.GetHashCode: {$IFDEF DELPHI}Int32;
{$ELSE}PtrInt; {$ENDIF DELPHI}
begin
  Result := (subfield as TObject).GetHashCode()
    xor Int32(TBits.RotateLeft32((MinimalPolynomial as TObject)
    .GetHashCode(), 16));
end;

function TGenericPolynomialExtensionField.GetMinimalPolynomial: IPolynomial;
begin
  Result := FminimalPolynomial;
end;

function TGenericPolynomialExtensionField.GetSubField: IFiniteField;
begin
  Result := Fsubfield;
end;

end.
