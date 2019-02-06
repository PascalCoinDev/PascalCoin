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

unit ClpFiniteFields;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpCryptoLibTypes,
  ClpPrimeField,
  ClpGF2Polynomial,
  ClpIGF2Polynomial,
  ClpGenericPolynomialExtensionField,
  ClpIPolynomialExtensionField,
  ClpIFiniteField;

resourcestring
  SInvalidCharacteristic = 'Must be >= 2 , " characteristic "';
  SUnConstantTerm =
    'Irreducible polynomials in GF(2) must have constant term, "exponents"';
  SPolynomialError =
    'Polynomial Exponents must be montonically increasing", "exponents"';

type
  TFiniteFields = class abstract(TObject)

  strict private
    class var

      FGF_2, FGF_3: IFiniteField;

    class procedure Boot(); static;
    class constructor FiniteFields();

  public
    class function GetBinaryExtensionField(const exponents
      : TCryptoLibInt32Array): IPolynomialExtensionField; static;

    class function GetPrimeField(const characteristic: TBigInteger)
      : IFiniteField; static;
  end;

implementation

{ TFiniteFields }

class procedure TFiniteFields.Boot;
begin

  FGF_2 := TPrimeField.Create(TBigInteger.ValueOf(2));
  FGF_3 := TPrimeField.Create(TBigInteger.ValueOf(3));
end;

class constructor TFiniteFields.FiniteFields;
begin
  TFiniteFields.Boot;
end;

class function TFiniteFields.GetBinaryExtensionField(const exponents
  : TCryptoLibInt32Array): IPolynomialExtensionField;
var
  i: Int32;
begin
  if (exponents[0] <> 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SUnConstantTerm);
  end;

  for i := 1 to System.Pred(System.Length(exponents)) do

  begin
    if (exponents[i] <= exponents[i - 1]) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SPolynomialError);
    end;
  end;

  Result := TGenericPolynomialExtensionField.Create(FGF_2,
    TGF2Polynomial.Create(exponents) as IGF2Polynomial);
end;

class function TFiniteFields.GetPrimeField(const characteristic: TBigInteger)
  : IFiniteField;
var
  bitLength: Int32;
begin
  bitLength := characteristic.bitLength;
  if ((characteristic.SignValue <= 0) or (bitLength < 2)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidCharacteristic);
  end;

  if (bitLength < 3) then
  begin
    case characteristic.Int32Value of
      2:
        begin
          Result := FGF_2;
          Exit;
        end;

      3:
        begin
          Result := FGF_3;
          Exit;
        end;
    end;

  end;

  Result := TPrimeField.Create(characteristic);

end;

end.
