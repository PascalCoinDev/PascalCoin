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

unit ClpGF2Polynomial;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpArrayUtils,
  ClpIGF2Polynomial,
  ClpIPolynomial;

type
  TGF2Polynomial = class(TInterfacedObject, IPolynomial, IGF2Polynomial)

  strict private
    function GetDegree: Int32; virtual;
    function GetExponents: TCryptoLibInt32Array; inline;
  strict protected
  var
    Fexponents: TCryptoLibInt32Array;

  public
    constructor Create(const exponents: TCryptoLibInt32Array);

    function GetExponentsPresent(): TCryptoLibInt32Array; virtual;

    function Equals(other: TObject): Boolean; overload; override;
    function Equals(const other: IGF2Polynomial): Boolean; reintroduce;
      overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property Degree: Int32 read GetDegree;

    property exponents: TCryptoLibInt32Array read GetExponents;

  end;

implementation

{ TGF2Polynomial }

constructor TGF2Polynomial.Create(const exponents: TCryptoLibInt32Array);
begin
  Fexponents := System.Copy(exponents);
end;

function TGF2Polynomial.Equals(const other: IGF2Polynomial): Boolean;
begin

  if ((Self as IGF2Polynomial) = other) then
  begin
    Result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    Result := false;
    Exit;
  end;
  Result := TArrayUtils.AreEqual(Fexponents, other.exponents);
end;

function TGF2Polynomial.Equals(other: TObject): Boolean;
begin
  Result := Self.Equals((other as TGF2Polynomial) as IGF2Polynomial);
end;

function TGF2Polynomial.GetDegree: Int32;
begin
  Result := Fexponents[System.Length(Fexponents) - 1];
end;

function TGF2Polynomial.GetExponents: TCryptoLibInt32Array;
begin
  Result := Fexponents;
end;

function TGF2Polynomial.GetExponentsPresent: TCryptoLibInt32Array;
begin
  Result := System.Copy(Fexponents);
end;

function TGF2Polynomial.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  Result := TArrayUtils.GetArrayHashCode(Fexponents);
end;

end.
