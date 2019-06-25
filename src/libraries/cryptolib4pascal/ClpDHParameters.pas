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

unit ClpDHParameters;

{$I CryptoLib.inc}

interface

uses
  Math,
  ClpICipherParameters,
  ClpIDHParameters,
  ClpIDHValidationParameters,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SPUnInitialized = '"P" Cannot Be Uninitialized';
  SGUnInitialized = '"G" Cannot Be Uninitialized';
  SMustBeOddPrime = 'Field must be an Odd Prime, "P"';
  SInvalidGeneratorRange = 'Generator must in the Range [2, p - 2], "G"';
  SQTooBigToBeAFactor = 'Q too Big to be a Factor of (P - 1), "Q"';
  SMTooBig = 'M value must be < BitLength of P, "M"';
  SLErrorOne = 'when L value specified, it must be less than bitlength(P), "L"';
  SLErrorTwo = 'when L value specified, it may not be less than m value, "L"';
  SInvalidSubGroupFactor = 'Subgroup factor must be >= 2, "j"';

type
  TDHParameters = class(TInterfacedObject, ICipherParameters, IDHParameters)

  strict private

  const
    DefaultMinimumLength = Int32(160);

  var
    Fp, Fq, Fg, Fj: TBigInteger;
    Fm, Fl: Int32;
    Fvalidation: IDHValidationParameters;

    function GetG: TBigInteger; inline;
    function GetP: TBigInteger; inline;
    function GetQ: TBigInteger; inline;
    function GetJ: TBigInteger; inline;
    function GetM: Int32; inline;
    function GetL: Int32; inline;
    function GetValidationParameters: IDHValidationParameters; inline;

    class function GetDefaultMParam(lParam: Int32): Int32; static; inline;

  public

    constructor Create(const p, g: TBigInteger); overload;

    constructor Create(const p, g, q: TBigInteger); overload;

    constructor Create(const p, g, q: TBigInteger; l: Int32); overload;

    constructor Create(const p, g, q: TBigInteger; m, l: Int32); overload;

    constructor Create(const p, g, q, j: TBigInteger;
      const validation: IDHValidationParameters); overload;

    constructor Create(const p, g, q: TBigInteger; m, l: Int32;
      const j: TBigInteger; const validation: IDHValidationParameters);
      overload;

    function Equals(const other: IDHParameters): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property p: TBigInteger read GetP;
    property q: TBigInteger read GetQ;
    property g: TBigInteger read GetG;
    property j: TBigInteger read GetJ;
    /// <summary>The minimum bitlength of the private value.</summary>
    property m: Int32 read GetM;
    /// <summary>The bitlength of the private value.</summary>
    property l: Int32 read GetL;
    property ValidationParameters: IDHValidationParameters
      read GetValidationParameters;

  end;

implementation

{ TDHParameters }

function TDHParameters.GetL: Int32;
begin
  result := Fl;
end;

function TDHParameters.GetM: Int32;
begin
  result := Fm;
end;

function TDHParameters.GetJ: TBigInteger;
begin
  result := Fj;
end;

function TDHParameters.GetP: TBigInteger;
begin
  result := Fp;
end;

function TDHParameters.GetQ: TBigInteger;
begin
  result := Fq;
end;

function TDHParameters.GetG: TBigInteger;
begin
  result := Fg;
end;

class function TDHParameters.GetDefaultMParam(lParam: Int32): Int32;
begin
  if (lParam = 0) then
  begin
    result := DefaultMinimumLength;
    Exit;
  end;

  result := Min(lParam, DefaultMinimumLength);
end;

constructor TDHParameters.Create(const p, g: TBigInteger);
begin
  Create(p, g, Default (TBigInteger), 0);
end;

constructor TDHParameters.Create(const p, g, q: TBigInteger);
begin
  Create(p, g, q, 0);
end;

constructor TDHParameters.Create(const p, g, q: TBigInteger; l: Int32);
begin
  Create(p, g, q, GetDefaultMParam(l), l, Default (TBigInteger), Nil);
end;

constructor TDHParameters.Create(const p, g, q: TBigInteger; m, l: Int32);
begin
  Create(p, g, q, m, l, Default (TBigInteger), Nil);
end;

constructor TDHParameters.Create(const p, g, q, j: TBigInteger;
  const validation: IDHValidationParameters);
begin
  Create(p, g, q, DefaultMinimumLength, 0, j, validation)
end;

constructor TDHParameters.Create(const p, g, q: TBigInteger; m, l: Int32;
  const j: TBigInteger; const validation: IDHValidationParameters);
begin
  Inherited Create();
  if (not(p.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SPUnInitialized);
  end;

  if (not(g.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SGUnInitialized);
  end;

  if (not p.TestBit(0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SMustBeOddPrime);
  end;

  if ((g.CompareTo(TBigInteger.Two) < 0) or
    (g.CompareTo(p.Subtract(TBigInteger.Two)) > 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidGeneratorRange);
  end;

  if ((q.IsInitialized) and (q.BitLength >= p.BitLength)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SQTooBigToBeAFactor);
  end;

  if (m >= p.BitLength) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SMTooBig);
  end;

  if (l <> 0) then
  begin

    if (l >= p.BitLength) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SLErrorOne);
    end;
    if (l < m) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SLErrorTwo);
    end;
  end;

  if ((j.IsInitialized) and (j.CompareTo(TBigInteger.Two) < 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidSubGroupFactor);
  end;

  // TODO If q, j both provided, validate p = jq + 1 ?

  Fp := p;
  Fg := g;
  Fq := q;
  Fm := m;
  Fl := l;
  Fj := j;
  Fvalidation := validation;
end;

function TDHParameters.Equals(const other: IDHParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDHParameters) = other) then
  begin
    result := True;
    Exit;
  end;
  result := p.Equals(other.p) and q.Equals(other.q) and g.Equals(other.g);
end;

function TDHParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := p.GetHashCode() xor g.GetHashCode();

  if Fq.IsInitialized then
  begin
    result := result xor q.GetHashCode();
  end;
end;

function TDHParameters.GetValidationParameters: IDHValidationParameters;
begin
  result := Fvalidation;
end;

end.
