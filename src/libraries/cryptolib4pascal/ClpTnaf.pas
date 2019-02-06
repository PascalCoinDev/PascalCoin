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

unit ClpTnaf;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIZTauElement,
  ClpZTauElement,
  ClpIECC,
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpSimpleBigDecimal;

resourcestring
  SInvalidMU = 'mu must be 1 or -1';
  SDifferentScales = 'lambda0 and lambda1 do not have same scale';
  SNoKoblitzCurve = 'No Koblitz curve (ABC), TNAF multiplication not possible';
  SNotKoblitzCurve = 'si is defined for Koblitz curves only';
  SInvalidCoFactor = 'h (Cofactor) must be 2 or 4';

type
  /// **
  // * Class holding methods for point multiplication based on the window
  // * &#964;-adic nonadjacent form (WTNAF). The algorithms are based on the
  // * paper "Improved Algorithms for Arithmetic on Anomalous Binary Curves"
  // * by Jerome A. Solinas. The paper first appeared in the Proceedings of
  // * Crypto 1997.
  // */
  TTnaf = class sealed(TObject)

  strict private
  class var

    FMinusOne, FMinusTwo, FMinusThree, FFour: TBigInteger;
    FAlpha0, FAlpha1: TCryptoLibGenericArray<IZTauElement>;
    FAlpha0Tnaf, FAlpha1Tnaf: TCryptoLibMatrixShortIntArray;

    class function GetAlpha0: TCryptoLibGenericArray<IZTauElement>;
      static; inline;
    class function GetAlpha0Tnaf: TCryptoLibMatrixShortIntArray; static; inline;
    class function GetAlpha1Tnaf: TCryptoLibMatrixShortIntArray; static; inline;
    class function GetAlpha1: TCryptoLibGenericArray<IZTauElement>;
      static; inline;

    class function GetShiftsForCofactor(const h: TBigInteger): Int32;
      static; inline;

    class procedure Boot(); static;
    class constructor Tnaf();

  public

  // /**
  // * The window width of WTNAF. The standard value of 4 is slightly less
  // * than optimal for running time, but keeps space requirements for
  // * precomputation low. For typical curves, a value of 5 or 6 results in
  // * a better running time. When changing this value, the
  // * <code>&#945;<sub>u</sub></code>'s must be computed differently, see
  // * e.g. "Guide to Elliptic Curve Cryptography", Darrel Hankerson,
  // * Alfred Menezes, Scott Vanstone, Springer-Verlag New York Inc., 2004,
  // * p. 121-122
  // */
    const
    Width = ShortInt(4);

    // /**
    // * 2<sup>4</sup>
    // */
    Pow2Width = ShortInt(16);
    // /**
    // * Computes the norm of an element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code>.
    // * @param mu The parameter <code>&#956;</code> of the elliptic curve.
    // * @param lambda The element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code>.
    // * @return The norm of <code>&#955;</code>.
    // */
    class function Norm(mu: ShortInt; const lambda: IZTauElement): TBigInteger;
      overload; static; inline;
    // /**
    // * Computes the norm of an element <code>&#955;</code> of
    // * <code><b>R</b>[&#964;]</code>, where <code>&#955; = u + v&#964;</code>
    // * and <code>u</code> and <code>u</code> are real numbers (elements of
    // * <code><b>R</b></code>).
    // * @param mu The parameter <code>&#956;</code> of the elliptic curve.
    // * @param u The real part of the element <code>&#955;</code> of
    // * <code><b>R</b>[&#964;]</code>.
    // * @param v The <code>&#964;</code>-adic part of the element
    // * <code>&#955;</code> of <code><b>R</b>[&#964;]</code>.
    // * @return The norm of <code>&#955;</code>.
    // */
    class function Norm(mu: ShortInt; const u, v: TSimpleBigDecimal)
      : TSimpleBigDecimal; overload; static; inline;
    // /**
    // * Rounds an element <code>&#955;</code> of <code><b>R</b>[&#964;]</code>
    // * to an element of <code><b>Z</b>[&#964;]</code>, such that their difference
    // * has minimal norm. <code>&#955;</code> is given as
    // * <code>&#955; = &#955;<sub>0</sub> + &#955;<sub>1</sub>&#964;</code>.
    // * @param lambda0 The component <code>&#955;<sub>0</sub></code>.
    // * @param lambda1 The component <code>&#955;<sub>1</sub></code>.
    // * @param mu The parameter <code>&#956;</code> of the elliptic curve. Must
    // * equal 1 or -1.
    // * @return The rounded element of <code><b>Z</b>[&#964;]</code>.
    // * @throws ArgumentException if <code>lambda0</code> and
    // * <code>lambda1</code> do not have same scale.
    // */
    class function Round(const lambda0, lambda1: TSimpleBigDecimal;
      mu: ShortInt): IZTauElement; static;
    // /**
    // * Approximate division by <code>n</code>. For an integer
    // * <code>k</code>, the value <code>&#955; = s k / n</code> is
    // * computed to <code>c</code> bits of accuracy.
    // * @param k The parameter <code>k</code>.
    // * @param s The curve parameter <code>s<sub>0</sub></code> or
    // * <code>s<sub>1</sub></code>.
    // * @param vm The Lucas Sequence element <code>V<sub>m</sub></code>.
    // * @param a The parameter <code>a</code> of the elliptic curve.
    // * @param m The bit length of the finite field
    // * <code><b>F</b><sub>m</sub></code>.
    // * @param c The number of bits of accuracy, i.e. the scale of the returned
    // * <code>SimpleBigDecimal</code>.
    // * @return The value <code>&#955; = s k / n</code> computed to
    // * <code>c</code> bits of accuracy.
    // */
    class function ApproximateDivisionByN(const k, s, vm: TBigInteger;
      a: ShortInt; m, c: Int32): TSimpleBigDecimal; static; inline;
    // /**
    // * Computes the <code>&#964;</code>-adic NAF (non-adjacent form) of an
    // * element <code>&#955;</code> of <code><b>Z</b>[&#964;]</code>.
    // * @param mu The parameter <code>&#956;</code> of the elliptic curve.
    // * @param lambda The element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code>.
    // * @return The <code>&#964;</code>-adic NAF of <code>&#955;</code>.
    // */
    class function TauAdicNaf(mu: ShortInt; const lambda: IZTauElement)
      : TCryptoLibShortIntArray; static;
    // /**
    // * Applies the operation <code>&#964;()</code> to an
    // * <code>AbstractF2mPoint</code>.
    // * @param p The AbstractF2mPoint to which <code>&#964;()</code> is applied.
    // * @return <code>&#964;(p)</code>
    // */
    class function Tau(const p: IAbstractF2mPoint): IAbstractF2mPoint;
      static; inline;
    // /**
    // * Returns the parameter <code>&#956;</code> of the elliptic curve.
    // * @param curve The elliptic curve from which to obtain <code>&#956;</code>.
    // * The curve must be a Koblitz curve, i.e. <code>a</code> Equals
    // * <code>0</code> or <code>1</code> and <code>b</code> Equals
    // * <code>1</code>.
    // * @return <code>&#956;</code> of the elliptic curve.
    // * @throws ArgumentException if the given ECCurve is not a Koblitz
    // * curve.
    // */
    class function GetMu(const curve: IAbstractF2mCurve): ShortInt; overload;
      static; inline;

    class function GetMu(const curveA: IECFieldElement): ShortInt; overload;
      static; inline;

    class function GetMu(curveA: Int32): ShortInt; overload; static; inline;
    // /**
    // * Calculates the Lucas Sequence elements <code>U<sub>k-1</sub></code> and
    // * <code>U<sub>k</sub></code> or <code>V<sub>k-1</sub></code> and
    // * <code>V<sub>k</sub></code>.
    // * @param mu The parameter <code>&#956;</code> of the elliptic curve.
    // * @param k The index of the second element of the Lucas Sequence to be
    // * returned.
    // * @param doV If set to true, computes <code>V<sub>k-1</sub></code> and
    // * <code>V<sub>k</sub></code>, otherwise <code>U<sub>k-1</sub></code> and
    // * <code>U<sub>k</sub></code>.
    // * @return An array with 2 elements, containing <code>U<sub>k-1</sub></code>
    // * and <code>U<sub>k</sub></code> or <code>V<sub>k-1</sub></code>
    // * and <code>V<sub>k</sub></code>.
    // */
    class function GetLucas(mu: ShortInt; k: Int32; doV: Boolean)
      : TCryptoLibGenericArray<TBigInteger>; static;
    // /**
    // * Computes the auxiliary value <code>t<sub>w</sub></code>. If the width is
    // * 4, then for <code>mu = 1</code>, <code>t<sub>w</sub> = 6</code> and for
    // * <code>mu = -1</code>, <code>t<sub>w</sub> = 10</code>
    // * @param mu The parameter <code>&#956;</code> of the elliptic curve.
    // * @param w The window width of the WTNAF.
    // * @return the auxiliary value <code>t<sub>w</sub></code>
    // */
    class function GetTw(mu: ShortInt; w: Int32): TBigInteger; static;
    // /**
    // * Computes the auxiliary values <code>s<sub>0</sub></code> and
    // * <code>s<sub>1</sub></code> used for partial modular reduction.
    // * @param curve The elliptic curve for which to compute
    // * <code>s<sub>0</sub></code> and <code>s<sub>1</sub></code>.
    // * @throws ArgumentException if <code>curve</code> is not a
    // * Koblitz curve (Anomalous Binary Curve, ABC).
    // */
    class function GetSi(const curve: IAbstractF2mCurve)
      : TCryptoLibGenericArray<TBigInteger>; overload; static;

    class function GetSi(fieldSize, curveA: Int32; const cofactor: TBigInteger)
      : TCryptoLibGenericArray<TBigInteger>; overload; static;

    // /**
    // * Partial modular reduction modulo
    // * <code>(&#964;<sup>m</sup> - 1)/(&#964; - 1)</code>.
    // * @param k The integer to be reduced.
    // * @param m The bitlength of the underlying finite field.
    // * @param a The parameter <code>a</code> of the elliptic curve.
    // * @param s The auxiliary values <code>s<sub>0</sub></code> and
    // * <code>s<sub>1</sub></code>.
    // * @param mu The parameter &#956; of the elliptic curve.
    // * @param c The precision (number of bits of accuracy) of the partial
    // * modular reduction.
    // * @return <code>&#961; := k partmod (&#964;<sup>m</sup> - 1)/(&#964; - 1)</code>
    // */
    class function PartModReduction(const k: TBigInteger; m: Int32; a: ShortInt;
      const s: TCryptoLibGenericArray<TBigInteger>; mu, c: ShortInt)
      : IZTauElement; static;

    // /**
    // * Multiplies an AbstractF2mPoint
    // * by an element <code>&#955;</code> of <code><b>Z</b>[&#964;]</code>
    // * using the <code>&#964;</code>-adic NAF (TNAF) method.
    // * @param p The AbstractF2mPoint to Multiply.
    // * @param lambda The element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code>.
    // * @return <code>&#955; * p</code>
    // */
    class function MultiplyTnaf(const p: IAbstractF2mPoint;
      const lambda: IZTauElement): IAbstractF2mPoint; static; inline;

    // /**
    // * Multiplies an AbstractF2mPoint
    // * by an element <code>&#955;</code> of <code><b>Z</b>[&#964;]</code>
    // * using the <code>&#964;</code>-adic NAF (TNAF) method, given the TNAF
    // * of <code>&#955;</code>.
    // * @param p The AbstractF2mPoint to Multiply.
    // * @param u The the TNAF of <code>&#955;</code>..
    // * @return <code>&#955; * p</code>
    // */
    class function MultiplyFromTnaf(const p: IAbstractF2mPoint;
      const u: TCryptoLibShortIntArray): IAbstractF2mPoint; static;

    // /**
    // * Computes the <code>[&#964;]</code>-adic window NAF of an element
    // * <code>&#955;</code> of <code><b>Z</b>[&#964;]</code>.
    // * @param mu The parameter &#956; of the elliptic curve.
    // * @param lambda The element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code> of which to compute the
    // * <code>[&#964;]</code>-adic NAF.
    // * @param width The window width of the resulting WNAF.
    // * @param pow2w 2<sup>width</sup>.
    // * @param tw The auxiliary value <code>t<sub>w</sub></code>.
    // * @param alpha The <code>&#945;<sub>u</sub></code>'s for the window width.
    // * @return The <code>[&#964;]</code>-adic window NAF of
    // * <code>&#955;</code>.
    // */
    class function TauAdicWNaf(mu: ShortInt; const lambda: IZTauElement;
      Width: ShortInt; const pow2w, tw: TBigInteger;
      const alpha: TCryptoLibGenericArray<IZTauElement>)
      : TCryptoLibShortIntArray; static;

    // /**
    // * Does the precomputation for WTNAF multiplication.
    // * @param p The <code>ECPoint</code> for which to do the precomputation.
    // * @param a The parameter <code>a</code> of the elliptic curve.
    // * @return The precomputation array for <code>p</code>.
    // */
    class function GetPreComp(const p: IAbstractF2mPoint; a: ShortInt)
      : TCryptoLibGenericArray<IAbstractF2mPoint>; static;

    // /**
    // * Multiplies an AbstractF2mPoint
    // * by a <code>BigInteger</code> using the reduced <code>&#964;</code>-adic
    // * NAF (RTNAF) method.
    // * @param p The AbstractF2mPoint to Multiply.
    // * @param k The <code>BigInteger</code> by which to Multiply <code>p</code>.
    // * @return <code>k * p</code>
    // */
    class function MultiplyRTnaf(const p: IAbstractF2mPoint;
      const k: TBigInteger): IAbstractF2mPoint; static; inline;

    // /**
    // * The <code>&#945;<sub>u</sub></code>'s for <code>a=0</code> as an array
    // * of <code>ZTauElement</code>s.
    // */
    class property Alpha0: TCryptoLibGenericArray<IZTauElement> read GetAlpha0;
    // /**
    // * The <code>&#945;<sub>u</sub></code>'s for <code>a=1</code> as an array
    // * of <code>ZTauElement</code>s.
    // */
    class property Alpha1: TCryptoLibGenericArray<IZTauElement> read GetAlpha1;
    // /**
    // * The <code>&#945;<sub>u</sub></code>'s for <code>a=0</code> as an array
    // * of TNAFs.
    // */
    class property Alpha0Tnaf: TCryptoLibMatrixShortIntArray read GetAlpha0Tnaf;

    // /**
    // * The <code>&#945;<sub>u</sub></code>'s for <code>a=1</code> as an array
    // * of TNAFs.
    // */
    class property Alpha1Tnaf: TCryptoLibMatrixShortIntArray read GetAlpha1Tnaf;

  end;

implementation

{ TTnaf }

class function TTnaf.ApproximateDivisionByN(const k, s, vm: TBigInteger;
  a: ShortInt; m, c: Int32): TSimpleBigDecimal;
var
  _k: Int32;
  ns, gs, hs, js, gsPlusJs, ls: TBigInteger;
begin
  _k := ((m + 5) shr 1) + c;
  ns := k.ShiftRight(m - _k - 2 + a);

  gs := s.Multiply(ns);

  hs := gs.ShiftRight(m);

  js := vm.Multiply(hs);

  gsPlusJs := gs.Add(js);
  ls := gsPlusJs.ShiftRight(_k - c);
  if (gsPlusJs.TestBit(_k - c - 1)) then
  begin
    // round up
    ls := ls.Add(TBigInteger.One);
  end;

  Result := TSimpleBigDecimal.Create(ls, c);
end;

class procedure TTnaf.Boot;
begin

  FMinusOne := TBigInteger.One.Negate();
  FMinusTwo := TBigInteger.Two.Negate();
  FMinusThree := TBigInteger.Three.Negate();
  FFour := TBigInteger.ValueOf(4);
  FAlpha0 := TCryptoLibGenericArray<IZTauElement>.Create(Nil,
    TZTauElement.Create(TBigInteger.One, TBigInteger.Zero), Nil,
    TZTauElement.Create(FMinusThree, FMinusOne), Nil,
    TZTauElement.Create(FMinusOne, FMinusOne), Nil,
    TZTauElement.Create(TBigInteger.One, FMinusOne), Nil);

  FAlpha1 := TCryptoLibGenericArray<IZTauElement>.Create(Nil,
    TZTauElement.Create(TBigInteger.One, TBigInteger.Zero), Nil,
    TZTauElement.Create(FMinusThree, TBigInteger.One), Nil,
    TZTauElement.Create(FMinusOne, TBigInteger.One), Nil,
    TZTauElement.Create(TBigInteger.One, TBigInteger.One), Nil);

  FAlpha0Tnaf := TCryptoLibMatrixShortIntArray.Create(Nil,
    TCryptoLibShortIntArray.Create(1), Nil, TCryptoLibShortIntArray.Create(-1,
    0, 1), Nil, TCryptoLibShortIntArray.Create(1, 0, 1), Nil,
    TCryptoLibShortIntArray.Create(-1, 0, 0, 1));

  FAlpha1Tnaf := TCryptoLibMatrixShortIntArray.Create(Nil,
    TCryptoLibShortIntArray.Create(1), Nil, TCryptoLibShortIntArray.Create(-1,
    0, 1), Nil, TCryptoLibShortIntArray.Create(1, 0, 1), Nil,
    TCryptoLibShortIntArray.Create(-1, 0, 0, -1));
end;

class function TTnaf.GetAlpha0: TCryptoLibGenericArray<IZTauElement>;
begin
  Result := FAlpha0;
end;

class function TTnaf.GetAlpha0Tnaf: TCryptoLibMatrixShortIntArray;
begin
  Result := FAlpha0Tnaf;
end;

class function TTnaf.GetAlpha1Tnaf: TCryptoLibMatrixShortIntArray;
begin
  Result := FAlpha1Tnaf;
end;

class function TTnaf.GetAlpha1: TCryptoLibGenericArray<IZTauElement>;
begin
  Result := FAlpha1;
end;

class function TTnaf.GetLucas(mu: ShortInt; k: Int32; doV: Boolean)
  : TCryptoLibGenericArray<TBigInteger>;
var
  u0, u1, u2, s: TBigInteger;
  i: Int32;
begin

  if (not((mu = 1) or (mu = -1))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidMU);
  end;

  if (doV) then
  begin
    u0 := TBigInteger.Two;
    u1 := TBigInteger.ValueOf(mu);
  end
  else
  begin
    u0 := TBigInteger.Zero;
    u1 := TBigInteger.One;
  end;

  i := 1;

  while i < k do
  begin
    // u2 = mu*u1 - 2*u0;
    s := Default (TBigInteger);
    if (mu = 1) then
    begin
      s := u1;
    end
    else
    begin
      // mu == -1
      s := u1.Negate();
    end;

    u2 := s.Subtract(u0.ShiftLeft(1));
    u0 := u1;
    u1 := u2;
    // System.out.println(i + ": " + u2);
    // System.out.println();
    System.Inc(i);
  end;

  Result := TCryptoLibGenericArray<TBigInteger>.Create(u0, u1);

end;

class function TTnaf.GetMu(curveA: Int32): ShortInt;
begin
  if (curveA = 0) then
  begin
    Result := ShortInt(-1);
  end
  else
  begin
    Result := ShortInt(1);
  end;
end;

class function TTnaf.GetMu(const curve: IAbstractF2mCurve): ShortInt;
var
  a: TBigInteger;
  mu: ShortInt;
begin
  a := curve.a.ToBigInteger();

  if (a.SignValue = 0) then
  begin
    mu := -1;
  end
  else if (a.Equals(TBigInteger.One)) then
  begin
    mu := 1;
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNoKoblitzCurve);
  end;
  Result := mu;
end;

class function TTnaf.GetMu(const curveA: IECFieldElement): ShortInt;
begin
  if curveA.IsZero then
  begin
    Result := ShortInt(-1);
  end
  else
  begin
    Result := ShortInt(1);
  end;
end;

class function TTnaf.GetPreComp(const p: IAbstractF2mPoint; a: ShortInt)
  : TCryptoLibGenericArray<IAbstractF2mPoint>;
var
  alphaTnaf: TCryptoLibMatrixShortIntArray;
  pu: TCryptoLibGenericArray<IAbstractF2mPoint>;
  precompLen, i: UInt32;
  ecPoints: TCryptoLibGenericArray<IECPoint>;
  j: Int32;
begin

  if (a = 0) then
  begin
    alphaTnaf := TTnaf.Alpha0Tnaf;
  end
  else
  begin
    alphaTnaf := TTnaf.Alpha1Tnaf;
  end;

  System.SetLength(pu, UInt32(System.Length(alphaTnaf) + 1) shr 1);

  pu[0] := p;

  precompLen := UInt32(System.Length(alphaTnaf));

  i := 3;
  while i < precompLen do
  begin
    pu[i shr 1] := TTnaf.MultiplyFromTnaf(p, alphaTnaf[i]);
    System.Inc(i, 2);
  end;

  // Since Generic Covariance and Contravariance are not supported in Delphi,
  // We have to manually loop through the array and convert its content to our desired type using an "as, Supports or Cast".
  System.SetLength(ecPoints, System.Length(pu));
  for j := System.Low(pu) to System.High(pu) do
  begin
    ecPoints[j] := pu[j] as IECPoint;
  end;

  p.curve.NormalizeAll(ecPoints);
  // after normalizing, convert back
  for j := System.Low(ecPoints) to System.High(ecPoints) do
  begin
    pu[j] := ecPoints[j] as IAbstractF2mPoint;
  end;

  Result := pu;
end;

class function TTnaf.GetShiftsForCofactor(const h: TBigInteger): Int32;
var
  hi: Int32;
begin
  if ((h.IsInitialized) and (h.BitLength < 4)) then
  begin
    hi := h.Int32Value;
    if (hi = 2) then
    begin
      Result := 1;
      Exit;
    end;
    if (hi = 4) then
    begin
      Result := 2;
      Exit;
    end;
  end;

  raise EArgumentCryptoLibException.CreateRes(@SInvalidCoFactor);
end;

class function TTnaf.GetSi(fieldSize, curveA: Int32;
  const cofactor: TBigInteger): TCryptoLibGenericArray<TBigInteger>;
var
  mu: ShortInt;
  shifts, index: Int32;
  ui: TCryptoLibGenericArray<TBigInteger>;
  dividend0, dividend1: TBigInteger;
begin

  mu := GetMu(curveA);
  shifts := GetShiftsForCofactor(cofactor);
  index := fieldSize + 3 - curveA;
  ui := GetLucas(mu, index, false);

  if (mu = 1) then
  begin
    ui[0] := ui[0].Negate();
    ui[1] := ui[1].Negate();
  end;

  dividend0 := TBigInteger.One.Add(ui[1]).ShiftRight(shifts);
  dividend1 := TBigInteger.One.Add(ui[0]).ShiftRight(shifts).Negate();

  Result := TCryptoLibGenericArray<TBigInteger>.Create(dividend0, dividend1);
end;

class function TTnaf.GetSi(const curve: IAbstractF2mCurve)
  : TCryptoLibGenericArray<TBigInteger>;
var
  m, a, shifts, index: Int32;
  mu: ShortInt;
  ui: TCryptoLibGenericArray<TBigInteger>;
  dividend0, dividend1: TBigInteger;
begin
  if (not curve.IsKoblitz) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNotKoblitzCurve);
  end;

  m := curve.fieldSize;
  a := curve.a.ToBigInteger().Int32Value;
  mu := GetMu(a);
  shifts := GetShiftsForCofactor(curve.cofactor);
  index := m + 3 - a;
  ui := GetLucas(mu, index, false);

  if (mu = 1) then
  begin
    ui[0] := ui[0].Negate();
    ui[1] := ui[1].Negate();
  end;

  dividend0 := TBigInteger.One.Add(ui[1]).ShiftRight(shifts);
  dividend1 := TBigInteger.One.Add(ui[0]).ShiftRight(shifts).Negate();

  Result := TCryptoLibGenericArray<TBigInteger>.Create(dividend0, dividend1);
end;

class function TTnaf.GetTw(mu: ShortInt; w: Int32): TBigInteger;
var
  us: TCryptoLibGenericArray<TBigInteger>;
  twoToW, u1invert: TBigInteger;
begin
  if (w = 4) then
  begin
    if (mu = 1) then
    begin
      Result := TBigInteger.ValueOf(6);
      Exit;
    end;

    // mu == -1
    Result := TBigInteger.ValueOf(10);
    Exit;

  end;

  // For w <> 4, the values must be computed
  us := GetLucas(mu, w, false);
  twoToW := TBigInteger.Zero.SetBit(w);
  u1invert := us[1].ModInverse(twoToW);

  Result := TBigInteger.Two.Multiply(us[0]).Multiply(u1invert).&Mod(twoToW);
  // System.out.println("mu = " + mu);
  // System.out.println("tw = " + tw);
end;

class function TTnaf.MultiplyFromTnaf(const p: IAbstractF2mPoint;
  const u: TCryptoLibShortIntArray): IAbstractF2mPoint;
var
  curve: IECCurve;
  q, pNeg: IAbstractF2mPoint;
  tauCount, i: Int32;
  ui: ShortInt;
  x: IECPoint;
begin
  curve := p.curve;
  q := curve.Infinity as IAbstractF2mPoint;
  pNeg := p.Negate() as IAbstractF2mPoint;
  tauCount := 0;

  i := System.Length(u) - 1;

  while i >= 0 do
  begin
    System.Inc(tauCount);
    ui := u[i];
    if (ui <> 0) then
    begin
      q := q.TauPow(tauCount);
      tauCount := 0;

      if ui > 0 then
      begin
        x := p;
      end
      else
      begin
        x := pNeg;
      end;

      q := q.Add(x) as IAbstractF2mPoint;
    end;
    System.Dec(i);
  end;
  if (tauCount > 0) then
  begin
    q := q.TauPow(tauCount);
  end;
  Result := q;
end;

class function TTnaf.MultiplyTnaf(const p: IAbstractF2mPoint;
  const lambda: IZTauElement): IAbstractF2mPoint;
var
  curve: IAbstractF2mCurve;
  mu: ShortInt;
  u: TCryptoLibShortIntArray;
  q: IAbstractF2mPoint;
begin
  curve := p.curve as IAbstractF2mCurve;
  mu := GetMu(curve.a);
  u := TauAdicNaf(mu, lambda);

  q := MultiplyFromTnaf(p, u);

  Result := q;
end;

class function TTnaf.MultiplyRTnaf(const p: IAbstractF2mPoint;
  const k: TBigInteger): IAbstractF2mPoint;
var
  curve: IAbstractF2mCurve;
  m, a: Int32;
  mu: ShortInt;
  s: TCryptoLibGenericArray<TBigInteger>;
  rho: IZTauElement;
begin
  curve := p.curve as IAbstractF2mCurve;
  m := curve.fieldSize;
  a := curve.a.ToBigInteger().Int32Value;
  mu := GetMu(a);
  s := curve.GetSi();
  rho := PartModReduction(k, m, ShortInt(a), s, mu, ShortInt(10));

  Result := MultiplyTnaf(p, rho);
end;

class function TTnaf.Norm(mu: ShortInt; const lambda: IZTauElement)
  : TBigInteger;
var
  LNorm, s1, s2, s3: TBigInteger;
begin
  // s1 = u^2
  s1 := lambda.u.Multiply(lambda.u);

  // s2 = u * v
  s2 := lambda.u.Multiply(lambda.v);

  // s3 = 2 * v^2
  s3 := lambda.v.Multiply(lambda.v).ShiftLeft(1);

  if (mu = 1) then
  begin
    LNorm := s1.Add(s2).Add(s3);
  end
  else if (mu = -1) then
  begin
    LNorm := s1.Subtract(s2).Add(s3);
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidMU);
  end;

  Result := LNorm;
end;

class function TTnaf.Norm(mu: ShortInt; const u, v: TSimpleBigDecimal)
  : TSimpleBigDecimal;
var
  LNorm, s1, s2, s3: TSimpleBigDecimal;
begin
  // s1 = u^2
  s1 := u.Multiply(u);

  // s2 = u * v
  s2 := u.Multiply(v);

  // s3 = 2 * v^2
  s3 := v.Multiply(v).ShiftLeft(1);

  if (mu = 1) then
  begin
    LNorm := s1.Add(s2).Add(s3);
  end
  else if (mu = -1) then
  begin
    LNorm := s1.Subtract(s2).Add(s3);
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidMU);
  end;

  Result := LNorm;
end;

class function TTnaf.PartModReduction(const k: TBigInteger; m: Int32;
  a: ShortInt; const s: TCryptoLibGenericArray<TBigInteger>; mu, c: ShortInt)
  : IZTauElement;
var
  d0, vm, r0, r1: TBigInteger;
  v: TCryptoLibGenericArray<TBigInteger>;
  lambda0, lambda1: TSimpleBigDecimal;
  q: IZTauElement;
begin
  // d0 = s[0] + mu*s[1]; mu is either 1 or -1

  if (mu = 1) then
  begin
    d0 := s[0].Add(s[1]);
  end
  else
  begin
    d0 := s[0].Subtract(s[1]);
  end;

  v := GetLucas(mu, m, True);
  vm := v[1];

  lambda0 := ApproximateDivisionByN(k, s[0], vm, a, m, c);

  lambda1 := ApproximateDivisionByN(k, s[1], vm, a, m, c);

  q := Round(lambda0, lambda1, mu);

  // r0 = n - d0*q0 - 2*s1*q1
  r0 := k.Subtract(d0.Multiply(q.u))
    .Subtract(TBigInteger.ValueOf(2).Multiply(s[1]).Multiply(q.v));

  // r1 = s1*q0 - s0*q1
  r1 := s[1].Multiply(q.u).Subtract(s[0].Multiply(q.v));

  Result := TZTauElement.Create(r0, r1);
end;

class function TTnaf.Round(const lambda0, lambda1: TSimpleBigDecimal;
  mu: ShortInt): IZTauElement;
var
  threeEta1, fourEta1, check1, check2, eta0, eta1, eta: TSimpleBigDecimal;
  f0, f1, q0, q1: TBigInteger;
  scale: Int32;
  h0, h1: ShortInt;
begin
  scale := lambda0.scale;
  if (lambda1.scale <> scale) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SDifferentScales);
  end;

  if (not((mu = 1) or (mu = -1))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidMU);
  end;

  f0 := lambda0.Round();
  f1 := lambda1.Round();

  eta0 := lambda0.Subtract(f0);
  eta1 := lambda1.Subtract(f1);

  // eta = 2*eta0 + mu*eta1
  eta := eta0.Add(eta0);
  if (mu = 1) then
  begin
    eta := eta.Add(eta1);
  end
  else
  begin
    // mu == -1
    eta := eta.Subtract(eta1);
  end;

  // check1 = eta0 - 3*mu*eta1
  // check2 = eta0 + 4*mu*eta1
  threeEta1 := eta1.Add(eta1).Add(eta1);
  fourEta1 := threeEta1.Add(eta1);

  if (mu = 1) then
  begin
    check1 := eta0.Subtract(threeEta1);
    check2 := eta0.Add(fourEta1);
  end
  else
  begin
    // mu == -1
    check1 := eta0.Add(threeEta1);
    check2 := eta0.Subtract(fourEta1);
  end;

  h0 := 0;
  h1 := 0;

  // if eta >= 1
  if (eta.CompareTo(TBigInteger.One) >= 0) then
  begin
    if (check1.CompareTo(FMinusOne) < 0) then
    begin
      h1 := mu;
    end
    else
    begin
      h0 := 1;
    end;
  end
  else
  begin
    // eta < 1
    if (check2.CompareTo(TBigInteger.Two) >= 0) then
    begin
      h1 := mu;
    end;
  end;

  // if eta < -1
  if (eta.CompareTo(FMinusOne) < 0) then
  begin
    if (check1.CompareTo(TBigInteger.One) >= 0) then
    begin
      h1 := ShortInt(-mu);
    end
    else
    begin
      h0 := -1;
    end
  end
  else
  begin
    // eta >= -1
    if (check2.CompareTo(FMinusTwo) < 0) then
    begin
      h1 := ShortInt(-mu);
    end;
  end;

  q0 := f0.Add(TBigInteger.ValueOf(h0));
  q1 := f1.Add(TBigInteger.ValueOf(h1));
  Result := TZTauElement.Create(q0, q1);
end;

class function TTnaf.Tau(const p: IAbstractF2mPoint): IAbstractF2mPoint;
begin
  Result := p.Tau();
end;

class function TTnaf.TauAdicNaf(mu: ShortInt; const lambda: IZTauElement)
  : TCryptoLibShortIntArray;
var
  LNorm, r0, r1, t, s: TBigInteger;
  log2Norm, maxLength, i, &length: Int32;
  u, LTnaf: TCryptoLibShortIntArray;
begin

  if (not((mu = 1) or (mu = -1))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidMU);
  end;

  LNorm := TTnaf.Norm(mu, lambda);

  // Ceiling of log2 of the norm
  log2Norm := LNorm.BitLength;

  // If length(TNAF) > 30, then length(TNAF) < log2Norm + 3.52
  if log2Norm > 30 then
  begin
    maxLength := log2Norm + 4;
  end
  else
  begin
    maxLength := 34;
  end;

  // The array holding the TNAF
  System.SetLength(u, maxLength);

  i := 0;

  // The actual length of the TNAF
  Length := 0;

  r0 := lambda.u;
  r1 := lambda.v;

  while (not((r0.Equals(TBigInteger.Zero)) and
    (r1.Equals(TBigInteger.Zero)))) do
  begin
    // If r0 is odd
    if (r0.TestBit(0)) then
    begin
      u[i] := ShortInt(TBigInteger.Two.Subtract((r0.Subtract(r1.ShiftLeft(1)))
        .&Mod(FFour)).Int32Value);

      // r0 = r0 - u[i]
      if (u[i] = 1) then
      begin
        r0 := r0.ClearBit(0);
      end
      else
      begin
        // u[i] == -1
        r0 := r0.Add(TBigInteger.One);
      end;
      Length := i;
    end
    else
    begin
      u[i] := 0;
    end;

    t := r0;
    s := r0.ShiftRight(1);
    if (mu = 1) then
    begin
      r0 := r1.Add(s);
    end
    else
    begin
      // mu == -1
      r0 := r1.Subtract(s);
    end;

    r1 := t.ShiftRight(1).Negate();
    System.Inc(i);
  end;

  System.Inc(Length);

  // Reduce the TNAF array to its actual length
  System.SetLength(LTnaf, Length);
  System.Move(u[0], LTnaf[0], Length * System.SizeOf(ShortInt));

  Result := LTnaf;
end;

class function TTnaf.TauAdicWNaf(mu: ShortInt; const lambda: IZTauElement;
  Width: ShortInt; const pow2w, tw: TBigInteger;
  const alpha: TCryptoLibGenericArray<IZTauElement>): TCryptoLibShortIntArray;
var
  LNorm, pow2wMin1, r0, r1, t, uUnMod: TBigInteger;
  log2Norm, maxLength, i: Int32;
  u: TCryptoLibShortIntArray;
  uLocal: ShortInt;
  s: Boolean;
begin

  if (not((mu = 1) or (mu = -1))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidMU);
  end;

  LNorm := Norm(mu, lambda);

  // Ceiling of log2 of the norm
  log2Norm := LNorm.BitLength;

  // If length(TNAF) > 30, then length(TNAF) < log2Norm + 3.52
  if log2Norm > 30 then
  begin
    maxLength := log2Norm + 4 + Width;
  end
  else
  begin
    maxLength := 34 + Width;
  end;

  // The array holding the TNAF
  System.SetLength(u, maxLength);

  // 2^(width - 1)
  pow2wMin1 := pow2w.ShiftRight(1);

  // Split lambda into two BigIntegers to simplify calculations
  r0 := lambda.u;
  r1 := lambda.v;
  i := 0;

  // while lambda <> (0, 0)
  while (not((r0.Equals(TBigInteger.Zero)) and
    (r1.Equals(TBigInteger.Zero)))) do
  begin
    // if r0 is odd
    if (r0.TestBit(0)) then
    begin
      // uUnMod = r0 + r1*tw Mod 2^width
      uUnMod := r0.Add(r1.Multiply(tw)).&Mod(pow2w);

      // if uUnMod >= 2^(width - 1)
      if (uUnMod.CompareTo(pow2wMin1) >= 0) then
      begin
        uLocal := ShortInt(uUnMod.Subtract(pow2w).Int32Value);
      end
      else
      begin
        uLocal := ShortInt(uUnMod.Int32Value);
      end;
      // uLocal is now in [-2^(width-1), 2^(width-1)-1]

      u[i] := uLocal;
      s := True;
      if (uLocal < 0) then
      begin
        s := false;
        uLocal := ShortInt(-uLocal);
      end;
      // uLocal is now >= 0

      if (s) then
      begin
        r0 := r0.Subtract(alpha[uLocal].u);
        r1 := r1.Subtract(alpha[uLocal].v);
      end
      else
      begin
        r0 := r0.Add(alpha[uLocal].u);
        r1 := r1.Add(alpha[uLocal].v);
      end;
    end
    else
    begin
      u[i] := 0;
    end;

    t := r0;

    if (mu = 1) then
    begin
      r0 := r1.Add(r0.ShiftRight(1));
    end
    else
    begin
      // mu == -1
      r0 := r1.Subtract(r0.ShiftRight(1));
    end;
    r1 := t.ShiftRight(1).Negate();
    System.Inc(i);

  end;
  Result := u;
end;

class constructor TTnaf.Tnaf;
begin
  TTnaf.Boot();
end;

end.
