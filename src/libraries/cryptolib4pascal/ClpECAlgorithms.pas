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

unit ClpECAlgorithms;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Math,
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpBits,
  ClpNat,
  ClpIECC,
  ClpECCompUtilities,
  ClpIWNafPreCompInfo,
  ClpIFiniteField,
  ClpIFixedPointPreCompInfo,
  ClpIGlvEndomorphism,
  ClpIMultipliers,
  ClpMultipliers,
  ClpIPolynomialExtensionField;

resourcestring
  SInvalidArray =
    'Point and Scalar Arrays Should be Non-Null, and of Equal, Non-Zero, Length';
  SInvalidPointLocation = 'Point Must be on the Same Curve';
  SInvalidPoint = 'Invalid Point, "P"';
  SInvalidResult = 'Invalid Result';
  SInvalidComputation =
    'Fixed-Point Comb Doesn''t Support Scalars Larger Than The Curve Order';

type
  TECAlgorithms = class sealed(TObject)

  strict private
    class function ImplShamirsTrickWNaf(const preCompP,
      preCompNegP: TCryptoLibGenericArray<IECPoint>;
      const wnafP: TCryptoLibByteArray;
      const preCompQ, preCompNegQ: TCryptoLibGenericArray<IECPoint>;
      const wnafQ: TCryptoLibByteArray): IECPoint; overload; static;

    class function ImplSumOfMultiplies(const negs: TCryptoLibBooleanArray;
      const infos: TCryptoLibGenericArray<IWNafPreCompInfo>;
      const wnafs: TCryptoLibMatrixByteArray): IECPoint; overload; static;

    class function ImplShamirsTrickFixedPoint(const p: IECPoint;
      const k: TBigInteger; const q: IECPoint; const l: TBigInteger)
      : IECPoint; static;

  public
    class function IsF2mCurve(const c: IECCurve): Boolean; static;
    class function IsF2mField(const field: IFiniteField): Boolean; static;
    class function IsFpCurve(const c: IECCurve): Boolean; static;
    class function IsFpField(const field: IFiniteField): Boolean; static;

    class function SumOfMultiplies(const ps: TCryptoLibGenericArray<IECPoint>;
      const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint; static;

    class function SumOfTwoMultiplies(const p: IECPoint; const a: TBigInteger;
      const q: IECPoint; const b: TBigInteger): IECPoint; static;

    // /*
    // * "Shamir's Trick", originally due to E. G. Straus
    // * (Addition chains of vectors. American Mathematical Monthly,
    // * 71(7):806-808, Aug./Sept. 1964)
    // *
    // * Input: The points P, Q, scalar k = (km?, ... , k1, k0)
    // * and scalar l = (lm?, ... , l1, l0).
    // * Output: R = k * P + l * Q.
    // * 1: Z <- P + Q
    // * 2: R <- O
    // * 3: for i from m-1 down to 0 do
    // * 4:        R <- R + R        {point doubling}
    // * 5:        if (ki = 1) and (li = 0) then R <- R + P end if
    // * 6:        if (ki = 0) and (li = 1) then R <- R + Q end if
    // * 7:        if (ki = 1) and (li = 1) then R <- R + Z end if
    // * 8: end for
    // * 9: return R
    // */
    class function ShamirsTrick(const p: IECPoint; const k: TBigInteger;
      const q: IECPoint; const l: TBigInteger): IECPoint; static;

    class function ImportPoint(const c: IECCurve; const p: IECPoint)
      : IECPoint; static;

    class procedure MontgomeryTrick(const zs
      : TCryptoLibGenericArray<IECFieldElement>; off, len: Int32); overload;
      static; inline;

    class procedure MontgomeryTrick(const zs
      : TCryptoLibGenericArray<IECFieldElement>; off, len: Int32;
      const scale: IECFieldElement); overload; static;

    // /**
    // * Simple shift-and-add multiplication. Serves as reference implementation
    // * to verify (possibly faster) implementations, and for very small scalars.
    // *
    // * @param p
    // *            The point to multiply.
    // * @param k
    // *            The multiplier.
    // * @return The result of the point multiplication <code>kP</code>.
    // */
    class function ReferenceMultiply(const p: IECPoint; const k: TBigInteger)
      : IECPoint; static;

    class function ImplCheckResult(const p: IECPoint): IECPoint; static;

    class function ValidatePoint(const p: IECPoint): IECPoint; static;

    class function CleanPoint(const c: IECCurve; const p: IECPoint)
      : IECPoint; static;

    class function ImplShamirsTrickJsf(const p: IECPoint; const k: TBigInteger;
      const q: IECPoint; const l: TBigInteger): IECPoint; static;

    class function ImplShamirsTrickWNaf(const p: IECPoint; const k: TBigInteger;
      const q: IECPoint; const l: TBigInteger): IECPoint; overload; static;

    class function ImplShamirsTrickWNaf(const endomorphism: IECEndomorphism;
      const p: IECPoint; const k, l: TBigInteger): IECPoint; overload; static;

    class function ImplSumOfMultiplies
      (const ps: TCryptoLibGenericArray<IECPoint>;
      const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint;
      overload; static;

    class function ImplSumOfMultipliesGlv
      (const ps: TCryptoLibGenericArray<IECPoint>;
      const ks: TCryptoLibGenericArray<TBigInteger>;
      const glvEndomorphism: IGlvEndomorphism): IECPoint; static;

    class function ImplSumOfMultiplies(const endomorphism: IECEndomorphism;
      const ps: TCryptoLibGenericArray<IECPoint>;
      const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint;
      overload; static;

  end;

implementation

{ TECAlgorithms }

class function TECAlgorithms.ImplCheckResult(const p: IECPoint): IECPoint;
begin
  if (not(p.IsValidPartial())) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidResult);
  end;

  result := p;
end;

class function TECAlgorithms.CleanPoint(const c: IECCurve; const p: IECPoint)
  : IECPoint;
var
  cp: IECCurve;
begin
  cp := p.Curve;
  if (not c.Equals(cp)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPointLocation);
  end;

  result := c.DecodePoint(p.getEncoded(false));
end;

class function TECAlgorithms.ValidatePoint(const p: IECPoint): IECPoint;
begin
  if (not p.IsValid()) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPoint);
  end;

  result := p;
end;

class function TECAlgorithms.ImplShamirsTrickFixedPoint(const p: IECPoint;
  const k: TBigInteger; const q: IECPoint; const l: TBigInteger): IECPoint;
var
  c: IECCurve;
  combSize, widthP, widthQ, width, d, fullComb, i, top, j: Int32;
  infoP, infoQ: IFixedPointPreCompInfo;
  lookupTableP, lookupTableQ: IECLookupTable;
  m: IFixedPointCombMultiplier;
  r1, r2, R, addP, addQ, t: IECPoint;
  BigK, BigL: TCryptoLibUInt32Array;
  secretBitK, secretBitL, secretIndexK, secretIndexL: UInt32;
begin
  c := p.Curve;
  combSize := TFixedPointUtilities.GetCombSize(c);

  if (((k.BitLength) > combSize) or (l.BitLength > combSize)) then
  begin
    (*
      * TODO The comb works best when the scalars are less than the (possibly unknown) order.
      * Still, if we want to handle larger scalars, we could allow customization of the comb
      * size, or alternatively we could deal with the 'extra' bits either by running the comb
      * multiple times as necessary, or by using an alternative multiplier as prelude.
    *)
    raise EInvalidOperationCryptoLibException.CreateRes(@SInvalidComputation);
  end;

  infoP := TFixedPointUtilities.Precompute(p);
  infoQ := TFixedPointUtilities.Precompute(q);

  lookupTableP := infoP.LookupTable;
  lookupTableQ := infoQ.LookupTable;

  widthP := infoP.width;
  widthQ := infoQ.width;

  // TODO This shouldn't normally happen, but a better "solution" is desirable anyway
  if (widthP <> widthQ) then
  begin
    m := TFixedPointCombMultiplier.Create();
    r1 := m.Multiply(p, k);
    r2 := m.Multiply(q, l);
    result := r1.Add(r2);
    Exit;
  end;

  width := widthP;

  d := ((combSize + width) - 1) div width;

  R := c.Infinity;

  fullComb := d * width;
  BigK := TNat.FromBigInteger(fullComb, k);
  BigL := TNat.FromBigInteger(fullComb, l);

  top := fullComb - 1;

  for i := 0 to System.Pred(d) do
  begin
    secretIndexK := 0;
    secretIndexL := 0;

    j := top - i;

    while j >= 0 do
    begin

      secretBitK := BigK[TBits.Asr32(j, 5)] shr (j and $1F);
      secretIndexK := secretIndexK xor (secretBitK shr 1);
      secretIndexK := secretIndexK shl 1;
      secretIndexK := secretIndexK xor secretBitK;

      secretBitL := BigL[TBits.Asr32(j, 5)] shr (j and $1F);
      secretIndexL := secretIndexL xor (secretBitL shr 1);
      secretIndexL := secretIndexL shl 1;
      secretIndexL := secretIndexL xor secretBitL;

      System.Dec(j, d);
    end;

    addP := lookupTableP.LookupVar(Int32(secretIndexK));
    addQ := lookupTableQ.LookupVar(Int32(secretIndexL));

    t := addP.Add(addQ);

    R := R.TwicePlus(t);
  end;

  result := R.Add(infoP.Offset).Add(infoQ.Offset);
end;

class function TECAlgorithms.ImplShamirsTrickJsf(const p: IECPoint;
  const k: TBigInteger; const q: IECPoint; const l: TBigInteger): IECPoint;
var
  Curve: IECCurve;
  Infinity, R: IECPoint;
  PaddQ, PsubQ: IECPoint;
  points, table: TCryptoLibGenericArray<IECPoint>;
  jsf: TCryptoLibByteArray;
  i, jsfi, kDigit, lDigit, index: Int32;
begin
  Curve := p.Curve;
  Infinity := Curve.Infinity;

  // TODO conjugate co-Z addition (ZADDC) can return both of these
  PaddQ := p.Add(q);
  PsubQ := p.Subtract(q);

  points := TCryptoLibGenericArray<IECPoint>.Create(q, PsubQ, p, PaddQ);
  Curve.NormalizeAll(points);

  table := TCryptoLibGenericArray<IECPoint>.Create(points[3].Negate(),
    points[2].Negate(), points[1].Negate(), points[0].Negate(), Infinity,
    points[0], points[1], points[2], points[3]);

  jsf := TWNafUtilities.GenerateJsf(k, l);

  R := Infinity;

  i := System.length(jsf);
  System.Dec(i);
  while (i >= 0) do
  begin
    jsfi := jsf[i];

    // NOTE: The shifting ensures the sign is extended correctly
    kDigit := (TBits.Asr32((jsfi shl 24), 28));
    lDigit := (TBits.Asr32((jsfi shl 28), 28));

    index := 4 + (kDigit * 3) + lDigit;
    R := R.TwicePlus(table[index]);
    System.Dec(i);
  end;

  result := R;
end;

class function TECAlgorithms.ImplShamirsTrickWNaf(const endomorphism
  : IECEndomorphism; const p: IECPoint; const k, l: TBigInteger): IECPoint;
var
  negK, negL: Boolean;
  minWidth, widthP, widthQ: Int32;
  q: IECPoint;
  infoP, infoQ: IWNafPreCompInfo;
  preCompP, preCompQ, preCompNegP, preCompNegQ
    : TCryptoLibGenericArray<IECPoint>;
  wnafP, wnafQ: TCryptoLibByteArray;
  LK, LL: TBigInteger;
begin
  LK := k;
  LL := l;
  negK := LK.SignValue < 0;
  negL := LL.SignValue < 0;

  LK := LK.Abs();
  LL := LL.Abs();

  minWidth := TWNafUtilities.GetWindowSize(Max(k.BitLength, l.BitLength), 8);

  infoP := TWNafUtilities.Precompute(p, minWidth, true);
  q := TEndoUtilities.MapPoint(endomorphism, p);
  infoQ := TWNafUtilities.PrecomputeWithPointMap(q, endomorphism.pointMap,
    infoP, true);

  widthP := Min(8, infoP.width);
  widthQ := Min(8, infoQ.width);

  case negK of
    true:
      preCompP := infoP.PreCompNeg;
    false:
      preCompP := infoP.PreComp;
  end;

  case negL of
    true:
      preCompQ := infoQ.PreCompNeg;
    false:
      preCompQ := infoQ.PreComp
  end;

  case negK of
    true:
      preCompNegP := infoP.PreComp;
    false:
      preCompNegP := infoP.PreCompNeg;
  end;

  case negL of
    true:
      preCompNegQ := infoQ.PreComp;
    false:
      preCompNegQ := infoQ.PreCompNeg
  end;

  wnafP := TWNafUtilities.GenerateWindowNaf(widthP, LK);
  wnafQ := TWNafUtilities.GenerateWindowNaf(widthQ, LL);

  result := ImplShamirsTrickWNaf(preCompP, preCompNegP, wnafP, preCompQ,
    preCompNegQ, wnafQ);

  infoP.PreComp := Nil; // Review
  infoP.PreCompNeg := Nil; // Review
  infoQ.PreComp := Nil; // Review
  infoQ.PreCompNeg := Nil; // Review

end;

class function TECAlgorithms.ImplShamirsTrickWNaf(const p: IECPoint;
  const k: TBigInteger; const q: IECPoint; const l: TBigInteger): IECPoint;
var
  negK, negL: Boolean;
  minWidthP, minWidthQ, widthP, widthQ, combSize: Int32;
  infoP, infoQ: IWNafPreCompInfo;
  preCompP, preCompQ, preCompNegP, preCompNegQ
    : TCryptoLibGenericArray<IECPoint>;
  wnafP, wnafQ: TCryptoLibByteArray;
  kAbs, lAbs: TBigInteger;
  c: IECCurve;
begin

  negK := k.SignValue < 0;
  negL := l.SignValue < 0;

  kAbs := k.Abs();
  lAbs := l.Abs();

  minWidthP := TWNafUtilities.GetWindowSize(kAbs.BitLength, 8);
  minWidthQ := TWNafUtilities.GetWindowSize(lAbs.BitLength, 8);

  infoP := TWNafUtilities.Precompute(p, minWidthP, true);
  infoQ := TWNafUtilities.Precompute(q, minWidthQ, true);

  // When P, Q are 'promoted' (i.e. reused several times), switch to fixed-point algorithm

  c := p.Curve;
  combSize := TFixedPointUtilities.GetCombSize(c);
  if ((not negK) and (not negL) and (k.BitLength <= combSize) and
    (l.BitLength <= combSize) and (infoP.IsPromoted) and (infoQ.IsPromoted))
  then
  begin
    result := ImplShamirsTrickFixedPoint(p, k, q, l);
    infoP.PreComp := Nil; // Review
    infoP.PreCompNeg := Nil; // Review
    infoQ.PreComp := Nil; // Review
    infoQ.PreCompNeg := Nil; // Review
    Exit;
  end;

  widthP := Min(8, infoP.width);
  widthQ := Min(8, infoQ.width);

  if negK then
  begin
    preCompP := infoP.PreCompNeg
  end
  else
  begin
    preCompP := infoP.PreComp
  end;

  if negL then
  begin
    preCompQ := infoQ.PreCompNeg
  end
  else
  begin
    preCompQ := infoQ.PreComp
  end;

  if negK then
  begin
    preCompNegP := infoP.PreComp
  end
  else
  begin
    preCompNegP := infoP.PreCompNeg
  end;

  if negL then
  begin
    preCompNegQ := infoQ.PreComp
  end
  else
  begin
    preCompNegQ := infoQ.PreCompNeg
  end;

  wnafP := TWNafUtilities.GenerateWindowNaf(widthP, kAbs);
  wnafQ := TWNafUtilities.GenerateWindowNaf(widthQ, lAbs);

  result := ImplShamirsTrickWNaf(preCompP, preCompNegP, wnafP, preCompQ,
    preCompNegQ, wnafQ);
  infoP.PreComp := Nil; // Review
  infoP.PreCompNeg := Nil; // Review
  infoQ.PreComp := Nil; // Review
  infoQ.PreCompNeg := Nil; // Review
end;

class function TECAlgorithms.ImplShamirsTrickWNaf(const preCompP,
  preCompNegP: TCryptoLibGenericArray<IECPoint>;
  const wnafP: TCryptoLibByteArray;
  const preCompQ, preCompNegQ: TCryptoLibGenericArray<IECPoint>;
  const wnafQ: TCryptoLibByteArray): IECPoint;
var
  len, zeroes, i, wiP, wiQ, nP, nQ: Int32;
  Curve: IECCurve;
  Infinity, R, point: IECPoint;
  tableP, tableQ: TCryptoLibGenericArray<IECPoint>;
begin
  len := Max(System.length(wnafP), System.length(wnafQ));

  Curve := preCompP[0].Curve;
  Infinity := Curve.Infinity;

  R := Infinity;
  zeroes := 0;

  i := len - 1;
  while (i >= 0) do
  begin

    if i < System.length(wnafP) then
    begin
      wiP := Int32(ShortInt(wnafP[i]));
    end
    else
    begin
      wiP := 0;
    end;

    if i < System.length(wnafQ) then
    begin
      wiQ := Int32(ShortInt(wnafQ[i]));
    end
    else
    begin
      wiQ := 0;
    end;

    if ((wiP or wiQ) = 0) then
    begin
      System.Inc(zeroes);
      System.Dec(i);
      continue;
    end;

    point := Infinity;
    if (wiP <> 0) then
    begin
      nP := System.Abs(wiP);
      if wiP < 0 then
      begin
        tableP := preCompNegP;
      end
      else
      begin
        tableP := preCompP;
      end;

      point := point.Add(tableP[TBits.Asr32(nP, 1)]);
    end;
    if (wiQ <> 0) then
    begin

      nQ := System.Abs(wiQ);
      if wiQ < 0 then
      begin
        tableQ := preCompNegQ;
      end
      else
      begin
        tableQ := preCompQ;
      end;

      point := point.Add(tableQ[TBits.Asr32(nQ, 1)]);

    end;

    if (zeroes > 0) then
    begin
      R := R.TimesPow2(zeroes);
      zeroes := 0;
    end;

    R := R.TwicePlus(point);
    System.Dec(i);
  end;

  if (zeroes > 0) then
  begin
    R := R.TimesPow2(zeroes);
  end;

  result := R;
end;

class function TECAlgorithms.ImplSumOfMultiplies(const endomorphism
  : IECEndomorphism; const ps: TCryptoLibGenericArray<IECPoint>;
  const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint;
var
  halfCount, fullCount: Int32;
  negs: TCryptoLibBooleanArray;
  infos: TCryptoLibGenericArray<IWNafPreCompInfo>;
  infoP, infoQ: IWNafPreCompInfo;
  wnafs: TCryptoLibMatrixByteArray;
  i, j0, j1, minWidth, widthP, widthQ: Int32;
  kj0, kj1: TBigInteger;
  p, q: IECPoint;
  pointMap: IECPointMap;
begin
  halfCount := System.length(ps);
  fullCount := halfCount shl 1;
  System.SetLength(negs, fullCount);
  System.SetLength(infos, fullCount);
  System.SetLength(wnafs, fullCount);

  pointMap := endomorphism.pointMap;

  for i := 0 to System.Pred(halfCount) do
  begin
    j0 := i shl 1;
    j1 := j0 + 1;

    kj0 := ks[j0];
    negs[j0] := kj0.SignValue < 0;
    kj0 := kj0.Abs();
    kj1 := ks[j1];
    negs[j1] := kj1.SignValue < 0;
    kj1 := kj1.Abs();

    minWidth := TWNafUtilities.GetWindowSize
      (Max(kj0.BitLength, kj1.BitLength), 8);

    p := ps[i];
    infoP := TWNafUtilities.Precompute(p, minWidth, true);
    q := TEndoUtilities.MapPoint(endomorphism, p);
    infoQ := TWNafUtilities.PrecomputeWithPointMap(q, pointMap, infoP, true);

    widthP := Min(8, infoP.width);
    widthQ := Min(8, infoQ.width);

    infos[j0] := infoP;
    infos[j1] := infoQ;
    wnafs[j0] := TWNafUtilities.GenerateWindowNaf(widthP, kj0);
    wnafs[j1] := TWNafUtilities.GenerateWindowNaf(widthQ, kj1);
  end;

  result := ImplSumOfMultiplies(negs, infos, wnafs);

  for i := System.Low(infos) to System.High(infos) do
  begin
    infos[i].PreComp := Nil; // Review
    infos[i].PreCompNeg := Nil; // Review
  end;

end;

class function TECAlgorithms.ImplSumOfMultiplies
  (const ps: TCryptoLibGenericArray<IECPoint>;
  const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint;
var
  count, i, width, minWidth: Int32;
  negs: TCryptoLibBooleanArray;
  info: IWNafPreCompInfo;
  infos: TCryptoLibGenericArray<IWNafPreCompInfo>;
  wnafs: TCryptoLibMatrixByteArray;
  ki: TBigInteger;
begin
  count := System.length(ps);
  System.SetLength(negs, count);

  System.SetLength(infos, count);

  System.SetLength(wnafs, count);

  for i := 0 to System.Pred(count) do
  begin
    ki := ks[i];
    negs[i] := ki.SignValue < 0;
    ki := ki.Abs();

    minWidth := TWNafUtilities.GetWindowSize(ki.BitLength, 8);
    info := TWNafUtilities.Precompute(ps[i], minWidth, true);
    width := Min(8, info.width);

    infos[i] := info;
    wnafs[i] := TWNafUtilities.GenerateWindowNaf(width, ki);
  end;

  result := ImplSumOfMultiplies(negs, infos, wnafs);

  for i := System.Low(infos) to System.High(infos) do
  begin
    infos[i].PreComp := Nil; // Review
    infos[i].PreCompNeg := Nil; // Review
  end;

end;

class function TECAlgorithms.ImplSumOfMultiplies
  (const negs: TCryptoLibBooleanArray;
  const infos: TCryptoLibGenericArray<IWNafPreCompInfo>;
  const wnafs: TCryptoLibMatrixByteArray): IECPoint;
var
  len, count, zeroes: Int32;
  i, j, wi, n: Int32;
  Curve: IECCurve;
  Infinity, R, point: IECPoint;
  wnaf: TCryptoLibByteArray;
  info: IWNafPreCompInfo;
  table: TCryptoLibGenericArray<IECPoint>;
begin
  len := 0;
  count := System.length(wnafs);

  for i := 0 to System.Pred(count) do
  begin
    len := Max(len, System.length(wnafs[i]));
  end;

  Curve := infos[0].PreComp[0].Curve;
  Infinity := Curve.Infinity;

  R := Infinity;
  zeroes := 0;

  i := len - 1;
  while (i >= 0) do
  begin
    point := Infinity;

    for j := 0 to System.Pred(count) do
    begin
      wnaf := wnafs[j];
      if i < System.length(wnaf) then
      begin
        wi := Int32(ShortInt(wnaf[i]));
      end
      else
      begin
        wi := 0;
      end;

      if (wi <> 0) then
      begin
        n := System.Abs(wi);
        info := infos[j];
        if (wi < 0 = negs[j]) then
        begin
          table := info.PreComp;
        end
        else
        begin
          table := info.PreCompNeg;
        end;

        point := point.Add(table[TBits.Asr32(n, 1)]);
      end;
    end;

    if (point = Infinity) then
    begin
      System.Inc(zeroes);
      System.Dec(i);
      continue;
    end;

    if (zeroes > 0) then
    begin
      R := R.TimesPow2(zeroes);
      zeroes := 0;
    end;

    R := R.TwicePlus(point);

    System.Dec(i);
  end;

  if (zeroes > 0) then
  begin
    R := R.TimesPow2(zeroes);
  end;

  result := R;

end;

class function TECAlgorithms.ImplSumOfMultipliesGlv
  (const ps: TCryptoLibGenericArray<IECPoint>;
  const ks: TCryptoLibGenericArray<TBigInteger>;
  const glvEndomorphism: IGlvEndomorphism): IECPoint;
var
  n: TBigInteger;
  len, i, j: Int32;
  &abs, ab: TCryptoLibGenericArray<TBigInteger>;
  pqs: TCryptoLibGenericArray<IECPoint>;
  p, q: IECPoint;
begin
  n := ps[0].Curve.Order;

  len := System.length(ps);

  System.SetLength(Abs, len shl 1);

  i := 0;
  j := 0;

  while (i < len) do
  begin
    ab := glvEndomorphism.DecomposeScalar(ks[i].&Mod(n));

    Abs[j] := ab[0];
    System.Inc(j);
    Abs[j] := ab[1];
    System.Inc(j);
    System.Inc(i);
  end;

  if (glvEndomorphism.HasEfficientPointMap) then
  begin
    result := ImplSumOfMultiplies(glvEndomorphism, ps, Abs);
    Exit;
  end;

  System.SetLength(pqs, len shl 1);

  i := 0;
  j := 0;

  while (i < len) do
  begin
    p := ps[i];
    q := TEndoUtilities.MapPoint(glvEndomorphism, p);

    pqs[j] := p;
    System.Inc(j);
    pqs[j] := q;
    System.Inc(j);
    System.Inc(i);
  end;

  result := ImplSumOfMultiplies(pqs, Abs);
end;

class function TECAlgorithms.ImportPoint(const c: IECCurve; const p: IECPoint)
  : IECPoint;
var
  cp: IECCurve;
begin
  cp := p.Curve;
  if (not c.Equals(cp)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPointLocation);
  end;

  result := c.ImportPoint(p);
end;

class function TECAlgorithms.IsF2mField(const field: IFiniteField): Boolean;
begin
  result := (field.Dimension > 1) and
    (field.Characteristic.Equals(TBigInteger.Two)) and
    (Supports(field, IPolynomialExtensionField));
end;

class function TECAlgorithms.IsF2mCurve(const c: IECCurve): Boolean;
begin
  result := IsF2mField(c.field);
end;

class function TECAlgorithms.IsFpField(const field: IFiniteField): Boolean;
begin
  result := field.Dimension = 1;
end;

class function TECAlgorithms.IsFpCurve(const c: IECCurve): Boolean;
begin
  result := IsFpField(c.field);
end;

class procedure TECAlgorithms.MontgomeryTrick
  (const zs: TCryptoLibGenericArray<IECFieldElement>; off, len: Int32;
  const scale: IECFieldElement);
var
  c: TCryptoLibGenericArray<IECFieldElement>;
  i, j: Int32;
  u, tmp: IECFieldElement;
begin
  // /*
  // * Uses the "Montgomery Trick" to invert many field elements, with only a single actual
  // * field inversion. See e.g. the paper:
  // * "Fast Multi-scalar Multiplication Methods on Elliptic Curves with Precomputation Strategy Using Montgomery Trick"
  // * by Katsuyuki Okeya, Kouichi Sakurai.
  // */

  System.SetLength(c, len);

  c[0] := zs[off];

  i := 0;
  System.Inc(i);
  while (i < len) do
  begin
    c[i] := c[i - 1].Multiply(zs[off + i]);
    System.Inc(i);
  end;
  System.Dec(i);

  if (scale <> Nil) then
  begin
    c[i] := c[i].Multiply(scale);
  end;

  u := c[i].Invert();

  while (i > 0) do
  begin
    j := off + i;
    System.Dec(i);
    tmp := zs[j];
    zs[j] := c[i].Multiply(u);
    u := u.Multiply(tmp);
  end;

  zs[off] := u;
end;

class procedure TECAlgorithms.MontgomeryTrick
  (const zs: TCryptoLibGenericArray<IECFieldElement>; off, len: Int32);
begin
  MontgomeryTrick(zs, off, len, Nil);
end;

class function TECAlgorithms.ReferenceMultiply(const p: IECPoint;
  const k: TBigInteger): IECPoint;
var
  x: TBigInteger;
  q, LP: IECPoint;
  t, i: Int32;
begin
  LP := p;
  x := k.Abs();
  q := LP.Curve.Infinity;
  t := x.BitLength;
  if (t > 0) then
  begin
    if (x.TestBit(0)) then
    begin
      q := LP;
    end;
    i := 1;
    while (i < t) do
    begin
      LP := LP.Twice();
      if (x.TestBit(i)) then
      begin
        q := q.Add(LP);
      end;
      System.Inc(i);
    end;

  end;

  if k.SignValue < 0 then
  begin
    result := q.Negate();
  end
  else
  begin
    result := q;
  end;

end;

class function TECAlgorithms.ShamirsTrick(const p: IECPoint;
  const k: TBigInteger; const q: IECPoint; const l: TBigInteger): IECPoint;
var
  cp: IECCurve;
  LQ: IECPoint;
begin
  cp := p.Curve;
  LQ := q;
  LQ := ImportPoint(cp, LQ);

  result := ImplCheckResult(ImplShamirsTrickJsf(p, k, LQ, l));
end;

class function TECAlgorithms.SumOfMultiplies
  (const ps: TCryptoLibGenericArray<IECPoint>;
  const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint;
var
  count: Int32;
  p: IECPoint;
  c: IECCurve;
  i: Int32;
  imported: TCryptoLibGenericArray<IECPoint>;
  glvEndomorphism: IGlvEndomorphism;
begin
  if ((ps = Nil) or (ks = Nil) or (System.length(ps) <> System.length(ks)) or
    (System.length(ps) < 1)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidArray);
  end;

  count := System.length(ps);

  case count of
    1:
      begin
        result := ps[0].Multiply(ks[0]);
        Exit;
      end;

    2:
      begin
        result := SumOfTwoMultiplies(ps[0], ks[0], ps[1], ks[1]);
        Exit;
      end;

  end;

  p := ps[0];
  c := p.Curve;
  System.SetLength(imported, count);
  imported[0] := p;

  for i := 1 to System.Pred(count) do
  begin
    imported[i] := ImportPoint(c, ps[i]);
  end;

  if Supports(c.GetEndomorphism(), IGlvEndomorphism, glvEndomorphism) then
  begin
    result := ImplCheckResult(ImplSumOfMultipliesGlv(imported, ks,
      glvEndomorphism));
    Exit;
  end;

  result := ImplCheckResult(ImplSumOfMultiplies(imported, ks));
end;

class function TECAlgorithms.SumOfTwoMultiplies(const p: IECPoint;
  const a: TBigInteger; const q: IECPoint; const b: TBigInteger): IECPoint;
var
  cp: IECCurve;
  f2mCurve: IAbstractF2mCurve;
  glvEndomorphism: IGlvEndomorphism;
  LQ: IECPoint;
begin
  cp := p.Curve;
  LQ := q;
  LQ := ImportPoint(cp, LQ);

  // Point multiplication for Koblitz curves (using WTNAF) beats Shamir's trick

  if (Supports(cp, IAbstractF2mCurve, f2mCurve) and (f2mCurve.IsKoblitz)) then
  begin
    result := ImplCheckResult(p.Multiply(a).Add(LQ.Multiply(b)));
    Exit;
  end;

  if Supports(cp.GetEndomorphism(), IGlvEndomorphism, glvEndomorphism) then
  begin
    result := ImplCheckResult
      (ImplSumOfMultipliesGlv(TCryptoLibGenericArray<IECPoint>.Create(p, LQ),
      TCryptoLibGenericArray<TBigInteger>.Create(a, b), glvEndomorphism));
    Exit;
  end;

  result := ImplCheckResult(ImplShamirsTrickWNaf(p, a, LQ, b));
end;

end.
