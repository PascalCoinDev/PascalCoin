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
  ClpBits,
  ClpBigInteger,
  ClpWNafPreCompInfo,
  ClpIPolynomialExtensionField,
  ClpIGlvEndomorphism,
  ClpIWNafPreCompInfo,
  ClpIPreCompInfo,
  ClpIPreCompCallBack,
  ClpIECC,
  ClpECCurveConstants,
  ClpIFiniteField;

resourcestring
  SInvalidArray =
    'Point and Scalar Arrays Should be Non-Null, and of Equal, Non-Zero, Length';
  SInvalidPointLocation = 'Point Must be on the Same Curve';
  SInvalidPoint = 'Invalid Point, "P"';
  SInvalidResult = 'Invalid Result';
  SInvalidRange = 'Must be in the Range [2, 16], "width"';
  SInvalidRange2 = 'Must be in the Range [2, 8], "width"';

type
  TWNafUtilities = class abstract(TObject)

  strict private
  const
    FDEFAULT_WINDOW_SIZE_CUTOFFS: array [0 .. 5] of Int32 = (13, 41, 121, 337,
      897, 2305);

  class var
    FEMPTY_BYTES: TCryptoLibByteArray;
    FEMPTY_INTS: TCryptoLibInt32Array;

  type
    IMapPointCallback = interface(IPreCompCallback)
      ['{730BF27F-D5C3-4DF4-AC77-B8653C457C10}']

    end;

  type
    TMapPointCallback = class(TInterfacedObject, IPreCompCallback,
      IMapPointCallback)

    strict private
    var
      Fm_wnafPreCompP: IWNafPreCompInfo;
      Fm_includeNegated: Boolean;
      Fm_pointMap: IECPointMap;

    public
      constructor Create(const wnafPreCompP: IWNafPreCompInfo;
        includeNegated: Boolean; const pointMap: IECPointMap);

      function Precompute(const existing: IPreCompInfo): IPreCompInfo;

    end;

  type
    IWNafCallback = interface(IPreCompCallback)
      ['{A439A606-7899-4720-937E-C2F3D94D4811}']

    end;

  type
    TWNafCallback = class(TInterfacedObject, IPreCompCallback, IWNafCallback)

    strict private

    var
      Fm_p: IECPoint;
      Fm_width: Int32;
      Fm_includeNegated: Boolean;

    public
      constructor Create(const p: IECPoint; width: Int32;
        includeNegated: Boolean);

      function Precompute(const existing: IPreCompInfo): IPreCompInfo;

    end;

  class function CheckExisting(const existingWNaf: IWNafPreCompInfo;
    reqPreCompLen: Int32; includeNegated: Boolean): Boolean; static; inline;

  class function CheckTable(const table: TCryptoLibGenericArray<IECPoint>;
    reqLen: Int32): Boolean; static; inline;

  class function Trim(const a: TCryptoLibByteArray; length: Int32)
    : TCryptoLibByteArray; overload; static; inline;

  class function Trim(const a: TCryptoLibInt32Array; length: Int32)
    : TCryptoLibInt32Array; overload; static; inline;

  class function ResizeTable(const a: TCryptoLibGenericArray<IECPoint>;
    length: Int32): TCryptoLibGenericArray<IECPoint>; static; inline;

  class procedure Boot(); static;
  class constructor CreateWNafUtilities();

  public

    const
    PRECOMP_NAME: String = 'bc_wnaf';

    class function GenerateCompactNaf(const k: TBigInteger)
      : TCryptoLibInt32Array; static;
    class function GenerateCompactWindowNaf(width: Int32; const k: TBigInteger)
      : TCryptoLibInt32Array; static;

    class function GenerateJsf(const g, h: TBigInteger)
      : TCryptoLibByteArray; static;
    class function GenerateNaf(const k: TBigInteger)
      : TCryptoLibByteArray; static;
    // /**
    // * Computes the Window NAF (non-adjacent Form) of an integer.
    // * @param width The width <code>w</code> of the Window NAF. The width is
    // * defined as the minimal number <code>w</code>, such that for any
    // * <code>w</code> consecutive digits in the resulting representation, at
    // * most one is non-zero.
    // * @param k The integer of which the Window NAF is computed.
    // * @return The Window NAF of the given width, such that the following holds:
    // * <code>k = &amp;sum;<sub>i=0</sub><sup>l-1</sup> k<sub>i</sub>2<sup>i</sup>
    // * </code>, where the <code>k<sub>i</sub></code> denote the elements of the
    // * returned <code>byte[]</code>.
    // */
    class function GenerateWindowNaf(width: Int32; const k: TBigInteger)
      : TCryptoLibByteArray; static;

    class function GetNafWeight(const k: TBigInteger): Int32; static; inline;

    class function GetWNafPreCompInfo(const p: IECPoint): IWNafPreCompInfo;
      overload; static; inline;

    class function GetWNafPreCompInfo(const preCompInfo: IPreCompInfo)
      : IWNafPreCompInfo; overload; static; inline;

    /// <summary>
    /// Determine window width to use for a scalar multiplication of the
    /// given size.
    /// </summary>
    /// <param name="bits">
    /// the bit-length of the scalar to multiply by
    /// </param>
    /// <returns>
    /// the window size to use
    /// </returns>
    class function GetWindowSize(bits: Int32): Int32; overload; static; inline;

    /// <summary>
    /// Determine window width to use for a scalar multiplication of the
    /// given size.
    /// </summary>
    /// <param name="bits">
    /// the bit-length of the scalar to multiply by
    /// </param>
    /// <param name="windowSizeCutoffs">
    /// a monotonically increasing list of bit sizes at which to increment
    /// the window width
    /// </param>
    /// <returns>
    /// the window size to use
    /// </returns>
    class function GetWindowSize(bits: Int32;
      const windowSizeCutoffs: array of Int32): Int32; overload; static;

    class function MapPointWithPrecomp(const p: IECPoint; width: Int32;
      includeNegated: Boolean; const pointMap: IECPointMap): IECPoint; static;

    class function Precompute(const p: IECPoint; width: Int32;
      includeNegated: Boolean): IWNafPreCompInfo; static;

  end;

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

  public
    class function IsF2mCurve(const c: IECCurve): Boolean; static;
    class function IsF2mField(const field: IFiniteField): Boolean; static;
    class function IsFpCurve(const c: IECCurve): Boolean; static;
    class function IsFpField(const field: IFiniteField): Boolean; static;

    class function SumOfMultiplies(const ps: TCryptoLibGenericArray<IECPoint>;
      const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint; static;

    class function SumOfTwoMultiplies(const p: IECPoint; const a: TBigInteger;
      const Q: IECPoint; const b: TBigInteger): IECPoint; static;

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
      const Q: IECPoint; const l: TBigInteger): IECPoint; static;

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
      const Q: IECPoint; const l: TBigInteger): IECPoint; static;

    class function ImplShamirsTrickWNaf(const p: IECPoint; const k: TBigInteger;
      const Q: IECPoint; const l: TBigInteger): IECPoint; overload; static;

    class function ImplShamirsTrickWNaf(const p: IECPoint; const k: TBigInteger;
      const pointMapQ: IECPointMap; const l: TBigInteger): IECPoint;
      overload; static;

    class function ImplSumOfMultiplies
      (const ps: TCryptoLibGenericArray<IECPoint>;
      const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint;
      overload; static;

    class function ImplSumOfMultipliesGlv
      (const ps: TCryptoLibGenericArray<IECPoint>;
      const ks: TCryptoLibGenericArray<TBigInteger>;
      const glvEndomorphism: IGlvEndomorphism): IECPoint; static;

    class function ImplSumOfMultiplies
      (const ps: TCryptoLibGenericArray<IECPoint>; const pointMap: IECPointMap;
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

class function TECAlgorithms.ImplShamirsTrickJsf(const p: IECPoint;
  const k: TBigInteger; const Q: IECPoint; const l: TBigInteger): IECPoint;
var
  Curve: IECCurve;
  infinity, R: IECPoint;
  PaddQ, PsubQ: IECPoint;
  points, table: TCryptoLibGenericArray<IECPoint>;
  jsf: TCryptoLibByteArray;
  i, jsfi, kDigit, lDigit, index: Int32;
begin
  Curve := p.Curve;
  infinity := Curve.infinity;

  // TODO conjugate co-Z addition (ZADDC) can return both of these
  PaddQ := p.Add(Q);
  PsubQ := p.Subtract(Q);

  points := TCryptoLibGenericArray<IECPoint>.Create(Q, PsubQ, p, PaddQ);
  Curve.NormalizeAll(points);

  table := TCryptoLibGenericArray<IECPoint>.Create(points[3].Negate(),
    points[2].Negate(), points[1].Negate(), points[0].Negate(), infinity,
    points[0], points[1], points[2], points[3]);

  jsf := TWNafUtilities.GenerateJsf(k, l);

  R := infinity;

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

class function TECAlgorithms.ImplShamirsTrickWNaf(const p: IECPoint;
  const k: TBigInteger; const pointMapQ: IECPointMap; const l: TBigInteger)
  : IECPoint;
var
  negK, negL: Boolean;
  width: Int32;
  Q: IECPoint;
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

  width := Max(2, Min(16, TWNafUtilities.GetWindowSize(Max(LK.BitLength,
    LL.BitLength))));

  Q := TWNafUtilities.MapPointWithPrecomp(p, width, true, pointMapQ);
  infoP := TWNafUtilities.GetWNafPreCompInfo(p);
  infoQ := TWNafUtilities.GetWNafPreCompInfo(Q);

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

  wnafP := TWNafUtilities.GenerateWindowNaf(width, LK);
  wnafQ := TWNafUtilities.GenerateWindowNaf(width, LL);

  result := ImplShamirsTrickWNaf(preCompP, preCompNegP, wnafP, preCompQ,
    preCompNegQ, wnafQ);

  infoP.PreComp := Nil; // Review
  infoP.PreCompNeg := Nil; // Review
  infoQ.PreComp := Nil; // Review
  infoQ.PreCompNeg := Nil; // Review

end;

class function TECAlgorithms.ImplShamirsTrickWNaf(const p: IECPoint;
  const k: TBigInteger; const Q: IECPoint; const l: TBigInteger): IECPoint;
var
  negK, negL: Boolean;
  widthP, widthQ: Int32;
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

  widthP := Max(2, Min(16, TWNafUtilities.GetWindowSize(LK.BitLength)));
  widthQ := Max(2, Min(16, TWNafUtilities.GetWindowSize(LL.BitLength)));

  infoP := TWNafUtilities.Precompute(p, widthP, true);
  infoQ := TWNafUtilities.Precompute(Q, widthQ, true);

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

  wnafP := TWNafUtilities.GenerateWindowNaf(widthP, LK);
  wnafQ := TWNafUtilities.GenerateWindowNaf(widthQ, LL);

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
  infinity, R, point: IECPoint;
  tableP, tableQ: TCryptoLibGenericArray<IECPoint>;
begin
  len := Math.Max(System.length(wnafP), System.length(wnafQ));

  Curve := preCompP[0].Curve;
  infinity := Curve.infinity;

  R := infinity;
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

    point := infinity;
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

class function TECAlgorithms.ImplSumOfMultiplies
  (const ps: TCryptoLibGenericArray<IECPoint>; const pointMap: IECPointMap;
  const ks: TCryptoLibGenericArray<TBigInteger>): IECPoint;
var
  halfCount, fullCount: Int32;
  negs: TCryptoLibBooleanArray;
  infos: TCryptoLibGenericArray<IWNafPreCompInfo>;
  wnafs: TCryptoLibMatrixByteArray;
  i, j0, j1, width: Int32;
  kj0, kj1: TBigInteger;
  p, Q: IECPoint;
begin
  halfCount := System.length(ps);
  fullCount := halfCount shl 1;
  System.SetLength(negs, fullCount);
  System.SetLength(infos, fullCount);
  System.SetLength(wnafs, fullCount);

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

    width := Max(2, Min(16, TWNafUtilities.GetWindowSize(Max(kj0.BitLength,
      kj1.BitLength))));

    p := ps[i];
    Q := TWNafUtilities.MapPointWithPrecomp(p, width, true, pointMap);
    infos[j0] := TWNafUtilities.GetWNafPreCompInfo(p);
    infos[j1] := TWNafUtilities.GetWNafPreCompInfo(Q);
    wnafs[j0] := TWNafUtilities.GenerateWindowNaf(width, kj0);
    wnafs[j1] := TWNafUtilities.GenerateWindowNaf(width, kj1);
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
  count, i, width: Int32;
  negs: TCryptoLibBooleanArray;
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

    width := Max(2, Min(16, TWNafUtilities.GetWindowSize(ki.BitLength)));
    infos[i] := TWNafUtilities.Precompute(ps[i], width, true);
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
  i, J, wi, n: Int32;
  Curve: IECCurve;
  infinity, R, point: IECPoint;
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
  infinity := Curve.infinity;

  R := infinity;
  zeroes := 0;

  i := len - 1;
  while (i >= 0) do
  begin
    point := infinity;

    for J := 0 to System.Pred(count) do
    begin
      wnaf := wnafs[J];
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
        info := infos[J];
        if (wi < 0 = negs[J]) then
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

    if (point = infinity) then
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
  len, i, J: Int32;
  &abs, ab: TCryptoLibGenericArray<TBigInteger>;
  pointMap: IECPointMap;
  pqs: TCryptoLibGenericArray<IECPoint>;
  p, Q: IECPoint;
begin
  n := ps[0].Curve.Order;

  len := System.length(ps);

  System.SetLength(Abs, len shl 1);

  i := 0;
  J := 0;

  while (i < len) do
  begin
    ab := glvEndomorphism.DecomposeScalar(ks[i].&Mod(n));

    Abs[J] := ab[0];
    System.Inc(J);
    Abs[J] := ab[1];
    System.Inc(J);
    System.Inc(i);
  end;

  pointMap := glvEndomorphism.pointMap;
  if (glvEndomorphism.HasEfficientPointMap) then
  begin
    result := TECAlgorithms.ImplSumOfMultiplies(ps, pointMap, Abs);
    Exit;
  end;

  System.SetLength(pqs, len shl 1);

  i := 0;
  J := 0;

  while (i < len) do
  begin
    p := ps[i];
    Q := pointMap.Map(p);

    pqs[J] := p;
    System.Inc(J);
    pqs[J] := Q;
    System.Inc(J);
    System.Inc(i);
  end;

  result := TECAlgorithms.ImplSumOfMultiplies(pqs, Abs);
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
  i, J: Int32;
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
    J := off + i;
    System.Dec(i);
    tmp := zs[J];
    zs[J] := c[i].Multiply(u);
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
  Q, LP: IECPoint;
  t, i: Int32;
begin
  LP := p;
  x := k.Abs();
  Q := LP.Curve.infinity;
  t := x.BitLength;
  if (t > 0) then
  begin
    if (x.TestBit(0)) then
    begin
      Q := LP;
    end;
    i := 1;
    while (i < t) do
    begin
      LP := LP.Twice();
      if (x.TestBit(i)) then
      begin
        Q := Q.Add(LP);
      end;
      System.Inc(i);
    end;

  end;

  if k.SignValue < 0 then
  begin
    result := Q.Negate();
  end
  else
  begin
    result := Q;
  end;

end;

class function TECAlgorithms.ShamirsTrick(const p: IECPoint;
  const k: TBigInteger; const Q: IECPoint; const l: TBigInteger): IECPoint;
var
  cp: IECCurve;
  LQ: IECPoint;
begin
  cp := p.Curve;
  LQ := Q;
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
  const a: TBigInteger; const Q: IECPoint; const b: TBigInteger): IECPoint;
var
  cp: IECCurve;
  f2mCurve: IAbstractF2mCurve;
  glvEndomorphism: IGlvEndomorphism;
  LQ: IECPoint;
begin
  cp := p.Curve;
  LQ := Q;
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

{ TWNafUtilities }

class function TWNafUtilities.ResizeTable
  (const a: TCryptoLibGenericArray<IECPoint>; length: Int32)
  : TCryptoLibGenericArray<IECPoint>;
begin
  result := System.Copy(a);
  System.SetLength(result, length);
end;

class function TWNafUtilities.Trim(const a: TCryptoLibInt32Array; length: Int32)
  : TCryptoLibInt32Array;
begin
  result := System.Copy(a, 0, length);
end;

class function TWNafUtilities.Trim(const a: TCryptoLibByteArray; length: Int32)
  : TCryptoLibByteArray;
begin
  result := System.Copy(a, 0, length);
end;

class function TWNafUtilities.CheckTable(const table
  : TCryptoLibGenericArray<IECPoint>; reqLen: Int32): Boolean;
begin
  result := (table <> Nil) and (System.length(table) >= reqLen);
end;

class procedure TWNafUtilities.Boot;
begin
  FEMPTY_BYTES := Nil;
  FEMPTY_INTS := Nil;
end;

class constructor TWNafUtilities.CreateWNafUtilities;
begin
  TWNafUtilities.Boot;
end;

class function TWNafUtilities.CheckExisting(const existingWNaf
  : IWNafPreCompInfo; reqPreCompLen: Int32; includeNegated: Boolean): Boolean;
begin
  result := (existingWNaf <> Nil) and CheckTable(existingWNaf.PreComp,
    reqPreCompLen) and ((not includeNegated) or
    CheckTable(existingWNaf.PreCompNeg, reqPreCompLen));
end;

class function TWNafUtilities.GenerateCompactNaf(const k: TBigInteger)
  : TCryptoLibInt32Array;
var
  _3k, diff: TBigInteger;
  bits, highBit, &length, zeroes, i, digit: Int32;
  naf: TCryptoLibInt32Array;
begin
  if ((TBits.Asr32(k.BitLength, 16)) <> 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidBitLength);
  end;
  if (k.SignValue = 0) then
  begin
    result := FEMPTY_INTS;
    Exit;
  end;

  _3k := k.ShiftLeft(1).Add(k);

  bits := _3k.BitLength;
  System.SetLength(naf, TBits.Asr32(bits, 1));

  diff := _3k.&Xor(k);

  highBit := bits - 1;
  &length := 0;
  zeroes := 0;

  i := 1;

  while (i < highBit) do
  begin
    if (not diff.TestBit(i)) then
    begin
      System.Inc(zeroes);
      System.Inc(i);
      continue;
    end;

    if k.TestBit(i) then
    begin
      digit := -1;
    end
    else
    begin
      digit := 1;
    end;

    naf[length] := (digit shl 16) or zeroes;
    System.Inc(length);
    zeroes := 1;

    System.Inc(i, 2);

  end;

  naf[length] := (1 shl 16) or zeroes;
  System.Inc(length);

  if (System.length(naf) > length) then
  begin
    naf := Trim(naf, length);
  end;

  result := naf;
end;

class function TWNafUtilities.GenerateCompactWindowNaf(width: Int32;
  const k: TBigInteger): TCryptoLibInt32Array;
var
  wnaf: TCryptoLibInt32Array;
  pow2, mask, sign, &length, &pos, digit, zeroes: Int32;
  carry: Boolean;
  LK: TBigInteger;
begin
  LK := k;
  if (width = 2) then
  begin
    result := GenerateCompactNaf(LK);
    Exit;
  end;

  if ((width < 2) or (width > 16)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidRange);
  end;
  if ((TBits.Asr32(LK.BitLength, 16)) <> 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidBitLength);
  end;
  if (LK.SignValue = 0) then
  begin
    result := FEMPTY_INTS;
    Exit;
  end;

  System.SetLength(wnaf, (LK.BitLength div width) + 1);

  // 2^width and a mask and sign bit set accordingly
  pow2 := 1 shl width;
  mask := pow2 - 1;
  sign := TBits.Asr32(pow2, 1);

  carry := false;
  length := 0;
  pos := 0;

  while (pos <= LK.BitLength) do
  begin
    if (LK.TestBit(pos) = carry) then
    begin
      System.Inc(pos);
      continue;
    end;

    LK := LK.ShiftRight(pos);

    digit := LK.Int32Value and mask;
    if (carry) then
    begin
      System.Inc(digit);
    end;

    carry := (digit and sign) <> 0;
    if (carry) then
    begin
      digit := digit - pow2;
    end;

    if length > 0 then
    begin
      zeroes := pos - 1;
    end
    else
    begin
      zeroes := pos;
    end;

    wnaf[length] := (digit shl 16) or zeroes;
    System.Inc(length);
    pos := width;
  end;

  // Reduce the WNAF array to its actual length
  if (System.length(wnaf) > length) then
  begin
    wnaf := Trim(wnaf, length);
  end;

  result := wnaf;
end;

class function TWNafUtilities.GenerateJsf(const g, h: TBigInteger)
  : TCryptoLibByteArray;
var
  digits, J, d0, d1, offset, n0, n1, u0, u1: Int32;
  jsf: TCryptoLibByteArray;
  k0, k1: TBigInteger;
begin
  digits := Max(g.BitLength, h.BitLength) + 1;

  System.SetLength(jsf, digits);

  k0 := g;
  k1 := h;
  J := 0;
  d0 := 0;
  d1 := 0;

  offset := 0;

  while (((d0 or d1) <> 0) or (k0.BitLength > offset) or
    (k1.BitLength > offset)) do
  begin
    n0 := (Int32(UInt32(k0.Int32Value) shr offset) + d0) and 7;
    n1 := (Int32(UInt32(k1.Int32Value) shr offset) + d1) and 7;

    u0 := n0 and 1;
    if (u0 <> 0) then
    begin
      u0 := u0 - (n0 and 2);
      if (((n0 + u0) = 4) and ((n1 and 3) = 2)) then
      begin
        u0 := -u0;
      end;
    end;

    u1 := n1 and 1;
    if (u1 <> 0) then
    begin
      u1 := u1 - (n1 and 2);
      if (((n1 + u1) = 4) and ((n0 and 3) = 2)) then
      begin
        u1 := -u1;
      end;
    end;

    if ((d0 shl 1) = (1 + u0)) then
    begin
      d0 := d0 xor 1;
    end;
    if ((d1 shl 1) = (1 + u1)) then
    begin
      d1 := d1 xor 1;
    end;

    System.Inc(offset);
    if (offset = 30) then
    begin
      offset := 0;
      k0 := k0.ShiftRight(30);
      k1 := k1.ShiftRight(30);
    end;

    jsf[J] := Byte((u0 shl 4) or (u1 and $F));
    System.Inc(J);
  end;

  // Reduce the JSF array to its actual length
  if (System.length(jsf) > J) then
  begin
    jsf := Trim(jsf, J);
  end;

  result := jsf;
end;

class function TWNafUtilities.GenerateNaf(const k: TBigInteger)
  : TCryptoLibByteArray;
var
  _3k, diff: TBigInteger;
  digits, i: Int32;
  naf: TCryptoLibByteArray;
begin
  if (k.SignValue = 0) then
  begin
    result := FEMPTY_BYTES;
    Exit;
  end;

  _3k := k.ShiftLeft(1).Add(k);

  digits := _3k.BitLength - 1;
  System.SetLength(naf, digits);

  diff := _3k.&Xor(k);

  i := 1;

  while i < digits do
  begin
    if (diff.TestBit(i)) then
    begin
      if k.TestBit(i) then
      begin
        naf[i - 1] := Byte(-1);
      end
      else
      begin
        naf[i - 1] := Byte(1);
      end;

      System.Inc(i);
    end;
    System.Inc(i);
  end;

  naf[digits - 1] := 1;

  result := naf;
end;

class function TWNafUtilities.GenerateWindowNaf(width: Int32;
  const k: TBigInteger): TCryptoLibByteArray;
var
  wnaf: TCryptoLibByteArray;
  pow2, mask, sign, &length, &pos, digit: Int32;
  carry: Boolean;
  LK: TBigInteger;
begin
  LK := k;
  if (width = 2) then
  begin
    result := GenerateNaf(LK);
    Exit;
  end;

  if ((width < 2) or (width > 8)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidRange2);
  end;
  if (LK.SignValue = 0) then
  begin
    result := FEMPTY_BYTES;
    Exit;
  end;

  System.SetLength(wnaf, LK.BitLength + 1);

  // 2^width and a mask and sign bit set accordingly
  pow2 := 1 shl width;
  mask := pow2 - 1;
  sign := TBits.Asr32(pow2, 1);

  carry := false;
  length := 0;
  pos := 0;

  while (pos <= LK.BitLength) do
  begin
    if (LK.TestBit(pos) = carry) then
    begin
      System.Inc(pos);
      continue;
    end;

    LK := LK.ShiftRight(pos);

    digit := LK.Int32Value and mask;
    if (carry) then
    begin
      System.Inc(digit);
    end;

    carry := (digit and sign) <> 0;
    if (carry) then
    begin
      digit := digit - pow2;
    end;

    if length > 0 then
    begin
      length := length + (pos - 1);
    end
    else
    begin
      length := length + (pos);
    end;

    wnaf[length] := Byte(digit);
    System.Inc(length);
    pos := width;
  end;

  // Reduce the WNAF array to its actual length
  if (System.length(wnaf) > length) then
  begin
    wnaf := Trim(wnaf, length);
  end;

  result := wnaf;
end;

class function TWNafUtilities.GetNafWeight(const k: TBigInteger): Int32;
var
  _3k, diff: TBigInteger;
begin
  if (k.SignValue = 0) then
  begin
    result := 0;
    Exit;
  end;

  _3k := k.ShiftLeft(1).Add(k);
  diff := _3k.&Xor(k);

  result := diff.BitCount;
end;

class function TWNafUtilities.GetWindowSize(bits: Int32;
  const windowSizeCutoffs: array of Int32): Int32;
var
  w: Int32;
begin
  w := 0;
  while (w < System.length(windowSizeCutoffs)) do
  begin
    if (bits < windowSizeCutoffs[w]) then
    begin
      break;
    end;
    System.Inc(w);
  end;

  result := w + 2;
end;

class function TWNafUtilities.GetWindowSize(bits: Int32): Int32;
begin
  result := GetWindowSize(bits, FDEFAULT_WINDOW_SIZE_CUTOFFS);
end;

class function TWNafUtilities.GetWNafPreCompInfo(const preCompInfo
  : IPreCompInfo): IWNafPreCompInfo;
begin
  result := preCompInfo as IWNafPreCompInfo;
end;

class function TWNafUtilities.GetWNafPreCompInfo(const p: IECPoint)
  : IWNafPreCompInfo;
var
  preCompInfo: IPreCompInfo;
begin
  preCompInfo := p.Curve.GetPreCompInfo(p, PRECOMP_NAME);
  result := GetWNafPreCompInfo(preCompInfo);
end;

class function TWNafUtilities.MapPointWithPrecomp(const p: IECPoint;
  width: Int32; includeNegated: Boolean; const pointMap: IECPointMap): IECPoint;
var
  c: IECCurve;
  wnafPreCompP: IWNafPreCompInfo;
  Q: IECPoint;
begin
  c := p.Curve;

  wnafPreCompP := Precompute(p, width, includeNegated);

  Q := pointMap.Map(p);

  c.Precompute(Q, PRECOMP_NAME, TMapPointCallback.Create(wnafPreCompP,
    includeNegated, pointMap) as IMapPointCallback);

  result := Q;

end;

class function TWNafUtilities.Precompute(const p: IECPoint; width: Int32;
  includeNegated: Boolean): IWNafPreCompInfo;
begin
  result := p.Curve.Precompute(p, PRECOMP_NAME, TWNafCallback.Create(p, width,
    includeNegated) as IWNafCallback) as IWNafPreCompInfo;
end;

{ TWNafUtilities.TMapPointCallback }

constructor TWNafUtilities.TMapPointCallback.Create(const wnafPreCompP
  : IWNafPreCompInfo; includeNegated: Boolean; const pointMap: IECPointMap);
begin
  Inherited Create();
  Fm_wnafPreCompP := wnafPreCompP;
  Fm_includeNegated := includeNegated;
  Fm_pointMap := pointMap;
end;

function TWNafUtilities.TMapPointCallback.Precompute(const existing
  : IPreCompInfo): IPreCompInfo;
var
  tempResult: IWNafPreCompInfo;
  twiceP, twiceQ: IECPoint;
  preCompP, preCompQ, preCompNegQ: TCryptoLibGenericArray<IECPoint>;
  i: Int32;
begin
  tempResult := TWNafPreCompInfo.Create();

  twiceP := Fm_wnafPreCompP.Twice;
  if (twiceP <> Nil) then
  begin
    twiceQ := Fm_pointMap.Map(twiceP);
    tempResult.Twice := twiceQ;
  end;

  preCompP := Fm_wnafPreCompP.PreComp;

  System.SetLength(preCompQ, System.length(preCompP));
  for i := 0 to System.Pred(System.length(preCompP)) do
  begin
    preCompQ[i] := Fm_pointMap.Map(preCompP[i]);
  end;

  tempResult.PreComp := preCompQ;

  if (Fm_includeNegated) then
  begin

    System.SetLength(preCompNegQ, System.length(preCompQ));

    for i := 0 to System.Pred(System.length(preCompNegQ)) do
    begin
      preCompNegQ[i] := preCompQ[i].Negate();
    end;

    tempResult.PreCompNeg := preCompNegQ;
  end;

  result := tempResult;
end;

{ TWNafUtilities.TWNafCallback }

constructor TWNafUtilities.TWNafCallback.Create(const p: IECPoint; width: Int32;
  includeNegated: Boolean);
begin
  Inherited Create();
  Fm_p := p;
  Fm_width := width;
  Fm_includeNegated := includeNegated;
end;

function TWNafUtilities.TWNafCallback.Precompute(const existing: IPreCompInfo)
  : IPreCompInfo;
var
  twiceP, isoTwiceP, last: IECPoint;
  c: IECCurve;
  PreComp, PreCompNeg, EMPTY_POINTS: TCryptoLibGenericArray<IECPoint>;
  tempRes, existingWNaf: IWNafPreCompInfo;
  reqPreCompLen, iniPreCompLen, curPreCompLen, pos: Int32;
  iso, iso2, iso3: IECFieldElement;
begin
  EMPTY_POINTS := Nil;
  existingWNaf := existing as IWNafPreCompInfo;

  reqPreCompLen := 1 shl Max(0, Fm_width - 2);

  if (CheckExisting(existingWNaf, reqPreCompLen, Fm_includeNegated)) then
  begin
    result := existingWNaf;
    Exit;
  end;

  c := Fm_p.Curve;

  if (existingWNaf <> Nil) then
  begin
    PreComp := existingWNaf.PreComp;
    PreCompNeg := existingWNaf.PreCompNeg;
    twiceP := existingWNaf.Twice;
  end;

  iniPreCompLen := 0;
  if (PreComp = Nil) then
  begin
    PreComp := EMPTY_POINTS;
  end
  else
  begin
    iniPreCompLen := System.length(PreComp);
  end;

  if (iniPreCompLen < reqPreCompLen) then
  begin
    PreComp := TWNafUtilities.ResizeTable(PreComp, reqPreCompLen);

    if (reqPreCompLen = 1) then
    begin
      PreComp[0] := Fm_p.Normalize();
    end
    else
    begin
      curPreCompLen := iniPreCompLen;
      if (curPreCompLen = 0) then
      begin
        PreComp[0] := Fm_p;
        curPreCompLen := 1;
      end;

      if (reqPreCompLen = 2) then
      begin
        PreComp[1] := Fm_p.threeTimes();
      end
      else
      begin
        isoTwiceP := twiceP;
        last := PreComp[curPreCompLen - 1];
        if (isoTwiceP = Nil) then
        begin
          isoTwiceP := PreComp[0].Twice();
          twiceP := isoTwiceP;
          //
          // /*
          // * For Fp curves with Jacobian projective coordinates, use a (quasi-)isomorphism
          // * where 'twiceP' is "affine", so that the subsequent additions are cheaper. This
          // * also requires scaling the initial point's X, Y coordinates, and reversing the
          // * isomorphism as part of the subsequent normalization.
          // *
          // *  NOTE: The correctness of this optimization depends on:
          // *      1) additions do not use the curve's A, B coefficients.
          // *      2) no special cases (i.e. Q +/- Q) when calculating 1P, 3P, 5P, ...
          // */
          if ((not(twiceP.IsInfinity)) and (TECAlgorithms.IsFpCurve(c)) and
            (c.FieldSize >= 64)) then
          begin
            case (c.CoordinateSystem) of
              TECCurveConstants.COORD_JACOBIAN,
                TECCurveConstants.COORD_JACOBIAN_CHUDNOVSKY,
                TECCurveConstants.COORD_JACOBIAN_MODIFIED:

                begin
                  iso := twiceP.GetZCoord(0);
                  isoTwiceP := c.CreatePoint(twiceP.XCoord.ToBigInteger,
                    twiceP.YCoord.ToBigInteger());

                  iso2 := iso.square();
                  iso3 := iso2.Multiply(iso);
                  last := last.scaleX(iso2).scaleY(iso3);

                  if (iniPreCompLen = 0) then
                  begin
                    PreComp[0] := last;
                  end;
                end;

            end;

          end;
        end;

        while (curPreCompLen < reqPreCompLen) do
        begin
          // /*
          // * Compute the new ECPoints for the precomputation array. The values 1, 3,
          // * 5, ..., 2^(width-1)-1 times p are computed
          // */
          last := last.Add(isoTwiceP);
          PreComp[curPreCompLen] := last;
          System.Inc(curPreCompLen);
        end;
      end;
      //
      // /*
      // * Having oft-used operands in affine form makes operations faster.
      // */
      c.NormalizeAll(PreComp, iniPreCompLen,
        reqPreCompLen - iniPreCompLen, iso);
    end;
  end;

  if (Fm_includeNegated) then
  begin

    if (PreCompNeg = Nil) then
    begin
      pos := 0;
      System.SetLength(PreCompNeg, reqPreCompLen);

    end
    else
    begin
      pos := System.length(PreCompNeg);
      if (pos < reqPreCompLen) then
      begin
        PreCompNeg := TWNafUtilities.ResizeTable(PreCompNeg, reqPreCompLen);
      end;
    end;

    while (pos < reqPreCompLen) do
    begin
      PreCompNeg[pos] := PreComp[pos].Negate();
      System.Inc(pos);
    end;
  end;

  tempRes := TWNafPreCompInfo.Create();
  tempRes.PreComp := PreComp;
  tempRes.PreCompNeg := PreCompNeg;
  tempRes.Twice := twiceP;

  result := tempRes;
end;

end.
