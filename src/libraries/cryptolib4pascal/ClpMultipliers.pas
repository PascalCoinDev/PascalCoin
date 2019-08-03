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

unit ClpMultipliers;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpBits,
  ClpNat,
  ClpTnaf,
  ClpIECC,
  ClpIGlvEndomorphism,
  ClpIFixedPointPreCompInfo,
  ClpIWTauNafPreCompInfo,
  ClpWTauNafPreCompInfo,
  ClpIZTauElement,
  ClpIPreCompInfo,
  ClpIPreCompCallBack,
  ClpIMultipliers,
  ClpIWNafPreCompInfo,
  ClpECCompUtilities,
  ClpSetWeakRef,
  ClpCryptoLibTypes;

resourcestring
  SInvalidComputation =
    'Fixed-Point Comb Doesn''t Support Scalars Larger Than The Curve Order';
  SCurveUnknownGroupOrder = 'Need Curve With Known Group Order, "curve"';
  SInCompatiblePoint = 'Only AbstractF2mPoint can be used in WTauNafMultiplier';

type
  TAbstractECMultiplier = class abstract(TInterfacedObject,
    IAbstractECMultiplier, IECMultiplier)

  strict protected

    function CheckResult(const p: IECPoint): IECPoint; virtual;
    function MultiplyPositive(const p: IECPoint; const k: TBigInteger)
      : IECPoint; virtual; abstract;

  public

    constructor Create();
    function Multiply(const p: IECPoint; const k: TBigInteger)
      : IECPoint; virtual;

  end;

type
  TFixedPointCombMultiplier = class sealed(TAbstractECMultiplier,
    IFixedPointCombMultiplier)

  strict protected
    function MultiplyPositive(const p: IECPoint; const k: TBigInteger)
      : IECPoint; override;

  public
    constructor Create();

  end;

type
  TGlvMultiplier = class(TAbstractECMultiplier, IGlvMultiplier)

  strict protected
  var
    Fcurve: IECCurve;
    FglvEndomorphism: IGlvEndomorphism;

    function MultiplyPositive(const p: IECPoint; const k: TBigInteger)
      : IECPoint; override;

  public
    constructor Create(const curve: IECCurve;
      const glvEndomorphism: IGlvEndomorphism);
    destructor Destroy; override;

  end;

type

  /// <summary>
  /// Class implementing the WNAF (Window Non-Adjacent Form) multiplication
  /// algorithm.
  /// </summary>
  TWNafL2RMultiplier = class(TAbstractECMultiplier, IWNafL2RMultiplier)

  strict protected
    // /**
    // * Multiplies <code>this</code> by an integer <code>k</code> using the
    // * Window NAF method.
    // * @param k The integer by which <code>this</code> is multiplied.
    // * @return A new <code>ECPoint</code> which equals <code>this</code>
    // * multiplied by <code>k</code>.
    // */
    function MultiplyPositive(const p: IECPoint; const k: TBigInteger)
      : IECPoint; override;

  public

    constructor Create();

  end;

type
  /// **
  // * Class implementing the WTNAF (Window
  // * <code>&#964;</code>-adic Non-Adjacent Form) algorithm.
  // */
  TWTauNafMultiplier = class(TAbstractECMultiplier, IWTauNafMultiplier)

  strict private
    // TODO Create WTauNafUtilities class and move various functionality into it

  type
    IWTauNafCallback = interface(IPreCompCallback)
      ['{4D6F7B4A-B925-42C9-8D60-B7F24632EDC1}']

    end;

  type
    TWTauNafCallback = class(TInterfacedObject, IPreCompCallback,
      IWTauNafCallback)

    strict private
    var
      Fm_p: IAbstractF2mPoint;
      Fm_a: ShortInt;

    public
      constructor Create(const p: IAbstractF2mPoint; a: ShortInt);

      function Precompute(const existing: IPreCompInfo): IPreCompInfo;

    end;

  const
    PRECOMP_NAME: String = 'bc_wtnaf';

    // /**
    // * Multiplies an AbstractF2mPoint
    // * by an element <code>&#955;</code> of <code><b>Z</b>[&#964;]</code> using
    // * the <code>&#964;</code>-adic NAF (TNAF) method.
    // * @param p The AbstractF2mPoint to multiply.
    // * @param lambda The element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code> of which to compute the
    // * <code>[&#964;]</code>-adic NAF.
    // * @return <code>p</code> multiplied by <code>&#955;</code>.
    // */
    function MultiplyWTnaf(const p: IAbstractF2mPoint;
      const lambda: IZTauElement; a, mu: ShortInt): IAbstractF2mPoint; inline;

    // /**
    // * Multiplies an AbstractF2mPoint
    // * by an element <code>&#955;</code> of <code><b>Z</b>[&#964;]</code>
    // * using the window <code>&#964;</code>-adic NAF (TNAF) method, given the
    // * WTNAF of <code>&#955;</code>.
    // * @param p The AbstractF2mPoint to multiply.
    // * @param u The the WTNAF of <code>&#955;</code>..
    // * @return <code>&#955; * p</code>
    // */
    class function MultiplyFromWTnaf(const p: IAbstractF2mPoint;
      const u: TCryptoLibShortIntArray): IAbstractF2mPoint; static;

  strict protected
    // /**
    // * Multiplies an AbstractF2mPoint
    // * by <code>k</code> using the reduced <code>&#964;</code>-adic NAF (RTNAF)
    // * method.
    // * @param p The AbstractF2mPoint to multiply.
    // * @param k The integer by which to multiply <code>k</code>.
    // * @return <code>p</code> multiplied by <code>k</code>.
    // */
    function MultiplyPositive(const point: IECPoint; const k: TBigInteger)
      : IECPoint; override;

  public
    constructor Create();

  end;

implementation

uses
  ClpECAlgorithms; // included here to avoid circular dependency :)

{ TAbstractECMultiplier }

function TAbstractECMultiplier.CheckResult(const p: IECPoint): IECPoint;
begin
  result := TECAlgorithms.ImplCheckResult(p);
end;

constructor TAbstractECMultiplier.Create;
begin
  Inherited Create();
end;

function TAbstractECMultiplier.Multiply(const p: IECPoint; const k: TBigInteger)
  : IECPoint;
var
  positive: IECPoint;
  sign: Int32;
begin

  sign := k.SignValue;
  if ((sign = 0) or (p.IsInfinity)) then
  begin
    result := p.curve.Infinity;
    Exit;
  end;

  positive := MultiplyPositive(p, k.Abs());

  if sign > 0 then
  begin
    result := positive
  end
  else
  begin
    result := positive.Negate();
  end;

  // /*
  // * Although the various multipliers ought not to produce invalid output under normal
  // * circumstances, a final check here is advised to guard against fault attacks.
  // */
  result := CheckResult(result);

end;

{ TFixedPointCombMultiplier }

constructor TFixedPointCombMultiplier.Create;
begin
  Inherited Create();
end;

function TFixedPointCombMultiplier.MultiplyPositive(const p: IECPoint;
  const k: TBigInteger): IECPoint;
var
  c: IECCurve;
  R, add: IECPoint;
  size, width, d, top, i, j, fullComb: Int32;
  secretIndex, secretBit: UInt32;
  info: IFixedPointPreCompInfo;
  lookupTable: IECLookupTable;
  LK: TCryptoLibUInt32Array;
begin
  c := p.curve;
  size := TFixedPointUtilities.GetCombSize(c);
  if (k.BitLength > size) then
  begin
    // /*
    // * TODO The comb works best when the scalars are less than the (possibly unknown) order.
    // * Still, if we want to handle larger scalars, we could allow customization of the comb
    // * size, or alternatively we could deal with the 'extra' bits either by running the comb
    // * multiple times as necessary, or by using an alternative multiplier as prelude.
    // */
    raise EInvalidOperationCryptoLibException.CreateRes(@SInvalidComputation);
  end;

  info := TFixedPointUtilities.Precompute(p);
  lookupTable := info.lookupTable;
  width := info.width;

  d := (size + width - 1) div width;

  R := c.Infinity;
  fullComb := d * width;
  LK := TNat.FromBigInteger(fullComb, k);

  top := fullComb - 1;

  for i := 0 to System.Pred(d) do
  begin

    secretIndex := 0;

    j := (top - i);

    while j >= 0 do
    begin

      secretBit := LK[TBits.Asr32(j, 5)] shr (j and $1F);
      secretIndex := secretIndex xor (secretBit shr 1);
      secretIndex := secretIndex shl 1;
      secretIndex := secretIndex xor secretBit;

      System.Dec(j, d);
    end;

    add := lookupTable.Lookup(Int32(secretIndex));
    R := R.TwicePlus(add);

  end;

  result := R.add(info.Offset);

end;

{ TGlvMultiplier }

constructor TGlvMultiplier.Create(const curve: IECCurve;
  const glvEndomorphism: IGlvEndomorphism);
begin
  inherited Create();
  if ((curve = Nil) or (not(curve.Order.IsInitialized))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SCurveUnknownGroupOrder);
  end;

  TSetWeakRef.SetWeakReference(@Fcurve, curve);
  FglvEndomorphism := glvEndomorphism;
end;

destructor TGlvMultiplier.Destroy;
begin
  TSetWeakRef.SetWeakReference(@Fcurve, Nil);
  inherited Destroy;
end;

function TGlvMultiplier.MultiplyPositive(const p: IECPoint;
  const k: TBigInteger): IECPoint;
var
  n, a, b: TBigInteger;
  ab: TCryptoLibGenericArray<TBigInteger>;
  q: IECPoint;
begin
  if (not(Fcurve.Equals(p.curve))) then
  begin
    raise EInvalidOperationCryptoLibException.Create('');
  end;

  n := p.curve.Order;
  ab := FglvEndomorphism.DecomposeScalar(k.&Mod(n));
  a := ab[0];
  b := ab[1];

  if (FglvEndomorphism.HasEfficientPointMap) then
  begin
    result := TECAlgorithms.ImplShamirsTrickWNaf(FglvEndomorphism, p, a, b);
    Exit;
  end;

  q := TEndoUtilities.MapPoint(FglvEndomorphism, p);

  result := TECAlgorithms.ImplShamirsTrickWNaf(p, a, q, b);
end;

{ TWNafL2RMultiplier }

constructor TWNafL2RMultiplier.Create;
begin
  Inherited Create();
end;

function TWNafL2RMultiplier.MultiplyPositive(const p: IECPoint;
  const k: TBigInteger): IECPoint;
var
  width, minWidth, i, wi, digit, zeroes, n, highest, scale, lowBits, i1,
    i2: Int32;
  info: IWNafPreCompInfo;
  preComp, preCompNeg, table: TCryptoLibGenericArray<IECPoint>;
  wnaf: TCryptoLibInt32Array;
  R, lr: IECPoint;
begin
  minWidth := TWNafUtilities.GetWindowSize(k.BitLength);

  info := TWNafUtilities.Precompute(p, minWidth, true);
  preComp := info.preComp;
  preCompNeg := info.preCompNeg;
  width := info.width;

  wnaf := TWNafUtilities.GenerateCompactWindowNaf(width, k);

  R := p.curve.Infinity;

  i := System.Length(wnaf);

  // /*
  // * NOTE: We try to optimize the first window using the precomputed points to substitute an
  // * addition for 2 or more doublings.
  // */
  if (i > 1) then
  begin
    System.Dec(i);
    wi := wnaf[i];
    digit := TBits.Asr32(wi, 16);
    zeroes := wi and $FFFF;

    n := System.Abs(digit);
    if digit < 0 then
    begin
      table := preCompNeg;
    end
    else
    begin
      table := preComp;
    end;

    // Optimization can only be used for values in the lower half of the table
    if ((n shl 2) < (1 shl width)) then
    begin
      highest := 32 - TBits.NumberOfLeadingZeros(n);

      // TODO Get addition/doubling cost ratio from curve and compare to 'scale' to see if worth substituting?
      scale := width - highest;
      lowBits := n xor (1 shl (highest - 1));

      i1 := ((1 shl (width - 1)) - 1);
      i2 := (lowBits shl scale) + 1;
      R := table[TBits.Asr32(i1, 1)].add(table[TBits.Asr32(i2, 1)]);

      zeroes := zeroes - scale;
    end
    else
    begin
      R := table[TBits.Asr32(n, 1)];
    end;

    R := R.TimesPow2(zeroes);
  end;

  while (i > 0) do
  begin
    System.Dec(i);
    wi := wnaf[i];
    digit := TBits.Asr32(wi, 16);
    zeroes := wi and $FFFF;

    n := System.Abs(digit);
    if digit < 0 then
    begin
      table := preCompNeg;
    end
    else
    begin
      table := preComp;
    end;

    lr := table[TBits.Asr32(n, 1)];

    R := R.TwicePlus(lr);
    R := R.TimesPow2(zeroes);
  end;

  result := R;

  info.preComp := Nil; // Review
  info.preCompNeg := Nil; // Review

end;

{ TWTauNafMultiplier }

constructor TWTauNafMultiplier.Create;
begin
  Inherited Create();
end;

class function TWTauNafMultiplier.MultiplyFromWTnaf(const p: IAbstractF2mPoint;
  const u: TCryptoLibShortIntArray): IAbstractF2mPoint;
var
  curve: IAbstractF2mCurve;
  a: ShortInt;
  i, tauCount, ui: Int32;
  pu, puNeg: TCryptoLibGenericArray<IAbstractF2mPoint>;
  pre: IWTauNafPreCompInfo;
  q: IAbstractF2mPoint;
  x: IECPoint;
begin
  curve := p.curve as IAbstractF2mCurve;
  a := ShortInt(curve.a.ToBigInteger().Int32Value);

  pre := curve.Precompute(p, PRECOMP_NAME, TWTauNafCallback.Create(p, a)
    as IWTauNafCallback) as IWTauNafPreCompInfo;

  pu := pre.preComp;
  // TODO Include negations in precomp (optionally) and use from here
  System.SetLength(puNeg, System.Length(pu));
  for i := 0 to System.Pred(System.Length(pu)) do
  begin
    puNeg[i] := pu[i].Negate() as IAbstractF2mPoint;
  end;

  q := p.curve.Infinity as IAbstractF2mPoint;
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
        x := pu[TBits.Asr32(ui, 1)];
      end
      else
      begin
        x := puNeg[TBits.Asr32(-ui, 1)];
      end;

      q := q.add(x) as IAbstractF2mPoint;
    end;
    System.Dec(i);
  end;
  if (tauCount > 0) then
  begin
    q := q.TauPow(tauCount);
  end;
  result := q;

  pre.preComp := Nil; // Review

end;

function TWTauNafMultiplier.MultiplyWTnaf(const p: IAbstractF2mPoint;
  const lambda: IZTauElement; a, mu: ShortInt): IAbstractF2mPoint;
var
  alpha: TCryptoLibGenericArray<IZTauElement>;
  tw: TBigInteger;
  u: TCryptoLibShortIntArray;
begin
  if a = 0 then
  begin
    alpha := TTnaf.Alpha0;
  end
  else
  begin
    alpha := TTnaf.Alpha1;
  end;

  tw := TTnaf.GetTw(mu, TTnaf.width);

  u := TTnaf.TauAdicWNaf(mu, lambda, TTnaf.width,
    TBigInteger.ValueOf(TTnaf.Pow2Width), tw, alpha);

  result := MultiplyFromWTnaf(p, u);
end;

function TWTauNafMultiplier.MultiplyPositive(const point: IECPoint;
  const k: TBigInteger): IECPoint;
var
  p: IAbstractF2mPoint;
  curve: IAbstractF2mCurve;
  m: Int32;
  a, mu: ShortInt;
  s: TCryptoLibGenericArray<TBigInteger>;
  rho: IZTauElement;
begin
  if (not(Supports(point, IAbstractF2mPoint))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInCompatiblePoint);
  end;

  p := point as IAbstractF2mPoint;
  curve := p.curve as IAbstractF2mCurve;
  m := curve.FieldSize;
  a := ShortInt(curve.a.ToBigInteger().Int32Value);
  mu := TTnaf.GetMu(a);
  s := curve.GetSi();

  rho := TTnaf.PartModReduction(k, m, a, s, mu, ShortInt(10));

  result := MultiplyWTnaf(p, rho, a, mu);

end;

{ TWTauNafMultiplier.TWTauNafCallback }

constructor TWTauNafMultiplier.TWTauNafCallback.Create
  (const p: IAbstractF2mPoint; a: ShortInt);
begin
  Inherited Create();
  Fm_p := p;
  Fm_a := a;
end;

function TWTauNafMultiplier.TWTauNafCallback.Precompute(const existing
  : IPreCompInfo): IPreCompInfo;
var
  tempResult: IWTauNafPreCompInfo;
begin

  // Review uncomment
  // if (Supports(existing, IWTauNafPreCompInfo)) then
  // begin
  // result := existing;
  // Exit;
  // end;

  tempResult := TWTauNafPreCompInfo.Create();
  tempResult.preComp := TTnaf.GetPreComp(Fm_p, Fm_a);
  result := tempResult;
end;

end.
