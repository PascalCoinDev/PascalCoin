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

unit ClpWTauNafMultiplier;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBits,
  ClpBigInteger,
  ClpCryptoLibTypes,
  ClpTNaf,
  ClpAbstractECMultiplier,
  ClpIWTauNafPreCompInfo,
  ClpWTauNafPreCompInfo,
  ClpIECC,
  ClpIPreCompCallBack,
  ClpIZTauElement,
  ClpIPreCompInfo,
  ClpIWTauNafMultiplier;

resourcestring
  SInCompatiblePoint = 'Only AbstractF2mPoint can be used in WTauNafMultiplier';

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
    destructor Destroy; override;

  end;

implementation

{ TWTauNafMultiplier }

constructor TWTauNafMultiplier.Create;
begin
  Inherited Create();
end;

destructor TWTauNafMultiplier.Destroy;
begin
  inherited Destroy;
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
  callback: IWTauNafCallback;
begin
  curve := p.curve as IAbstractF2mCurve;
  a := ShortInt(curve.a.ToBigInteger().Int32Value);

  callback := TWTauNafCallback.Create(p, a);
  pre := curve.Precompute(p, PRECOMP_NAME, callback) as IWTauNafPreCompInfo;

  pu := pre.PreComp;
  // TODO Include negations in precomp (optionally) and use from here
  System.SetLength(puNeg, System.Length(pu));
  for i := 0 to System.Pred(System.Length(pu)) do
  begin
    puNeg[i] := pu[i].Negate() as IAbstractF2mPoint;
  end;

  // q = infinity
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

      q := q.Add(x) as IAbstractF2mPoint;
    end;
    System.Dec(i);
  end;
  if (tauCount > 0) then
  begin
    q := q.TauPow(tauCount);
  end;
  result := q;

  pre.PreComp := Nil; // Review

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

  tw := TTnaf.GetTw(mu, TTnaf.Width);

  u := TTnaf.TauAdicWNaf(mu, lambda, TTnaf.Width,
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
  tempResult.PreComp := TTnaf.GetPreComp(Fm_p, Fm_a);
  result := tempResult;
end;

end.
