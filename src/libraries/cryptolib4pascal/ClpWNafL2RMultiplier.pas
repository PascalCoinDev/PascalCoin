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

unit ClpWNafL2RMultiplier;

{$I CryptoLib.inc}

interface

uses
  Math,
  ClpBits,
  ClpBigInteger,
  ClpLongArray,
  ClpCryptoLibTypes,
  ClpIECC,
  ClpIWNafPreCompInfo,
  ClpECAlgorithms,
  ClpAbstractECMultiplier,
  ClpIWNafL2RMultiplier;

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
    function GetWindowSize(bits: Int32): Int32; virtual;

  public

    constructor Create();
    destructor Destroy; override;

  end;

implementation

{ TWNafL2RMultiplier }

constructor TWNafL2RMultiplier.Create;
begin
  Inherited Create();
end;

destructor TWNafL2RMultiplier.Destroy;
begin
  inherited Destroy;
end;

function TWNafL2RMultiplier.GetWindowSize(bits: Int32): Int32;
begin
  Result := TWNafUtilities.GetWindowSize(bits);
end;

function TWNafL2RMultiplier.MultiplyPositive(const p: IECPoint;
  const k: TBigInteger): IECPoint;
var
  width, i, wi, digit, zeroes, n, highest, scale, lowBits, i1, i2: Int32;
  wnafPreCompInfo: IWNafPreCompInfo;
  preComp, preCompNeg, table: TCryptoLibGenericArray<IECPoint>;
  wnaf: TCryptoLibInt32Array;
  R, lr: IECPoint;
begin
  // Clamp the window width in the range [2, 16]
  width := Math.Max(2, Math.Min(16, GetWindowSize(k.BitLength)));

  wnafPreCompInfo := TWNafUtilities.Precompute(p, width, true);
  preComp := wnafPreCompInfo.preComp;
  preCompNeg := wnafPreCompInfo.preCompNeg;

  wnaf := TWNafUtilities.GenerateCompactWindowNaf(width, k);

  R := p.Curve.Infinity;

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
      highest := TLongArray.BitLengths[n];

      // TODO Get addition/doubling cost ratio from curve and compare to 'scale' to see if worth substituting?
      scale := width - highest;
      lowBits := n xor (1 shl (highest - 1));

      i1 := ((1 shl (width - 1)) - 1);
      i2 := (lowBits shl scale) + 1;
      R := table[TBits.Asr32(i1, 1)].Add(table[TBits.Asr32(i2, 1)]);

      zeroes := zeroes - scale;

      // Console.WriteLine("Optimized: 2^" + scale + " * " + n + " = " + i1 + " + " + i2);
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

  Result := R;

  wnafPreCompInfo.preComp := Nil; // Review
  wnafPreCompInfo.preCompNeg := Nil; // Review

end;

end.
