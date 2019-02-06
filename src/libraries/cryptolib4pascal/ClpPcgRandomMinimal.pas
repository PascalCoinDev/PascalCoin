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

unit ClpPcgRandomMinimal;

{$I CryptoLib.inc}
(*
  * PCG Random Number Generation for Pascal.
  *
  * Copyright 2015 Ugochukwu Mmaduekwe <ugo4brain@gmail.com>
  * Copyright 2015 Kevin Harris <kevin@studiotectorum.com>
  * Copyright 2014 Melissa O'Neill <oneill@pcg-random.org>
  *
  * For additional information about the PCG random number generation scheme,
  * including its license and other licensing options, visit
  *
  *     http://www.pcg-random.org
*)

interface

uses
  SysUtils;

type

  /// <summary>
  /// The Pcg Random Number Class.
  /// </summary>

  TPcg = class sealed(TObject)

  strict private

  class var
    /// <summary>
    /// The RNG state. All values are possible.
    /// </summary>
    Fm_state: UInt64;

    /// <summary>
    /// Controls which RNG sequence (stream) is selected.
    /// Must <strong>always</strong> be odd.
    /// </summary>
    Fm_inc: UInt64;

    /// <summary>
    /// Record to help Cast a Comp to an Int64.
    /// </summary>
    /// <returns> Int64 </returns>

  type

    CompConverter = record
    private
      /// <summary>
      /// Internal variable used for Casting.
      /// </summary>
      FI64: Int64;
    end;

    /// <summary>
    /// static class constructor.
    /// Initializes a new instance of the <see cref="TPcg"/> class
    /// <strong>FOR USAGE</strong> with <strong>SYSTEM TIME</strong> as initState.
    /// </summary>

  class constructor Create;

  /// <summary>
  /// Seed Pcg in two parts, a state initializer
  /// and a sequence selection constant (a.k.a.
  /// stream id).
  /// </summary>
  /// <param name="initState">Initial state.</param>
  /// <param name="initSeq">Initial sequence</param>

  class procedure Seed(initState: UInt64; initSeq: UInt64); static;

  /// <summary>
  /// Generates a uniformly distributed number, r,
  /// where 0 &lt;= r &lt; exclusiveBound.
  /// </summary>
  /// <param name="exclusiveBound">Exclusive bound.</param>

  class function Range32(exclusiveBound: UInt32): UInt32; static;

  /// <summary>
  /// Generates an Init State from System Time.
  /// </summary>
  /// <param name="initSeq">Calculated initSeq.</param>
  /// <returns> UInt64 </returns>

  class function GetInitState(out initSeq: UInt64): UInt64; static; inline;

  /// <summary>
  /// Generates an Init Sequence from GetInitState value * 181783497276652981.
  /// <param name="tempVal">Previous value from GetInitState.</param>
  /// </summary>
  /// <returns> UInt64 </returns>

  class function GetInitSeq(tempVal: UInt64): UInt64; static; inline;

  public

    /// <summary>
    /// Initializes a new instance of the <see cref="TPcg"/> class
    /// <strong>FOR TESTING</strong> with a <strong>KNOWN</strong> seed.
    /// </summary>

    constructor Create(); overload;

    /// <summary>
    /// Initializes a new instance of the <see cref="TPcg"/> class.
    /// </summary>
    /// <param name="initState">Initial state.</param>
    /// <param name="initSeq">Initial sequence</param>

    constructor Create(initState: UInt64; initSeq: UInt64); overload;

    /// <summary>
    /// Generates a uniformly-distributed 32-bit random number.
    /// </summary>

    class function NextUInt32(): UInt32; overload; inline;

    /// <summary>
    /// Generates a uniformly distributed number, r,
    /// where minimum &lt;= r &lt; exclusiveBound.
    /// </summary>
    /// <param name="minimum">The minimum inclusive value.</param>
    /// <param name="exclusiveBound">The maximum exclusive bound.</param>

    class function NextUInt32(minimum: UInt32; exclusiveBound: UInt32): UInt32;
      overload; inline;

    /// <summary>
    /// Generates a uniformly distributed number, r,
    /// where minimum &lt;= r &lt; exclusiveBound.
    /// </summary>
    /// <param name="minimum">The minimum inclusive value.</param>
    /// <param name="exclusiveBound">The maximum exclusive bound.</param>

    class function NextInt(minimum: Integer; exclusiveBound: Integer)
      : Integer; inline;

  end;

implementation

class function TPcg.NextUInt32(): UInt32;
var
  oldState, LValue: UInt64;
  xorShifted: UInt32;
  rot: Int32;
begin
  oldState := Fm_state;
  LValue := UInt64(6364136223846793005);
  Fm_state := oldState * LValue + Fm_inc;
  xorShifted := UInt32(((oldState shr 18) xor oldState) shr 27);
  rot := Int32(oldState shr 59);
  result := (xorShifted shr rot) or (xorShifted shl ((-rot) and 31));
end;

class function TPcg.NextUInt32(minimum: UInt32; exclusiveBound: UInt32): UInt32;
var
  boundRange, rangeResult: UInt32;
begin
  boundRange := exclusiveBound - minimum;
  rangeResult := Range32(boundRange);
  result := rangeResult + minimum;
end;

class function TPcg.Range32(exclusiveBound: UInt32): UInt32;
var
  r, threshold: UInt32;
begin
  // To avoid bias, we need to make the range of the RNG
  // a multiple of bound, which we do by dropping output
  // less than a threshold. A naive scheme to calculate the
  // threshold would be to do
  //
  // threshold = UInt64($100000000) mod exclusiveBound;
  //
  // but 64-bit div/mod is slower than 32-bit div/mod
  // (especially on 32-bit platforms). In essence, we do
  //
  // threshold := UInt32((UInt64($100000000) - exclusiveBound) mod exclusiveBound);
  //
  // because this version will calculate the same modulus,
  // but the LHS value is less than 2^32.
  threshold := UInt32((UInt64($100000000) - exclusiveBound) mod exclusiveBound);

  // Uniformity guarantees that this loop will terminate.
  // In practice, it should terminate quickly; on average
  // (assuming all bounds are equally likely), 82.25% of
  // the time, we can expect it to require just one
  // iteration. In the worst case, someone passes a bound
  // of 2^31 + 1 (i.e., 2147483649), which invalidates
  // almost 50% of the range. In practice bounds are
  // typically small and only a tiny amount of the range
  // is eliminated.
  while True do
  begin
    r := NextUInt32();
    if (r >= threshold) then
    begin
      result := r mod exclusiveBound;
      Exit;
    end;

  end;
  result := 0; // to make FixInsight Happy :)
end;

class procedure TPcg.Seed(initState: UInt64; initSeq: UInt64);
begin
  Fm_state := UInt32(0);
  Fm_inc := (initSeq shl 1) or UInt64(1);
  NextUInt32();
  Fm_state := Fm_state + initState;
  NextUInt32();
end;

class function TPcg.GetInitSeq(tempVal: UInt64): UInt64;

begin
  result := tempVal * UInt64(181783497276652981);
end;

class function TPcg.GetInitState(out initSeq: UInt64): UInt64;

begin
  result := UInt64
    (CompConverter(TimeStampToMsecs(DateTimeToTimeStamp(Now))).FI64);
  initSeq := GetInitSeq(result) * UInt64(Int64(1000000));

end;

// static class constructor
class constructor TPcg.Create();
var
  LinitState, LinitSeq: UInt64;
begin

  LinitState := GetInitState(LinitSeq);
  // ==> initializes using system time as initState and calculated value as
  // initSeq
  Seed(LinitState, LinitSeq);

end;

constructor TPcg.Create();
var
  LinitState, LinitSeq: UInt64;
begin
  // ==> initializes using default seeds. you can change it to any reasonable
  // value
  LinitState := UInt64($853C49E6748FEA9B);
  LinitSeq := UInt64($DA3E39CB94B95BDB);
  Seed(LinitState, LinitSeq);
end;

constructor TPcg.Create(initState: UInt64; initSeq: UInt64);
begin
  Seed(initState, initSeq);
end;

class function TPcg.NextInt(minimum: Integer; exclusiveBound: Integer): Integer;
var
  boundRange, rangeResult: UInt32;
begin
  boundRange := UInt32(exclusiveBound - minimum);
  rangeResult := Range32(boundRange);
  result := Integer(rangeResult) + Integer(minimum);
end;

end.
