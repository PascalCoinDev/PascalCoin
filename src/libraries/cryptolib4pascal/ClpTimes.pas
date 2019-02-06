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

unit ClpTimes;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  DateUtils;

type
  TTimes = class sealed(TObject)

  strict private
  const
    NanosecondsPerTick = Int64(100);
    TicksMask = UInt64($3FFFFFFFFFFFFFFF);

    // Returns the tick count for this DateTime. The returned value is
    // the number of 100-nanosecond intervals that have elapsed since 1/1/0001
    // 12:00am.
    //
    class function Ticks(): Int64; static; inline;

    class function Now(): TDateTime; static; inline;

  public

    class function NanoTime(): Int64; static; inline;

  end;

implementation

{ TTimes }

class function TTimes.Now: TDateTime;
begin
  Result := SysUtils.Now;
end;

class function TTimes.Ticks: Int64;
var
  dt: TDateTime;
  value: UInt64;
begin
  dt := EncodeDateTime(1, 1, 1, 0, 0, 0, 0);
  value := UInt64(((MilliSecondsBetween(Now, dt) * 10000)));

  Result := Int64(value and TicksMask);
end;

class function TTimes.NanoTime: Int64;
begin
  Result := Ticks * NanosecondsPerTick;
end;

end.
