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

unit ClpHMacDsaKCalculator;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  Math,
  ClpHMac,
  ClpIHMac,
  ClpIDigest,
  ClpISecureRandom,
  ClpBigInteger,
  ClpBigIntegers,
  ClpKeyParameter,
  ClpIKeyParameter,
  ClpIDsaKCalculator,
  ClpIHMacDsaKCalculator,
  ClpArrayUtils,
  ClpCryptoLibTypes;

{$IFNDEF _FIXINSIGHT_}

resourcestring
  SUnSupportedOperation = 'Operation not Supported';
{$ENDIF}

type

  /// <summary>
  /// A deterministic K calculator based on the algorithm in section 3.2 of
  /// RFC 6979.
  /// </summary>
  THMacDsaKCalculator = class(TInterfacedObject, IDsaKCalculator,
    IHMacDsaKCalculator)

  strict private
  var
    FhMac: IHMac;
    FK, FV: TCryptoLibByteArray;
    Fn: TBigInteger;

    function BitsToInt(const t: TCryptoLibByteArray): TBigInteger; inline;

    function GetIsDeterministic: Boolean; virtual;

  public

    /// <summary>
    /// Base constructor.
    /// </summary>
    /// <param name="digest">
    /// digest to build the HMAC on.
    /// </param>
    constructor Create(const digest: IDigest);

    procedure Init(const n: TBigInteger; const random: ISecureRandom);
      overload; virtual;

    procedure Init(const n, d: TBigInteger;
      const &message: TCryptoLibByteArray); overload;

    function NextK(): TBigInteger; virtual;

    property IsDeterministic: Boolean read GetIsDeterministic;

  end;

implementation

{ THMacDsaKCalculator }

function THMacDsaKCalculator.GetIsDeterministic: Boolean;
begin
  result := True;
end;

function THMacDsaKCalculator.BitsToInt(const t: TCryptoLibByteArray)
  : TBigInteger;
begin
  result := TBigInteger.Create(1, t);
  if ((System.Length(t) * 8) > Fn.BitLength) then
  begin
    result := result.ShiftRight((System.Length(t) * 8) - Fn.BitLength);
  end;
end;

constructor THMacDsaKCalculator.Create(const digest: IDigest);
begin
  Inherited Create();
  FhMac := THMac.Create(digest);
  System.SetLength(FV, FhMac.GetMacSize());
  System.SetLength(FK, FhMac.GetMacSize());
end;

{$IFNDEF _FIXINSIGHT_}

procedure THMacDsaKCalculator.Init(const n: TBigInteger;
  const random: ISecureRandom);
begin
  raise EInvalidOperationCryptoLibException.CreateRes(@SUnSupportedOperation);
end;
{$ENDIF}

procedure THMacDsaKCalculator.Init(const n, d: TBigInteger;
  const &message: TCryptoLibByteArray);
var
  x, dVal, m, mVal: TCryptoLibByteArray;
  mInt: TBigInteger;
  size: Int32;
begin
  Fn := n;
  TArrayUtils.Fill(FV, 0, System.Length(FV), Byte($01));
  TArrayUtils.Fill(FK, 0, System.Length(FK), Byte(0));

  size := TBigIntegers.GetUnsignedByteLength(n);
  System.SetLength(x, size);

  dVal := TBigIntegers.AsUnsignedByteArray(d);

  System.Move(dVal[0], x[System.Length(x) - System.Length(dVal)],
    System.Length(dVal));

  System.SetLength(m, size);

  mInt := BitsToInt(&message);

  if (mInt.CompareTo(n) >= 0) then
  begin
    mInt := mInt.Subtract(n);
  end;

  mVal := TBigIntegers.AsUnsignedByteArray(mInt);

  System.Move(mVal[0], m[System.Length(m) - System.Length(mVal)],
    System.Length(mVal));

  FhMac.Init(TKeyParameter.Create(FK) as IKeyParameter);

  FhMac.BlockUpdate(FV, 0, System.Length(FV));
  FhMac.Update(Byte($00));
  FhMac.BlockUpdate(x, 0, System.Length(x));
  FhMac.BlockUpdate(m, 0, System.Length(m));

  FhMac.DoFinal(FK, 0);

  FhMac.Init(TKeyParameter.Create(FK) as IKeyParameter);

  FhMac.BlockUpdate(FV, 0, System.Length(FV));

  FhMac.DoFinal(FV, 0);

  FhMac.BlockUpdate(FV, 0, System.Length(FV));
  FhMac.Update(Byte($01));
  FhMac.BlockUpdate(x, 0, System.Length(x));
  FhMac.BlockUpdate(m, 0, System.Length(m));

  FhMac.DoFinal(FK, 0);

  FhMac.Init(TKeyParameter.Create(FK) as IKeyParameter);

  FhMac.BlockUpdate(FV, 0, System.Length(FV));

  FhMac.DoFinal(FV, 0);
end;

function THMacDsaKCalculator.NextK: TBigInteger;
var
  t: TCryptoLibByteArray;
  tOff, len: Int32;
begin
  result := Default (TBigInteger);
  System.SetLength(t, TBigIntegers.GetUnsignedByteLength(Fn));

  while True do

  begin
    tOff := 0;

    while (tOff < System.Length(t)) do
    begin
      FhMac.BlockUpdate(FV, 0, System.Length(FV));

      FhMac.DoFinal(FV, 0);

      len := Min(System.Length(t) - tOff, System.Length(FV));
      System.Move(FV[0], t[tOff], len * System.SizeOf(Byte));
      tOff := tOff + len;
    end;

    result := BitsToInt(t);

    if ((result.SignValue > 0) and (result.CompareTo(Fn) < 0)) then
    begin
      Exit;
    end;

    FhMac.BlockUpdate(FV, 0, System.Length(FV));
    FhMac.Update(Byte($00));

    FhMac.DoFinal(FK, 0);

    FhMac.Init(TKeyParameter.Create(FK) as IKeyParameter);

    FhMac.BlockUpdate(FV, 0, System.Length(FV));

    FhMac.DoFinal(FV, 0);
  end;
end;

end.
