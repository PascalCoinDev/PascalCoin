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

unit ClpSignersEncodings;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  Math,
  ClpAsn1Objects,
  ClpBigInteger,
  ClpISignersEncodings,
  ClpIAsn1Objects,
  ClpArrayUtils,
  ClpBigIntegers,
  ClpCryptoLibTypes;

resourcestring
  SMalformedSignature = 'Malformed signature, "%s"';
  SValueOutOfRange = 'Value out of range, "%s"';
  SInvalidEncodingLength = 'Encoding has incorrect length, "%s"';

type
  TStandardDsaEncoding = class(TInterfacedObject, IDsaEncoding,
    IStandardDsaEncoding)

  strict private

    class function GetInstance: IStandardDsaEncoding; static; inline;

  strict protected

    function CheckValue(const n, x: TBigInteger): TBigInteger; virtual;
    function DecodeValue(const n: TBigInteger; const s: IAsn1Sequence;
      pos: Int32): TBigInteger; virtual;
    function EncodeValue(const n, x: TBigInteger): IDerInteger; virtual;

  public

    function Decode(const n: TBigInteger; const encoding: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    function Encode(const n, r, s: TBigInteger): TCryptoLibByteArray; virtual;

    class property Instance: IStandardDsaEncoding read GetInstance;

  end;

type
  TPlainDsaEncoding = class(TInterfacedObject, IDsaEncoding, IPlainDsaEncoding)

  strict private

    class function GetInstance: IPlainDsaEncoding; static; inline;

  strict protected

    function CheckValue(const n, x: TBigInteger): TBigInteger; virtual;
    function DecodeValue(const n: TBigInteger; const buf: TCryptoLibByteArray;
      off, len: Int32): TBigInteger; virtual;
    procedure EncodeValue(const n, x: TBigInteger;
      const buf: TCryptoLibByteArray; off, len: Int32); virtual;

  public

    function Decode(const n: TBigInteger; const encoding: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    function Encode(const n, r, s: TBigInteger): TCryptoLibByteArray; virtual;

    class property Instance: IPlainDsaEncoding read GetInstance;

  end;

type
  TPlainSchnorrEncoding = class(TInterfacedObject, ISchnorrEncoding,
    IPlainSchnorrEncoding)

  strict private

    class function GetInstance: IPlainSchnorrEncoding; static; inline;

  strict protected

    function CheckValue(const n, x: TBigInteger): TBigInteger; virtual;
    function DecodeValue(const n: TBigInteger; const buf: TCryptoLibByteArray;
      off, len: Int32): TBigInteger; virtual;
    procedure EncodeValue(const n, x: TBigInteger;
      const buf: TCryptoLibByteArray; off, len: Int32); virtual;

  public

    function Decode(const n: TBigInteger; const encoding: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    function Encode(const n, r, s: TBigInteger): TCryptoLibByteArray; virtual;

    class property Instance: IPlainSchnorrEncoding read GetInstance;

  end;

implementation

{ TStandardDsaEncoding }

function TStandardDsaEncoding.CheckValue(const n, x: TBigInteger): TBigInteger;
begin
  if ((x.SignValue < 0) or ((n.IsInitialized) and (x.CompareTo(n) >= 0))) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SValueOutOfRange, ['x']);
  end;
  result := x;
end;

function TStandardDsaEncoding.Decode(const n: TBigInteger;
  const encoding: TCryptoLibByteArray): TCryptoLibGenericArray<TBigInteger>;
var
  seq: IAsn1Sequence;
  r, s: TBigInteger;
  expectedEncoding: TCryptoLibByteArray;
begin
  seq := TAsn1Object.FromByteArray(encoding) as IAsn1Sequence;
  if (seq.Count = 2) then
  begin
    r := DecodeValue(n, seq, 0);
    s := DecodeValue(n, seq, 1);
    expectedEncoding := Encode(n, r, s);
    if (TArrayUtils.AreEqual(expectedEncoding, encoding)) then
    begin
      result := TCryptoLibGenericArray<TBigInteger>.Create(r, s);
      Exit;
    end;
  end;
  raise EArgumentCryptoLibException.CreateResFmt(@SMalformedSignature,
    ['encoding']);
end;

function TStandardDsaEncoding.DecodeValue(const n: TBigInteger;
  const s: IAsn1Sequence; pos: Int32): TBigInteger;
begin
  result := CheckValue(n, (s[pos] as IDerInteger).Value);
end;

function TStandardDsaEncoding.Encode(const n, r, s: TBigInteger)
  : TCryptoLibByteArray;
var
  LTemp: IDerSequence;
begin
  LTemp := TDerSequence.Create([EncodeValue(n, r), EncodeValue(n, s)])
    as IDerSequence;
  result := LTemp.GetEncoded(TAsn1Encodable.Der);
end;

function TStandardDsaEncoding.EncodeValue(const n, x: TBigInteger): IDerInteger;
begin
  result := TDerInteger.Create(CheckValue(n, x));
end;

class function TStandardDsaEncoding.GetInstance: IStandardDsaEncoding;
begin
  result := TStandardDsaEncoding.Create();
end;

{ TPlainDsaEncoding }

function TPlainDsaEncoding.CheckValue(const n, x: TBigInteger): TBigInteger;
begin
  if ((x.SignValue < 0) or ((x.CompareTo(n) >= 0))) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SValueOutOfRange, ['x']);
  end;
  result := x;
end;

function TPlainDsaEncoding.Decode(const n: TBigInteger;
  const encoding: TCryptoLibByteArray): TCryptoLibGenericArray<TBigInteger>;
var
  valueLength: Int32;
begin
  valueLength := TBigIntegers.GetUnsignedByteLength(n);
  if (System.Length(encoding) <> (valueLength * 2)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidEncodingLength,
      ['encoding']);
  end;
  result := TCryptoLibGenericArray<TBigInteger>.Create
    (DecodeValue(n, encoding, 0, valueLength), DecodeValue(n, encoding,
    valueLength, valueLength));
end;

function TPlainDsaEncoding.DecodeValue(const n: TBigInteger;
  const buf: TCryptoLibByteArray; off, len: Int32): TBigInteger;
begin
  result := CheckValue(n, TBigInteger.Create(1, buf, off, len));
end;

function TPlainDsaEncoding.Encode(const n, r, s: TBigInteger)
  : TCryptoLibByteArray;
var
  valueLength: Int32;
begin
  valueLength := TBigIntegers.GetUnsignedByteLength(n);
  System.SetLength(result, valueLength * 2);
  EncodeValue(n, r, result, 0, valueLength);
  EncodeValue(n, s, result, valueLength, valueLength);
end;

procedure TPlainDsaEncoding.EncodeValue(const n, x: TBigInteger;
  const buf: TCryptoLibByteArray; off, len: Int32);
var
  bs: TCryptoLibByteArray;
  bsOff, bsLen, pos: Int32;
begin
  bs := CheckValue(n, x).ToByteArrayUnsigned();
  bsOff := Max(0, System.Length(bs) - len);
  bsLen := System.Length(bs) - bsOff;
  pos := len - bsLen;
  TArrayUtils.Fill(buf, off, off + pos, Byte(0));
  System.Move(bs[bsOff], buf[off + pos], bsLen * System.SizeOf(Byte));
end;

class function TPlainDsaEncoding.GetInstance: IPlainDsaEncoding;
begin
  result := TPlainDsaEncoding.Create();
end;

{ TPlainSchnorrEncoding }

function TPlainSchnorrEncoding.CheckValue(const n, x: TBigInteger): TBigInteger;
begin
  if ((x.SignValue < 0) or ((x.CompareTo(n) >= 0))) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SValueOutOfRange, ['x']);
  end;
  result := x;
end;

function TPlainSchnorrEncoding.Decode(const n: TBigInteger;
  const encoding: TCryptoLibByteArray): TCryptoLibGenericArray<TBigInteger>;
var
  valueLength: Int32;
begin
  valueLength := TBigIntegers.GetUnsignedByteLength(n);
  if (System.Length(encoding) <> (valueLength * 2)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidEncodingLength,
      ['encoding']);
  end;
  result := TCryptoLibGenericArray<TBigInteger>.Create
    (DecodeValue(n, encoding, 0, valueLength), DecodeValue(n, encoding,
    valueLength, valueLength));
end;

function TPlainSchnorrEncoding.DecodeValue(const n: TBigInteger;
  const buf: TCryptoLibByteArray; off, len: Int32): TBigInteger;
begin
  result := CheckValue(n, TBigInteger.Create(1, buf, off, len));
end;

function TPlainSchnorrEncoding.Encode(const n, r, s: TBigInteger)
  : TCryptoLibByteArray;
var
  valueLength: Int32;
begin
  valueLength := TBigIntegers.GetUnsignedByteLength(n);
  System.SetLength(result, valueLength * 2);
  EncodeValue(n, r, result, 0, valueLength);
  EncodeValue(n, s, result, valueLength, valueLength);
end;

procedure TPlainSchnorrEncoding.EncodeValue(const n, x: TBigInteger;
  const buf: TCryptoLibByteArray; off, len: Int32);
var
  bs: TCryptoLibByteArray;
  bsOff, bsLen, pos: Int32;
begin
  bs := CheckValue(n, x).ToByteArrayUnsigned();
  bsOff := Max(0, System.Length(bs) - len);
  bsLen := System.Length(bs) - bsOff;
  pos := len - bsLen;
  TArrayUtils.Fill(buf, off, off + pos, Byte(0));
  System.Move(bs[bsOff], buf[off + pos], bsLen * System.SizeOf(Byte));
end;

class function TPlainSchnorrEncoding.GetInstance: IPlainSchnorrEncoding;
begin
  result := TPlainSchnorrEncoding.Create();
end;

end.
