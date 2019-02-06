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

unit ClpMod;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
{$IFDEF DELPHI}
  ClpBitConverter,
{$ENDIF DELPHI}
  ClpNat,
  ClpConverters,
  ClpSecureRandom,
  ClpISecureRandom;

resourcestring
  SCannotBeZero = 'cannot be 0, "x"';

type
  TMod = class abstract(TObject)

  strict private

    class var

      FRandomSource: ISecureRandom;

    class procedure Boot(); static;
    class constructor &Mod();

    class procedure InversionResult(const p: TCryptoLibUInt32Array; ac: Int32;
      const a: TCryptoLibUInt32Array; const z: TCryptoLibUInt32Array);
      static; inline;
    class procedure InversionStep(const p, u: TCryptoLibUInt32Array;
      uLen: Int32; const x: TCryptoLibUInt32Array; var xc: Int32); static;

  public
    class procedure Invert(const p, x, z: TCryptoLibUInt32Array); static;
    class function Random(const p: TCryptoLibUInt32Array)
      : TCryptoLibUInt32Array; static;
    class procedure Add(const p, x, y, z: TCryptoLibUInt32Array);
      static; inline;
    class procedure Subtract(const p, x, y, z: TCryptoLibUInt32Array);
      static; inline;
    class function GetTrailingZeroes(x: UInt32): Int32; static; inline;

  end;

implementation

{ TMod }

class procedure TMod.Add(const p, x, y, z: TCryptoLibUInt32Array);
var
  len: Int32;
  c: UInt32;
begin
  len := System.Length(p);
  c := TNat.Add(len, x, y, z);
  if (c <> 0) then
  begin
    TNat.SubFrom(len, p, z);
  end;
end;

class procedure TMod.Boot;
begin

  FRandomSource := TSecureRandom.Create();
end;

class function TMod.GetTrailingZeroes(x: UInt32): Int32;
var
  count: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(x <> 0);
{$ENDIF DEBUG}
  count := 0;
  while ((x and 1) = 0) do
  begin
    x := x shr 1;
    System.Inc(count);
  end;
  result := count;
end;

class procedure TMod.InversionResult(const p: TCryptoLibUInt32Array; ac: Int32;
  const a, z: TCryptoLibUInt32Array);
begin
  if (ac < 0) then
  begin
    TNat.Add(System.Length(p), a, p, z);
  end
  else
  begin
    System.Move(a[0], z[0], System.Length(p) * System.SizeOf(UInt32));
  end;
end;

class procedure TMod.InversionStep(const p, u: TCryptoLibUInt32Array;
  uLen: Int32; const x: TCryptoLibUInt32Array; var xc: Int32);
var
  len, count, zeroes, i: Int32;
begin
  len := System.Length(p);
  count := 0;
  while (u[0] = 0) do
  begin
    TNat.ShiftDownWord(uLen, u, 0);
    count := count + 32;
  end;

  zeroes := GetTrailingZeroes(u[0]);
  if (zeroes > 0) then
  begin
    TNat.ShiftDownBits(uLen, u, zeroes, 0);
    count := count + zeroes;
  end;

  i := 0;
  while i < count do
  begin

    if ((x[0] and 1) <> 0) then
    begin
      if (xc < 0) then
      begin
        xc := xc + Int32(TNat.AddTo(len, p, x));
      end
      else
      begin
        xc := xc + (TNat.SubFrom(len, p, x));
      end;
    end;

{$IFDEF DEBUG}
    System.Assert((xc = 0) or (xc = -1));
{$ENDIF DEBUG}
    TNat.ShiftDownBit(len, x, UInt32(xc));

    System.Inc(i);
  end;

end;

class procedure TMod.Invert(const p, x, z: TCryptoLibUInt32Array);
var
  len, ac, bc, uvLen: Int32;
  u, a, v, b: TCryptoLibUInt32Array;
begin
  len := System.Length(p);
  if (TNat.IsZero(len, x)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SCannotBeZero);
  end;
  if (TNat.IsOne(len, x)) then
  begin
    System.Move(x[0], z[0], len * System.SizeOf(UInt32));
    Exit;
  end;

  u := TNat.Copy(len, x);
  a := TNat.Create(len);
  a[0] := 1;
  ac := 0;

  if ((u[0] and 1) = 0) then
  begin
    InversionStep(p, u, len, a, ac);
  end;

  if (TNat.IsOne(len, u)) then
  begin
    InversionResult(p, ac, a, z);
    Exit;
  end;

  v := TNat.Copy(len, p);
  b := TNat.Create(len);
  bc := 0;

  uvLen := len;

  while True do

  begin
    while ((u[uvLen - 1] = 0) and (v[uvLen - 1] = 0)) do
    begin
      System.Dec(uvLen);
    end;

    if (TNat.Gte(len, u, v)) then
    begin
      TNat.SubFrom(len, v, u);
{$IFDEF DEBUG}
      System.Assert((u[0] and 1) = 0);
{$ENDIF DEBUG}
      ac := ac + (TNat.SubFrom(len, b, a) - bc);
      InversionStep(p, u, uvLen, a, ac);
      if (TNat.IsOne(len, u)) then
      begin
        InversionResult(p, ac, a, z);
        Exit;
      end;
    end
    else
    begin
      TNat.SubFrom(len, u, v);
{$IFDEF DEBUG}
      System.Assert((v[0] and 1) = 0);
{$ENDIF DEBUG}
      bc := bc + (TNat.SubFrom(len, a, b) - ac);
      InversionStep(p, v, uvLen, b, bc);
      if (TNat.IsOne(len, v)) then
      begin
        InversionResult(p, bc, b, z);
        Exit;
      end;
    end;
  end;
end;

class constructor TMod.&Mod;
begin
  TMod.Boot;
end;

class function TMod.Random(const p: TCryptoLibUInt32Array)
  : TCryptoLibUInt32Array;
var
  len: Int32;
  m: UInt32;
  s: TCryptoLibUInt32Array;
  bytes: TCryptoLibByteArray;
begin
  len := System.Length(p);
  s := TNat.Create(len);

  m := p[len - 1];
  m := m or (m shr 1);
  m := m or (m shr 2);
  m := m or (m shr 4);
  m := m or (m shr 8);
  m := m or (m shr 16);

  System.SetLength(bytes, len shl 2);

  repeat
    FRandomSource.NextBytes(bytes);
    TConverters.be32_copy(PByte(bytes), 0, PCardinal(s), 0, System.Length(s));
    s[len - 1] := s[len - 1] and m;

  until (not(TNat.Gte(len, s, p)));

  result := s;
end;

class procedure TMod.Subtract(const p, x, y, z: TCryptoLibUInt32Array);
var
  len, c: Int32;
begin
  len := System.Length(p);
  c := TNat.Sub(len, x, y, z);
  if (c <> 0) then
  begin
    TNat.AddTo(len, p, z);
  end;
end;

end.
