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

unit ClpNat320;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpConverters,
  ClpCryptoLibTypes;

type
  TNat320 = class abstract(TObject)

  public
    class procedure Copy64(const x, z: TCryptoLibUInt64Array); overload;
      static; inline;

    class procedure Copy64(const x: TCryptoLibUInt64Array; xOff: Int32;
      const z: TCryptoLibUInt64Array; zOff: Int32); overload; static; inline;

    class function Create64(): TCryptoLibUInt64Array; static; inline;

    class function CreateExt64(): TCryptoLibUInt64Array; static; inline;

    class function Eq64(const x, y: TCryptoLibUInt64Array): Boolean; static;

    class function FromBigInteger64(const x: TBigInteger)
      : TCryptoLibUInt64Array; static;

    class function IsOne64(const x: TCryptoLibUInt64Array): Boolean; static;

    class function IsZero64(const x: TCryptoLibUInt64Array): Boolean; static;

    class function ToBigInteger64(const x: TCryptoLibUInt64Array)
      : TBigInteger; static;

  end;

implementation

{ TNat320 }

class procedure TNat320.Copy64(const x, z: TCryptoLibUInt64Array);
begin
  System.Move(x[0], z[0], 5 * System.SizeOf(UInt64));
end;

class procedure TNat320.Copy64(const x: TCryptoLibUInt64Array; xOff: Int32;
  const z: TCryptoLibUInt64Array; zOff: Int32);
begin
  System.Move(x[xOff], z[zOff], 5 * System.SizeOf(UInt64));
end;

class function TNat320.Create64: TCryptoLibUInt64Array;
begin
  System.SetLength(Result, 5);
end;

class function TNat320.CreateExt64: TCryptoLibUInt64Array;
begin
  System.SetLength(Result, 10);
end;

class function TNat320.Eq64(const x, y: TCryptoLibUInt64Array): Boolean;
var
  i: Int32;
begin
  i := 4;
  while i >= 0 do
  begin
    if (x[i] <> y[i]) then
    begin
      Result := False;
      Exit;
    end;
    System.Dec(i);
  end;
  Result := True;
end;

class function TNat320.FromBigInteger64(const x: TBigInteger)
  : TCryptoLibUInt64Array;
var
  i: Int32;
  Lx: TBigInteger;
begin
  Lx := x;
  if ((Lx.SignValue < 0) or (Lx.BitLength > 320)) then
  begin
    raise EArgumentCryptoLibException.Create('');
  end;

  Result := Create64();
  i := 0;
  while (Lx.SignValue <> 0) do
  begin
    Result[i] := UInt64(Lx.Int64Value);
    System.Inc(i);
    Lx := Lx.ShiftRight(64);
  end;
end;

class function TNat320.IsOne64(const x: TCryptoLibUInt64Array): Boolean;
var
  i: Int32;
begin
  if (x[0] <> UInt64(1)) then
  begin
    Result := False;
    Exit;
  end;
  i := 1;
  while i < 5 do
  begin
    if (x[i] <> UInt64(0)) then
    begin
      Result := False;
      Exit;
    end;
    System.Inc(i);
  end;
  Result := True;
end;

class function TNat320.IsZero64(const x: TCryptoLibUInt64Array): Boolean;
var
  i: Int32;
begin
  i := 0;
  while i < 5 do
  begin
    if (x[i] <> UInt64(0)) then
    begin
      Result := False;
      Exit;
    end;
    System.Inc(i);
  end;
  Result := True;
end;

class function TNat320.ToBigInteger64(const x: TCryptoLibUInt64Array)
  : TBigInteger;
var
  bs, temp: TCryptoLibByteArray;
  i: Int32;
  x_i: UInt64;
begin
  System.SetLength(bs, 40);
  for i := 0 to System.Pred(5) do

  begin
    x_i := x[i];
    if (x_i <> Int64(0)) then
    begin
      temp := TConverters.ReadUInt64AsBytesBE(x_i);
      System.Move(temp[0], bs[(4 - i) shl 3], System.Length(temp) *
        System.SizeOf(Byte))
    end;
  end;
  Result := TBigInteger.Create(1, bs);
end;

end.
