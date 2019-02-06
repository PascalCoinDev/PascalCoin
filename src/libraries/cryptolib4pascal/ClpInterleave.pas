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

unit ClpInterleave;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type
  TInterleave = class sealed(TObject)

  strict private
  const
    M32 = UInt64($55555555);
    M64 = UInt64($5555555555555555);
    M64R = UInt64($AAAAAAAAAAAAAAAA);

  public
    class function Expand8to16(x: UInt32): UInt32; static; inline;
    class function Expand16to32(x: UInt32): UInt32; static; inline;
    class function Expand32to64(x: UInt32): UInt64; static;
    class procedure Expand64To128(x: UInt64; z: TCryptoLibUInt64Array;
      zOff: Int32); static;
    class procedure Expand64To128Rev(x: UInt64; z: TCryptoLibUInt64Array;
      zOff: Int32); static;
    class function Shuffle(x: UInt32): UInt32; overload; static;
    class function Shuffle(x: UInt64): UInt64; overload; static;
    class function Shuffle2(x: UInt32): UInt32; static;
    class function Unshuffle(x: UInt32): UInt32; overload; static;
    class function Unshuffle(x: UInt64): UInt64; overload; static;
    class function Unshuffle2(x: UInt32): UInt32; static;

  end;

implementation

{ TInterleave }

class function TInterleave.Expand8to16(x: UInt32): UInt32;
begin
  x := x and UInt32($FF);
  x := (x or (x shl 4)) and UInt32($0F0F);
  x := (x or (x shl 2)) and UInt32($3333);
  x := (x or (x shl 1)) and UInt32($5555);
  result := x;
end;

class function TInterleave.Expand16to32(x: UInt32): UInt32;
begin
  x := x and UInt32($FFFF);
  x := (x or (x shl 8)) and UInt32($00FF00FF);
  x := (x or (x shl 4)) and UInt32($0F0F0F0F);
  x := (x or (x shl 2)) and UInt32($33333333);
  x := (x or (x shl 1)) and UInt32($55555555);
  result := x;
end;

class function TInterleave.Expand32to64(x: UInt32): UInt64;
var
  t: UInt32;
begin
  // "shuffle" low half to even bits and high half to odd bits
  t := (x xor (x shr 8)) and UInt32($0000FF00);
  x := x xor (t xor (t shl 8));
  t := (x xor (x shr 4)) and UInt32($00F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 2)) and UInt32($0C0C0C0C);
  x := x xor (t xor (t shl 2));
  t := (x xor (x shr 1)) and UInt32($22222222);
  x := x xor (t xor (t shl 1));

  result := ((x shr 1) and M32) shl 32 or (x and M32);
end;

class procedure TInterleave.Expand64To128(x: UInt64; z: TCryptoLibUInt64Array;
  zOff: Int32);
var
  t: UInt64;
begin
  // "shuffle" low half to even bits and high half to odd bits
  t := (x xor (x shr 16)) and UInt64($00000000FFFF0000);
  x := x xor (t xor (t shl 16));
  t := (x xor (x shr 8)) and UInt64($0000FF000000FF00);
  x := x xor (t xor (t shl 8));
  t := (x xor (x shr 4)) and UInt64($00F000F000F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 2)) and UInt64($0C0C0C0C0C0C0C0C);
  x := x xor (t xor (t shl 2));
  t := (x xor (x shr 1)) and UInt64($2222222222222222);
  x := x xor (t xor (t shl 1));

  z[zOff] := (x) and M64;
  z[zOff + 1] := (x shr 1) and M64;
end;

class procedure TInterleave.Expand64To128Rev(x: UInt64;
  z: TCryptoLibUInt64Array; zOff: Int32);
var
  t: UInt64;
begin
  // "shuffle" low half to even bits and high half to odd bits
  t := (x xor (x shr 16)) and UInt64($00000000FFFF0000);
  x := x xor (t xor (t shl 16));
  t := (x xor (x shr 8)) and UInt64($0000FF000000FF00);
  x := x xor (t xor (t shl 8));
  t := (x xor (x shr 4)) and UInt64($00F000F000F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 2)) and UInt64($0C0C0C0C0C0C0C0C);
  x := x xor (t xor (t shl 2));
  t := (x xor (x shr 1)) and UInt64($2222222222222222);
  x := x xor (t xor (t shl 1));

  z[zOff] := (x) and M64R;
  z[zOff + 1] := (x shl 1) and M64R;
end;

class function TInterleave.Shuffle(x: UInt32): UInt32;
var
  t: UInt32;
begin
  // "shuffle" low half to even bits and high half to odd bits
  t := (x xor (x shr 8)) and UInt32($0000FF00);
  x := x xor (t xor (t shl 8));
  t := (x xor (x shr 4)) and UInt32($00F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 2)) and UInt32($0C0C0C0C);
  x := x xor (t xor (t shl 2));
  t := (x xor (x shr 1)) and UInt32($22222222);
  x := x xor (t xor (t shl 1));
  result := x;
end;

class function TInterleave.Shuffle(x: UInt64): UInt64;
var
  t: UInt64;
begin
  // "shuffle" low half to even bits and high half to odd bits
  t := (x xor (x shr 16)) and UInt64($00000000FFFF0000);
  x := x xor (t xor (t shl 16));
  t := (x xor (x shr 8)) and UInt64($0000FF000000FF00);
  x := x xor (t xor (t shl 8));
  t := (x xor (x shr 4)) and UInt64($00F000F000F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 2)) and UInt64($0C0C0C0C0C0C0C0C);
  x := x xor (t xor (t shl 2));
  t := (x xor (x shr 1)) and UInt64($2222222222222222);
  x := x xor (t xor (t shl 1));
  result := x;
end;

class function TInterleave.Shuffle2(x: UInt32): UInt32;
var
  t: UInt32;
begin
  // "shuffle" (twice) low half to even bits and high half to odd bits
  t := (x xor (x shr 7)) and UInt32($00AA00AA);
  x := x xor (t xor (t shl 7));
  t := (x xor (x shr 14)) and UInt32($0000CCCC);
  x := x xor (t xor (t shl 14));
  t := (x xor (x shr 4)) and UInt32($00F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 8)) and UInt32($0000FF00);
  x := x xor (t xor (t shl 8));
  result := x;
end;

class function TInterleave.Unshuffle(x: UInt32): UInt32;
var
  t: UInt32;
begin
  // "unshuffle" even bits to low half and odd bits to high half
  t := (x xor (x shr 1)) and UInt32($22222222);
  x := x xor (t xor (t shl 1));
  t := (x xor (x shr 2)) and UInt32($0C0C0C0C);
  x := x xor (t xor (t shl 2));
  t := (x xor (x shr 4)) and UInt32($00F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 8)) and UInt32($0000FF00);
  x := x xor (t xor (t shl 8));
  result := x;
end;

class function TInterleave.Unshuffle(x: UInt64): UInt64;
var
  t: UInt64;
begin
  // "unshuffle" even bits to low half and odd bits to high half
  t := (x xor (x shr 1)) and UInt64($2222222222222222);
  x := x xor (t xor (t shl 1));
  t := (x xor (x shr 2)) and UInt64($0C0C0C0C0C0C0C0C);
  x := x xor (t xor (t shl 2));
  t := (x xor (x shr 4)) and UInt64($00F000F000F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 8)) and UInt64($0000FF000000FF00);
  x := x xor (t xor (t shl 8));
  t := (x xor (x shr 16)) and UInt64($00000000FFFF0000);
  x := x xor (t xor (t shl 16));
  result := x;
end;

class function TInterleave.Unshuffle2(x: UInt32): UInt32;
var
  t: UInt32;
begin
  // "unshuffle" (twice) even bits to low half and odd bits to high half
  t := (x xor (x shr 8)) and UInt32($0000FF00);
  x := x xor (t xor (t shl 8));
  t := (x xor (x shr 4)) and UInt32($00F000F0);
  x := x xor (t xor (t shl 4));
  t := (x xor (x shr 14)) and UInt32($0000CCCC);
  x := x xor (t xor (t shl 14));
  t := (x xor (x shr 7)) and UInt32($00AA00AA);
  x := x xor (t xor (t shl 7));
  result := x;
end;

end.
