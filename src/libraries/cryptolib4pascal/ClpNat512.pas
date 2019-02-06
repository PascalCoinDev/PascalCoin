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

unit ClpNat512;

{$I CryptoLib.inc}

interface

uses
  ClpNat,
  ClpNat256,
  ClpCryptoLibTypes;

type
  TNat512 = class sealed(TObject)

  public
    class procedure Mul(const x, y, zz: TCryptoLibUInt32Array); static;
    class procedure Square(const x, zz: TCryptoLibUInt32Array); static;

  end;

implementation

{ TNat512 }

class procedure TNat512.Mul(const x, y, zz: TCryptoLibUInt32Array);
var
  c24, c16: UInt32;
  dx, dy, tt: TCryptoLibUInt32Array;
  neg: Boolean;
begin
  TNat256.Mul(x, y, zz);
  TNat256.Mul(x, 8, y, 8, zz, 16);

  c24 := TNat256.AddToEachOther(zz, 8, zz, 16);
  c16 := c24 + TNat256.AddTo(zz, 0, zz, 8, 0);
  c24 := c24 + (TNat256.AddTo(zz, 24, zz, 16, c16));

  dx := TNat256.Create();
  dy := TNat256.Create();
  neg := TNat256.Diff(x, 8, x, 0, dx, 0) <> TNat256.Diff(y, 8, y, 0, dy, 0);

  tt := TNat256.CreateExt();
  TNat256.Mul(dx, dy, tt);

  if neg then
  begin
    c24 := c24 + TNat.AddTo(16, tt, 0, zz, 8);
  end
  else
  begin
    c24 := c24 + UInt32(TNat.SubFrom(16, tt, 0, zz, 8));
  end;

  TNat.AddWordAt(32, c24, zz, 24);
end;

class procedure TNat512.Square(const x, zz: TCryptoLibUInt32Array);
var
  c24, c16: UInt32;
  dx, m: TCryptoLibUInt32Array;
begin
  TNat256.Square(x, zz);
  TNat256.Square(x, 8, zz, 16);

  c24 := TNat256.AddToEachOther(zz, 8, zz, 16);
  c16 := c24 + TNat256.AddTo(zz, 0, zz, 8, 0);
  c24 := c24 + (TNat256.AddTo(zz, 24, zz, 16, c16));

  dx := TNat256.Create();
  TNat256.Diff(x, 8, x, 0, dx, 0);

  m := TNat256.CreateExt();
  TNat256.Square(dx, m);

  c24 := c24 + UInt32(TNat.SubFrom(16, m, 0, zz, 8));
  TNat.AddWordAt(32, c24, zz, 24);
end;

end.
