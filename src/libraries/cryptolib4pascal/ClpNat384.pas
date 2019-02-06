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

unit ClpNat384;

{$I CryptoLib.inc}

interface

uses
  ClpNat,
  ClpNat192,
  ClpCryptoLibTypes;

type
  TNat384 = class sealed(TObject)

  public
    class procedure Mul(const x, y, zz: TCryptoLibUInt32Array); static;
    class procedure Square(const x, zz: TCryptoLibUInt32Array); static;

  end;

implementation

{ TNat384 }

class procedure TNat384.Mul(const x, y, zz: TCryptoLibUInt32Array);
var
  c18, c12: UInt32;
  dx, dy, tt: TCryptoLibUInt32Array;
  neg: Boolean;
begin
  TNat192.Mul(x, y, zz);
  TNat192.Mul(x, 6, y, 6, zz, 12);

  c18 := TNat192.AddToEachOther(zz, 6, zz, 12);
  c12 := c18 + TNat192.AddTo(zz, 0, zz, 6, 0);
  c18 := c18 + (TNat192.AddTo(zz, 18, zz, 12, c12));

  dx := TNat192.Create();
  dy := TNat192.Create();
  neg := TNat192.Diff(x, 6, x, 0, dx, 0) <> TNat192.Diff(y, 6, y, 0, dy, 0);

  tt := TNat192.CreateExt();
  TNat192.Mul(dx, dy, tt);

  if neg then
  begin
    c18 := c18 + TNat.AddTo(12, tt, 0, zz, 6);
  end
  else
  begin
    c18 := c18 + UInt32(TNat.SubFrom(12, tt, 0, zz, 6));
  end;
  TNat.AddWordAt(24, c18, zz, 18);
end;

class procedure TNat384.Square(const x, zz: TCryptoLibUInt32Array);
var
  c18, c12: UInt32;
  dx, m: TCryptoLibUInt32Array;
begin
  TNat192.Square(x, zz);
  TNat192.Square(x, 6, zz, 12);

  c18 := TNat192.AddToEachOther(zz, 6, zz, 12);
  c12 := c18 + TNat192.AddTo(zz, 0, zz, 6, 0);
  c18 := c18 + TNat192.AddTo(zz, 18, zz, 12, c12);

  dx := TNat192.Create();
  TNat192.Diff(x, 6, x, 0, dx, 0);

  m := TNat192.CreateExt();
  TNat192.Square(dx, m);

  c18 := c18 + UInt32(TNat.SubFrom(12, m, 0, zz, 6));
  TNat.AddWordAt(24, c18, zz, 18);
end;

end.
