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

unit ClpFixedPointUtilities;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpCryptoLibTypes,
  ClpIPreCompInfo,
  ClpIPreCompCallback,
  ClpFixedPointPreCompInfo,
  ClpIFixedPointPreCompInfo,
  ClpIECC;

type
  TFixedPointUtilities = class sealed(TObject)
  strict private

  type
    IFixedPointCallback = interface(IPreCompCallback)
      ['{E6DFE8D3-A890-4568-AA4A-3D8BC6AF16E9}']

    end;

  type
    TFixedPointCallback = class(TInterfacedObject, IPreCompCallback,
      IFixedPointCallback)

    strict private
    var
      Fm_p: IECPoint;

    public
      constructor Create(const p: IECPoint);

      function Precompute(const existing: IPreCompInfo): IPreCompInfo;

    end;

  const
    PRECOMP_NAME: String = 'bc_fixed_point';

    class function CheckExisting(const existingFP: IFixedPointPreCompInfo;
      n: Int32): Boolean; static; inline;

    class function CheckTable(const table: IECLookupTable; n: Int32): Boolean;
      static; inline;

  public

    class function GetFixedPointPreCompInfo(const preCompInfo: IPreCompInfo)
      : IFixedPointPreCompInfo; static; inline;

    class function GetCombSize(const c: IECCurve): Int32; static; inline;

    class function Precompute(const p: IECPoint)
      : IFixedPointPreCompInfo; static;
  end;

implementation

{ TFixedPointUtilities }

class function TFixedPointUtilities.CheckTable(const table: IECLookupTable;
  n: Int32): Boolean;
begin
  result := (table <> Nil) and (table.Size >= n);
end;

class function TFixedPointUtilities.CheckExisting(const existingFP
  : IFixedPointPreCompInfo; n: Int32): Boolean;
begin
  result := (existingFP <> Nil) and CheckTable(existingFP.LookUpTable, n);
end;

class function TFixedPointUtilities.GetCombSize(const c: IECCurve): Int32;
var
  order: TBigInteger;
begin
  order := c.order;
  if (not(order.IsInitialized)) then
  begin
    result := c.FieldSize + 1;
  end
  else
  begin
    result := order.BitLength;
  end;
end;

class function TFixedPointUtilities.GetFixedPointPreCompInfo(const preCompInfo
  : IPreCompInfo): IFixedPointPreCompInfo;
begin
  result := preCompInfo as IFixedPointPreCompInfo;
end;

class function TFixedPointUtilities.Precompute(const p: IECPoint)
  : IFixedPointPreCompInfo;
var
  c: IECCurve;
begin
  c := p.Curve;

  result := c.Precompute(p, PRECOMP_NAME, TFixedPointCallback.Create(p)
    as IFixedPointCallback) as IFixedPointPreCompInfo;
end;

{ TFixedPointUtilities.TFixedPointCallback }

constructor TFixedPointUtilities.TFixedPointCallback.Create(const p: IECPoint);
begin
  Inherited Create();
  Fm_p := p;
end;

function TFixedPointUtilities.TFixedPointCallback.Precompute(const existing
  : IPreCompInfo): IPreCompInfo;
var
  bit, bits, minWidth, n, d, i, step: Int32;
  existingFP: IFixedPointPreCompInfo;
  pow2Table, LookUpTable: TCryptoLibGenericArray<IECPoint>;
  pow2: IECPoint;
  c: IECCurve;
  tempResult: IFixedPointPreCompInfo;
begin
  if Supports(existing, IFixedPointPreCompInfo) then
  begin
    existingFP := existing as IFixedPointPreCompInfo;
  end
  else
  begin
    existingFP := Nil;
  end;

  c := Fm_p.Curve;
  bits := TFixedPointUtilities.GetCombSize(c);
  if bits > 250 then
  begin
    minWidth := 6
  end
  else
  begin
    minWidth := 5
  end;
  n := 1 shl minWidth;

  if (CheckExisting(existingFP, n)) then
  begin
    result := existingFP;
    Exit;
  end;

  d := (bits + minWidth - 1) div minWidth;

  System.SetLength(pow2Table, minWidth + 1);

  pow2Table[0] := Fm_p;
  for i := 1 to System.Pred(minWidth) do
  begin
    pow2Table[i] := pow2Table[i - 1].TimesPow2(d);
  end;

  // This will be the 'offset' value
  pow2Table[minWidth] := pow2Table[0].Subtract(pow2Table[1]);

  c.NormalizeAll(pow2Table);

  System.SetLength(LookUpTable, n);
  LookUpTable[0] := pow2Table[0];

  bit := minWidth - 1;
  while bit >= 0 do
  begin
    pow2 := pow2Table[bit];

    step := 1 shl bit;

    i := step;

    while i < n do
    begin
      LookUpTable[i] := LookUpTable[i - step].Add(pow2);

      System.Inc(i, step shl 1);
    end;

    System.Dec(bit);
  end;

  c.NormalizeAll(LookUpTable);

  tempResult := TFixedPointPreCompInfo.Create();
  tempResult.LookUpTable := c.CreateCacheSafeLookupTable(LookUpTable, 0,
    System.length(LookUpTable));
  tempResult.Offset := pow2Table[minWidth];
  tempResult.Width := minWidth;
  result := tempResult;
end;

end.
