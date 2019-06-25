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

unit ClpDHKeyGeneratorHelper;

{$I CryptoLib.inc}

interface

uses

  ClpISecureRandom,
  ClpBits,
  ClpBigInteger,
  ClpBigIntegers,
  ClpECAlgorithms,
  ClpIDHParameters,
  ClpIDHKeyGeneratorHelper;

type
  TDHKeyGeneratorHelper = class sealed(TInterfacedObject, IDHKeyGeneratorHelper)

  strict private
  class var

    FIsBooted: Boolean;
    FInstance: IDHKeyGeneratorHelper;

    class function GetInstance: IDHKeyGeneratorHelper; static; inline;

    class procedure Boot(); static;
    class constructor DHKeyGeneratorHelper();
  public

    function CalculatePrivate(const dhParams: IDHParameters;
      const random: ISecureRandom): TBigInteger;

    function CalculatePublic(const dhParams: IDHParameters;
      const x: TBigInteger): TBigInteger;

    class property Instance: IDHKeyGeneratorHelper read GetInstance;

  end;

implementation

{ TDHKeyGeneratorHelper }

class procedure TDHKeyGeneratorHelper.Boot;
begin
  if not FIsBooted then
  begin
    FInstance := TDHKeyGeneratorHelper.Create();

    FIsBooted := True;
  end;
end;

function TDHKeyGeneratorHelper.CalculatePrivate(const dhParams: IDHParameters;
  const random: ISecureRandom): TBigInteger;
var
  limit, minWeight, m: Int32;
  x, min, q, max: TBigInteger;
begin
  Result := Default (TBigInteger);
  limit := dhParams.L;

  if (limit <> 0) then
  begin
    minWeight := TBits.Asr32(limit, 2);

    while True do
    begin
      x := TBigInteger.Create(limit, random).SetBit(limit - 1);
      if (TWNafUtilities.GetNafWeight(x) >= minWeight) then
      begin
        Result := x;
        Exit;
      end;
    end;
  end;

  min := TBigInteger.Two;
  m := dhParams.m;
  if (m <> 0) then
  begin
    min := TBigInteger.One.ShiftLeft(m - 1);
  end;

  q := dhParams.q;
  if (not(q.IsInitialized)) then
  begin
    q := dhParams.P;
  end;
  max := q.Subtract(TBigInteger.Two);

  minWeight := TBits.Asr32(max.BitLength, 2);

  while True do
  begin
    x := TBigIntegers.CreateRandomInRange(min, max, random);
    if (TWNafUtilities.GetNafWeight(x) >= minWeight) then
    begin
      Result := x;
      Exit;
    end;
  end;
end;

function TDHKeyGeneratorHelper.CalculatePublic(const dhParams: IDHParameters;
  const x: TBigInteger): TBigInteger;
begin
  Result := dhParams.G.ModPow(x, dhParams.P);
end;

class constructor TDHKeyGeneratorHelper.DHKeyGeneratorHelper;
begin
  TDHKeyGeneratorHelper.Boot();
end;

class function TDHKeyGeneratorHelper.GetInstance: IDHKeyGeneratorHelper;
begin
  Result := FInstance;
end;

end.
