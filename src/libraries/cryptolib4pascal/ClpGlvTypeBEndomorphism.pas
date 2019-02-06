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

unit ClpGlvTypeBEndomorphism;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpScaleXPointMap,
  ClpIGlvTypeBEndomorphism,
  ClpIECC,
  ClpIGlvTypeBParameters,
  ClpIGlvEndomorphism;

type
  TGlvTypeBEndomorphism = class(TInterfacedObject, IECEndomorphism,
    IGlvEndomorphism, IGlvTypeBEndomorphism)

  strict private
    function GetHasEfficientPointMap: Boolean; virtual;
    function GetPointMap: IECPointMap; virtual;

  strict protected
  var
    Fm_parameters: IGlvTypeBParameters;
    Fm_pointMap: IECPointMap;
    Fm_curve: IECCurve;

    function CalculateB(const k, g: TBigInteger; t: Int32)
      : TBigInteger; virtual;

  public
    constructor Create(const curve: IECCurve;
      const parameters: IGlvTypeBParameters);
    destructor Destroy; override;
    function DecomposeScalar(const k: TBigInteger)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    property PointMap: IECPointMap read GetPointMap;
    property HasEfficientPointMap: Boolean read GetHasEfficientPointMap;
  end;

implementation

{ TGlvTypeBEndomorphism }

function TGlvTypeBEndomorphism.CalculateB(const k, g: TBigInteger; t: Int32)
  : TBigInteger;
var
  negative, extra: Boolean;
  b: TBigInteger;
begin
  negative := (g.SignValue < 0);
  b := k.Multiply(g.Abs());
  extra := b.TestBit(t - 1);
  b := b.ShiftRight(t);
  if (extra) then
  begin
    b := b.Add(TBigInteger.One);
  end;
  if negative then
  begin
    Result := b.Negate();
  end
  else
  begin
    Result := b;
  end;
end;

constructor TGlvTypeBEndomorphism.Create(const curve: IECCurve;
  const parameters: IGlvTypeBParameters);
begin
  Inherited Create();
  Fm_curve := curve;
  Fm_parameters := parameters;
  Fm_pointMap := TScaleXPointMap.Create(curve.FromBigInteger(parameters.Beta));
end;

function TGlvTypeBEndomorphism.DecomposeScalar(const k: TBigInteger)
  : TCryptoLibGenericArray<TBigInteger>;
var
  bits: Int32;
  b1, b2, a, b: TBigInteger;
  v1, v2: TCryptoLibGenericArray<TBigInteger>;
begin
  bits := Fm_parameters.bits;
  b1 := CalculateB(k, Fm_parameters.G1, bits);
  b2 := CalculateB(k, Fm_parameters.G2, bits);

  v1 := Fm_parameters.v1;
  v2 := Fm_parameters.v2;
  a := k.Subtract((b1.Multiply(v1[0])).Add(b2.Multiply(v2[0])));
  b := (b1.Multiply(v1[1])).Add(b2.Multiply(v2[1])).Negate();

  Result := TCryptoLibGenericArray<TBigInteger>.Create(a, b);
end;

destructor TGlvTypeBEndomorphism.Destroy;
begin
  inherited Destroy;
end;

function TGlvTypeBEndomorphism.GetHasEfficientPointMap: Boolean;
begin
  Result := true;
end;

function TGlvTypeBEndomorphism.GetPointMap: IECPointMap;
begin
  Result := Fm_pointMap;
end;

end.
