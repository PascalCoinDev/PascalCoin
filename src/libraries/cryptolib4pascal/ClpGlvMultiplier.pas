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

unit ClpGlvMultiplier;

{$I CryptoLib.inc}

interface

uses
  ClpSetWeakRef,
  ClpAbstractECMultiplier,
  ClpIECC,
  ClpIGlvEndomorphism,
  ClpCryptoLibTypes,
  ClpECAlgorithms,
  ClpBigInteger,
  ClpIGlvMultiplier;

resourcestring
  SCurveUnknownGroupOrder = 'Need Curve With Known Group Order, "curve"';

type
  TGlvMultiplier = class(TAbstractECMultiplier, IGlvMultiplier)

  strict protected
  var
    Fcurve: IECCurve;
    FglvEndomorphism: IGlvEndomorphism;

    function MultiplyPositive(const p: IECPoint; const k: TBigInteger)
      : IECPoint; override;

  public
    constructor Create(const curve: IECCurve;
      const glvEndomorphism: IGlvEndomorphism);
    destructor Destroy; override;

  end;

implementation

{ TGlvMultiplier }

constructor TGlvMultiplier.Create(const curve: IECCurve;
  const glvEndomorphism: IGlvEndomorphism);
begin
  inherited Create();
  if ((curve = Nil) or (not(curve.Order.IsInitialized))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SCurveUnknownGroupOrder);
  end;

  // Fcurve := curve;
  TSetWeakRef.SetWeakReference(@Fcurve, curve);
  FglvEndomorphism := glvEndomorphism;
end;

destructor TGlvMultiplier.Destroy;
begin
  TSetWeakRef.SetWeakReference(@Fcurve, Nil);
  inherited Destroy;
end;

function TGlvMultiplier.MultiplyPositive(const p: IECPoint;
  const k: TBigInteger): IECPoint;
var
  n, a, b: TBigInteger;
  ab: TCryptoLibGenericArray<TBigInteger>;
  pointMap: IECPointMap;
begin
  if (not(Fcurve.Equals(p.curve))) then
  begin
    raise EInvalidOperationCryptoLibException.Create('');
  end;

  n := p.curve.Order;
  ab := FglvEndomorphism.DecomposeScalar(k.&Mod(n));
  a := ab[0];
  b := ab[1];

  pointMap := FglvEndomorphism.pointMap;
  if (FglvEndomorphism.HasEfficientPointMap) then
  begin
    Result := TECAlgorithms.ImplShamirsTrickWNaf(p, a, pointMap, b);
    Exit;
  end;

  Result := TECAlgorithms.ImplShamirsTrickWNaf(p, a, pointMap.Map(p), b);
end;

end.
