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

unit ClpGlvTypeAEndomorphism;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpECCompUtilities,
  ClpScaleYNegateXPointMap,
  ClpIGlvTypeAEndomorphism,
  ClpIECC,
  ClpIGlvTypeAParameters,
  ClpIGlvEndomorphism;

type
  TGlvTypeAEndomorphism = class(TInterfacedObject, IECEndomorphism,
    IGlvEndomorphism, IGlvTypeAEndomorphism)

  strict private
    function GetHasEfficientPointMap: Boolean; virtual;
    function GetPointMap: IECPointMap; virtual;

  strict protected
  var
    FParameters: IGlvTypeAParameters;
    FPointMap: IECPointMap;

  public
    constructor Create(const curve: IECCurve;
      const parameters: IGlvTypeAParameters);

    function DecomposeScalar(const k: TBigInteger)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    property PointMap: IECPointMap read GetPointMap;
    property HasEfficientPointMap: Boolean read GetHasEfficientPointMap;
  end;

implementation

{ TGlvTypeAEndomorphism }

constructor TGlvTypeAEndomorphism.Create(const curve: IECCurve;
  const parameters: IGlvTypeAParameters);
begin
  Inherited Create();
  (*
    * NOTE: 'curve' MUST only be used to create a suitable ECFieldElement. Due to the way
    * ECCurve configuration works, 'curve' will not be the actual instance of ECCurve that the
    * endomorphism is being used with.
  *)
  FParameters := parameters;
  FPointMap := TScaleYNegateXPointMap.Create
    (curve.FromBigInteger(parameters.I));
end;

function TGlvTypeAEndomorphism.DecomposeScalar(const k: TBigInteger)
  : TCryptoLibGenericArray<TBigInteger>;
begin
  Result := TEndoUtilities.DecomposeScalar(FParameters.SplitParams, k);
end;

function TGlvTypeAEndomorphism.GetHasEfficientPointMap: Boolean;
begin
  Result := true;
end;

function TGlvTypeAEndomorphism.GetPointMap: IECPointMap;
begin
  Result := FPointMap;
end;

end.
