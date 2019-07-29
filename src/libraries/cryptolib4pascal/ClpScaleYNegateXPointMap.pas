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

unit ClpScaleYNegateXPointMap;

{$I CryptoLib.inc}

interface

uses
  ClpIECC,
  ClpIScaleYNegateXPointMap;

type
  TScaleYNegateXPointMap = class(TInterfacedObject, IECPointMap,
    IScaleYNegateXPointMap)

  strict protected
  var
    Fscale: IECFieldElement;

  public
    constructor Create(const scale: IECFieldElement);
    function Map(const p: IECPoint): IECPoint; virtual;
  end;

implementation

{ TScaleYNegateXPointMap }

constructor TScaleYNegateXPointMap.Create(const scale: IECFieldElement);
begin
  Inherited Create();
  Fscale := scale;
end;

function TScaleYNegateXPointMap.Map(const p: IECPoint): IECPoint;
begin
  Result := p.ScaleYNegateX(Fscale);
end;

end.
