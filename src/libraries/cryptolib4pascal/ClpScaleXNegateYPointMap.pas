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

unit ClpScaleXNegateYPointMap;

{$I CryptoLib.inc}

interface

uses
  ClpIECC,
  ClpIScaleXNegateYPointMap;

type
  TScaleXNegateYPointMap = class(TInterfacedObject, IECPointMap,
    IScaleXNegateYPointMap)

  strict protected
  var
    Fscale: IECFieldElement;

  public
    constructor Create(const scale: IECFieldElement);
    function Map(const p: IECPoint): IECPoint; virtual;
  end;

implementation

{ TScaleXNegateYPointMap }

constructor TScaleXNegateYPointMap.Create(const scale: IECFieldElement);
begin
  Inherited Create();
  Fscale := scale;
end;

function TScaleXNegateYPointMap.Map(const p: IECPoint): IECPoint;
begin
  Result := p.ScaleXNegateY(Fscale);
end;

end.
