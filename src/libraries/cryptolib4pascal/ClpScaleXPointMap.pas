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

unit ClpScaleXPointMap;

{$I CryptoLib.inc}

interface

uses
  ClpIECC,
  ClpIScaleXPointMap;

type
  TScaleXPointMap = class(TInterfacedObject, IECPointMap, IScaleXPointMap)

  strict protected
  var
    Fscale: IECFieldElement;

  public
    constructor Create(const scale: IECFieldElement);
    destructor Destroy(); override;
    function Map(const p: IECPoint): IECPoint; virtual;
  end;

implementation

{ TScaleXPointMap }

constructor TScaleXPointMap.Create(const scale: IECFieldElement);
begin
  Inherited Create();
  Fscale := scale;
end;

destructor TScaleXPointMap.Destroy;
begin
  inherited Destroy;
end;

function TScaleXPointMap.Map(const p: IECPoint): IECPoint;
begin
  Result := p.ScaleX(Fscale);
end;

end.
