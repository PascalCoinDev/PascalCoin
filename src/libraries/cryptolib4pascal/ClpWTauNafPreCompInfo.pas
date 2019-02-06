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

unit ClpWTauNafPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpIPreCompInfo,
  ClpIECC,
  ClpCryptoLibTypes,
  ClpIWTauNafPreCompInfo;

type
  /// **
  // * Class holding precomputation data for the WTNAF (Window
  // * <code>&#964;</code>-adic Non-Adjacent Form) algorithm.
  // */
  TWTauNafPreCompInfo = class(TInterfacedObject, IPreCompInfo,
    IWTauNafPreCompInfo)

  strict private
    function GetPreComp: TCryptoLibGenericArray<IAbstractF2mPoint>; virtual;
    procedure SetPreComp(const value
      : TCryptoLibGenericArray<IAbstractF2mPoint>); virtual;
  strict protected
  var
    // /**
    // * Array holding the precomputed <code>AbstractF2mPoint</code>s used for the
    // * WTNAF multiplication in <code>
    // * math.ec.multiplier.WTauNafMultiplier.multiply()
    // * WTauNafMultiplier.multiply()</code>.
    // */
    Fm_preComp: TCryptoLibGenericArray<IAbstractF2mPoint>;

  public
    property PreComp: TCryptoLibGenericArray<IAbstractF2mPoint> read GetPreComp
      write SetPreComp;
  end;

implementation

{ TWTauNafPreCompInfo }

function TWTauNafPreCompInfo.GetPreComp
  : TCryptoLibGenericArray<IAbstractF2mPoint>;
begin
  Result := Fm_preComp;
end;

procedure TWTauNafPreCompInfo.SetPreComp(const value
  : TCryptoLibGenericArray<IAbstractF2mPoint>);
begin
  Fm_preComp := value;
end;

end.
