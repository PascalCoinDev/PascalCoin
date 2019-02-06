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

unit ClpIWTauNafMultiplier;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC,
  ClpIZTauElement,
  ClpIAbstractECMultiplier;

type
  IWTauNafMultiplier = interface(IAbstractECMultiplier)
    ['{B71E75E5-FB6D-4A54-BE8A-820FC9A1E509}']

    // /**
    // * Multiplies an AbstractF2mPoint
    // * by an element <code>&#955;</code> of <code><b>Z</b>[&#964;]</code> using
    // * the <code>&#964;</code>-adic NAF (TNAF) method.
    // * @param p The AbstractF2mPoint to multiply.
    // * @param lambda The element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code> of which to compute the
    // * <code>[&#964;]</code>-adic NAF.
    // * @return <code>p</code> multiplied by <code>&#955;</code>.
    // */
    function MultiplyWTnaf(const p: IAbstractF2mPoint;
      const lambda: IZTauElement; a, mu: ShortInt): IAbstractF2mPoint;

  end;

implementation

end.
