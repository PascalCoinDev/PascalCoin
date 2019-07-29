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

unit ClpIMultipliers;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIZTauElement,
  ClpIECC;

type
  IAbstractECMultiplier = interface(IECMultiplier)
    ['{DD63984C-7D4D-46DE-9004-20FD909C2EFB}']

    function MultiplyPositive(const p: IECPoint; const k: TBigInteger)
      : IECPoint;

  end;

type
  IFixedPointCombMultiplier = interface(IAbstractECMultiplier)
    ['{A3345E31-4D5C-4442-9C3D-ACC7F6DA4A14}']

  end;

type
  IGlvMultiplier = interface(IAbstractECMultiplier)
    ['{F54D54F5-F544-421B-89FC-1D8058FB8F33}']

  end;

type
  IWNafL2RMultiplier = interface(IAbstractECMultiplier)

    ['{E2A5E4EF-C092-4F83-ACCF-0FC8731FB274}']

  end;

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
