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

unit ClpIAbstractECMultiplier;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC;

type

  IAbstractECMultiplier = interface(IECMultiplier)
    ['{DD63984C-7D4D-46DE-9004-20FD909C2EFB}']

    function MultiplyPositive(const p: IECPoint; const k: TBigInteger)
      : IECPoint;

  end;

implementation

end.
