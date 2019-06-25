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

unit ClpIDHKeyGeneratorHelper;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpBigInteger,
  ClpIDHParameters;

type
  IDHKeyGeneratorHelper = interface(IInterface)
    ['{D4A55B45-E354-45A8-BCB7-871D7208A855}']

    function CalculatePrivate(const dhParams: IDHParameters;
      const random: ISecureRandom): TBigInteger;

    function CalculatePublic(const dhParams: IDHParameters;
      const x: TBigInteger): TBigInteger;

  end;

implementation

end.
