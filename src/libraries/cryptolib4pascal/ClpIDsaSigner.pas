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

unit ClpIDsaSigner;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpIDsaExt,
  ClpISecureRandom,
  ClpBigInteger,
  ClpCryptoLibTypes;

type
  IDsaSigner = interface(IDsaExt)
    ['{687C14CD-F126-4886-87FC-535DEB083C2F}']

    function CalculateE(const n: TBigInteger;
      const &message: TCryptoLibByteArray): TBigInteger;

    function InitSecureRandom(needed: Boolean; const provided: ISecureRandom)
      : ISecureRandom;

  end;

implementation

end.
