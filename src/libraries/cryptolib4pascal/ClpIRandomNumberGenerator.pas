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

unit ClpIRandomNumberGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type
  IRandomNumberGenerator = interface(IInterface)
    ['{48F39DBB-8BE4-4167-8CE9-265F9B3B785E}']

    procedure GetBytes(const data: TCryptoLibByteArray);

    procedure GetNonZeroBytes(const data: TCryptoLibByteArray);

  end;

type
  IOSRandomNumberGenerator = interface(IRandomNumberGenerator)
    ['{EF52111D-1E69-42D7-99E0-D1C733D17995}']

  end;

type
  IPCGRandomNumberGenerator = interface(IRandomNumberGenerator)
    ['{49D3C867-E4F0-4EA3-BD81-0BCD6C0F08A8}']

  end;

implementation

end.
