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

unit ClpIPaddingModes;

{$I CryptoLib.inc}

interface

uses
  ClpIBlockCipherPadding;

type
  IISO10126d2Padding = interface(IBlockCipherPadding)

    ['{42C927E4-57D2-4179-BEB0-250B7E2F7166}']

  end;

type
  IISO7816d4Padding = interface(IBlockCipherPadding)

    ['{0550BE74-BEDB-4723-9D31-F9E145C8C7AE}']

  end;

type
  IPkcs7Padding = interface(IBlockCipherPadding)

    ['{131D8DF8-27C6-43EC-A6C0-2B2E02E23996}']

  end;

type
  ITBCPadding = interface(IBlockCipherPadding)

    ['{D279C067-7DB6-406C-82CC-607DECD79F60}']

  end;

type
  IX923Padding = interface(IBlockCipherPadding)

    ['{8815D63C-936C-497F-9B00-29F6F9E178A7}']

  end;

type
  IZeroBytePadding = interface(IBlockCipherPadding)

    ['{7B154AD1-F4DD-48A4-81B6-63A1DA8BB5A3}']

  end;

implementation

end.
