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

unit ClpIScryptParametersGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpIPbeParametersGenerator,
  ClpCryptoLibTypes;

type
  IScryptParametersGenerator = interface(IPbeParametersGenerator)

    ['{1EB9E081-1F90-409F-A5B9-3A999EB6CC70}']

    procedure Init(const password, salt: TCryptoLibByteArray;
      cost, blockSize, parallelism: Int32);

  end;

implementation

end.
