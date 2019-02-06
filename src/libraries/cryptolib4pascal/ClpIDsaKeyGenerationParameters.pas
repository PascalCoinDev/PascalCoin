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

unit ClpIDsaKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDsaParameters,
  ClpIKeyGenerationParameters;

type
  IDsaKeyGenerationParameters = interface(IKeyGenerationParameters)
    ['{0EBFC33A-31D3-4F20-8836-35250F53EA73}']

    function GetParameters: IDsaParameters;

    property parameters: IDsaParameters read GetParameters;

  end;

implementation

end.
