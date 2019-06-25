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

unit ClpIDHKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHParameters,
  ClpIKeyGenerationParameters;

type
  IDHKeyGenerationParameters = interface(IKeyGenerationParameters)
    ['{B513182A-1697-468E-A090-0E09C246BD8B}']

    function GetParameters: IDHParameters;

    property parameters: IDHParameters read GetParameters;

  end;

implementation

end.
