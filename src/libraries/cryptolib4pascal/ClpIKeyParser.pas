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

unit ClpIKeyParser;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpIAsymmetricKeyParameter;

type
  IKeyParser = interface(IInterface)

    ['{12C134F7-7A7F-4F52-B963-ACA8DC933B48}']

    // raises EIOCryptoLibException if read fails
    function ReadKey(const stream: TStream): IAsymmetricKeyParameter;

  end;

implementation

end.
