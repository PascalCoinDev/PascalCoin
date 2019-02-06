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

unit ClpIRandom;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type

  IRandom = interface(IInterface)
    ['{509F9F51-2FC4-40E6-8E4A-68B59808BF5A}']

    procedure NextBytes(const buf: TCryptoLibByteArray); overload;

    function NextDouble(): Double;

    function Next(): Int32; overload;
    function Next(maxValue: Int32): Int32; overload;
    function Next(minValue, maxValue: Int32): Int32; overload;

  end;

implementation

end.
