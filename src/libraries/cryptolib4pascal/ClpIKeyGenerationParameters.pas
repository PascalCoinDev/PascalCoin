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

unit ClpIKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom;

type

  IKeyGenerationParameters = interface(IInterface)
    ['{3DBCEB51-7E2D-4C81-A47C-51D73A564352}']

    /// <returns>
    /// return the generators random source.
    /// </returns>
    function GetRandom: ISecureRandom;

    /// <returns>
    /// return the strength of the keys this generator produces (in bits).
    /// </returns>
    function GetStrength: Int32;

    /// <value>
    /// return the random source associated with this generator.
    /// </value>
    property Random: ISecureRandom read GetRandom;

    /// <value>
    /// return the bit strength for keys produced by this generator.
    /// </value>
    property Strength: Int32 read GetStrength;
  end;

implementation

end.
