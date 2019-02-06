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

unit ClpIHkdfParameters;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpIDerivationParameters,
  ClpCryptoLibTypes;

type
  IHkdfParameters = interface(IDerivationParameters)
    ['{433636F6-C14E-41EB-84C6-B57746ABD13D}']

    /// <summary>
    /// Returns the input keying material or seed.
    /// </summary>
    /// <returns>
    /// the keying material
    /// </returns>
    function GetIkm(): TCryptoLibByteArray;

    /// <summary>
    /// Returns the salt, or null if the salt should be generated as a byte
    /// array <br />of HashLen zeros.
    /// </summary>
    /// <returns>
    /// the salt, or null
    /// </returns>
    function GetSalt(): TCryptoLibByteArray;

    /// <summary>
    /// Returns the info field, which may be empty (null is converted to
    /// empty).
    /// </summary>
    /// <returns>
    /// the info field, never null
    /// </returns>
    function GetInfo(): TCryptoLibByteArray;

    /// <summary>
    /// Returns if step 1: extract has to be skipped or not
    /// </summary>
    /// <returns>
    /// true for skipping, false for no skipping of step 1
    /// </returns>
    function GetSkipExtract: Boolean;

    property SkipExtract: Boolean read GetSkipExtract;
  end;

implementation

end.
