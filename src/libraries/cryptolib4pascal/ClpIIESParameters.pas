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

unit ClpIIESParameters;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpCryptoLibTypes;

type

  IIESParameters = interface(ICipherParameters)
    ['{F95232BB-594C-492E-AF63-C5A6822C96FD}']

    function GetDerivationV: TCryptoLibByteArray;

    /// <summary>
    /// Return the derivation vector.
    /// </summary>
    /// <value>
    /// the derivation vector.
    /// </value>
    property DerivationV: TCryptoLibByteArray read GetDerivationV;

    function GetEncodingV: TCryptoLibByteArray;

    /// <summary>
    /// Return the encoding vector.
    /// </summary>
    /// <value>
    /// the encoding vector.
    /// </value>
    property EncodingV: TCryptoLibByteArray read GetEncodingV;

    function GetMacKeySize: Int32;

    /// <summary>
    /// Return the key size in bits for the MAC used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the MAC used with the message
    /// </value>
    property macKeySize: Int32 read GetMacKeySize;

  end;

implementation

end.
