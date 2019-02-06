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

unit ClpIIESWithCipherParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIIESParameters,
  ClpCryptoLibTypes;

type

  IIESWithCipherParameters = interface(IIESParameters)
    ['{77F38EA8-08F2-4D0D-A8E9-F3796DCCCA54}']

    function GetCipherKeySize: Int32;

    /// <summary>
    /// Return the key size in bits for the block cipher used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the block cipher used with the message
    /// </value>
    property CipherKeySize: Int32 read GetCipherKeySize;

    function GetNonce: TCryptoLibByteArray;

    /// <summary>
    /// Return the nonce (IV) value to be associated with message.
    /// </summary>
    /// <value>
    /// block cipher IV for message.
    /// </value>
    property Nonce: TCryptoLibByteArray read GetNonce;

    function GetPointCompression: Boolean;

    /// <summary>
    /// Return the 'point compression' flag.
    /// </summary>
    /// <value>
    /// the point compression flag
    /// </value>
    property PointCompression: Boolean read GetPointCompression;

  end;

implementation

end.
