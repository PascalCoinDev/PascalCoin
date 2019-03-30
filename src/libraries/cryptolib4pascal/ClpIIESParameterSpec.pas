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

unit ClpIIESParameterSpec;

{$I CryptoLib.inc}

interface

uses
  ClpIAlgorithmParameterSpec,
  ClpCryptoLibTypes;

type
  IIESParameterSpec = interface(IAlgorithmParameterSpec)
    ['{F83CD14B-C049-4878-8D78-0214FD9D2B8A}']

    /// <summary>
    /// Returns the derivation vector.
    /// </summary>
    /// <value>
    /// the derivation vector.
    /// </value>
    function GetDerivationV: TCryptoLibByteArray;
    property DerivationV: TCryptoLibByteArray read GetDerivationV;

    /// <summary>
    /// Returns the encoding vector.
    /// </summary>
    /// <value>
    /// the encoding vector.
    /// </value>
    function GetEncodingV: TCryptoLibByteArray;
    property EncodingV: TCryptoLibByteArray read GetEncodingV;

    /// <summary>
    /// Return the key size in bits for the MAC used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the MAC used with the message
    /// </value>
    function GetMacKeySize: Int32;
    property MacKeySize: Int32 read GetMacKeySize;

    /// <summary>
    /// Return the key size in bits for the block cipher used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the block cipher used with the message
    /// </value>
    function GetCipherKeySize: Int32;
    property CipherKeySize: Int32 read GetCipherKeySize;

    /// <summary>
    /// Return the Nonce (IV) value to be associated with message.
    /// </summary>
    /// <value>
    /// block cipher IV for message.
    /// </value>
    function GetNonce: TCryptoLibByteArray;
    property Nonce: TCryptoLibByteArray read GetNonce;

    /// <summary>
    /// Return the 'point compression' flag.
    /// </summary>
    /// <value>
    /// the point compression flag
    /// </value>
    function GetPointCompression: Boolean;
    property PointCompression: Boolean read GetPointCompression;

  end;

implementation

end.
