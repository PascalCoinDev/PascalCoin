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

unit ClpIIESEngine;

{$I CryptoLib.inc}

interface

uses
  ClpIMac,
  ClpIBufferedBlockCipher,
  ClpICipherParameters,
  ClpIAsymmetricKeyParameter,
  ClpIEphemeralKeyPairGenerator,
  ClpIKeyParser,
  ClpCryptoLibTypes;

type

  IIESEngine = interface(IInterface)
    ['{9FA0E287-9988-467D-9E00-3BECEE4A78C6}']

    function GetCipher: IBufferedBlockCipher;
    function GetMac: IMac;

    /// <summary>
    /// Initialise the encryptor/decryptor.
    /// </summary>
    /// <param name="forEncryption">
    /// whether or not this is encryption/decryption.
    /// </param>
    /// <param name="privParam">
    /// our private key parameters
    /// </param>
    /// <param name="pubParam">
    /// the recipient's/sender's public key parameters
    /// </param>
    /// <param name="params">
    /// encoding and derivation parameters, may be wrapped to include an IV
    /// for an underlying block cipher.
    /// </param>
    procedure Init(forEncryption: Boolean; const privParam, pubParam,
      params: ICipherParameters); overload;

    /// <summary>
    /// Initialise the encryptor.
    /// </summary>
    /// <param name="publicKey">
    /// the recipient's/sender's public key parameters
    /// </param>
    /// <param name="params">
    /// encoding and derivation parameters, may be wrapped to include an IV
    /// for an underlying block cipher.
    /// </param>
    /// <param name="ephemeralKeyPairGenerator">
    /// the ephemeral key pair generator to use.
    /// </param>
    procedure Init(const publicKey: IAsymmetricKeyParameter;
      const params: ICipherParameters;
      const ephemeralKeyPairGenerator: IEphemeralKeyPairGenerator); overload;

    /// <summary>
    /// Initialise the decryptor.
    /// </summary>
    /// <param name="privateKey">
    /// the recipient's private key.
    /// </param>
    /// <param name="params">
    /// encoding and derivation parameters, may be wrapped to include an IV
    /// for an underlying block cipher.
    /// </param>
    /// <param name="publicKeyParser">
    /// the parser for reading the ephemeral public key.
    /// </param>
    procedure Init(const privateKey: IAsymmetricKeyParameter;
      const params: ICipherParameters;
      const publicKeyParser: IKeyParser); overload;

    function ProcessBlock(const &in: TCryptoLibByteArray; inOff, inLen: Int32)
      : TCryptoLibByteArray;

    property cipher: IBufferedBlockCipher read GetCipher;
    property mac: IMac read GetMac;
  end;

implementation

end.
