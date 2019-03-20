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

unit ClpIStreamCipher;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpCryptoLibTypes;

type
  /// <remarks>the interface stream ciphers conform to.</remarks>
  IStreamCipher = interface(IInterface)
    ['{A4366B7A-2BC4-4D92-AEF2-512B621CA746}']

    /// <summary>The name of the algorithm this cipher implements.</summary>
    function GetAlgorithmName: String;
    property AlgorithmName: String read GetAlgorithmName;

    /// <summary>Initialise the cipher.</summary>
    /// <param name="forEncryption">Initialise for encryption if true, for decryption if false.</param>
    /// <param name="parameters">The key or other data required by the cipher.</param>
    procedure Init(forEncryption: Boolean; const parameters: ICipherParameters);

    /// <summary>encrypt/decrypt a single byte returning the result.</summary>
    /// <param name="input">the byte to be processed.</param>
    /// <returns>the result of processing the input byte.</returns>
    function ReturnByte(input: Byte): Byte;

    /// <summary>
    /// Process a block of bytes from <c>input</c> putting the result into <c>
    /// output</c>.
    /// </summary>
    /// <param name="inBytes">
    /// The input byte array.
    /// </param>
    /// <param name="inOff">
    /// The offset into <c>input</c> where the data to be processed starts.
    /// </param>
    /// <param name="len">
    /// The number of bytes to be processed.
    /// </param>
    /// <param name="outBytes">
    /// The output buffer the processed bytes go into.
    /// </param>
    /// <param name="outOff">
    /// The offset into <c>output</c> the processed data starts at.
    /// </param>
    /// <exception cref="EDataLengthCryptoLibException">
    /// If the output buffer is too small.
    /// </exception>
    procedure ProcessBytes(const inBytes: TCryptoLibByteArray;
      inOff, len: Int32; const outBytes: TCryptoLibByteArray; outOff: Int32);

    /// <summary>
    /// Reset the cipher to the same state as it was after the last init (if there was one).
    /// </summary>
    procedure Reset();

  end;

implementation

end.
