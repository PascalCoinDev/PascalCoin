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

{$I ..\Include\CryptoLib.inc}

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

    /// <summary>Indicates whether this cipher can handle partial blocks.</summary>
    function GetIsPartialBlockOkay: Boolean;
    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

    /// <summary>Process a block.</summary>
    /// <param name="inBuf">The input buffer.</param>
    /// <param name="inOff">The offset into <paramref>inBuf</paramref> that the input block begins.</param>
    /// <param name="outBuf">The output buffer.</param>
    /// <param name="outOff">The offset into <paramref>outBuf</paramref> to write the output block.</param>
    /// <exception cref="DataLengthException">If input block is wrong size, or outBuf too small.</exception>
    /// <returns>The number of bytes processed and produced.</returns>
    function ProcessBlock(inBuf: TCryptoLibByteArray; inOff: Int32;
      outBuf: TCryptoLibByteArray; outOff: Int32): Int32;

    /// <summary>
    /// Reset the cipher to the same state as it was after the last init (if there was one).
    /// </summary>
    procedure Reset();

  end;

implementation

end.
