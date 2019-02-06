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

unit ClpIDigest;

{$I CryptoLib.inc}

interface

uses
  HlpIHash,
  ClpCryptoLibTypes;

type
  // interface that a message digest conforms to.
  IDigest = interface(IInterface)
    ['{4AF1A541-DABE-4F89-8E9E-26DB61097330}']

    function GetAlgorithmName: string;

    /// <summary>
    /// the algorithm name
    /// </summary>
    property AlgorithmName: String read GetAlgorithmName;

    /// <summary>
    /// Gets the Underlying <b>IHash</b> Instance
    /// </summary>
    function GetUnderlyingIHash: IHash;

    /// <summary>
    /// the size, in bytes, of the digest produced by this message digest.
    /// </summary>
    function GetDigestSize(): Int32;

    /// <summary>
    /// the size, in bytes, of the internal buffer used by this digest.
    /// </summary>
    function GetByteLength(): Int32;

    /// <summary>
    /// update the message digest with a single byte.
    /// </summary>
    procedure Update(input: Byte);

    /// <summary>
    /// update the message digest with a block of bytes.
    /// </summary>
    /// <param name="input">
    /// the byte array containing the data.
    /// </param>
    /// <param name="inOff">
    /// the offset into the byte array where the data starts.
    /// </param>
    /// <param name="len">
    /// the length of the data.
    /// </param>
    procedure BlockUpdate(const input: TCryptoLibByteArray; inOff, len: Int32);

    /// <summary>
    /// Close the digest, producing the final digest value. The doFinal call
    /// leaves the digest reset.
    /// </summary>
    /// <param name="output">
    /// the array the digest is to be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the out array the digest is to start at.
    /// </param>
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload;
    function DoFinal: TCryptoLibByteArray; overload;

    /// <summary>
    /// Resets the digest back to it's initial state.
    /// </summary>
    procedure Reset();

  end;

implementation

end.
