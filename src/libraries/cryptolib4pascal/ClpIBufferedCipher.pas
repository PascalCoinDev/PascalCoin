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

unit ClpIBufferedCipher;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpICipherParameters,
  ClpCryptoLibTypes;

type
  TBufferedCipherProgressEvent = procedure(AProcessed, ATotal: Int64);

type
  /// <remarks>Block cipher engines are expected to conform to this interface.</remarks>
  IBufferedCipher = interface(IInterface)
    ['{44FE589C-EDAE-4FDC-90AE-90C0935B4BC7}']
    /// <summary>The name of the algorithm this cipher implements.</summary>
    function GetAlgorithmName: String;
    property AlgorithmName: String read GetAlgorithmName;

    function GetBufferSize: Int32;
    procedure SetBufferSize(value: Int32);
    /// <summary>
    /// property for determining the buffer size to use for stream based
    /// encryption/decryption.
    /// </summary>
    property BufferSize: Int32 read GetBufferSize write SetBufferSize;

    function GetOnProgress: TBufferedCipherProgressEvent;
    procedure SetOnProgress(const value: TBufferedCipherProgressEvent);

    property OnProgress: TBufferedCipherProgressEvent read GetOnProgress
      write SetOnProgress;

    /// <summary>Initialise the cipher.</summary>
    /// <param name="forEncryption">If true the cipher is initialised for encryption,
    /// if false for decryption.</param>
    /// <param name="parameters">The key and other data required by the cipher.</param>
    procedure Init(forEncryption: Boolean; const parameters: ICipherParameters);

    function GetBlockSize(): Int32;

    function GetOutputSize(inputLen: Int32): Int32;

    function GetUpdateOutputSize(inputLen: Int32): Int32;

    procedure ProcessStream(const inputStream, outputStream: TStream;
      Length: Int64); overload;

    procedure ProcessStream(const inputStream: TStream; inOff: Int64;
      const outputStream: TStream; outOff: Int64; Length: Int64); overload;

    function ProcessByte(input: Byte): TCryptoLibByteArray; overload;
    function ProcessByte(input: Byte; const output: TCryptoLibByteArray;
      outOff: Int32): Int32; overload;

    function ProcessBytes(const input: TCryptoLibByteArray)
      : TCryptoLibByteArray; overload;
    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, Length: Int32): TCryptoLibByteArray; overload;
    function ProcessBytes(const input, output: TCryptoLibByteArray;
      outOff: Int32): Int32; overload;
    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, Length: Int32; const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload;

    function DoFinal(): TCryptoLibByteArray; overload;
    function DoFinal(const input: TCryptoLibByteArray)
      : TCryptoLibByteArray; overload;
    function DoFinal(const input: TCryptoLibByteArray; inOff, Length: Int32)
      : TCryptoLibByteArray; overload;
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload;
    function DoFinal(const input, output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload;
    function DoFinal(const input: TCryptoLibByteArray; inOff, Length: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32; overload;

    /// <summary>
    /// Reset the cipher. After resetting the cipher is in the same state
    /// as it was after the last init (if there was one).
    /// </summary>
    procedure Reset();
  end;

implementation

end.
