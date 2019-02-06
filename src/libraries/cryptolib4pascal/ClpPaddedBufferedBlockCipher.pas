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

unit ClpPaddedBufferedBlockCipher;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Math,
  ClpCheck,
  ClpIBlockCipher,
  ClpPaddingModes,
  ClpIPaddingModes,
  ClpBufferedBlockCipher,
  ClpIPaddedBufferedBlockCipher,
  ClpIBlockCipherPadding,
  ClpICipherParameters,
  ClpIParametersWithRandom,
  ClpISecureRandom,
  ClpCryptoLibTypes;

resourcestring
  SOutputBufferTooSmall = 'Output Buffer too Short';
  SIncompleteLastBlockInDecryption = 'Last Block Incomplete in Decryption';
  SNegativeInputLength = 'Can''t Have a Negative Input Length!';

type

  /// <summary>
  /// <para>
  /// A wrapper class that allows block ciphers to be used to process
  /// data in a piecemeal fashion with padding.
  /// </para>
  /// <para>
  /// The PaddedBufferedBlockCipher outputs a block only when the buffer
  /// is full and more data is being added, or on a doFinal (unless the
  /// current block in the buffer is a pad block). <br />The default
  /// padding mechanism used is the one outlined in Pkcs5/Pkcs7. <br />
  /// </para>
  /// </summary>
  TPaddedBufferedBlockCipher = class sealed(TBufferedBlockCipher,
    IPaddedBufferedBlockCipher)

  strict private
    Fpadding: IBlockCipherPadding;

  public

    /// <summary>
    /// Create a buffered block cipher with the desired padding.
    /// </summary>
    /// <param name="cipher">
    /// the underlying block cipher this buffering object wraps.
    /// </param>
    /// <param name="padding">
    /// the padding type.
    /// </param>
    constructor Create(const cipher: IBlockCipher;
      const padding: IBlockCipherPadding); overload;

    /// <summary>
    /// Create a buffered block cipher Pkcs7 padding
    /// </summary>
    /// <param name="cipher">
    /// the underlying block cipher this buffering object wraps.
    /// </param>
    constructor Create(const cipher: IBlockCipher); overload;

    /// <summary>
    /// initialise the cipher.
    /// </summary>
    /// <param name="forEncryption">
    /// if true the cipher is initialised for encryption, if false for
    /// decryption.
    /// </param>
    /// <param name="parameters">
    /// the key and other data required by the cipher.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the parameters argument is inappropriate.
    /// </exception>
    procedure Init(forEncryption: Boolean;
      const parameters: ICipherParameters); override;

    /// <summary>
    /// return the minimum size of the output buffer required for an update
    /// plus a doFinal with an input of len bytes.
    /// </summary>
    /// <param name="length">
    /// the length of the input.
    /// </param>
    /// <returns>
    /// the space required to accommodate a call to update and doFinal with
    /// len bytes of input.
    /// </returns>
    function GetOutputSize(length: Int32): Int32; override;

    /// <summary>
    /// return the size of the output buffer required for an update an input
    /// of len bytes.
    /// </summary>
    /// <param name="length">
    /// the length of the input.
    /// </param>
    /// <returns>
    /// the space required to accommodate a call to update with length bytes
    /// of input.
    /// </returns>
    function GetUpdateOutputSize(length: Int32): Int32; override;

    /// <summary>
    /// process a single byte, producing an output block if necessary.
    /// </summary>
    /// <param name="input">
    /// the input byte.
    /// </param>
    /// <param name="output">
    /// the space for any output that might be produced.
    /// </param>
    /// <param name="outOff">
    /// the offset from which the output will be copied.
    /// </param>
    /// <returns>
    /// the number of output bytes copied to output.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function ProcessByte(input: Byte; const output: TCryptoLibByteArray;
      outOff: Int32): Int32; override;

    /// <summary>
    /// process an array of bytes, producing output if necessary.
    /// </summary>
    /// <param name="input">
    /// the input byte array.
    /// </param>
    /// <param name="inOff">
    /// the offset at which the input data starts.
    /// </param>
    /// <param name="length">
    /// the number of bytes to be copied out of the input array.
    /// </param>
    /// <param name="output">
    /// the space for any output that might be produced.
    /// </param>
    /// <param name="outOff">
    /// the offset from which the output will be copied.
    /// </param>
    /// <returns>
    /// the number of output bytes copied to output.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, length: Int32; const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; override;

    /// <summary>
    /// Process the last block in the buffer. If the buffer is currently full
    /// and padding needs to be added a call to doFinal will produce 2 *
    /// GetBlockSize() bytes.
    /// </summary>
    /// <param name="output">
    /// the array the block currently being held is copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset at which the copying starts.
    /// </param>
    /// <returns>
    /// the number of output bytes copied to output.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there is insufficient space in output for the output or we are
    /// decrypting and the input is not block size aligned.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the underlying cipher is not initialised.
    /// </exception>
    /// <exception cref="EInvalidCipherTextCryptoLibException">
    /// if padding is expected and not found.
    /// </exception>
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; override;

  end;

implementation

{ TPaddedBufferedBlockCipher }

constructor TPaddedBufferedBlockCipher.Create(const cipher: IBlockCipher;
  const padding: IBlockCipherPadding);
begin
  Inherited Create();
  Fcipher := cipher;
  Fpadding := padding;

  System.SetLength(Fbuf, cipher.GetBlockSize());
  FbufOff := 0;
end;

constructor TPaddedBufferedBlockCipher.Create(const cipher: IBlockCipher);
begin
  Create(cipher, TPkcs7Padding.Create() as IPkcs7Padding);
end;

function TPaddedBufferedBlockCipher.DoFinal(const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
var
  blockSize, resultLen, resultTotalLen: Int32;
begin
  blockSize := Fcipher.GetBlockSize();
  resultLen := 0;

  if (FforEncryption) then
  begin
    if (FbufOff = blockSize) then
    begin
      if ((outOff + 2 * blockSize) > System.length(output)) then
      begin
        Reset();

        raise EOutputLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
      end;

      resultLen := Fcipher.ProcessBlock(Fbuf, 0, output, outOff);
      FbufOff := 0;
    end;

    Fpadding.AddPadding(Fbuf, FbufOff);

    resultLen := resultLen + Fcipher.ProcessBlock(Fbuf, 0, output,
      outOff + resultLen);

    Reset();
  end
  else
  begin
    if (FbufOff = blockSize) then
    begin
      resultLen := Fcipher.ProcessBlock(Fbuf, 0, Fbuf, 0);
      FbufOff := 0;
    end
    else
    begin
      Reset();

      raise EDataLengthCryptoLibException.CreateRes
        (@SIncompleteLastBlockInDecryption);
    end;

    try
      resultLen := resultLen - Fpadding.PadCount(Fbuf);
      resultTotalLen := resultLen * System.SizeOf(Byte);
      if resultTotalLen > 0 then
      begin
        System.Move(Fbuf[0], output[outOff], resultTotalLen);
      end;

    finally
      Reset();
    end;

  end;

  result := resultLen;
end;

function TPaddedBufferedBlockCipher.GetOutputSize(length: Int32): Int32;
var
  total, leftOver: Int32;
begin
  total := length + FbufOff;
  leftOver := total mod System.length(Fbuf);

  if (leftOver = 0) then
  begin
    if (FforEncryption) then
    begin
      result := total + System.length(Fbuf);
      Exit;
    end;

    result := total;
    Exit;
  end;

  result := total - leftOver + System.length(Fbuf);
end;

function TPaddedBufferedBlockCipher.GetUpdateOutputSize(length: Int32): Int32;
var
  total, leftOver: Int32;
begin
  total := length + FbufOff;
  leftOver := total mod System.length(Fbuf);

  if (leftOver = 0) then
  begin
    // result := total - System.length(Fbuf);
    result := Max(0, total - System.length(Fbuf));
    Exit;
  end;

  result := total - leftOver;
end;

procedure TPaddedBufferedBlockCipher.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  initRandom: ISecureRandom;
  Lparameters: ICipherParameters;
  p: IParametersWithRandom;
begin
  FforEncryption := forEncryption;
  Lparameters := parameters;
  initRandom := Nil;

  if Supports(Lparameters, IParametersWithRandom, p) then
  begin
    initRandom := p.Random;
    Lparameters := p.parameters;
  end;

  Reset();
  Fpadding.Init(initRandom);
  Fcipher.Init(forEncryption, Lparameters);
end;

function TPaddedBufferedBlockCipher.ProcessByte(input: Byte;
  const output: TCryptoLibByteArray; outOff: Int32): Int32;
var
  resultLen: Int32;
begin
  resultLen := 0;

  if (FbufOff = System.length(Fbuf)) then
  begin
    resultLen := Fcipher.ProcessBlock(Fbuf, 0, output, outOff);
    FbufOff := 0;
  end;

  Fbuf[FbufOff] := input;
  System.Inc(FbufOff);

  result := resultLen;
end;

function TPaddedBufferedBlockCipher.ProcessBytes(const input
  : TCryptoLibByteArray; inOff, length: Int32;
  const output: TCryptoLibByteArray; outOff: Int32): Int32;
var
  blockSize, outLength, resultLen, gapLen: Int32;
begin
  if (length < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNegativeInputLength);
  end;

  blockSize := GetBlockSize();
  outLength := GetUpdateOutputSize(length);

  if (outLength > 0) then
  begin
    TCheck.OutputLength(output, outOff, outLength, SOutputBufferTooSmall);
  end;

  resultLen := 0;
  gapLen := System.length(Fbuf) - FbufOff;

  if (length > gapLen) then
  begin
    System.Move(input[inOff], Fbuf[FbufOff], gapLen * System.SizeOf(Byte));

    resultLen := resultLen + Fcipher.ProcessBlock(Fbuf, 0, output, outOff);

    FbufOff := 0;
    length := length - gapLen;
    inOff := inOff + gapLen;

    while (length > System.length(Fbuf)) do
    begin
      resultLen := resultLen + Fcipher.ProcessBlock(input, inOff, output,
        outOff + resultLen);

      length := length - blockSize;
      inOff := inOff + blockSize;
    end;
  end;

  System.Move(input[inOff], Fbuf[FbufOff], length * System.SizeOf(Byte));

  FbufOff := FbufOff + length;

  result := resultLen;
end;

end.
