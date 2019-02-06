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

unit ClpBufferedBlockCipher;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpCheck,
  ClpBufferedCipherBase,
  ClpIBlockCipher,
  ClpIBufferedBlockCipher,
  ClpICipherParameters,
  ClpIParametersWithRandom,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SInvalidLength = 'Can''t Have a Negative Input Length!';
  SInputNil = 'Input Cannot be Nil';
  SCipherNil = 'Cipher Cannot be Nil';
  SOutputBufferTooSmall = 'Output Buffer too Short';
  SDataNotBlockSizeAligned = 'Data not Block Size Aligned';
  SOutputBufferTooSmallForDoFinal = 'Output Buffer too Short for DoFinal()';

type

  /// <summary>
  /// <para>
  /// A wrapper class that allows block ciphers to be used to process
  /// data in a piecemeal fashion. The BufferedBlockCipher outputs a
  /// block only when the buffer is full and more data is being added, or
  /// on a doFinal.
  /// </para>
  /// <para>
  /// Note: in the case where the underlying cipher is either a CFB
  /// cipher or an OFB one the last block may not be a multiple of the
  /// block size.
  /// </para>
  /// </summary>
  TBufferedBlockCipher = class(TBufferedCipherBase, IBufferedBlockCipher)

  strict protected
  var
    Fbuf: TCryptoLibByteArray;
    FbufOff: Int32;
    FforEncryption: Boolean;
    Fcipher: IBlockCipher;

    /// <summary>
    /// constructor for subclasses
    /// </summary>
    constructor Create(); overload;

  public
    /// <summary>
    /// Create a buffered block cipher without padding.
    /// </summary>
    /// <param name="cipher">
    /// the underlying block cipher this buffering object wraps.
    /// </param>
    constructor Create(const cipher: IBlockCipher); overload;

    /// <summary>
    /// initialise the cipher.
    /// </summary>
    /// <param name="forEncryption">
    /// forEncryption if true the cipher is initialised for encryption, if
    /// false for decryption.
    /// </param>
    /// <param name="parameters">
    /// the key and other data required by the cipher.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the parameters argument is inappropriate.
    /// </exception>
    // Note: This doubles as the Init in the event that this cipher is being used as an IWrapper
    procedure Init(forEncryption: Boolean;
      const parameters: ICipherParameters); override;

    /// <summary>
    /// return the blocksize for the underlying cipher.
    /// </summary>
    /// <returns>
    /// return the blocksize for the underlying cipher.
    /// </returns>
    function GetBlockSize(): Int32; override;

    /// <summary>
    /// return the size of the output buffer required for an update an input
    /// of len bytes.
    /// </summary>
    /// <param name="length">
    /// the length of the input.
    /// </param>
    /// <returns>
    /// return the space required to accommodate a call to update with length
    /// bytes of input.
    /// </returns>
    function GetUpdateOutputSize(length: Int32): Int32; override;

    /// <summary>
    /// return the size of the output buffer required for an update plus a
    /// doFinal with an input of length bytes.
    /// </summary>
    /// <param name="length">
    /// the length of the input.
    /// </param>
    /// <returns>
    /// the space required to accommodate a call to update and doFinal with
    /// length bytes of input.
    /// </returns>
    function GetOutputSize(length: Int32): Int32; override;

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
      outOff: Int32): Int32; overload; override;

    function ProcessByte(input: Byte): TCryptoLibByteArray; overload; override;

    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, length: Int32): TCryptoLibByteArray; overload; override;

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
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, length: Int32; const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload; override;

    function DoFinal(): TCryptoLibByteArray; overload; override;
    function DoFinal(const input: TCryptoLibByteArray; inOff, inLen: Int32)
      : TCryptoLibByteArray; overload; override;

    /// <summary>
    /// Process the last block in the buffer.
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
    /// if there is insufficient space in output for the output, or the input
    /// is not block size aligned and should be.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the underlying cipher is not initialised.
    /// </exception>
    /// <exception cref="EInvalidCipherTextCryptoLibException">
    /// if padding is expected and not found.
    /// </exception>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if the input is not block size aligned.
    /// </exception>
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32): Int32;
      overload; override;

    /// <summary>
    /// Reset the buffer and cipher. After resetting the object is in the
    /// same state as it was after the last init (if there was one).
    /// </summary>
    procedure Reset(); override;

    function GetAlgorithmName: String; override;
    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

{ TBufferedBlockCipher }

constructor TBufferedBlockCipher.Create(const cipher: IBlockCipher);
begin
  Inherited Create();
  if (cipher = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SCipherNil);
  end;

  Fcipher := cipher;
  System.SetLength(Fbuf, cipher.GetBlockSize());
  FbufOff := 0;
end;

constructor TBufferedBlockCipher.Create;
begin
  Inherited Create();
end;

function TBufferedBlockCipher.DoFinal(const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
begin
  try
    if (FbufOff <> 0) then
    begin
      TCheck.DataLength(not Fcipher.IsPartialBlockOkay,
        SDataNotBlockSizeAligned);
      TCheck.OutputLength(output, outOff, FbufOff,
        SOutputBufferTooSmallForDoFinal);

      // NB: Can't copy directly, or we may write too much output
      Fcipher.ProcessBlock(Fbuf, 0, Fbuf, 0);
      System.Move(Fbuf[0], output[outOff], FbufOff * System.SizeOf(Byte));
    end;

    result := FbufOff;
    Exit;
  finally
    Reset();
  end;
end;

function TBufferedBlockCipher.DoFinal(const input: TCryptoLibByteArray;
  inOff, inLen: Int32): TCryptoLibByteArray;
var
  &length, &pos: Int32;
  outBytes, tmp: TCryptoLibByteArray;
begin
  if (input = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SInputNil);
  end;

  &length := GetOutputSize(inLen);

  outBytes := EmptyBuffer;

  if (&length > 0) then
  begin
    System.SetLength(outBytes, length);

    if (inLen > 0) then
    begin
      &pos := ProcessBytes(input, inOff, inLen, outBytes, 0);
    end
    else
    begin
      &pos := 0;
    end;

    &pos := &pos + DoFinal(outBytes, &pos);

    if (&pos < System.length(outBytes)) then
    begin
      System.SetLength(tmp, &pos);
      System.Move(outBytes[0], tmp[0], &pos * System.SizeOf(Byte));
      outBytes := tmp;
    end
  end
  else
  begin
    Reset();
  end;

  result := outBytes;
end;

function TBufferedBlockCipher.DoFinal: TCryptoLibByteArray;
var
  outBytes, tmp: TCryptoLibByteArray;
  &length, &pos: Int32;
begin
  outBytes := EmptyBuffer;

  &length := GetOutputSize(0);
  if (&length > 0) then
  begin
    System.SetLength(outBytes, &length);

    &pos := DoFinal(outBytes, 0);
    if (&pos < System.length(outBytes)) then
    begin
      System.SetLength(tmp, &pos);
      System.Move(outBytes[0], tmp[0], &pos * System.SizeOf(Byte));
      outBytes := tmp;
    end
  end
  else
  begin
    Reset();
  end;

  result := outBytes;
end;

function TBufferedBlockCipher.GetAlgorithmName: String;
begin
  result := Fcipher.AlgorithmName;
end;

function TBufferedBlockCipher.GetBlockSize: Int32;
begin
  result := Fcipher.GetBlockSize();
end;

function TBufferedBlockCipher.GetOutputSize(length: Int32): Int32;
begin
  // Note: Can assume IsPartialBlockOkay is true for purposes of this calculation
  result := length + FbufOff;
end;

function TBufferedBlockCipher.GetUpdateOutputSize(length: Int32): Int32;
var
  total, leftOver: Int32;
begin
  total := length + FbufOff;
  leftOver := total mod System.length(Fbuf);
  result := total - leftOver;
end;

procedure TBufferedBlockCipher.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  pwr: IParametersWithRandom;
  Lparameters: ICipherParameters;
begin
  FforEncryption := forEncryption;
  Lparameters := parameters;

  if Supports(Lparameters, IParametersWithRandom, pwr) then
  begin
    Lparameters := pwr.parameters;
  end;;

  Reset();

  Fcipher.Init(forEncryption, Lparameters);

end;

function TBufferedBlockCipher.ProcessByte(input: Byte;
  const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin

  Fbuf[FbufOff] := input;
  System.Inc(FbufOff);

  if (FbufOff = System.length(Fbuf)) then
  begin
    if ((outOff + System.length(Fbuf)) > System.length(output)) then
    begin
      raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
    end;

    FbufOff := 0;
    result := Fcipher.ProcessBlock(Fbuf, 0, output, outOff);
    Exit;
  end;

  result := 0;
end;

function TBufferedBlockCipher.ProcessByte(input: Byte): TCryptoLibByteArray;
var
  outLength, &pos: Int32;
  outBytes, tmp: TCryptoLibByteArray;
begin
  outLength := GetUpdateOutputSize(1);

  if outLength > 0 then
  begin
    System.SetLength(outBytes, outLength);
  end
  else
  begin
    outBytes := Nil;
  end;

  &pos := ProcessByte(input, outBytes, 0);

  if ((outLength > 0) and (pos < outLength)) then
  begin
    System.SetLength(tmp, &pos);
    System.Move(outBytes[0], tmp[0], &pos * System.SizeOf(Byte));

    outBytes := tmp;
  end;

  result := outBytes;
end;

function TBufferedBlockCipher.ProcessBytes(const input: TCryptoLibByteArray;
  inOff, length: Int32; const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
var
  blockSize, outLength, resultLen, gapLen: Int32;
begin
  if (length < 1) then
  begin
    if (length < 0) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidLength);
    end;
    result := 0;
    Exit;
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
  if (FbufOff = System.length(Fbuf)) then
  begin
    resultLen := resultLen + Fcipher.ProcessBlock(Fbuf, 0, output,
      outOff + resultLen);
    FbufOff := 0;
  end;
  result := resultLen;
end;

function TBufferedBlockCipher.ProcessBytes(const input: TCryptoLibByteArray;
  inOff, length: Int32): TCryptoLibByteArray;
var
  outLength, &pos: Int32;
  outBytes, tmp: TCryptoLibByteArray;
begin
  if (input = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SInputNil);
  end;
  if (length < 1) then
  begin
    result := Nil;
    Exit;
  end;

  outLength := GetUpdateOutputSize(length);

  if outLength > 0 then
  begin
    System.SetLength(outBytes, outLength);
  end
  else
  begin
    outBytes := Nil;
  end;

  &pos := ProcessBytes(input, inOff, length, outBytes, 0);

  if ((outLength > 0) and (pos < outLength)) then
  begin
    System.SetLength(tmp, &pos);
    System.Move(outBytes[0], tmp[0], &pos * System.SizeOf(Byte));

    outBytes := tmp;
  end;

  result := outBytes;

end;

procedure TBufferedBlockCipher.Reset;
begin
  TArrayUtils.Fill(Fbuf, 0, System.length(Fbuf), Byte(0));
  FbufOff := 0;

  Fcipher.Reset();
end;

end.
