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

unit ClpBlockCipherModes;

{$I CryptoLib.inc}

interface

uses
  Math,
  SysUtils,
  ClpIBlockCipher,
  ClpBufferedBlockCipher,
  ClpICipherParameters,
  ClpIParametersWithIV,
  ClpArrayUtils,
  ClpCryptoLibTypes,
  ClpIBlockCipherModes;

resourcestring
  SInvalidIVLength =
    'Initialisation Vector Must be the Same Length as Block Size';
  SInvalidChangeState = 'Cannot Change Encrypting State Without Providing Key.';
  SInputBufferTooShort = 'Input Buffer too Short';
  SOutputBufferTooShort = 'Output Buffer too Short';
{$IFNDEF _FIXINSIGHT_}
  SInvalidParameterArgument = 'CTR/SIC Mode Requires ParametersWithIV';
  SInvalidTooLargeIVLength =
    'CTR/SIC mode requires IV no greater than: %u bytes';
  SInvalidTooSmallIVLength = 'CTR/SIC mode requires IV of at least: %u bytes';
{$ENDIF}
  SUnsupportedCipher = 'CtsBlockCipher Can Only Accept ECB or CBC Ciphers';
  SNegativeInputLength = 'Can''t Have a Negative Input Length!';
  SCTSDoFinalError = 'Need at Least One Block of Input For CTS';

type

  /// <summary>
  /// implements Cipher-Block-Chaining (CBC) mode on top of a simple cipher.
  /// </summary>
  TCbcBlockCipher = class sealed(TInterfacedObject, ICbcBlockCipher,
    IBlockCipher)

  strict private

  var
    FIV, FcbcV, FcbcNextV: TCryptoLibByteArray;
    FblockSize: Int32;
    Fcipher: IBlockCipher;
    Fencrypting: Boolean;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <returns>
    /// return the name of the underlying algorithm followed by "/CBC"
    /// </returns>
    function GetAlgorithmName: String; inline;

    function GetIsPartialBlockOkay: Boolean; inline;

    /// <summary>
    /// Do the appropriate chaining step for CBC mode encryption.
    /// </summary>
    /// <param name="input">
    /// the array containing the data to be encrypted.
    /// </param>
    /// <param name="inOff">
    /// offset into the in array the data starts at.
    /// </param>
    /// <param name="outBytes">
    /// the array the encrypted data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the out array the output will start at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function EncryptBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;

    /// <summary>
    /// Do the appropriate chaining step for CBC mode decryption.
    /// </summary>
    /// <param name="input">
    /// the array containing the data to be decrypted.
    /// </param>
    /// <param name="inOff">
    /// offset into the in array the data starts at.
    /// </param>
    /// <param name="outBytes">
    /// the array the decrypted data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the out array the output will start at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function DecryptBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;

  public

    /// <summary>
    /// Basic constructor.
    /// </summary>
    /// <param name="cipher">
    /// the block cipher to be used as the basis of chaining.
    /// </param>
    constructor Create(const cipher: IBlockCipher);

    /// <summary>
    /// return the underlying block cipher that we are wrapping.
    /// </summary>
    /// <returns>
    /// return the underlying block cipher that we are wrapping.
    /// </returns>
    function GetUnderlyingCipher(): IBlockCipher;

    /// <summary>
    /// Initialise the cipher and, possibly, the initialisation vector (IV). <br />
    /// If an IV isn't passed as part of the parameter, the IV will be all
    /// zeros.
    /// </summary>
    /// <param name="forEncryption">
    /// forEncryption if true the cipher is initialised for encryption, if
    /// false for decryption.
    /// </param>
    /// <param name="parameters">
    /// the key and other data required by the cipher.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the parameters argument is inappropriate
    /// </exception>
    procedure Init(forEncryption: Boolean; const parameters: ICipherParameters);

    /// <summary>
    /// return the block size of the underlying cipher.
    /// </summary>
    /// <returns>
    /// return the block size of the underlying cipher.
    /// </returns>
    function GetBlockSize(): Int32; inline;

    /// <summary>
    /// Process one block of input from the input array and write it to the
    /// output array.
    /// </summary>
    /// <param name="input">
    /// the array containing the input data.
    /// </param>
    /// <param name="inOff">
    /// offset into the input array the data starts at.
    /// </param>
    /// <param name="output">
    /// the array the output data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the output array the data starts at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32;

    /// <summary>
    /// reset the chaining vector back to the IV and reset the underlying
    /// cipher.
    /// </summary>
    procedure Reset(); inline;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <value>
    /// return the name of the underlying algorithm followed by "/CBC"
    /// </value>
    property AlgorithmName: String read GetAlgorithmName;

    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

  end;

type

  /// <summary>
  /// implements a Cipher-FeedBack (CFB) mode on top of a simple cipher.
  /// </summary>
  TCfbBlockCipher = class sealed(TInterfacedObject, ICfbBlockCipher,
    IBlockCipher)

  strict private

  var
    FIV, FcfbV, FcfbOutV: TCryptoLibByteArray;
    FblockSize: Int32;
    Fcipher: IBlockCipher;
    Fencrypting: Boolean;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <returns>
    /// return the name of the underlying algorithm followed by "/CFB"
    /// </returns>
    function GetAlgorithmName: String; inline;

    function GetIsPartialBlockOkay: Boolean; inline;

    /// <summary>
    /// Do the appropriate processing for CFB mode encryption.
    /// </summary>
    /// <param name="input">
    /// the array containing the data to be encrypted.
    /// </param>
    /// <param name="inOff">
    /// offset into the in array the data starts at.
    /// </param>
    /// <param name="outBytes">
    /// the array the encrypted data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the out array the output will start at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function EncryptBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;

    /// <summary>
    /// Do the appropriate chaining step for CBC mode decryption.
    /// </summary>
    /// <param name="input">
    /// the array containing the data to be decrypted.
    /// </param>
    /// <param name="inOff">
    /// offset into the in array the data starts at.
    /// </param>
    /// <param name="outBytes">
    /// the array the decrypted data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the out array the output will start at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function DecryptBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;

  public

    /// <summary>
    /// Basic constructor.
    /// </summary>
    /// <param name="cipher">
    /// the block cipher to be used as the basis of the feedback mode.
    /// </param>
    /// <param name="bitBlockSize">
    /// the block size in bits (note: a multiple of 8)
    /// </param>
    constructor Create(const cipher: IBlockCipher; bitBlockSize: Int32);

    /// <summary>
    /// return the underlying block cipher that we are wrapping.
    /// </summary>
    /// <returns>
    /// return the underlying block cipher that we are wrapping.
    /// </returns>
    function GetUnderlyingCipher(): IBlockCipher;

    /// <summary>
    /// Initialise the cipher and, possibly, the initialisation vector (IV). <br />
    /// If an IV isn't passed as part of the parameter, the IV will be all
    /// zeros.
    /// An IV which is too short is handled in FIPS compliant fashion.
    /// </summary>
    /// <param name="forEncryption">
    /// forEncryption if true the cipher is initialised for encryption, if
    /// false for decryption.
    /// </param>
    /// <param name="parameters">
    /// the key and other data required by the cipher.
    /// </param>
    procedure Init(forEncryption: Boolean; const parameters: ICipherParameters);

    /// <summary>
    /// return the block size we are operating at.
    /// </summary>
    /// <returns>
    /// the block size we are operating at (in bytes).
    /// </returns>
    function GetBlockSize(): Int32; inline;

    /// <summary>
    /// Process one block of input from the input array and write it to the
    /// output array.
    /// </summary>
    /// <param name="input">
    /// the array containing the input data.
    /// </param>
    /// <param name="inOff">
    /// offset into the input array the data starts at.
    /// </param>
    /// <param name="output">
    /// the array the output data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the output array the data starts at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32;

    /// <summary>
    /// reset the chaining vector back to the IV and reset the underlying
    /// cipher.
    /// </summary>
    procedure Reset(); inline;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <value>
    /// return the name of the underlying algorithm followed by "/CFB"
    /// </value>
    property AlgorithmName: String read GetAlgorithmName;

    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

  end;

type

  /// <summary>
  /// implements a Output-FeedBack (OFB) mode on top of a simple cipher.
  /// </summary>
  TOfbBlockCipher = class sealed(TInterfacedObject, IOfbBlockCipher,
    IBlockCipher)

  strict private

  var
    FIV, FofbV, FofbOutV: TCryptoLibByteArray;
    FblockSize: Int32;
    Fcipher: IBlockCipher;
    Fencrypting: Boolean;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <returns>
    /// return the name of the underlying algorithm followed by "/OFB"
    /// </returns>
    function GetAlgorithmName: String; inline;

    function GetIsPartialBlockOkay: Boolean; inline;

  public

    /// <summary>
    /// Basic constructor.
    /// </summary>
    /// <param name="cipher">
    /// the block cipher to be used as the basis of the feedback mode.
    /// </param>
    /// <param name="blockSize">
    /// the block size in bits (note: a multiple of 8)
    /// </param>
    constructor Create(const cipher: IBlockCipher; blockSize: Int32);

    /// <summary>
    /// return the underlying block cipher that we are wrapping.
    /// </summary>
    /// <returns>
    /// return the underlying block cipher that we are wrapping.
    /// </returns>
    function GetUnderlyingCipher(): IBlockCipher;

    /// <summary>
    /// Initialise the cipher and, possibly, the initialisation vector (IV). <br />
    /// If an IV isn't passed as part of the parameter, the IV will be all
    /// zeros.
    /// An IV which is too short is handled in FIPS compliant fashion.
    /// </summary>
    /// <param name="forEncryption">
    /// forEncryption if true the cipher is initialised for encryption, if
    /// false for decryption.
    /// ignored by this OFB mode though
    /// </param>
    /// <param name="parameters">
    /// the key and other data required by the cipher.
    /// </param>
    procedure Init(forEncryption: Boolean; const parameters: ICipherParameters);

    /// <summary>
    /// return the block size we are operating at.
    /// </summary>
    /// <returns>
    /// the block size we are operating at (in bytes).
    /// </returns>
    function GetBlockSize(): Int32; inline;

    /// <summary>
    /// Process one block of input from the input array and write it to the
    /// output array.
    /// </summary>
    /// <param name="input">
    /// the array containing the input data.
    /// </param>
    /// <param name="inOff">
    /// offset into the input array the data starts at.
    /// </param>
    /// <param name="output">
    /// the array the output data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the output array the data starts at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32;

    /// <summary>
    /// reset the chaining vector back to the IV and reset the underlying
    /// cipher.
    /// </summary>
    procedure Reset(); inline;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <value>
    /// return the name of the underlying algorithm followed by "/OFB"
    /// </value>
    property AlgorithmName: String read GetAlgorithmName;

    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

  end;

type

  /// <summary>
  /// Implements the Segmented Integer Counter (SIC) mode on top of a simple block cipher.
  /// </summary>
  TSicBlockCipher = class sealed(TInterfacedObject, ISicBlockCipher,
    IBlockCipher)

  strict private

  var
    FIV, Fcounter, FcounterOut: TCryptoLibByteArray;
    FblockSize: Int32;
    Fcipher: IBlockCipher;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <returns>
    /// return the name of the underlying algorithm followed by "/SIC"
    /// </returns>
    function GetAlgorithmName: String; inline;

    function GetIsPartialBlockOkay: Boolean; inline;

  public

    /// <summary>
    /// Basic constructor.
    /// </summary>
    /// <param name="cipher">
    /// the block cipher to be used.
    /// </param>
    constructor Create(const cipher: IBlockCipher);

    /// <summary>
    /// return the underlying block cipher that we are wrapping.
    /// </summary>
    /// <returns>
    /// return the underlying block cipher that we are wrapping.
    /// </returns>
    function GetUnderlyingCipher(): IBlockCipher;

    /// <summary>
    /// Initialise the cipher and, possibly, the initialisation vector (IV). <br />
    /// If an IV isn't passed as part of the parameter, the IV will be all
    /// zeros.
    /// An IV which is required in this mode.
    /// </summary>
    /// <param name="forEncryption">
    /// forEncryption if true the cipher is initialised for encryption, if
    /// false for decryption.
    /// ignored by this CTR mode though
    /// </param>
    /// <param name="parameters">
    /// the key and other data required by the cipher.
    /// </param>
    procedure Init(forEncryption: Boolean; const parameters: ICipherParameters);

    /// <summary>
    /// return the block size we are operating at.
    /// </summary>
    /// <returns>
    /// the block size we are operating at (in bytes).
    /// </returns>
    function GetBlockSize(): Int32; inline;

    /// <summary>
    /// Process one block of input from the input array and write it to the
    /// output array.
    /// </summary>
    /// <param name="input">
    /// the array containing the input data.
    /// </param>
    /// <param name="inOff">
    /// offset into the input array the data starts at.
    /// </param>
    /// <param name="output">
    /// the array the output data will be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the output array the data starts at.
    /// </param>
    /// <returns>
    /// the number of bytes processed and produced.
    /// </returns>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in input, or space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the cipher isn't initialised.
    /// </exception>
    function ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32;

    /// <summary>
    /// reset the chaining vector back to the IV and reset the underlying
    /// cipher.
    /// </summary>
    procedure Reset(); inline;

    /// <summary>
    /// return the algorithm name and mode.
    /// </summary>
    /// <value>
    /// return the name of the underlying algorithm followed by "/SIC"
    /// </value>
    property AlgorithmName: String read GetAlgorithmName;

    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

  end;

type

  /// <summary>
  /// A Cipher Text Stealing (CTS) mode cipher. CTS allows block ciphers to
  /// be used to produce cipher text which is the same outLength as the plain
  /// text.
  /// </summary>
  TCtsBlockCipher = class sealed(TBufferedBlockCipher, ICtsBlockCipher)

  strict private
    FblockSize: Int32;

  public

    /// <summary>
    /// Create a buffered block cipher that uses Cipher Text Stealing
    /// </summary>
    /// <param name="cipher">
    /// the underlying block cipher this buffering object wraps.
    /// </param>
    constructor Create(const cipher: IBlockCipher);

    /// <summary>
    /// return the size of the output buffer required for an update plus a
    /// doFinal with an input of len bytes.
    /// </summary>
    /// <param name="inputLen">
    /// the outLength of the input.
    /// </param>
    /// <returns>
    /// the space required to accommodate a call to update and doFinal with
    /// length bytes of input.
    /// </returns>
    function GetOutputSize(inputLen: Int32): Int32; override;

    /// <summary>
    /// return the size of the output buffer required for an update an input
    /// of len bytes.
    /// </summary>
    /// <param name="inputLen">
    /// the outLength of the input.
    /// </param>
    /// <returns>
    /// the space required to accommodate a call to update with length bytes
    /// of input.
    /// </returns>
    function GetUpdateOutputSize(inputLen: Int32): Int32; override;

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
    /// <param name="Length">
    /// the number of bytes to be copied out of the input array.
    /// </param>
    /// <param name="output">
    /// the space for any output that might be produced.
    /// </param>
    /// <param name="outOff">
    /// the offset from which the output will be copied.
    /// </param>
    /// <returns>
    /// the number of output bytes copied to out.
    /// </returns>
    function ProcessBytes(const input: TCryptoLibByteArray; inOff, len: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32; override;

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
    /// the number of output bytes copied to out.
    /// </returns>
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; override;

  end;

implementation

{ TCbcBlockCipher }

constructor TCbcBlockCipher.Create(const cipher: IBlockCipher);
begin
  inherited Create();
  Fcipher := cipher;
  FblockSize := cipher.GetBlockSize();

  System.SetLength(FIV, FblockSize);
  System.SetLength(FcbcV, FblockSize);
  System.SetLength(FcbcNextV, FblockSize);
end;

function TCbcBlockCipher.DecryptBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;
var
  Length, I: Int32;
  tmp: TCryptoLibByteArray;
begin
  if ((inOff + FblockSize) > System.Length(input)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputBufferTooShort);
  end;

  System.Move(input[inOff], FcbcNextV[0], FblockSize * System.SizeOf(Byte));

  Length := Fcipher.ProcessBlock(input, inOff, outBytes, outOff);


  // XOR the FcbcV and the output

  for I := 0 to System.Pred(FblockSize) do
  begin
    outBytes[outOff + I] := outBytes[outOff + I] xor FcbcV[I];
  end;


  // swap the back up buffer into next position

  tmp := FcbcV;
  FcbcV := FcbcNextV;
  FcbcNextV := tmp;

  result := &length;
end;

function TCbcBlockCipher.EncryptBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;
var
  I, &length: Int32;
begin
  if ((inOff + FblockSize) > System.Length(input)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputBufferTooShort);
  end;

  // XOR the FcbcV and the input, then encrypt the FcbcV

  for I := 0 to System.Pred(FblockSize) do
  begin
    FcbcV[I] := FcbcV[I] xor input[inOff + I];
  end;

  &length := Fcipher.ProcessBlock(FcbcV, 0, outBytes, outOff);


  // copy ciphertext to FcbcV

  System.Move(outBytes[outOff], FcbcV[0], System.Length(FcbcV) *
    System.SizeOf(Byte));

  result := &length;
end;

procedure TCbcBlockCipher.Reset;
begin
  System.Move(FIV[0], FcbcV[0], System.Length(FIV));
  TArrayUtils.Fill(FcbcNextV, 0, System.Length(FcbcNextV), Byte(0));

  Fcipher.Reset();
end;

function TCbcBlockCipher.GetAlgorithmName: String;
begin
  result := Fcipher.AlgorithmName + '/CBC';
end;

function TCbcBlockCipher.GetBlockSize: Int32;
begin
  result := Fcipher.GetBlockSize();
end;

function TCbcBlockCipher.GetIsPartialBlockOkay: Boolean;
begin
  result := false;
end;

function TCbcBlockCipher.GetUnderlyingCipher: IBlockCipher;
begin
  result := Fcipher;
end;

procedure TCbcBlockCipher.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  oldEncrypting: Boolean;
  ivParam: IParametersWithIV;
  iv: TCryptoLibByteArray;
  Lparameters: ICipherParameters;
begin
  oldEncrypting := Fencrypting;
  Fencrypting := forEncryption;
  Lparameters := parameters;

  if Supports(Lparameters, IParametersWithIV, ivParam) then
  begin
    iv := ivParam.GetIV();

    if (System.Length(iv) <> FblockSize) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidIVLength);
    end;

    System.Move(iv[0], FIV[0], System.Length(iv) * System.SizeOf(Byte));

    Lparameters := ivParam.parameters;
  end;

  Reset();

  // if Nil it's an IV changed only.
  if (Lparameters <> Nil) then
  begin
    Fcipher.Init(Fencrypting, Lparameters);
  end
  else if (oldEncrypting <> Fencrypting) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidChangeState);
  end;

end;

function TCbcBlockCipher.ProcessBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  if Fencrypting then
  begin
    result := EncryptBlock(input, inOff, output, outOff);
  end
  else
  begin
    result := DecryptBlock(input, inOff, output, outOff);
  end;
end;

{ TCfbBlockCipher }

constructor TCfbBlockCipher.Create(const cipher: IBlockCipher;
  bitBlockSize: Int32);
begin
  inherited Create();
  Fcipher := cipher;
  FblockSize := bitBlockSize div 8;

  System.SetLength(FIV, Fcipher.GetBlockSize);
  System.SetLength(FcfbV, Fcipher.GetBlockSize);
  System.SetLength(FcfbOutV, Fcipher.GetBlockSize);
end;

function TCfbBlockCipher.DecryptBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;
var
  I, count: Int32;
begin
  if ((inOff + FblockSize) > System.Length(input)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputBufferTooShort);
  end;

  if ((outOff + FblockSize) > System.Length(outBytes)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooShort);
  end;

  Fcipher.ProcessBlock(FcfbV, 0, FcfbOutV, 0);

  //
  // change over the input block.
  //
  count := (System.Length(FcfbV) - FblockSize) * System.SizeOf(Byte);
  if count > 0 then
  begin
    System.Move(FcfbV[FblockSize], FcfbV[0], count);
  end;

  System.Move(input[inOff], FcfbV[(System.Length(FcfbV) - FblockSize)],
    FblockSize * System.SizeOf(Byte));

  // XOR the FcfbV with the ciphertext producing the plaintext

  for I := 0 to System.Pred(FblockSize) do
  begin
    outBytes[outOff + I] := Byte(FcfbOutV[I] xor input[inOff + I]);
  end;

  result := FblockSize;
end;

function TCfbBlockCipher.EncryptBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const outBytes: TCryptoLibByteArray; outOff: Int32): Int32;
var
  I, count: Int32;
begin
  if ((inOff + FblockSize) > System.Length(input)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputBufferTooShort);
  end;

  if ((outOff + FblockSize) > System.Length(outBytes)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooShort);
  end;

  Fcipher.ProcessBlock(FcfbV, 0, FcfbOutV, 0);

  // XOR the FcfbV with the plaintext producing the ciphertext

  for I := 0 to System.Pred(FblockSize) do
  begin
    outBytes[outOff + I] := Byte(FcfbOutV[I] xor input[inOff + I]);
  end;

  //
  // change over the input block.
  //
  count := (System.Length(FcfbV) - FblockSize) * System.SizeOf(Byte);

  if count > 0 then
  begin
    System.Move(FcfbV[FblockSize], FcfbV[0], count);
  end;

  System.Move(outBytes[outOff], FcfbV[(System.Length(FcfbV) - FblockSize)],
    FblockSize * System.SizeOf(Byte));

  result := FblockSize;
end;

procedure TCfbBlockCipher.Reset;
begin
  System.Move(FIV[0], FcfbV[0], System.Length(FIV));

  Fcipher.Reset();
end;

function TCfbBlockCipher.GetAlgorithmName: String;
begin
  result := Fcipher.AlgorithmName + '/CFB' + IntToStr(FblockSize * 8);
end;

function TCfbBlockCipher.GetBlockSize: Int32;
begin
  result := FblockSize;
end;

function TCfbBlockCipher.GetIsPartialBlockOkay: Boolean;
begin
  result := true;
end;

function TCfbBlockCipher.GetUnderlyingCipher: IBlockCipher;
begin
  result := Fcipher;
end;

procedure TCfbBlockCipher.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  ivParam: IParametersWithIV;
  iv: TCryptoLibByteArray;
  Lparameters: ICipherParameters;
  diff: Int32;
begin
  Fencrypting := forEncryption;
  Lparameters := parameters;

  if Supports(Lparameters, IParametersWithIV, ivParam) then
  begin
    iv := ivParam.GetIV();

    diff := System.Length(FIV) - System.Length(iv);

    System.Move(iv[0], FIV[diff], System.Length(iv) * System.SizeOf(Byte));
    TArrayUtils.Fill(FIV, 0, diff, Byte(0));

    Lparameters := ivParam.parameters;
  end;

  Reset();

  // if it's Nil, key is to be reused.
  if (Lparameters <> Nil) then
  begin
    Fcipher.Init(true, Lparameters);
  end;

end;

function TCfbBlockCipher.ProcessBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  if Fencrypting then
  begin
    result := EncryptBlock(input, inOff, output, outOff);
  end
  else
  begin
    result := DecryptBlock(input, inOff, output, outOff);
  end;
end;

{ TOfbBlockCipher }

constructor TOfbBlockCipher.Create(const cipher: IBlockCipher;
  blockSize: Int32);
begin
  inherited Create();
  Fcipher := cipher;
  FblockSize := blockSize div 8;

  System.SetLength(FIV, Fcipher.GetBlockSize);
  System.SetLength(FofbV, Fcipher.GetBlockSize);
  System.SetLength(FofbOutV, Fcipher.GetBlockSize);
end;

procedure TOfbBlockCipher.Reset;
begin
  System.Move(FIV[0], FofbV[0], System.Length(FIV));

  Fcipher.Reset();

end;

function TOfbBlockCipher.GetAlgorithmName: String;
begin
  result := Fcipher.AlgorithmName + '/OFB' + IntToStr(FblockSize * 8);
end;

function TOfbBlockCipher.GetBlockSize: Int32;
begin
  result := FblockSize;
end;

function TOfbBlockCipher.GetIsPartialBlockOkay: Boolean;
begin
  result := true;
end;

function TOfbBlockCipher.GetUnderlyingCipher: IBlockCipher;
begin
  result := Fcipher;
end;

procedure TOfbBlockCipher.Init(forEncryption: Boolean;
  // forEncryption ignored by this OFB mode
  const parameters: ICipherParameters);
var
  ivParam: IParametersWithIV;
  iv: TCryptoLibByteArray;
  Lparameters: ICipherParameters;
  I: Int32;
begin
  Fencrypting := forEncryption;
  Lparameters := parameters;

  if Supports(Lparameters, IParametersWithIV, ivParam) then
  begin
    iv := ivParam.GetIV();

    if (System.Length(iv) < System.Length(FIV)) then
    begin
      // prepend the supplied IV with zeros (per FIPS PUB 81)
      System.Move(iv[0], FIV[System.Length(FIV) - System.Length(iv)],
        System.Length(iv) * System.SizeOf(Byte));

      for I := 0 to System.Pred(System.Length(FIV) - System.Length(iv)) do
      begin
        FIV[I] := 0;
      end;

    end
    else
    begin
      System.Move(iv[0], FIV[0], System.Length(FIV) * System.SizeOf(Byte));
    end;

    Lparameters := ivParam.parameters;
  end;

  Reset();

  // if it's Nil, key is to be reused.
  if (Lparameters <> Nil) then
  begin
    Fcipher.Init(true, Lparameters);
  end;

end;

function TOfbBlockCipher.ProcessBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
var
  I, count: Int32;
begin
  if ((inOff + FblockSize) > System.Length(input)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputBufferTooShort);
  end;

  if ((outOff + FblockSize) > System.Length(output)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooShort);
  end;

  Fcipher.ProcessBlock(FofbV, 0, FofbOutV, 0);

  //
  // XOR the ofbV with the plaintext producing the cipher text (and
  // the next input block).
  //

  for I := 0 to System.Pred(FblockSize) do
  begin
    output[outOff + I] := Byte(FofbOutV[I] xor input[inOff + I]);
  end;

  //
  // change over the input block.
  //
  count := (System.Length(FofbV) - FblockSize) * System.SizeOf(Byte);

  if count > 0 then
  begin
    System.Move(FofbV[FblockSize], FofbV[0], count);
  end;

  System.Move(FofbOutV[0], FofbV[(System.Length(FofbV) - FblockSize)],
    FblockSize * System.SizeOf(Byte));

  result := FblockSize;
end;

{ TSicBlockCipher }

constructor TSicBlockCipher.Create(const cipher: IBlockCipher);
begin
  inherited Create();
  Fcipher := cipher;
  FblockSize := Fcipher.GetBlockSize;

  System.SetLength(Fcounter, FblockSize);
  System.SetLength(FcounterOut, FblockSize);
  System.SetLength(FIV, FblockSize);
end;

procedure TSicBlockCipher.Reset;
begin
  TArrayUtils.Fill(Fcounter, 0, System.Length(Fcounter), Byte(0));
  System.Move(FIV[0], Fcounter[0], System.Length(FIV) * System.SizeOf(Byte));

  Fcipher.Reset();

end;

function TSicBlockCipher.GetAlgorithmName: String;
begin
  result := Fcipher.AlgorithmName + '/SIC';
end;

function TSicBlockCipher.GetBlockSize: Int32;
begin
  result := Fcipher.GetBlockSize();
end;

function TSicBlockCipher.GetIsPartialBlockOkay: Boolean;
begin
  result := true;
end;

function TSicBlockCipher.GetUnderlyingCipher: IBlockCipher;
begin
  result := Fcipher;
end;

{$IFNDEF _FIXINSIGHT_}

procedure TSicBlockCipher.Init(forEncryption: Boolean;
  // forEncryption ignored by this CTR mode
  const parameters: ICipherParameters);
var
  ivParam: IParametersWithIV;
  Lparameters: ICipherParameters;
  maxCounterSize: Int32;
begin
  Lparameters := parameters;

  if Supports(Lparameters, IParametersWithIV, ivParam) then
  begin
    FIV := ivParam.GetIV();

    if (FblockSize < System.Length(FIV)) then
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SInvalidTooLargeIVLength,
        [FblockSize]);
    end;

    maxCounterSize := Min(8, FblockSize div 2);

    if ((FblockSize - System.Length(FIV)) > maxCounterSize) then
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SInvalidTooSmallIVLength,
        [FblockSize - maxCounterSize]);
    end;

    Lparameters := ivParam.parameters;
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidParameterArgument);
  end;

  // if it's Nil, key is to be reused.
  if (Lparameters <> Nil) then
  begin
    Fcipher.Init(true, Lparameters);
  end;

  Reset();

end;
{$ENDIF}

function TSicBlockCipher.ProcessBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
var
  I, J: Int32;
begin

  if ((inOff + FblockSize) > System.Length(input)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SInputBufferTooShort);
  end;

  if ((outOff + FblockSize) > System.Length(output)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooShort);
  end;

  Fcipher.ProcessBlock(Fcounter, 0, FcounterOut, 0);

  //
  // XOR the counterOut with the plaintext producing the cipher text
  //
  for I := 0 to System.Pred(System.Length(FcounterOut)) do
  begin

    output[outOff + I] := Byte(FcounterOut[I] xor input[inOff + I]);
  end;

  // Increment the counter
  J := System.Length(Fcounter);
  System.Dec(J);
  System.Inc(Fcounter[J]);
  while ((J >= 0) and (Fcounter[J] = 0)) do
  begin
    System.Dec(J);
    System.Inc(Fcounter[J]);
  end;

  result := System.Length(Fcounter);
end;

{ TCtsBlockCipher }

constructor TCtsBlockCipher.Create(const cipher: IBlockCipher);
begin
  Inherited Create();
  if Supports(cipher, ICfbBlockCipher) or Supports(cipher, IOfbBlockCipher) or
    Supports(cipher, ISicBlockCipher) or Supports(cipher, ICtsBlockCipher) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SUnsupportedCipher);
  end;

  Fcipher := cipher;

  FblockSize := Fcipher.GetBlockSize();
  System.SetLength(Fbuf, FblockSize * 2);
  FbufOff := 0;
end;

function TCtsBlockCipher.DoFinal(const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
var
  blockSize, len, I: Int32;
  block, lastBlock: TCryptoLibByteArray;
  c: IBlockCipher;
begin
  if ((FbufOff + outOff) > System.Length(output)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes
      (@SOutputBufferTooSmallForDoFinal);
  end;

  blockSize := Fcipher.GetBlockSize();
  len := FbufOff - blockSize;
  System.SetLength(block, blockSize);

  if (FforEncryption) then
  begin
    if (FbufOff < blockSize) then
    begin
      raise EDataLengthCryptoLibException.CreateRes(@SCTSDoFinalError);
    end;

    Fcipher.ProcessBlock(Fbuf, 0, block, 0);

    if (FbufOff > blockSize) then
    begin

      I := FbufOff;
      while I <> System.Length(Fbuf) do
      begin
        Fbuf[I] := block[I - blockSize];
        System.Inc(I);
      end;

      I := blockSize;
      while I <> FbufOff do
      begin
        Fbuf[I] := Fbuf[I] xor (block[I - blockSize]);
        System.Inc(I);
      end;

      if Supports(Fcipher, ICbcBlockCipher) then
      begin
        c := (Fcipher as ICbcBlockCipher).GetUnderlyingCipher();

        c.ProcessBlock(Fbuf, blockSize, output, outOff);
      end
      else
      begin
        Fcipher.ProcessBlock(Fbuf, blockSize, output, outOff);
      end;

      System.Move(block[0], output[outOff + blockSize],
        len * System.SizeOf(Byte));
    end
    else
    begin
      System.Move(block[0], output[outOff], blockSize * System.SizeOf(Byte));
    end;
  end
  else
  begin

    if (FbufOff < blockSize) then
    begin
      raise EDataLengthCryptoLibException.CreateRes(@SCTSDoFinalError);
    end;

    System.SetLength(lastBlock, blockSize);

    if (FbufOff > blockSize) then
    begin

      if Supports(Fcipher, ICbcBlockCipher) then
      begin
        c := (Fcipher as ICbcBlockCipher).GetUnderlyingCipher();

        c.ProcessBlock(Fbuf, 0, block, 0);
      end
      else
      begin
        Fcipher.ProcessBlock(Fbuf, 0, block, 0);
      end;

      I := blockSize;
      while I <> FbufOff do
      begin
        lastBlock[I - blockSize] := Byte(block[I - blockSize] xor Fbuf[I]);
        System.Inc(I);
      end;

      System.Move(Fbuf[blockSize], block[0], len * System.SizeOf(Byte));
      Fcipher.ProcessBlock(block, 0, output, outOff);
      System.Move(lastBlock[0], output[outOff + blockSize],
        len * System.SizeOf(Byte));
    end
    else
    begin
      Fcipher.ProcessBlock(Fbuf, 0, block, 0);
      System.Move(block[0], output[outOff], blockSize * System.SizeOf(Byte));
    end;

  end;

  result := FbufOff;

  Reset();
end;

function TCtsBlockCipher.GetOutputSize(inputLen: Int32): Int32;
begin
  result := inputLen + FbufOff;
end;

function TCtsBlockCipher.GetUpdateOutputSize(inputLen: Int32): Int32;
var
  total, leftOver: Int32;
begin
  total := inputLen + FbufOff;
  leftOver := total mod System.Length(Fbuf);

  if (leftOver = 0) then
  begin
    result := total - System.Length(Fbuf);
    Exit;
  end;
  result := total - leftOver;
end;

function TCtsBlockCipher.ProcessByte(input: Byte;
  const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  result := 0;

  if (FbufOff = System.Length(Fbuf)) then
  begin
    result := Fcipher.ProcessBlock(Fbuf, 0, output, outOff);
    System.Move(Fbuf[FblockSize], Fbuf[0], FblockSize * System.SizeOf(Byte));
    FbufOff := FblockSize;
  end;

  Fbuf[FbufOff] := input;
  System.Inc(FbufOff);
end;

function TCtsBlockCipher.ProcessBytes(const input: TCryptoLibByteArray;
  inOff, len: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
var
  blockSize, Length, gapLen: Int32;
begin
  if (len < 0) then
  begin
    raise EInvalidArgument.CreateRes(@SNegativeInputLength);
  end;

  blockSize := GetBlockSize();
  Length := GetUpdateOutputSize(len);

  if (Length > 0) then
  begin
    if ((outOff + Length) > System.Length(output)) then
    begin
      raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooShort);
    end;
  end;

  result := 0;
  gapLen := System.Length(Fbuf) - FbufOff;

  if (len > gapLen) then
  begin
    System.Move(input[inOff], Fbuf[FbufOff], gapLen * System.SizeOf(Byte));

    result := result + Fcipher.ProcessBlock(Fbuf, 0, output, outOff);
    System.Move(Fbuf[blockSize], Fbuf[0], blockSize * System.SizeOf(Byte));

    FbufOff := blockSize;

    len := len - gapLen;
    inOff := inOff + gapLen;

    while (len > blockSize) do
    begin
      System.Move(input[inOff], Fbuf[FbufOff], blockSize * System.SizeOf(Byte));
      result := result + Fcipher.ProcessBlock(Fbuf, 0, output, outOff + result);
      System.Move(Fbuf[blockSize], Fbuf[0], blockSize * System.SizeOf(Byte));

      len := len - blockSize;
      inOff := inOff + blockSize;
    end;
  end;

  System.Move(input[inOff], Fbuf[FbufOff], len * System.SizeOf(Byte));

  FbufOff := FbufOff + len;

end;

end.
