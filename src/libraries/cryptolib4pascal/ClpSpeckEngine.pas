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

unit ClpSpeckEngine;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpCheck,
  ClpISpeckEngine,
  ClpIBlockCipher,
  ClpICipherParameters,
  ClpIKeyParameter,
  ClpCryptoLibTypes;

resourcestring
  SSpeckEngineNotInitialised = '%s Engine not Initialised';
  SInputBuffertooShort = 'Input Buffer too Short';
  SOutputBuffertooShort = 'Output Buffer too Short';
  SInvalidArgumentEncountered = 'Invalid Argument Encountered.';
  SInvalidParameterSpeckInit = 'Invalid Parameter Passed to Speck Init - "%s"';
  SSpeck32InvalidKeySize =
    'Speck32 requires a key of 64 bits but input was "%d" bits.';
  SSpeck48InvalidKeySize =
    'Speck48 requires a key of 72 or 96 bits but input was "%d" bits.';
  SSpeck64InvalidKeySize =
    'Speck64 requires a key of 96 or 128 bits but input was "%d" bits.';
  SSpeck96InvalidKeySize =
    'Speck96 requires a key of 96 or 144 bits but input was "%d" bits.';
  SSpeck128InvalidKeySize =
    'Speck128 requires a key of 128, 192 or 256 bits but input was "%d" bits.';

type

  /// <summary>
  /// <para>
  /// The Speck family of block ciphers, described in <i>The Simon and
  /// Speck Families of Lightweight Block Ciphers</i> by <i>Ray Beaulieu,
  /// Douglas Shors, Jason Smith, Stefan Treatman-Clark, Bryan Weeks,
  /// Louis Wingers</i> All block size and key size variants are
  /// supported, with the key size determined from the key during <see cref="ClpSpeckEngine|TSpeckEngine.Init(Boolean,ICipherParameters)" />
  /// .
  /// </para>
  /// </summary>
  TSpeckEngine = class abstract(TInterfacedObject, ISpeckEngine, IBlockCipher)

  strict private
  var

    Finitialised, FforEncryption: Boolean;

    function GetIsPartialBlockOkay: Boolean; virtual;

    /// <summary>
    /// Internal method to Initialise this cipher instance.
    /// <code>true</code> for encryption, <code>false</code> for decryption.
    /// the bytes of the key to use.
    /// </summary>
    procedure EngineInit(forEncryption: Boolean;
      const keyBytes: TCryptoLibByteArray); virtual;

  strict protected

  var
    FblockSize, FwordSize, FwordSizeBits, Falpha, Fbeta, FbaseRounds,
      Frounds: Int32;

    /// <summary>
    /// Gets the algorithm name of this Speck engine.
    /// </summary>
    /// <value>
    /// the name of the Speck variant, specified to the level of the block size (e.g.
    /// <em>Speck96</em>).
    /// </value>
    function GetAlgorithmName: String; virtual;
    function GetBlockSize(): Int32; virtual;

    /// <summary>
    /// Checks whether a key size provided to the <see cref="ClpSpeckEngine|TSpeckEngine.EngineInit(Boolean,TCryptoLibByteArray)" />
    /// method is valid.
    /// </summary>
    procedure CheckKeySize(keySizeBytes: Int32); virtual; abstract;

    /// <summary>
    /// Sets a key for this cipher instance, calculating the key schedule.
    /// </summary>
    procedure SetKey(const keyBytes: TCryptoLibByteArray); virtual; abstract;

    /// <summary>
    /// Unpack a block of data into working state prior to an
    /// encrypt/decrypt operation.
    /// </summary>
    /// <param name="bytes">
    /// the input data.
    /// </param>
    /// <param name="off">
    /// the offset to begin reading the input data at.
    /// </param>
    procedure UnPackBlock(const bytes: TCryptoLibByteArray; off: Int32);
      virtual; abstract;

    /// <summary>
    /// Packs the 2 word working state following an encrypt/decrypt into a
    /// byte sequence.
    /// </summary>
    /// <param name="bytes">
    /// the output buffer.
    /// </param>
    /// <param name="off">
    /// the offset to begin writing the output data at.
    /// </param>
    procedure PackBlock(const bytes: TCryptoLibByteArray; off: Int32);
      virtual; abstract;

    /// <summary>
    /// Encrypts the plaintext words loaded with a previous call to <see cref="ClpSpeckEngine|TSpeckEngine.UnPackBlock(TCryptoLibByteArray,Int32)" />
    /// leaving the resulting ciphertext words in the working state.
    /// </summary>
    procedure EncryptBlock(); virtual; abstract;

    /// <summary>
    /// Decrypts the ciphertext words loaded with a previous call to <see cref="ClpSpeckEngine|TSpeckEngine.UnPackBlock(TCryptoLibByteArray,Int32)" />
    /// leaving the resulting ciphertext words in the working state.
    /// </summary>

    procedure DecryptBlock(); virtual; abstract;

    /// <summary>
    /// Constructs a Speck engine.
    /// </summary>
    /// <param name="wordSize">
    /// the size of the word to use, in bytes.
    /// </param>
    /// <param name="baseRounds">
    /// the base number of rounds (for a 2 word key variant) for the
    /// specified word/block size.
    /// </param>
    /// <param name="alpha">
    /// the alpha rotation constant to use.
    /// </param>
    /// <param name="beta">
    /// the beta rotation constant to use.
    /// </param>
    constructor Create(wordSize, baseRounds, alpha, beta: Int32);

    /// <summary>
    /// initialise a Speck cipher.
    /// </summary>
    /// <param name="forEncryption">
    /// whether or not we are for encryption.
    /// </param>
    /// <param name="parameters">
    /// the parameters required to set up the cipher.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the parameters argument is inappropriate.
    /// </exception>
    procedure Init(forEncryption: Boolean;
      const parameters: ICipherParameters); virtual;

    function ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32; virtual;

    procedure Reset(); virtual;

    property AlgorithmName: String read GetAlgorithmName;
    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

  end;

type

  /// <summary>
  /// Base class of Speck variants that fit in 32 bit Pascal Integers:
  /// Speck32, Speck48, Speck64.
  /// Speck32 and Speck48 (16 and 24 bit word sizes) are implemented using masking.
  /// </summary>
  TSpeckUInt32Engine = class abstract(TSpeckEngine)

  strict private
  var

    /// <summary>
    /// The expanded key schedule for all <see cref="ClpSpeckEngine|TSpeckEngine.Frounds" />
    /// </summary>
    Fk: TCryptoLibUInt32Array;

    /// <summary>
    /// The 2 words of the working state;
    /// </summary>
    Fx, Fy: UInt32;

    /// <summary>
    /// Rotates a word left by the specified distance. <br />The rotation is
    /// on the word size of the cipher instance, not on the full 32 bits of
    /// the UInt32.
    /// </summary>
    /// <param name="i">
    /// the word to rotate.
    /// </param>
    /// <param name="distance">
    /// the distance in bits to rotate.
    /// </param>
    /// <returns>
    /// the rotated word, which may have unmasked high (&gt; word size) bits.
    /// </returns>
    function Rotl(i: UInt32; distance: Int32): UInt32; inline;

    /// <summary>
    /// Rotates a word right by the specified distance. <br />The rotation is
    /// on the word size of the cipher instance, not on the full 32 bits of
    /// the UInt32.
    /// </summary>
    /// <param name="i">
    /// the word to rotate.
    /// </param>
    /// <param name="distance">
    /// the distance in bits to rotate.
    /// </param>
    /// <returns>
    /// the rotated word, which may have unmasked high (&gt; word size) bits.
    /// </returns>
    function Rotr(i: UInt32; distance: Int32): UInt32; inline;

    /// <summary>
    /// Read <see cref="ClpSpeckEngine|TSpeckEngine.FwordSize" /> bytes from
    /// the input data in <b>little-endian</b> order.
    /// </summary>
    /// <param name="bytes">
    /// the data to read a word from.
    /// </param>
    /// <param name="off">
    /// the offset to read the word from.
    /// </param>
    /// <returns>
    /// the read word, with zeroes in any bits higher than the word size.
    /// </returns>
    function BytesToWord(const bytes: TCryptoLibByteArray; off: Int32)
      : UInt32; inline;

    /// <summary>
    /// Writes <see cref="ClpSpeckEngine|TSpeckEngine.FwordSize" /> bytes
    /// into a buffer in <b>little-endian</b> order.
    /// </summary>
    /// <param name="word">
    /// the word to write.
    /// </param>
    /// <param name="bytes">
    /// the buffer to write the word bytes to.
    /// </param>
    /// <param name="off">
    /// the offset to write the data at.
    /// </param>
    procedure WordToBytes(word: UInt32; const bytes: TCryptoLibByteArray;
      off: Int32); inline;

  strict protected

    /// <summary>
    /// Masks all bits higher than the word size of this cipher in the
    /// supplied value.
    /// </summary>
    /// <param name="val">
    /// the value to mask.
    /// </param>
    /// <returns>
    /// the masked value.
    /// </returns>
    function Mask(val: UInt32): UInt32; virtual; abstract;

    procedure SetKey(const keyBytes: TCryptoLibByteArray); override;

    procedure UnPackBlock(const bytes: TCryptoLibByteArray;
      off: Int32); override;

    procedure PackBlock(const bytes: TCryptoLibByteArray; off: Int32); override;

    procedure EncryptBlock(); override;

    procedure DecryptBlock(); override;

    /// <summary>
    /// Constructs a Speck cipher with &lt;= 32 bit word size, using the
    /// standard 8,3 rotation constants.
    /// </summary>
    /// <param name="wordSize">
    /// the word size in bytes.
    /// </param>
    /// <param name="baseRounds">
    /// the base (for 2 word key) round count.
    /// </param>
    constructor Create(wordSize, baseRounds: Int32); overload;

    /// <summary>
    /// Constructs a Speck cipher with &lt;= 32 bit word size, using custom
    /// rotation constants.
    /// </summary>
    /// <param name="wordSize">
    /// the word size in bytes.
    /// </param>
    /// <param name="baseRounds">
    /// the base (for 2 word key) round count.
    /// </param>
    /// <param name="alpha">
    /// the <em>alpha</em> rotation constant.
    /// </param>
    /// <param name="beta">
    /// the <em>beta</em> rotation constant.
    /// </param>
    constructor Create(wordSize, baseRounds, alpha, beta: Int32); overload;

  end;

type

  /// <summary>
  /// Base class of Speck variants that fit in 64 bit Pascal Integers:
  /// Speck96, Speck128.
  /// Speck96 (48 bit word size) is implemented using masking.
  /// </summary>
  TSpeckUInt64Engine = class abstract(TSpeckEngine)

  strict private
  var

    /// <summary>
    /// The expanded key schedule for all <see cref="ClpSpeckEngine|TSpeckEngine.Frounds" />
    /// </summary>
    Fk: TCryptoLibUInt64Array;

    /// <summary>
    /// The 2 words of the working state;
    /// </summary>
    Fx, Fy: UInt64;

    /// <summary>
    /// Rotates a word left by the specified distance. <br />The rotation is
    /// on the word size of the cipher instance, not on the full 64 bits of
    /// the UInt64.
    /// </summary>
    /// <param name="i">
    /// the word to rotate.
    /// </param>
    /// <param name="distance">
    /// the distance in bits to rotate.
    /// </param>
    /// <returns>
    /// the rotated word, which may have unmasked high (&gt; word size) bits.
    /// </returns>
    function Rotl(i: UInt64; distance: Int32): UInt64; inline;

    /// <summary>
    /// Rotates a word right by the specified distance. <br />The rotation is
    /// on the word size of the cipher instance, not on the full 64 bits of
    /// the UInt64.
    /// </summary>
    /// <param name="i">
    /// the word to rotate.
    /// </param>
    /// <param name="distance">
    /// the distance in bits to rotate.
    /// </param>
    /// <returns>
    /// the rotated word, which may have unmasked high (&gt; word size) bits.
    /// </returns>
    function Rotr(i: UInt64; distance: Int32): UInt64; inline;

    /// <summary>
    /// Read <see cref="ClpSpeckEngine|TSpeckEngine.FwordSize" /> bytes from
    /// the input data in little-endian order.
    /// </summary>
    /// <param name="bytes">
    /// the data to read a word from.
    /// </param>
    /// <param name="off">
    /// the offset to read the word from.
    /// </param>
    /// <returns>
    /// the read word, with zeroes in any bits higher than the word size.
    /// </returns>
    function BytesToWord(const bytes: TCryptoLibByteArray; off: Int32)
      : UInt64; inline;

    /// <summary>
    /// Writes <see cref="ClpSpeckEngine|TSpeckEngine.FwordSize" /> bytes
    /// into a buffer in little-endian order.
    /// </summary>
    /// <param name="word">
    /// the word to write.
    /// </param>
    /// <param name="bytes">
    /// the buffer to write the word bytes to.
    /// </param>
    /// <param name="off">
    /// the offset to write the data at.
    /// </param>
    procedure WordToBytes(word: UInt64; const bytes: TCryptoLibByteArray;
      off: Int32); inline;

  strict protected

    /// <summary>
    /// Masks all bits higher than the word size of this cipher in the
    /// supplied value.
    /// </summary>
    /// <param name="val">
    /// the value to mask.
    /// </param>
    /// <returns>
    /// the masked value.
    /// </returns>
    function Mask(val: UInt64): UInt64; virtual; abstract;

    procedure SetKey(const keyBytes: TCryptoLibByteArray); override;

    procedure UnPackBlock(const bytes: TCryptoLibByteArray;
      off: Int32); override;

    procedure PackBlock(const bytes: TCryptoLibByteArray; off: Int32); override;

    procedure EncryptBlock(); override;

    procedure DecryptBlock(); override;

    /// <summary>
    /// Constructs a Speck cipher with &lt;= 64 bit word size, using the
    /// standard 8,3 rotation constants.
    /// </summary>
    /// <param name="wordSize">
    /// the word size in bytes.
    /// </param>
    /// <param name="baseRounds">
    /// the base (for 2 word key) round count.
    /// </param>
    constructor Create(wordSize, baseRounds: Int32); overload;

    /// <summary>
    /// Constructs a Speck cipher with &lt;= 64 bit word size, using custom
    /// rotation constants.
    /// </summary>
    /// <param name="wordSize">
    /// the word size in bytes.
    /// </param>
    /// <param name="baseRounds">
    /// the base (for 2 word key) round count.
    /// </param>
    /// <param name="alpha">
    /// the <em>alpha</em> rotation constant.
    /// </param>
    /// <param name="beta">
    /// the <em>beta</em> rotation constant.
    /// </param>
    constructor Create(wordSize, baseRounds, alpha, beta: Int32); overload;

  end;

type

  /// <summary>
  /// Speck32: 2 byte words, 7/2 rotation constants.
  /// <p>
  /// 20 base rounds (hypothetical)
  /// </p>
  /// 64 bit key/22 rounds.
  /// </summary>
  TSpeck32Engine = class sealed(TSpeckUInt32Engine)

  strict protected
    function Mask(val: UInt32): UInt32; override;
    procedure CheckKeySize(keySizeBytes: Int32); override;

  public
    constructor Create();

  end;

type

  /// <summary>
  /// Speck48: 3 byte words, 8/3 rotation constants.
  /// <p>
  /// 21 base rounds (hypothetical)
  /// </p>
  /// 72 bit key/22 rounds.
  /// 96 bit key/23 rounds.
  /// </summary>
  TSpeck48Engine = class sealed(TSpeckUInt32Engine)

  strict protected
    function Mask(val: UInt32): UInt32; override;
    procedure CheckKeySize(keySizeBytes: Int32); override;

  public
    constructor Create();

  end;

type

  /// <summary>
  /// Speck64: 4 byte words, 8/3 rotation constants.
  /// <p>
  /// 25 base rounds (hypothetical)
  /// </p>
  /// 96 bit key/26 rounds.
  /// 128 bit key/27 rounds.
  /// </summary>
  TSpeck64Engine = class sealed(TSpeckUInt32Engine)

  strict protected
    function Mask(val: UInt32): UInt32; override;
    procedure CheckKeySize(keySizeBytes: Int32); override;

  public
    constructor Create();

  end;

type

  /// <summary>
  /// Speck96: 6 byte words, 8/3 rotation constants.
  /// <p>
  /// 28 base rounds
  /// </p>
  /// 96 bit key/28 rounds.
  /// 144 bit key/29 rounds.
  /// </summary>
  TSpeck96Engine = class sealed(TSpeckUInt64Engine)

  strict protected
    function Mask(val: UInt64): UInt64; override;
    procedure CheckKeySize(keySizeBytes: Int32); override;

  public
    constructor Create();

  end;

type

  /// <summary>
  /// Speck128: 8 byte words, 8/3 rotation constants.
  /// <p>
  /// 32 base rounds
  /// </p>
  /// 128 bit key/32 rounds.
  /// 192 bit key/33 rounds.
  /// 256 bit key/34 rounds.
  /// </summary>
  TSpeck128Engine = class sealed(TSpeckUInt64Engine)

  strict protected
    function Mask(val: UInt64): UInt64; override;
    procedure CheckKeySize(keySizeBytes: Int32); override;

  public
    constructor Create();

  end;

implementation

{ TSpeckEngine }

constructor TSpeckEngine.Create(wordSize, baseRounds, alpha, beta: Int32);
begin
  Inherited Create();
  FwordSize := wordSize;
  FbaseRounds := baseRounds;
  Frounds := baseRounds;
  FblockSize := wordSize * 2;
  FwordSizeBits := wordSize * 8;
  Falpha := alpha;
  Fbeta := beta;
end;

function TSpeckEngine.GetBlockSize: Int32;
begin
  result := FblockSize;
end;

procedure TSpeckEngine.EngineInit(forEncryption: Boolean;
  const keyBytes: TCryptoLibByteArray);
begin
  FforEncryption := forEncryption;
  CheckKeySize(System.Length(keyBytes));
  SetKey(keyBytes);
  Finitialised := true;
end;

function TSpeckEngine.GetAlgorithmName: String;
begin
  result := Format('Speck%d', [FblockSize * 8]);
end;

function TSpeckEngine.GetIsPartialBlockOkay: Boolean;
begin
  result := false;
end;

procedure TSpeckEngine.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  keyParameter: IKeyParameter;
  keyBytes: TCryptoLibByteArray;
begin

  if not Supports(parameters, IKeyParameter, keyParameter) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidParameterSpeckInit,
      [(parameters as TObject).ToString]);
  end;
  keyBytes := keyParameter.GetKey;
  EngineInit(forEncryption, keyBytes);
end;

function TSpeckEngine.ProcessBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  if (not Finitialised) then
  begin
    raise EInvalidOperationCryptoLibException.CreateResFmt
      (@SSpeckEngineNotInitialised, [AlgorithmName]);
  end;

  TCheck.DataLength((inOff + FblockSize) > System.Length(input),
    SInputBuffertooShort);
  TCheck.DataLength((outOff + FblockSize) > System.Length(output),
    SOutputBuffertooShort);

  UnPackBlock(input, inOff);
  if (FforEncryption) then
  begin
    EncryptBlock();
  end
  else
  begin
    DecryptBlock();
  end;
  PackBlock(output, outOff);

  result := FblockSize;
end;

procedure TSpeckEngine.Reset;
begin
  // nothing to do.
end;

{ TSpeckUInt32Engine }

function TSpeckUInt32Engine.Rotl(i: UInt32; distance: Int32): UInt32;
begin
  result := ((i shl distance) or (i shr (FwordSizeBits - distance)));
end;

function TSpeckUInt32Engine.Rotr(i: UInt32; distance: Int32): UInt32;
begin
  result := ((i shr distance) or (i shl (FwordSizeBits - distance)));
end;

function TSpeckUInt32Engine.BytesToWord(const bytes: TCryptoLibByteArray;
  off: Int32): UInt32;
var
  index: Int32;
begin
  TCheck.DataLength((off + FwordSize) > System.Length(bytes),
    SInvalidArgumentEncountered);

  index := off + FwordSize - 1;
  result := (bytes[index]);
  System.Dec(index);
  result := (result shl 8) or (bytes[index]);
  System.Dec(index);
  if (FwordSize > 2) then
  begin
    result := (result shl 8) or (bytes[index]);
    System.Dec(index);
    if (FwordSize > 3) then
    begin
      result := (result shl 8) or (bytes[index]);
    end;
  end;

end;

procedure TSpeckUInt32Engine.WordToBytes(word: UInt32;
  const bytes: TCryptoLibByteArray; off: Int32);
begin
  TCheck.DataLength((off + FwordSize) > System.Length(bytes),
    SInvalidArgumentEncountered);

  bytes[off] := Byte(word);
  System.Inc(off);
  bytes[off] := Byte(word shr 8);
  System.Inc(off);
  if (FwordSize > 2) then
  begin
    bytes[off] := Byte(word shr 16);
    System.Inc(off);
    if (FwordSize > 3) then
    begin
      bytes[off] := Byte(word shr 24);
    end;
  end;

end;

constructor TSpeckUInt32Engine.Create(wordSize, baseRounds: Int32);
begin
  Create(wordSize, baseRounds, 8, 3);
end;

constructor TSpeckUInt32Engine.Create(wordSize, baseRounds, alpha, beta: Int32);
begin
  Inherited Create(wordSize, baseRounds, alpha, beta);
end;

procedure TSpeckUInt32Engine.EncryptBlock;
var
  x, y: UInt32;
  r: Int32;
begin
  x := Fx;
  y := Fy;

  for r := 0 to System.Pred(Frounds) do
  begin
    x := Mask((Rotr(x, Falpha) + y) xor Fk[r]);
    y := Mask(Rotl(y, Fbeta) xor x);
  end;

  Fx := x;
  Fy := y;
end;

procedure TSpeckUInt32Engine.DecryptBlock;
var
  x, y: UInt32;
  r: Int32;
begin
  x := Fx;
  y := Fy;

  for r := System.Pred(Frounds) downto 0 do
  begin
    y := Mask(Rotr(x xor y, Fbeta));
    x := Mask(Rotl(Mask((x xor Fk[r]) - y), Falpha));
  end;

  Fx := x;
  Fy := y;

end;

procedure TSpeckUInt32Engine.PackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
begin
  WordToBytes(Fx, bytes, off + FwordSize);
  WordToBytes(Fy, bytes, off);
end;

procedure TSpeckUInt32Engine.UnPackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
begin
  Fx := BytesToWord(bytes, off + FwordSize);
  Fy := BytesToWord(bytes, off);
end;

procedure TSpeckUInt32Engine.SetKey(const keyBytes: TCryptoLibByteArray);
var
  keyWords, i, lw: Int32;
  L: TCryptoLibUInt32Array;
begin
  // Determine number of key words m
  keyWords := System.Length(keyBytes) div FwordSize;

  // Number of rounds is increased by 1 for each key word > 2
  Frounds := FbaseRounds + (keyWords - 2);
  System.SetLength(Fk, Frounds);

  // Load k[0]
  Fk[0] := BytesToWord(keyBytes, 0);

  // Load l[m-2]...l[0], leave space for l[m-1] in key expansion
  System.SetLength(L, keyWords);

  for i := 0 to System.Pred(keyWords - 1) do
  begin
    L[(keyWords - 2) - i] := BytesToWord(keyBytes, ((keyWords - 1) - i) *
      FwordSize);
  end;
  // Key expansion using round function over l[m-2]...l[0],k[0] with round number as key
  for i := 0 to System.Pred(Frounds - 1) do
  begin
    lw := (i + keyWords - 1) mod keyWords;
    L[lw] := Mask((Rotr(L[i mod keyWords], Falpha) + Fk[i]) xor UInt32(i));
    Fk[i + 1] := Mask(Rotl(Fk[i], Fbeta) xor L[lw]);

  end;

end;

{ TSpeckUInt64Engine }

function TSpeckUInt64Engine.Rotl(i: UInt64; distance: Int32): UInt64;
begin
  result := ((i shl distance) or (i shr (FwordSizeBits - distance)));
end;

function TSpeckUInt64Engine.Rotr(i: UInt64; distance: Int32): UInt64;
begin
  result := ((i shr distance) or (i shl (FwordSizeBits - distance)));
end;

function TSpeckUInt64Engine.BytesToWord(const bytes: TCryptoLibByteArray;
  off: Int32): UInt64;
var
  index: Int32;
begin
  TCheck.DataLength((off + FwordSize) > System.Length(bytes),
    SInvalidArgumentEncountered);

  index := off + FwordSize - 1;
  result := (bytes[index]);
  System.Dec(index);
  result := (result shl 8) or (bytes[index]);
  System.Dec(index);
  result := (result shl 8) or (bytes[index]);
  System.Dec(index);
  result := (result shl 8) or (bytes[index]);
  System.Dec(index);
  result := (result shl 8) or (bytes[index]);
  System.Dec(index);
  result := (result shl 8) or (bytes[index]);
  System.Dec(index);
  if (FwordSize = 8) then
  begin
    result := (result shl 8) or (bytes[index]);
    System.Dec(index);
    result := (result shl 8) or (bytes[index]);
  end;
end;

procedure TSpeckUInt64Engine.WordToBytes(word: UInt64;
  const bytes: TCryptoLibByteArray; off: Int32);
begin
  TCheck.DataLength((off + FwordSize) > System.Length(bytes),
    SInvalidArgumentEncountered);

  bytes[off] := Byte(word);
  System.Inc(off);
  bytes[off] := Byte(word shr 8);
  System.Inc(off);
  bytes[off] := Byte(word shr 16);
  System.Inc(off);
  bytes[off] := Byte(word shr 24);
  System.Inc(off);
  bytes[off] := Byte(word shr 32);
  System.Inc(off);
  bytes[off] := Byte(word shr 40);
  System.Inc(off);
  if (FwordSize = 8) then
  begin
    bytes[off] := Byte(word shr 48);
    System.Inc(off);
    bytes[off] := Byte(word shr 56);
  end;

end;

constructor TSpeckUInt64Engine.Create(wordSize, baseRounds: Int32);
begin
  Create(wordSize, baseRounds, 8, 3);
end;

constructor TSpeckUInt64Engine.Create(wordSize, baseRounds, alpha, beta: Int32);
begin
  Inherited Create(wordSize, baseRounds, alpha, beta);
end;

procedure TSpeckUInt64Engine.EncryptBlock;
var
  x, y: UInt64;
  r: Int32;
begin
  x := Fx;
  y := Fy;

  for r := 0 to System.Pred(Frounds) do
  begin
    x := Mask((Rotr(x, Falpha) + y) xor Fk[r]);
    y := Mask(Rotl(y, Fbeta) xor x);
  end;

  Fx := x;
  Fy := y;
end;

procedure TSpeckUInt64Engine.DecryptBlock;
var
  x, y: UInt64;
  r: Int32;
begin
  x := Fx;
  y := Fy;

  for r := System.Pred(Frounds) downto 0 do
  begin
    y := Mask(Rotr(x xor y, Fbeta));
    x := Mask(Rotl(Mask((x xor Fk[r]) - y), Falpha));
  end;

  Fx := x;
  Fy := y;

end;

procedure TSpeckUInt64Engine.PackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
begin
  WordToBytes(Fx, bytes, off + FwordSize);
  WordToBytes(Fy, bytes, off);
end;

procedure TSpeckUInt64Engine.UnPackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
begin
  Fx := BytesToWord(bytes, off + FwordSize);
  Fy := BytesToWord(bytes, off);
end;

procedure TSpeckUInt64Engine.SetKey(const keyBytes: TCryptoLibByteArray);
var
  keyWords, i, lw: Int32;
  L: TCryptoLibUInt64Array;
begin
  // Determine number of key words m
  keyWords := System.Length(keyBytes) div FwordSize;

  // Number of rounds is increased by 1 for each key word > 2
  Frounds := FbaseRounds + (keyWords - 2);
  System.SetLength(Fk, Frounds);

  // Load k[0]
  Fk[0] := BytesToWord(keyBytes, 0);

  // Load l[m-2]...l[0], leave space for l[m-1] in key expansion
  System.SetLength(L, keyWords);

  for i := 0 to System.Pred(keyWords - 1) do
  begin
    L[(keyWords - 2) - i] := BytesToWord(keyBytes, ((keyWords - 1) - i) *
      FwordSize);
  end;
  // Key expansion using round function over l[m-2]...l[0],k[0] with round number as key
  for i := 0 to System.Pred(Frounds - 1) do
  begin
    lw := (i + keyWords - 1) mod keyWords;
    L[lw] := Mask((Rotr(L[i mod keyWords], Falpha) + Fk[i]) xor UInt64(i));
    Fk[i + 1] := Mask(Rotl(Fk[i], Fbeta) xor L[lw]);

  end;

end;

{ TSpeck32Engine }

constructor TSpeck32Engine.Create;
begin
  Inherited Create(2, 20, 7, 2);
end;

function TSpeck32Engine.Mask(val: UInt32): UInt32;
begin
  result := (val and $FFFF);
end;

procedure TSpeck32Engine.CheckKeySize(keySizeBytes: Int32);
begin
  if (keySizeBytes <> 8) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SSpeck32InvalidKeySize,
      [keySizeBytes * 8]);
  end;
end;

{ TSpeck48Engine }

constructor TSpeck48Engine.Create;
begin
  Inherited Create(3, 21);
end;

function TSpeck48Engine.Mask(val: UInt32): UInt32;
begin
  result := (val and $FFFFFF);
end;

procedure TSpeck48Engine.CheckKeySize(keySizeBytes: Int32);
begin
  if not(keySizeBytes in [9, 12]) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SSpeck48InvalidKeySize,
      [keySizeBytes * 8]);
  end;
end;

{ TSpeck64Engine }

constructor TSpeck64Engine.Create;
begin
  Inherited Create(4, 25);
end;

function TSpeck64Engine.Mask(val: UInt32): UInt32;
begin
  result := val;
end;

procedure TSpeck64Engine.CheckKeySize(keySizeBytes: Int32);
begin
  if not(keySizeBytes in [12, 16]) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SSpeck64InvalidKeySize,
      [keySizeBytes * 8]);
  end;
end;

{ TSpeck96Engine }

constructor TSpeck96Engine.Create;
begin
  Inherited Create(6, 28);
end;

function TSpeck96Engine.Mask(val: UInt64): UInt64;
begin
  result := (val and $0000FFFFFFFFFFFF);
end;

procedure TSpeck96Engine.CheckKeySize(keySizeBytes: Int32);
begin
  if not(keySizeBytes in [12, 18]) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SSpeck96InvalidKeySize,
      [keySizeBytes * 8]);
  end;
end;

{ TSpeck128Engine }

constructor TSpeck128Engine.Create;
begin
  Inherited Create(8, 32);
end;

function TSpeck128Engine.Mask(val: UInt64): UInt64;
begin
  result := val;
end;

procedure TSpeck128Engine.CheckKeySize(keySizeBytes: Int32);
begin
  if not(keySizeBytes in [16, 24, 32]) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SSpeck128InvalidKeySize,
      [keySizeBytes * 8]);
  end;
end;

end.
