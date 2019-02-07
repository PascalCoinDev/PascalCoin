unit SbpBase85;

{$I ..\Include\SimpleBaseLib.inc}

interface

uses
  SbpBits,
  SbpUtilities,
  SbpSimpleBaseLibTypes,
  SbpBase85Alphabet,
  SbpIBase85Alphabet,
  SbpIBase85;

resourcestring
  SAlphabetNil = 'Alphabet Instance cannot be Nil "%s"';
  SUnExpectedCharacter =
    'Unexpected Shortcut Character In The Middle Of a Regular Block';

type
  TBase85 = class sealed(TInterfacedObject, IBase85)

  strict private
  const
    baseLength = Int32(85);
    byteBlockSize = Int32(4);
    stringBlockSize = Int32(5);
    allSpace = Int64($20202020);

    class var

      FZ85, FAscii85: IBase85;

    class procedure WriteDecodedValue(var pOutput: PByte; value: Int64;
      numBytesToWrite: Int32); static; inline;

    class procedure WriteShortcut(var pOutput: PByte; blockIndex: Int32;
      value: Int64); static; inline;

    class function GetDecodeBufferLength(textLen: Int32;
      usingShortcuts: Boolean): Int32; static; inline;

    class function GetZ85: IBase85; static; inline;
    class function GetAscii85: IBase85; static; inline;

    class constructor Base85();

  var
    Falphabet: IBase85Alphabet;

    /// <summary>
    /// Decode a Base85 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base85 characters</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: TSimpleBaseLibCharArray)
      : TSimpleBaseLibByteArray; overload;

    procedure WriteOutput(var pOutput: PChar; const table: String; input: Int64;
      stringLength: Int32; usesZeroShortcut, usesSpaceShortcut
      : Boolean); inline;

  public

    /// <summary>
    /// Encode a byte array into a Base85 string
    /// </summary>
    /// <param name="bytes">Buffer to be encoded</param>
    /// <param name="padding">Append padding characters in the output</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray): String;
    /// <summary>
    /// Decode a Base85 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base85 string</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray; overload;
    /// <summary>
    /// ZeroMQ Z85 Alphabet
    /// </summary>
    class property Z85: IBase85 read GetZ85;
    /// <summary>
    /// Adobe Ascii85 Alphabet (each character is directly produced by raw
    /// value + 33)
    /// </summary>
    class property Ascii85: IBase85 read GetAscii85;

    constructor Create(const alphabet: IBase85Alphabet);
    destructor Destroy; override;

  end;

implementation

{ TBase85 }

procedure TBase85.WriteOutput(var pOutput: PChar; const table: String;
  input: Int64; stringLength: Int32; usesZeroShortcut, usesSpaceShortcut
  : Boolean);
var
  i: Int32;
  result: Int64;
begin
  // handle shortcuts
  if (input = 0) and (usesZeroShortcut) then
  begin
    pOutput^ := Falphabet.AllZeroShortcut;
    System.Inc(pOutput);
    Exit;
  end;

  if (input = allSpace) and (usesSpaceShortcut) then
  begin
    pOutput^ := Falphabet.AllSpaceShortcut;
    System.Inc(pOutput);
    Exit;
  end;

  // map the 4-byte packet to to 5-byte octets
  i := stringBlockSize - 1;
  while i >= 0 do
  begin
    // DivMod(input, baseLength, input, result);
    result := input mod baseLength;
    input := input div baseLength;
    pOutput[i] := table[Int32(result) + 1];
    System.Dec(i);
  end;
  pOutput := pOutput + stringLength;
end;

class procedure TBase85.WriteDecodedValue(var pOutput: PByte; value: Int64;
  numBytesToWrite: Int32);
var
  i: Int32;
  b: Byte;
begin
  i := byteBlockSize - 1;
  while ((i >= 0) and (numBytesToWrite > 0)) do
  begin
    b := Byte(TBits.Asr64(value, (i shl 3)) and $FF);
    pOutput^ := b;
    System.Inc(pOutput);
    System.Dec(i);
    System.Dec(numBytesToWrite);
  end;
end;

class procedure TBase85.WriteShortcut(var pOutput: PByte; blockIndex: Int32;
  value: Int64);
begin
  if (blockIndex <> 0) then
  begin
    raise EArgumentSimpleBaseLibException.CreateRes(@SUnExpectedCharacter);
  end;
  WriteDecodedValue(pOutput, value, byteBlockSize);
end;

class function TBase85.GetDecodeBufferLength(textLen: Int32;
  usingShortcuts: Boolean): Int32;
begin
  if (usingShortcuts) then
  begin
    result := textLen * byteBlockSize; // max possible size using shortcuts
    Exit;
  end;
  // max possible size without shortcuts
  result := (((textLen - 1) div stringBlockSize) + 1) * byteBlockSize;
end;

class constructor TBase85.Base85;
begin
  FZ85 := TBase85.Create(TBase85Alphabet.Z85 as IBase85Alphabet);
  FAscii85 := TBase85.Create(TBase85Alphabet.Ascii85 as IBase85Alphabet);
end;

constructor TBase85.Create(const alphabet: IBase85Alphabet);
begin
  Inherited Create();
  if (alphabet = Nil) then
  begin
    raise EArgumentNilSimpleBaseLibException.CreateResFmt(@SAlphabetNil,
      ['alphabet']);
  end;
  Falphabet := alphabet;
end;

function TBase85.Decode(const text: TSimpleBaseLibCharArray)
  : TSimpleBaseLibByteArray;
var
  textLen, decodeBufferLen, blockIndex, x, i, actualOutputLength: Int32;
  value: Int64;
  allZeroChar, allSpaceChar, c: Char;
  checkZero, checkSpace, usingShortcuts: Boolean;
  decodeBuffer, table: TSimpleBaseLibByteArray;
  inputPtr, pInput, pInputEnd: PChar;
  decodeBufferPtr, pDecodeBuffer, resultPtr: PByte;
begin
  result := Nil;
  textLen := System.Length(text);
  if (textLen = 0) then
  begin
    Exit;
  end;
  allZeroChar := Falphabet.AllZeroShortcut;
  allSpaceChar := Falphabet.AllSpaceShortcut;
  checkZero := allZeroChar <> TBase85Alphabet.NullChar;
  checkSpace := allSpaceChar <> TBase85Alphabet.NullChar;
  usingShortcuts := checkZero or checkSpace;
  // allocate a larger buffer if we're using shortcuts

  decodeBufferLen := GetDecodeBufferLength(textLen, usingShortcuts);
  System.SetLength(decodeBuffer, decodeBufferLen);
  table := Falphabet.ReverseLookupTable;
  inputPtr := PChar(text);
  decodeBufferPtr := PByte(decodeBuffer);
  pDecodeBuffer := decodeBufferPtr;
  pInput := inputPtr;
  pInputEnd := pInput + textLen;

  blockIndex := 0;
  value := 0;
  while (pInput <> pInputEnd) do
  begin

    c := pInput^;
    System.Inc(pInput);

    if (TUtilities.IsWhiteSpace(c)) then
    begin
      continue;
    end;

    // handle shortcut characters
    if ((checkZero) and (c = allZeroChar)) then
    begin
      WriteShortcut(pDecodeBuffer, blockIndex, 0);
      continue;
    end;
    if ((checkSpace) and (c = allSpaceChar)) then
    begin
      WriteShortcut(pDecodeBuffer, blockIndex, allSpace);
      continue;
    end;

    // handle regular blocks
    x := table[Ord(c)] - 1; // map character to byte value
    if (x < 0) then
    begin
      Falphabet.InvalidCharacter(c);
    end;
    value := (value * baseLength) + x;
    blockIndex := blockIndex + 1;
    if (blockIndex = stringBlockSize) then
    begin
      WriteDecodedValue(pDecodeBuffer, value, byteBlockSize);
      blockIndex := 0;
      value := 0;
    end;
  end;

  if (blockIndex > 0) then
  begin
    // handle padding by treating the rest of the characters
    // as "u"s. so both big endianness and bit weirdness work out okay.

    i := 0;
    while i < (stringBlockSize - blockIndex) do
    begin
      value := (value * baseLength) + (baseLength - 1);
      System.Inc(i);
    end;

    WriteDecodedValue(pDecodeBuffer, value, blockIndex - 1);
  end;
  actualOutputLength := Int32(pDecodeBuffer - decodeBufferPtr);
  System.SetLength(result, actualOutputLength);
  resultPtr := PByte(result);
  // we are bound to allocate a new buffer since we don't know
  // the correct output size at the beginning. that incurs an overhead of
  // System.Length(text) x 4 bytes during processing. this API isn't designed to decode
  // megabytes of data anyways.
  System.Move(decodeBufferPtr^, resultPtr^, actualOutputLength);
end;

function TBase85.Decode(const text: String): TSimpleBaseLibByteArray;
begin
  result := Decode(TUtilities.StringToCharArray(text));
end;

destructor TBase85.Destroy;
begin
  inherited Destroy;
end;

function TBase85.Encode(const bytes: TSimpleBaseLibByteArray): String;
var
  bytesLen, maxOutputLen, fullLen, remainingBytes, n, outputLen: Int32;
  input: Int64;
  t1, t2, t3, t4: UInt32;
  usesZeroShortcut, usesSpaceShortcut: Boolean;
  output: TSimpleBaseLibCharArray;
  table: string;
  inputPtr, pInput, pInputEnd: PByte;
  outputPtr, pOutput: PChar;
begin
  result := '';
  bytesLen := System.Length(bytes);
  if (bytesLen = 0) then
  begin
    Exit;
  end;
  usesZeroShortcut := Falphabet.AllZeroShortcut <> TBase85Alphabet.NullChar;
  usesSpaceShortcut := Falphabet.AllSpaceShortcut <> TBase85Alphabet.NullChar;

  // adjust output length based on prefix and suffix settings
  // we know output length is too large but we will handle it later.
  maxOutputLen := (bytesLen * stringBlockSize);

  System.SetLength(output, maxOutputLen);

  fullLen := TBits.Asr32(bytesLen, 2) shl 2; // rounded

  table := Falphabet.value;
  inputPtr := PByte(bytes);
  outputPtr := PChar(output);
  pOutput := outputPtr;
  pInput := inputPtr;
  pInputEnd := pInput + fullLen;

  while (pInput <> pInputEnd) do
  begin
    // build a 32-bit representation of input
    t1 := (UInt32(pInput^) shl 24);
    System.Inc(pInput);
    t2 := (UInt32(pInput^) shl 16);
    System.Inc(pInput);
    t3 := (UInt32(pInput^) shl 8);
    System.Inc(pInput);
    t4 := (UInt32(pInput^));
    System.Inc(pInput);
    input := t1 or t2 or t3 or t4;
    WriteOutput(pOutput, table, input, stringBlockSize, usesZeroShortcut,
      usesSpaceShortcut);
  end;

  // check if a part is remaining
  remainingBytes := bytesLen - fullLen;
  if (remainingBytes > 0) then
  begin
    input := 0;
    n := 0;
    while n < remainingBytes do
    begin
      input := input or (UInt32(pInput^) shl ((3 - n) shl 3));
      System.Inc(pInput);
      System.Inc(n);
    end;

    WriteOutput(pOutput, table, input, remainingBytes + 1, usesZeroShortcut,
      usesSpaceShortcut);
  end;

  outputLen := Int32(pOutput - outputPtr);
  System.SetString(result, outputPtr, outputLen);
end;

class function TBase85.GetAscii85: IBase85;
begin
  result := FAscii85;
end;

class function TBase85.GetZ85: IBase85;
begin
  result := FZ85;
end;

end.
