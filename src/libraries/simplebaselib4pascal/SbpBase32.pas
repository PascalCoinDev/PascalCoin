unit SbpBase32;

{$I ..\Include\SimpleBaseLib.inc}

interface

uses
  SbpSimpleBaseLibTypes,
  SbpUtilities,
  SbpBits,
  SbpBase32Alphabet,
  SbpIBase32Alphabet,
  SbpIBase32;

resourcestring
  SAlphabetNil = 'Alphabet Instance cannot be Nil "%s"';

type
  TBase32 = class sealed(TInterfacedObject, IBase32)

  strict private
  const
    bitsPerByte = Int32(8);
    bitsPerChar = Int32(5);
    paddingChar = Char('=');

    class var

      FCrockford, FRfc4648, FExtendedHex: IBase32;

    class function GetCrockford: IBase32; static; inline;
    class function GetRfc4648: IBase32; static; inline;
    class function GetExtendedHex: IBase32; static; inline;

    class constructor Base32();

  var
    Falphabet: IBase32Alphabet;

    /// <summary>
    /// Decode a Base32 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base32 characters</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: TSimpleBaseLibCharArray)
      : TSimpleBaseLibByteArray; overload;

  public

    /// <summary>
    /// Encode a byte array into a Base32 string
    /// </summary>
    /// <param name="bytes">Buffer to be encoded</param>
    /// <param name="padding">Append padding characters in the output</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray;
      padding: Boolean): String;
    /// <summary>
    /// Decode a Base32 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base32 string</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray; overload;
    /// <summary>
    /// Douglas Crockford's Base32 flavor with substitution characters.
    /// </summary>
    class property Crockford: IBase32 read GetCrockford;
    /// <summary>
    /// RFC 4648 variant of Base32 converter
    /// </summary>
    class property Rfc4648: IBase32 read GetRfc4648;
    /// <summary>
    /// Extended Hex variant of Base32 converter
    /// </summary>
    /// <remarks>Also from RFC 4648</remarks>
    class property ExtendedHex: IBase32 read GetExtendedHex;

    constructor Create(const alphabet: IBase32Alphabet);
    destructor Destroy; override;

  end;

implementation

{ TBase32 }

class constructor TBase32.Base32;
begin
  FCrockford := TBase32.Create(TBase32Alphabet.Crockford as IBase32Alphabet);
  FRfc4648 := TBase32.Create(TBase32Alphabet.Rfc4648 as IBase32Alphabet);
  FExtendedHex := TBase32.Create
    (TBase32Alphabet.ExtendedHex as IBase32Alphabet);
end;

constructor TBase32.Create(const alphabet: IBase32Alphabet);
begin
  Inherited Create();
  if (alphabet = Nil) then
  begin
    raise EArgumentNilSimpleBaseLibException.CreateResFmt(@SAlphabetNil,
      ['alphabet']);
  end;
  Falphabet := alphabet;
end;

function TBase32.Decode(const text: TSimpleBaseLibCharArray)
  : TSimpleBaseLibByteArray;
var
  textLen, bitsLeft, outputLen, outputPad, b, shiftBits: Int32;
  table: TSimpleBaseLibByteArray;
  resultPtr, pResult: PByte;
  inputPtr, pInput, pEnd: PChar;
  c: Char;
begin
  Result := Nil;
  textLen := System.Length(text);
  // ignore trailing padding chars and whitespace
  while (textLen > 0) do
  begin
    c := text[textLen - 1];
    if ((c <> paddingChar) and (not TUtilities.IsWhiteSpace(text[textLen - 1])))
    then
    begin
      break;
    end;
    System.Dec(textLen);
  end;
  if (textLen = 0) then
  begin
    Exit;
  end;

  bitsLeft := bitsPerByte;
  outputLen := textLen * bitsPerChar div bitsPerByte;
  System.SetLength(Result, outputLen);
  outputPad := 0;
  table := Falphabet.ReverseLookupTable;

  resultPtr := PByte(Result);
  inputPtr := PChar(text);

  pResult := resultPtr;
  pInput := inputPtr;
  pEnd := inputPtr + textLen;
  while (pInput <> pEnd) do
  begin
    c := pInput^;
    b := table[Ord(c)] - 1;
    System.Inc(pInput);
    if (b < 0) then
    begin
      Falphabet.InvalidCharacter(c);
    end;
    if (bitsLeft > bitsPerChar) then
    begin
      bitsLeft := bitsLeft - bitsPerChar;
      outputPad := outputPad or (b shl bitsLeft);
      continue;
    end;
    shiftBits := bitsPerChar - bitsLeft;
    outputPad := outputPad or (TBits.Asr32(b, shiftBits));
    pResult^ := Byte(outputPad);
    System.Inc(pResult);
    b := b and ((1 shl shiftBits) - 1);
    bitsLeft := bitsPerByte - shiftBits;
    outputPad := b shl bitsLeft;
  end;
end;

function TBase32.Decode(const text: String): TSimpleBaseLibByteArray;
begin
  Result := Decode(TUtilities.StringToCharArray(text));
end;

destructor TBase32.Destroy;
begin
  inherited Destroy;
end;

function TBase32.Encode(const bytes: TSimpleBaseLibByteArray;
  padding: Boolean): String;
var
  bytesLen, outputLen, bitsLeft, currentByte, outputPad, finalOutputLen,
    nextBits: Int32;
  output: string;
  inputPtr, pInput, pEnd: PByte;
  outputPtr, tablePtr, pOutput, pOutputEnd: PChar;
begin
  Result := '';
  bytesLen := System.Length(bytes);
  if (bytesLen = 0) then
  begin
    Exit;
  end;

  // we are ok with slightly larger buffer since the output string will always
  // have the exact length of the output produced.
  outputLen := (((bytesLen - 1) div bitsPerChar) + 1) * bitsPerByte;
  output := StringOfChar(Char(0), outputLen);

  inputPtr := PByte(bytes);
  outputPtr := PChar(output);
  tablePtr := PChar(Falphabet.Value);

  pOutput := outputPtr;
  pInput := inputPtr;
  pEnd := pInput + bytesLen;

  bitsLeft := bitsPerByte;
  currentByte := Int32(Byte(pInput^));

  while (pInput <> pEnd) do
  begin

    if (bitsLeft > bitsPerChar) then
    begin
      bitsLeft := bitsLeft - bitsPerChar;
      outputPad := TBits.Asr32(currentByte, bitsLeft);
      pOutput^ := tablePtr[outputPad];
      System.Inc(pOutput);
      currentByte := currentByte and ((1 shl bitsLeft) - 1);
    end;
    nextBits := bitsPerChar - bitsLeft;
    bitsLeft := bitsPerByte - nextBits;
    outputPad := currentByte shl nextBits;
    System.Inc(pInput);
    if (pInput <> pEnd) then
    begin
      currentByte := Int32(Byte(pInput^));
      outputPad := outputPad or TBits.Asr32(currentByte, bitsLeft);
      currentByte := currentByte and ((1 shl bitsLeft) - 1);
    end;
    pOutput^ := tablePtr[outputPad];
    System.Inc(pOutput);
  end;
  if (padding) then
  begin
    pOutputEnd := outputPtr + outputLen;
    while (pOutput <> pOutputEnd) do
    begin
      pOutput^ := paddingChar;
      System.Inc(pOutput);
    end;
  end;

  finalOutputLen := Int32(pOutput - outputPtr);
  if (finalOutputLen = outputLen) then
  begin
    Result := output; // avoid unnecessary copying
    Exit;
  end;

  System.SetString(Result, outputPtr, finalOutputLen);
end;

class function TBase32.GetCrockford: IBase32;
begin
  Result := FCrockford;
end;

class function TBase32.GetExtendedHex: IBase32;
begin
  Result := FExtendedHex;
end;

class function TBase32.GetRfc4648: IBase32;
begin
  Result := FRfc4648;
end;

end.
