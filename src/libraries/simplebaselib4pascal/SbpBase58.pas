unit SbpBase58;

{$I SimpleBaseLib.inc}

interface

uses
  SbpSimpleBaseLibTypes,
  SbpUtilities,
  SbpBase58Alphabet,
  SbpIBase58Alphabet,
  SbpIBase58;

resourcestring
  SAlphabetNil = 'Alphabet Instance cannot be Nil "%s"';

type
  TBase58 = class sealed(TInterfacedObject, IBase58)

  strict private
  const
    baseLength = Int32(58);
    class var

      FBitCoin, FRipple, FFlickr: IBase58;

    class function GetBitCoin: IBase58; static; inline;
    class function GetFlickr: IBase58; static; inline;
    class function GetRipple: IBase58; static; inline;

    class constructor Base58();

  var
    Falphabet: IBase58Alphabet;

    /// <summary>
    /// Decode a Base58 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base58 characters</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: TSimpleBaseLibCharArray)
      : TSimpleBaseLibByteArray; overload;

  public

    /// <summary>
    /// Encode to Base58 representation
    /// </summary>
    /// <param name="bytes">Bytes to encode</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray): String;
    /// <summary>
    /// Decode a Base58 representation
    /// </summary>
    /// <param name="text">Base58 encoded text</param>
    /// <returns>Array of decoded bytes</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray; overload;

    class property BitCoin: IBase58 read GetBitCoin;
    class property Ripple: IBase58 read GetRipple;
    class property Flickr: IBase58 read GetFlickr;

    constructor Create(const alphabet: IBase58Alphabet);
    destructor Destroy; override;

  end;

implementation

{ TBase58 }

class constructor TBase58.Base58;
begin
  FBitCoin := TBase58.Create(TBase58Alphabet.BitCoin as IBase58Alphabet);
  FRipple := TBase58.Create(TBase58Alphabet.Ripple as IBase58Alphabet);
  FFlickr := TBase58.Create(TBase58Alphabet.Flickr as IBase58Alphabet);
end;

constructor TBase58.Create(const alphabet: IBase58Alphabet);
begin
  Inherited Create();
  if (alphabet = Nil) then
  begin
    raise EArgumentNilSimpleBaseLibException.CreateResFmt(@SAlphabetNil,
      ['alphabet']);
  end;
  Falphabet := alphabet;
end;

function TBase58.Decode(const text: TSimpleBaseLibCharArray)
  : TSimpleBaseLibByteArray;
const
  // https://github.com/bitcoin/bitcoin/blob/master/src/base58.cpp
  reductionFactor = Int32(733);
var
  textLen, numZeroes, outputLen, carry, resultLen, LowPoint: Int32;
  inputPtr, pEnd, pInput: PChar;
  outputPtr, pOutputEnd, pDigit, pOutput: PByte;
  FirstChar, c: Char;
  Value: string;
  output, table: TSimpleBaseLibByteArray;
begin
  result := Nil;
  textLen := System.Length(text);
  if (textLen = 0) then
  begin
    Exit;
  end;

  inputPtr := PChar(text);

  pEnd := inputPtr + textLen;
  pInput := inputPtr;
{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(String);
{$ELSE}
  LowPoint := 1;
{$ENDIF DELPHIXE3_UP}
  Value := Falphabet.Value;
  FirstChar := Value[LowPoint];
  while ((pInput^ = FirstChar) and (pInput <> pEnd)) do
  begin
    System.Inc(pInput);
  end;

  numZeroes := Int32(pInput - inputPtr);
  if (pInput = pEnd) then
  begin
    System.SetLength(result, numZeroes);
    Exit;
  end;

  outputLen := ((textLen * reductionFactor) div 1000) + 1;
  table := Falphabet.ReverseLookupTable;
  System.SetLength(output, outputLen);
  outputPtr := PByte(output);

  pOutputEnd := outputPtr + outputLen - 1;
  while (pInput <> pEnd) do
  begin
    c := pInput^;
    System.Inc(pInput);
    carry := table[Ord(c)] - 1;
    if (carry < 0) then
    begin
      Falphabet.InvalidCharacter(c);
    end;
    pDigit := pOutputEnd;
    while pDigit >= outputPtr do
    begin
      carry := carry + (baseLength * pDigit^);
      pDigit^ := Byte(carry);
      // carry := carry div 256;
      carry := carry shr 8;
      System.Dec(pDigit);
    end;

  end;

  pOutput := outputPtr;
  while ((pOutput <> pOutputEnd) and (pOutput^ = 0)) do
  begin
    System.Inc(pOutput);
  end;

  resultLen := Int32(pOutputEnd - pOutput) + 1;
  if (resultLen = outputLen) then
  begin
    result := output;
    Exit;
  end;
  System.SetLength(result, numZeroes + resultLen);
  System.Move(output[Int32(pOutput - outputPtr)], result[numZeroes], resultLen);

end;

function TBase58.Decode(const text: String): TSimpleBaseLibByteArray;
begin
  result := Decode(TUtilities.StringToCharArray(text));
end;

destructor TBase58.Destroy;
begin
  inherited Destroy;
end;

function TBase58.Encode(const bytes: TSimpleBaseLibByteArray): String;
const
  growthPercentage = Int32(138);
var
  bytesLen, numZeroes, outputLen, Length, carry, i, resultLen: Int32;
  inputPtr, pInput, pEnd, outputPtr, pOutputEnd, pDigit, pOutput: PByte;
  alphabetPtr, resultPtr, pResult: PChar;
  ZeroChar: Char;
  output: TSimpleBaseLibByteArray;
  Value: String;
begin
  result := '';
  bytesLen := System.Length(bytes);
  if (bytesLen = 0) then
  begin
    Exit;
  end;
  inputPtr := PByte(bytes);
  Value := Falphabet.Value;
  alphabetPtr := PChar(Value);

  pInput := inputPtr;
  pEnd := inputPtr + bytesLen;
  while ((pInput <> pEnd) and (pInput^ = 0)) do
  begin
    System.Inc(pInput);
  end;
  numZeroes := Int32(pInput - inputPtr);

  ZeroChar := alphabetPtr^;

  if (pInput = pEnd) then
  begin
    result := StringOfChar(ZeroChar, numZeroes);
    Exit;
  end;

  outputLen := bytesLen * growthPercentage div 100 + 1;
  Length := 0;
  System.SetLength(output, outputLen);
  outputPtr := PByte(output);

  pOutputEnd := outputPtr + outputLen - 1;
  while (pInput <> pEnd) do
  begin
    carry := pInput^;
    i := 0;
    pDigit := pOutputEnd;
    while (((carry <> 0) or (i < Length)) and (pDigit >= outputPtr)) do
    begin
      carry := carry + (256 * pDigit^);
      pDigit^ := Byte(carry mod baseLength);
      carry := carry div baseLength;
      System.Dec(pDigit);
      System.Inc(i);
    end;

    Length := i;
    System.Inc(pInput);
  end;

  System.Inc(pOutputEnd);
  pOutput := outputPtr;
  while ((pOutput <> pOutputEnd) and (pOutput^ = 0)) do
  begin
    System.Inc(pOutput);
  end;

  resultLen := numZeroes + Int32(pOutputEnd - pOutput);
  result := StringOfChar(ZeroChar, resultLen);
  resultPtr := PChar(result);

  pResult := resultPtr + numZeroes;
  while (pOutput <> pOutputEnd) do
  begin
    pResult^ := alphabetPtr[pOutput^];
    System.Inc(pOutput);
    System.Inc(pResult);
  end;

end;

class function TBase58.GetBitCoin: IBase58;
begin
  result := FBitCoin;
end;

class function TBase58.GetFlickr: IBase58;
begin
  result := FFlickr;
end;

class function TBase58.GetRipple: IBase58;
begin
  result := FRipple;
end;

end.
