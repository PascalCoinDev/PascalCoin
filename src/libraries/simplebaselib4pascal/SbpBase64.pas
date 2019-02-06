unit SbpBase64;

{$I SimpleBaseLib.inc}

interface

uses
  SbpBits,
  SbpUtilities,
  SbpSimpleBaseLibTypes,
  SbpBase64Alphabet,
  SbpIBase64Alphabet,
  SbpIBase64;

resourcestring
  SAlphabetNil = 'Alphabet Instance cannot be Nil "%s"';

type
  TBase64 = class sealed(TInterfacedObject, IBase64)

  strict private
  const
    paddingChar = Char('=');

    class var

      FDefault, FDefaultNoPadding, FUrlEncoding, FXmlEncoding, FRegExEncoding,
      FFileEncoding: IBase64;

    class function GetDefault: IBase64; static; inline;
    class function GetDefaultNoPadding: IBase64; static; inline;
    class function GetFileEncoding: IBase64; static; inline;
    class function GetRegExEncoding: IBase64; static; inline;
    class function GetUrlEncoding: IBase64; static; inline;
    class function GetXmlEncoding: IBase64; static; inline;

    class constructor Base64();

  var
    Falphabet: IBase64Alphabet;

    /// <summary>
    /// Decode a Base64 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base64 characters</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: TSimpleBaseLibCharArray)
      : TSimpleBaseLibByteArray; overload;

  public

    /// <summary>
    /// Encode a byte array into a Base64 string
    /// </summary>
    /// <param name="bytes">Buffer to be encoded</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray): String;
    /// <summary>
    /// Decode a Base64 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base64 string</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray; overload;

    class property Default: IBase64 read GetDefault;
    class property DefaultNoPadding: IBase64 read GetDefaultNoPadding;
    class property UrlEncoding: IBase64 read GetUrlEncoding;
    class property XmlEncoding: IBase64 read GetXmlEncoding;
    class property RegExEncoding: IBase64 read GetRegExEncoding;
    class property FileEncoding: IBase64 read GetFileEncoding;

    constructor Create(const alphabet: IBase64Alphabet);
    destructor Destroy; override;

  end;

implementation

{ TBase64 }

class constructor TBase64.Base64;
begin
  FDefault := TBase64.Create(TBase64Alphabet.Default as IBase64Alphabet);
  FDefaultNoPadding := TBase64.Create
    (TBase64Alphabet.DefaultNoPadding as IBase64Alphabet);
  FUrlEncoding := TBase64.Create
    (TBase64Alphabet.UrlEncoding as IBase64Alphabet);
  FXmlEncoding := TBase64.Create
    (TBase64Alphabet.XmlEncoding as IBase64Alphabet);
  FRegExEncoding := TBase64.Create
    (TBase64Alphabet.RegExEncoding as IBase64Alphabet);
  FFileEncoding := TBase64.Create
    (TBase64Alphabet.FileEncoding as IBase64Alphabet);
end;

constructor TBase64.Create(const alphabet: IBase64Alphabet);
begin
  Inherited Create();
  if (alphabet = Nil) then
  begin
    raise EArgumentNilSimpleBaseLibException.CreateResFmt(@SAlphabetNil,
      ['alphabet']);
  end;
  Falphabet := alphabet;
end;

function TBase64.Decode(const text: TSimpleBaseLibCharArray)
  : TSimpleBaseLibByteArray;

  function ProcessDecode(var pInput: PChar; pEnd: PChar; pDecodeTable: PByte;
    PaddingCount: Int32): Byte;
  var
    c: Char;
    b: Int32;
  begin
    Result := 0;
    if pInput >= pEnd then
    begin
      Exit;
    end;

    c := pInput^;

    b := pDecodeTable[Ord(c)] - 1;
    if (b < 0) then
    begin
      if ((pInput + PaddingCount) <> pEnd) then
      begin
        Falphabet.InvalidCharacter(c);
      end;
    end
    else
    begin
      Result := Byte(b);
      System.Inc(pInput);
    end;

  end;

var
  textLen, blocks, padding, i, bytes: Int32;
  temp1, temp2: Byte;
  table: TSimpleBaseLibByteArray;
  InputPtr, pEnd: PChar;
  resultPtr, tablePtr: PByte;
begin
  Result := Nil;
  textLen := System.Length(text);
  if (textLen = 0) then
  begin
    Exit;
  end;

  InputPtr := PChar(text);
  pEnd := InputPtr + textLen;

  blocks := ((textLen - 1) div 4) + 1;
  bytes := blocks * 3;

  padding := (blocks * 4) - textLen;

  if ((textLen > 2) and (InputPtr[textLen - 2] = paddingChar)) then
  begin
    padding := 2;
  end
  else if ((textLen > 1) and (InputPtr[textLen - 1] = paddingChar)) then
  begin
    padding := 1;
  end;

  System.SetLength(Result, bytes - padding);

  table := Falphabet.ReverseLookupTable;
  tablePtr := PByte(table);

  resultPtr := PByte(Result);

  i := 1;

  while i < blocks do
  begin

    temp1 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);

    temp2 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);

    resultPtr^ := Byte(Int32(temp1 shl 2) or (TBits.Asr32(temp2 and $30, 4)));
    System.Inc(resultPtr);

    temp1 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);

    resultPtr^ := Byte((TBits.Asr32(temp1 and $3C, 2)) or
      ((temp2 and $0F) shl 4));
    System.Inc(resultPtr);

    temp2 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);

    resultPtr^ := Byte(((temp1 and $03) shl 6) or temp2);
    System.Inc(resultPtr);

    System.Inc(i);
  end;

  temp1 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);

  temp2 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);

  resultPtr^ := Byte(Int32(temp1 shl 2) or (TBits.Asr32(temp2 and $30, 4)));
  System.Inc(resultPtr);

  temp1 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);

  if (padding <> 2) then
  begin
    resultPtr^ := Byte((TBits.Asr32(temp1 and $3C, 2) or
      ((temp2 and $0F) shl 4)));
    System.Inc(resultPtr);
  end;

  temp2 := ProcessDecode(InputPtr, pEnd, tablePtr, padding);
  if (padding = 0) then
  begin
    resultPtr^ := Byte(((temp1 and $03) shl 6) or temp2);
    System.Inc(resultPtr);
  end;

end;

function TBase64.Decode(const text: String): TSimpleBaseLibByteArray;
begin
  Result := Decode(TUtilities.StringToCharArray(text));
end;

destructor TBase64.Destroy;
begin
  inherited Destroy;
end;

function TBase64.Encode(const bytes: TSimpleBaseLibByteArray): String;
var
  bytesLen, padding, blocks, BufferLen, i: Int32;
  b1, b2, b3: Byte;
  InputPtr: PByte;
  OutputPtr: PChar;
  Buffer: TSimpleBaseLibCharArray;
  pad2, pad1: Boolean;
  table: string;
begin
  Result := '';
  bytesLen := System.Length(bytes);
  if (bytesLen = 0) then
  begin
    Exit;
  end;

  InputPtr := PByte(bytes);
  table := Falphabet.Value;

  padding := bytesLen mod 3;
  if (padding > 0) then
  begin
    padding := 3 - padding;
  end;
  blocks := (bytesLen - 1) div 3 + 1;

  BufferLen := blocks * 4;

  System.SetLength(Buffer, BufferLen);

  OutputPtr := PChar(Buffer);

  i := 1;

  while i < blocks do
  begin
    b1 := InputPtr^;
    System.Inc(InputPtr);
    b2 := InputPtr^;
    System.Inc(InputPtr);
    b3 := InputPtr^;
    System.Inc(InputPtr);

    OutputPtr^ := table[(TBits.Asr32((b1 and $FC), 2)) + 1];
    System.Inc(OutputPtr);
    OutputPtr^ := table[(TBits.Asr32((b2 and $F0), 4) or (b1 and $03)
      shl 4) + 1];
    System.Inc(OutputPtr);
    OutputPtr^ := table[(TBits.Asr32((b3 and $C0), 6) or (b2 and $0F)
      shl 2) + 1];
    System.Inc(OutputPtr);
    OutputPtr^ := table[(b3 and $3F) + 1];
    System.Inc(OutputPtr);

    System.Inc(i);
  end;

  pad2 := padding = 2;
  pad1 := padding > 0;

  b1 := InputPtr^;
  System.Inc(InputPtr);
  if pad2 then
  begin
    b2 := Byte(0)
  end
  else
  begin
    b2 := InputPtr^;
    System.Inc(InputPtr);
  end;

  if pad1 then
  begin
    b3 := Byte(0)
  end
  else
  begin
    b3 := InputPtr^;
    System.Inc(InputPtr);
  end;

  OutputPtr^ := table[(TBits.Asr32((b1 and $FC), 2)) + 1];
  System.Inc(OutputPtr);
  OutputPtr^ := table[(TBits.Asr32((b2 and $F0), 4) or (b1 and $03) shl 4) + 1];
  System.Inc(OutputPtr);
  if pad2 then
  begin
    OutputPtr^ := '='
  end
  else
  begin
    OutputPtr^ := table[(TBits.Asr32((b3 and $C0), 6) or (b2 and $0F)
      shl 2) + 1]
  end;

  System.Inc(OutputPtr);

  if pad1 then
  begin
    OutputPtr^ := '='
  end
  else
  begin
    OutputPtr^ := table[(b3 and $3F) + 1]
  end;

  System.Inc(OutputPtr);

  if (not Falphabet.PaddingEnabled) then
  begin
    if (pad2) then
    begin
      System.Dec(BufferLen);
    end;
    if (pad1) then
    begin
      System.Dec(BufferLen);
    end;

  end;

  System.SetString(Result, PChar(@Buffer[0]), BufferLen);

end;

class function TBase64.GetDefault: IBase64;
begin
  Result := FDefault;
end;

class function TBase64.GetDefaultNoPadding: IBase64;
begin
  Result := FDefaultNoPadding;
end;

class function TBase64.GetFileEncoding: IBase64;
begin
  Result := FFileEncoding;
end;

class function TBase64.GetRegExEncoding: IBase64;
begin
  Result := FRegExEncoding;
end;

class function TBase64.GetUrlEncoding: IBase64;
begin
  Result := FUrlEncoding;
end;

class function TBase64.GetXmlEncoding: IBase64;
begin
  Result := FXmlEncoding;
end;

end.
