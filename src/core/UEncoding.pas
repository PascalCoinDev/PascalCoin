unit UEncoding;

{ Copyright (c) 2020 by Herman Schoenfeld

  Contains text encoding schemes used through PascalCoin.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils,
  TypInfo,
  uregexpr,
  UCommon,
  UCrypto;


type

  { TCustomRegex }

  TCustomRegex = class sealed(TObject)
    var
      FRegex: TRegExpr;
    public
      constructor Create(const ARegexExpression: String);
      destructor Destroy(); override;
      function IsMatch(const AInputString: String): Boolean;
      function GetMatchFromName(const AGroupName: String): String;
      function Value(): String;
      procedure Match(const AInputString: String);
    end;


  { TPascalAsciiEncoding }

  TPascalAsciiEncoding = class sealed(TObject)
    strict private
      class var FEscapedStringRegex: TCustomRegex;
      class constructor CreatePascalAsciiEncoding();
      class destructor DestroyPascalAsciiEncoding();

    public
      const
        EscapeChar: Char = '\';
        CharSet = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
        CharSetEscaped = '"():<>[\]{}';
        CharSetUnescaped = ' !#$%&''*+,-./0123456789;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`abcdefghijklmnopqrstuvwxyz|~';
        CharPattern = '( |!|\\"|#|\$|%|&|''|\\\(|\\\)|\*|\+|,|-|\.|/|0|1|2|3|4|5|6|7|8|9|\\:|;|\\<|=|\\>|\?|@|A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|\\\[|\\\\|\\]|\^|_|`|a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z|\\\{|\||\\\}|~)';
        StringPattern = CharPattern + '+';

      class function IsValidEscaped(const ASafeAnsiString: String) : Boolean; static;
      class function IsValidUnescaped(const AUnescapedPascalAsciiString: String) : Boolean; static;
      class function Escape(const APascalAsciiString: String): String; static;
      class function Unescape(const APascalAsciiString: String): String; static;
  end;

  { TPascal64Encoding }

  TPascal64Encoding = class sealed(TObject)
    strict private
      class var FEscapedRegex: TCustomRegex;
      class constructor CreatePascal64Encoding();
      class destructor DestroyPascal64Encoding();
     public
       const
         EscapeChar: Char = '\';
         CharSet = 'abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-+{}[]_:"`|<>,.?/~';
         CharSetStart = 'abcdefghijklmnopqrstuvwxyz!@#$%^&*()-+{}[]_:"`|<>,.?/~';
         CharSetEscaped = '(){}[]:"<>';
         CharSetUnescaped = 'abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*-+_`|,.?/~';
         StartCharPattern = '(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z|!|@|#|\$|%|\^|&|\*|\\\(|\\\)|-|\+|\\\{|\\\}|\\\[|\\]|_|\\:|\\"|`|\||\\<|\\>|,|\.|\?|/|~)';
         NextCharPattern = '(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z|0|1|2|3|4|5|6|7|8|9|!|@|#|\$|%|\^|&|\*|\\\(|\\\)|-|\+|\\\{|\\\}|\\\[|\\]|_|\\:|\\"|`|\||\\<|\\>|,|\.|\?|/|~)';
         StringPattern = StartCharPattern + NextCharPattern + '*';
         StringOnlyPattern = StringPattern + '$';

       class function IsValidEscaped(const AEscapedPascal64String: String) : Boolean; static;
       class function IsValidUnescaped(const AUnescapedPascal64String: String) : Boolean; static;
       class function Escape(const APascal64String: String): String; static;
       class function Unescape(const APascal64String: String): String; static;
  end;


  { THexEncoding }

  THexEncoding = class sealed(TObject)
    private
      class var FHexStringRegex: TCustomRegex;
      class constructor CreateHexEncoding();
      class destructor DestroyHexEncoding();

    public
      const
        CharSet = '0123456789abcdef';
        NibblePattern = '[0-9a-f]';
        BytePattern = NibblePattern + '{2}';
        SubStringPattern = '(?:' + BytePattern + ')+';
        StringPattern = SubStringPattern + '$';

      class function IsValid(const AHexString: String): Boolean; static;
      class function Decode(const AHexString: String): TArray<Byte>; static;
      class function TryDecode(const AHexString: String; out AResult: TArray<Byte>): Boolean; static;
      class function Encode(const ABytes: TArray<Byte>; AOmitPrefix: Boolean = True): String; static;
  end;

  { TPascalBase58Encoding }

  TPascalBase58Encoding = class sealed(TObject)
    private
      class var FStringRegex: TCustomRegex;
      class constructor CreatePascalBase58Encoding();
      class destructor DestroyPascalBase58Encoding();

    public
      const
        CharSet = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
        CharPattern = '[123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz]';
        SubStringPattern = CharPattern + '+';
        StringPattern = SubStringPattern + '$';

        class function IsValid(const ABase58String: String): Boolean; static;
        class function Decode(const ABase58String: String): TArray<Byte>; static;
        class function TryDecode(const ABase58String: String; out AResult: TArray<Byte>): Boolean; static;
        class function Encode(const ABytes: TArray<Byte>): String; static;
  end;

  { TPASCEncoding }

  TPASCEncoding = class sealed(TObject)
    public
      class function IsValid(const APasc: String): Boolean; static;
      class function Decode(const APasc: String): Int64; static;
      class function TryDecode(const APasc: String; out AMolinas: Int64) : Boolean; static;
      class function Encode(const AMolinas: Int64): String; static;
    end;


  { TStringExtensions }

  TStringExtensions = class sealed(TObject)
  public
    class function Escape(const AStr: String; AEscapeSymbol: Char; const AEscapedChars: TArray<Char>): String; static;
    class function Unescape(const AStr: String; AEscapeSymbol: Char; const AEscapedChars: TArray<Char>): String; static;
    class function All(const ANeedle: String; const AStack: String) : Boolean; static;
  end;

resourcestring
  SUnknownPayloadEncoding = 'Unknown payload encoding.';
  SInvalidEPasaFormat = 'Invalid E-PASA format, %s';
  SInvalidEPasa = 'Invalid EPASA "%s": %s';
  SInvalidHexString = 'Invalid hex-formatted string, %s';
  SInvalidBase58String = 'Invalid Base58-formatted string, %s';
  SBase58EncodeError = 'Error Encoding to Base58';
  SUnRecognizedStartCharacter = 'Unrecognized start character, %s';
  SInvalidPASCQuantity = 'Invalid PASC quantity string, %s';

implementation

uses
  HlpHashFactory,
  HlpIHashInfo,
  HlpConverters,
  UJSONFunctions;

{ TCustomRegex }

constructor TCustomRegex.Create(const ARegexExpression: String);
begin
  inherited Create();
  FRegex := TRegExpr.Create(ARegexExpression);
end;

destructor TCustomRegex.Destroy;
begin
  FRegex.Free;
  inherited Destroy;
end;

function TCustomRegex.GetMatchFromName(const AGroupName: String): String;
var
  i: Int32;
begin
  Result := #0;
  // carefull: E:\SourceCode\PascalCoin-master\src\libraries\regex must be added to project file
  // otherwise wrong unit will be used from:
  // Drive:\Tools\Lazarus\fpc\3.2.0\source\packages\regexpr\src
  i := FRegex.MatchIndexFromName(AGroupName);
  if i >= 0 then begin
    Result := FRegex.Match[i];
  end;
end;

function TCustomRegex.IsMatch(const AInputString: String): Boolean;
begin
  Result := FRegex.Exec(AInputString);
end;

procedure TCustomRegex.Match(const AInputString: String);
begin
  FRegex.Exec(AInputString);
end;

function TCustomRegex.Value: String;
begin
  Result := FRegex.Match[0];
end;

{ TPascalAsciiEncoding }

class function TPascalAsciiEncoding.IsValidEscaped(const ASafeAnsiString : String): Boolean;
begin
  Result := FEscapedStringRegex.IsMatch(ASafeAnsiString);
end;

class function TPascalAsciiEncoding.IsValidUnescaped (const AUnescapedPascalAsciiString: String): Boolean;
begin
  Result := TStringExtensions.All(AUnescapedPascalAsciiString, CharSet);
end;

class function TPascalAsciiEncoding.Escape(const APascalAsciiString : String): String;
begin
  Result := TStringExtensions.Escape(APascalAsciiString, EscapeChar, CharSetEscaped.ToCharArray);
end;

class function TPascalAsciiEncoding.Unescape(const APascalAsciiString : String): String;
begin
  Result := TStringExtensions.Unescape(APascalAsciiString, EscapeChar, CharSetEscaped.ToCharArray);
end;

class constructor TPascalAsciiEncoding.CreatePascalAsciiEncoding;
begin
  FEscapedStringRegex := TCustomRegex.Create(StringPattern);
end;

class destructor TPascalAsciiEncoding.DestroyPascalAsciiEncoding;
begin
  FEscapedStringRegex.Free;
end;

{ TPascal64Encoding }

class function TPascal64Encoding.IsValidEscaped(const AEscapedPascal64String : String): Boolean;
begin
  Result := FEscapedRegex.IsMatch(AEscapedPascal64String);
end;

class function TPascal64Encoding.IsValidUnescaped(const AUnescapedPascal64String : String): Boolean;
begin
  Result := (3 <= AUnescapedPascal64String.Length) and
    (AUnescapedPascal64String.Length <= 64) and
    (StartCharPattern.Contains(AUnescapedPascal64String[1])) and
    (TStringExtensions.All(AUnescapedPascal64String, CharSet));
end;

class function TPascal64Encoding.Escape(const APascal64String: String): String;
begin
  Result := TStringExtensions.Escape(APascal64String, EscapeChar,
    CharSetEscaped.ToCharArray);
end;

class function TPascal64Encoding.Unescape(const APascal64String : String): String;
begin
  Result := TStringExtensions.Unescape(APascal64String, EscapeChar,
    CharSetEscaped.ToCharArray);
end;

class constructor TPascal64Encoding.CreatePascal64Encoding;
begin
  FEscapedRegex := TCustomRegex.Create(StringOnlyPattern);
end;

class destructor TPascal64Encoding.DestroyPascal64Encoding;
begin
  FEscapedRegex.Free;
end;

{ THexEncoding }

class function THexEncoding.IsValid(const AHexString: String): Boolean;
begin
  Result := FHexStringRegex.IsMatch(AHexString);
end;

class function THexEncoding.Decode(const AHexString: String): TArray<Byte>;
begin
  if (not TryDecode(AHexString, Result)) then
    raise EArgumentException.CreateResFmt(@SInvalidHexString, ['AHexString']);
end;

class function THexEncoding.TryDecode(const AHexString: String; out AResult: TArray<Byte>): Boolean;
begin
  AResult := Nil;
  if (not IsValid(AHexString)) then
    Exit(False);

  Result := TryHex2Bytes(AHexString, AResult);
end;

class function THexEncoding.Encode(const ABytes: TArray<Byte>;   AOmitPrefix: Boolean): String;
begin
  Result := Bytes2Hex(ABytes, not AOmitPrefix).ToLowerInvariant;
end;

class constructor THexEncoding.CreateHexEncoding;
begin
  FHexStringRegex := TCustomRegex.Create(StringPattern);
end;

class destructor THexEncoding.DestroyHexEncoding;
begin
  FHexStringRegex.Free;
end;

{ TPascalBase58Encoding }

class function TPascalBase58Encoding.IsValid(const ABase58String : String): Boolean;
begin
  Result := FStringRegex.IsMatch(ABase58String);
end;

class function TPascalBase58Encoding.Decode(const ABase58String: String) : TArray<Byte>;
begin
  if (not TryDecode(ABase58String, Result)) then
    raise EArgumentException.CreateResFmt(@SInvalidBase58String, ['ABase58String']);
end;

class function TPascalBase58Encoding.TryDecode(const ABase58String: String;  out AResult: TArray<Byte>): Boolean;
var
  LBN, LBNAux, LBNBase: TBigNum;
  i, LOffset: Int32;
begin
  Result := False;
  LBN := TBigNum.Create(0);
  LBNAux := TBigNum.Create;
  LBNBase := TBigNum.Create(1);

  try
    for i := ABase58String.Length downto 1 do begin
      LOffset := System.Pos(ABase58String[i], CharSet) - 1;
      if LOffset < 0 then begin
        Exit(False);
      end;
      LBNAux.Value := LOffset;
      LBNAux.Multiply(LBNBase);
      LBN.Add(LBNAux);
      LBNBase.Multiply(CharSet.Length);
    end;
    AResult := THexEncoding.Decode(LBN.HexaValue);
  finally
    LBN.Free;
    LBNAux.Free;
    LBNBase.Free;
  end;
  TArrayTool<Byte>.RemoveAt(AResult, 0);
  Result := True;
end;

class function TPascalBase58Encoding.Encode(const ABytes: TArray<Byte>): String;
Var
  LBN, LBNMod, LBNDiv: TBigNum;
begin
  Result := string.Empty;
  LBN := TBigNum.Create;
  LBNMod := TBigNum.Create;
  LBNDiv := TBigNum.Create(CharSet.Length);

  try
    LBN.HexaValue := '01' + THexEncoding.Encode(ABytes);
    while (not LBN.IsZero) do begin
      LBN.Divide(LBNDiv, LBNMod);
      If (LBNMod.Value >= 0) and (LBNMod.Value < CharSet.Length) then begin
        Result := CharSet[Byte(LBNMod.Value) + 1] + Result;
      end else begin
        raise Exception.CreateRes(@SBase58EncodeError);
      end;
    end;
  finally
    LBN.Free;
    LBNMod.Free;
    LBNDiv.Free;
  end;
end;

class constructor TPascalBase58Encoding.CreatePascalBase58Encoding;
begin
  FStringRegex := TCustomRegex.Create(StringPattern);
end;

class destructor TPascalBase58Encoding.DestroyPascalBase58Encoding;
begin
  FStringRegex.Free;
end;

{ TPASCEncoding }

class function TPASCEncoding.IsValid(const APasc: String): Boolean;
var
  temp: Int64;
begin
  Result := TryDecode(APasc, temp);
end;

class function TPASCEncoding.Decode(const APasc: String): Int64;
begin
  if (not TryDecode(APasc, Result)) then
  begin
    raise EArgumentException.CreateResFmt(@SInvalidPASCQuantity, ['APasc']);
  end;
end;

class function TPASCEncoding.TryDecode(const APasc: String;  out AMolinas: Int64): Boolean; var s : String;
  LPosThousand, LPosDecimal : Integer;
  LMoneyString : String;
begin
  AMolinas := 0;
  LMoneyString := Trim(APasc);
  if LMoneyString.Length=0 then begin
    Result := true;
    exit;
  end;
  try
    LPosThousand := LMoneyString.IndexOf( TPCJSONData.JSONFormatSettings.ThousandSeparator );
    LPosDecimal  := LMoneyString.IndexOf( TPCJSONData.JSONFormatSettings.DecimalSeparator );

    if (LPosThousand>0) then begin
      if (LPosThousand < LPosDecimal ) then begin
        // Remove thousand values
        LMoneyString := LMoneyString.Replace(String(TPCJSONData.JSONFormatSettings.ThousandSeparator),'',[rfReplaceAll]);
      end else begin
        // Possible 15.123.456,7890 format ( coma (,) = decimal separator )
        // Remove decimal "." and convert thousand to decimal
        LMoneyString := LMoneyString.Replace(String(TPCJSONData.JSONFormatSettings.DecimalSeparator),'',[rfReplaceAll]);
        LMoneyString := LMoneyString.Replace(TPCJSONData.JSONFormatSettings.ThousandSeparator,TPCJSONData.JSONFormatSettings.DecimalSeparator,[rfReplaceAll]);
      end;
    end;

    AMolinas := Round( StrToFloat(LMoneyString,TPCJSONData.JSONFormatSettings)*10000 );
    Result := true;
  Except
    result := false;
  end;

end;

class function TPASCEncoding.Encode(const AMolinas: Int64): String;
begin
  Result := FormatFloat('#,###0.0000',(AMolinas/10000.0), TPCJSONData.JSONFormatSettings);
end;

{ TStringExtensions }

class function TStringExtensions.Escape(const AStr: String; AEscapeSymbol: Char; const AEscapedChars: TArray<Char>): String;
var
  LPPtr: PChar;
  LPeek, LNext: Char;
begin
  Result := String.Empty;
  LPPtr := PChar(AStr);

  while LPPtr^ <> #0 do
  begin
    LPeek := LPPtr^;
    if LPeek = AEscapeSymbol then
    begin
      Result := Result + LPPtr^; // append escape symbol
      System.Inc(LPPtr);
      LNext := LPPtr^;
      if LNext = #0 then
      begin
        // end of string, last char was escape symbol
        if (TArrayTool<Char>.Contains(AEscapedChars, AEscapeSymbol)) then
        begin
          // need to escape it
          Result := Result + AEscapeSymbol;
        end;
      end
      else if (TArrayTool<Char>.Contains(AEscapedChars, LNext)) then
      begin
        // is an escape sequence, append next char
        Result := Result + LPPtr^;
        System.Inc(LPPtr);
      end
      else
      begin
        // is an invalid escape sequence
        if (TArrayTool<Char>.Contains(AEscapedChars, AEscapeSymbol)) then
        begin
          // need to escape symbol, since it's an escaped char
          Result := Result + AEscapeSymbol;
        end;
      end;

    end
    else if (TArrayTool<Char>.Contains(AEscapedChars, LPeek)) then
    begin
      // char needs escaping
      Result := Result + AEscapeSymbol + LPPtr^;
      System.Inc(LPPtr);
    end
    else
    begin
      // normal char
      Result := Result + LPPtr^;
      System.Inc(LPPtr);
    end;
  end;
end;

class function TStringExtensions.Unescape(const AStr: String; AEscapeSymbol: Char; const AEscapedChars: TArray<Char>): String;
var
  LPPtr: PChar;
  LPeek: Char;
begin
  Result := String.Empty;
  LPPtr := PChar(AStr);

  while LPPtr^ <> #0 do
  begin
    LPeek := LPPtr^;
    if LPeek = AEscapeSymbol then begin
      System.Inc(LPPtr); // omit the escape symbol
      LPeek := LPPtr^;
      if LPeek = #0 then begin
        // last character was the escape symbol, so include it
        Result := Result + AEscapeSymbol;
        break;
      end;

      if (not(TArrayTool<Char>.Contains(AEscapedChars, LPeek))) then begin
        // was not an escaped char, so include the escape symbol
        Result := Result + AEscapeSymbol;
        continue;
      end;

    end;
    // include the char (or escaped char)
    Result := Result + LPPtr^;
    System.Inc(LPPtr);
  end;
end;

class function TStringExtensions.All(const ANeedle: String; const AStack: String): Boolean;
var
  c: Char;
begin
  Result := True;
  for c in ANeedle do
  begin
    if not AStack.Contains(c) then
      Exit(False);
  end;
end;

end.
