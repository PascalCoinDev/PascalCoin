unit SbpBase64Alphabet;

{$I SimpleBaseLib.inc}

interface

uses
  SysUtils,
  SbpEncodingAlphabet,
  SbpSimpleBaseLibTypes,
  SbpIBase64Alphabet;

type
  TBase64Alphabet = class sealed(TEncodingAlphabet, IBase64Alphabet)

  strict private
  const
    B64CharacterSet
      : String =
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

    class var

      FDefault, FDefaultNoPadding, FUrlEncoding, FXmlEncoding, FRegExEncoding,
      FFileEncoding: IBase64Alphabet;

  var
    FPaddingEnabled: Boolean;

    function GetPaddingEnabled: Boolean; inline;

    class function GetDefault: IBase64Alphabet; static; inline;
    class function GetDefaultNoPadding: IBase64Alphabet; static; inline;
    class function GetFileEncoding: IBase64Alphabet; static; inline;
    class function GetRegExEncoding: IBase64Alphabet; static; inline;
    class function GetUrlEncoding: IBase64Alphabet; static; inline;
    class function GetXmlEncoding: IBase64Alphabet; static; inline;

    class constructor Base64Alphabet();

  public

    property PaddingEnabled: Boolean read GetPaddingEnabled;
    class property Default: IBase64Alphabet read GetDefault;
    class property DefaultNoPadding: IBase64Alphabet read GetDefaultNoPadding;
    class property UrlEncoding: IBase64Alphabet read GetUrlEncoding;
    class property XmlEncoding: IBase64Alphabet read GetXmlEncoding;
    class property RegExEncoding: IBase64Alphabet read GetRegExEncoding;
    class property FileEncoding: IBase64Alphabet read GetFileEncoding;
    constructor Create(const alphabet: String; plusChar, slashChar: Char;
      PaddingEnabled: Boolean);
    destructor Destroy; override;
  end;

implementation

{ TBase64Alphabet }

class constructor TBase64Alphabet.Base64Alphabet;
begin
  FDefault := TBase64Alphabet.Create(B64CharacterSet, '+', '/', true);
  FDefaultNoPadding := TBase64Alphabet.Create(B64CharacterSet, '+', '/', false);
  FUrlEncoding := TBase64Alphabet.Create(B64CharacterSet, '-', '_', false);
  FXmlEncoding := TBase64Alphabet.Create(B64CharacterSet, '_', ':', false);
  FRegExEncoding := TBase64Alphabet.Create(B64CharacterSet, '!', '-', false);
  FFileEncoding := TBase64Alphabet.Create(B64CharacterSet, '+', '-', false);
end;

constructor TBase64Alphabet.Create(const alphabet: String;
  plusChar, slashChar: Char; PaddingEnabled: Boolean);
var
  newChars: String;
begin
  newChars := alphabet + plusChar + slashChar;
  Inherited Create(64, newChars);

  FPaddingEnabled := PaddingEnabled;
end;

destructor TBase64Alphabet.Destroy;
begin
  inherited Destroy;
end;

class function TBase64Alphabet.GetDefault: IBase64Alphabet;
begin
  Result := FDefault;
end;

class function TBase64Alphabet.GetDefaultNoPadding: IBase64Alphabet;
begin
  Result := FDefaultNoPadding;
end;

class function TBase64Alphabet.GetFileEncoding: IBase64Alphabet;
begin
  Result := FFileEncoding;
end;

function TBase64Alphabet.GetPaddingEnabled: Boolean;
begin
  Result := FPaddingEnabled;
end;

class function TBase64Alphabet.GetRegExEncoding: IBase64Alphabet;
begin
  Result := FRegExEncoding;
end;

class function TBase64Alphabet.GetUrlEncoding: IBase64Alphabet;
begin
  Result := FUrlEncoding;
end;

class function TBase64Alphabet.GetXmlEncoding: IBase64Alphabet;
begin
  Result := FXmlEncoding;
end;

end.
