unit SbpBase85Alphabet;

{$I ..\Include\SimpleBaseLib.inc}

interface

uses
  SbpEncodingAlphabet,
  SbpSimpleBaseLibTypes,
  SbpIBase85Alphabet;

type
  TBase85Alphabet = class(TEncodingAlphabet, IBase85Alphabet)

  strict private

  const
    Null_Char = Char(0);

  var
    FAllZeroShortcut, FAllSpaceShortcut: Char;

    class var

      FZ85, FAscii85: IBase85Alphabet;

    function GetHasShortcut: Boolean; inline;
    function GetAllSpaceShortcut: Char; inline;
    function GetAllZeroShortcut: Char; inline;

    class function GetNullChar: Char; static; inline;

    class function GetZ85: IBase85Alphabet; static; inline;
    class function GetAscii85: IBase85Alphabet; static; inline;

    class constructor Base85Alphabet();

  public

    /// <summary>
    /// Gets a value indicating whether the alphabet uses one of shortcut characters for all spaces
    /// or all zeros.
    /// </summary>
    property HasShortcut: Boolean read GetHasShortcut;

    /// <summary>
    /// Gets the character to be used for "all zeros"
    /// </summary>

    property AllZeroShortcut: Char read GetAllZeroShortcut;
    /// <summary>
    /// Gets the character to be used for "all spaces"
    /// </summary>
    property AllSpaceShortcut: Char read GetAllSpaceShortcut;

    /// <summary>
    /// Gets ZeroMQ Z85 Alphabet
    /// </summary>
    class property Z85: IBase85Alphabet read GetZ85;

    /// <summary>
    /// Get Adobe Ascii85 Alphabet (each character is directly produced by raw
    /// value + 33)
    /// </summary>
    class property Ascii85: IBase85Alphabet read GetAscii85;

    class property NullChar: Char read GetNullChar;

    constructor Create(const alphabet: String;
      AllZeroShortcut: Char = Null_Char; AllSpaceShortcut: Char = Null_Char);
    destructor Destroy; override;
  end;

implementation

{ TBase85Alphabet }

class constructor TBase85Alphabet.Base85Alphabet;
begin
  FZ85 := TBase85Alphabet.Create
    ('0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#');
  FAscii85 := TBase85Alphabet.Create
    ('!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstu',
    'z', 'y');
end;

constructor TBase85Alphabet.Create(const alphabet: String;
  AllZeroShortcut, AllSpaceShortcut: Char);
begin
  Inherited Create(85, alphabet);
  FAllZeroShortcut := AllZeroShortcut;
  FAllSpaceShortcut := AllSpaceShortcut;
end;

destructor TBase85Alphabet.Destroy;
begin
  inherited Destroy;
end;

function TBase85Alphabet.GetAllSpaceShortcut: Char;
begin
  Result := FAllSpaceShortcut;
end;

function TBase85Alphabet.GetAllZeroShortcut: Char;
begin
  Result := FAllZeroShortcut;
end;

class function TBase85Alphabet.GetNullChar: Char;
begin
  Result := Null_Char;
end;

function TBase85Alphabet.GetHasShortcut: Boolean;
begin
  Result := (AllSpaceShortcut <> NullChar) or (AllZeroShortcut <> NullChar);
end;

class function TBase85Alphabet.GetZ85: IBase85Alphabet;
begin
  Result := FZ85;
end;

class function TBase85Alphabet.GetAscii85: IBase85Alphabet;
begin
  Result := FAscii85;
end;

end.
