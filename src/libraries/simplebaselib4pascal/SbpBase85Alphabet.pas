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
    No_Shortcut = Char($FFFF);

  var
    FAllZeroShortcut, FAllSpaceShortcut: Char;

    class var

      FZ85, FAscii85: IBase85Alphabet;

    function GetHasShortcut: Boolean; inline;
    function GetAllSpaceShortcut: Char; inline;
    function GetAllZeroShortcut: Char; inline;

    class function GetNoShortcut: Char; static; inline;
    class function GetZ85: IBase85Alphabet; static; inline;
    class function GetAscii85: IBase85Alphabet; static; inline;

    class constructor Base85Alphabet();

  public

    property HasShortcut: Boolean read GetHasShortcut;
    property AllZeroShortcut: Char read GetAllZeroShortcut;
    property AllSpaceShortcut: Char read GetAllSpaceShortcut;

    class property NoShortcut: Char read GetNoShortcut;

    /// <summary>
    /// ZeroMQ Z85 Alphabet
    /// </summary>
    class property Z85: IBase85Alphabet read GetZ85;

    /// <summary>
    /// Adobe Ascii85 Alphabet (each character is directly produced by raw
    /// value + 33)
    /// </summary>
    class property Ascii85: IBase85Alphabet read GetAscii85;

    constructor Create(const alphabet: String;
      AllZeroShortcut: Char = No_Shortcut;
      AllSpaceShortcut: Char = No_Shortcut);
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

class function TBase85Alphabet.GetNoShortcut: Char;
begin
  Result := No_Shortcut;
end;

function TBase85Alphabet.GetHasShortcut: Boolean;
begin
  Result := (AllSpaceShortcut <> NoShortcut) or (AllZeroShortcut <> NoShortcut);
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
