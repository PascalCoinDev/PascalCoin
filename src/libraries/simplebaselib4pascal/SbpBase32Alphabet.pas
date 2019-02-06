unit SbpBase32Alphabet;

{$I ..\Include\SimpleBaseLib.inc}

interface

uses
  SysUtils,
  SbpUtilities,
  SbpEncodingAlphabet,
  SbpSimpleBaseLibTypes,
  SbpIBase32Alphabet;

type
  TBase32Alphabet = class(TEncodingAlphabet, IBase32Alphabet)

  strict private

    class var

      FCrockford, FRfc4648, FExtendedHex: IBase32Alphabet;

    procedure MapAlternate(source, destination: Char); inline;
    procedure MapLowerCaseCounterParts(const alphabet: String); inline;

    class function GetCrockford: IBase32Alphabet; static; inline;
    class function GetRfc4648: IBase32Alphabet; static; inline;
    class function GetExtendedHex: IBase32Alphabet; static; inline;

    class constructor Base32Alphabet();

  public
    class property Crockford: IBase32Alphabet read GetCrockford;
    class property Rfc4648: IBase32Alphabet read GetRfc4648;
    class property ExtendedHex: IBase32Alphabet read GetExtendedHex;
    constructor Create(const alphabet: String);
    constructor CreateCrockford();
    destructor Destroy; override;
  end;

implementation

{ TBase32Alphabet }

procedure TBase32Alphabet.MapAlternate(source, destination: Char);
var
  result: Byte;
begin
  result := ReverseLookupTable[Ord(destination)] - 1;
  Map(source, result);
  Map(TUtilities.LowCase(source), result);
end;

procedure TBase32Alphabet.MapLowerCaseCounterParts(const alphabet: String);
var
  LowPoint, HighPoint, I: Int32;
  c: Char;
begin
{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(alphabet);
  HighPoint := System.High(alphabet);
{$ELSE}
  LowPoint := 1;
  HighPoint := System.Length(alphabet);
{$ENDIF DELPHIXE3_UP}
  for I := LowPoint to HighPoint do
  begin
    c := alphabet[I];
    if TUtilities.IsUpper(c) then
    begin
      Map(TUtilities.LowCase(c), ReverseLookupTable[Ord(c)] - 1);
    end;
  end;
end;

class constructor TBase32Alphabet.Base32Alphabet;
begin
  FCrockford := TBase32Alphabet.CreateCrockford();
  FRfc4648 := TBase32Alphabet.Create('ABCDEFGHIJKLMNOPQRSTUVWXYZ234567');
  FExtendedHex := TBase32Alphabet.Create('0123456789ABCDEFGHIJKLMNOPQRSTUV');
end;

constructor TBase32Alphabet.Create(const alphabet: String);
begin
  Inherited Create(32, alphabet);
  MapLowerCaseCounterParts(alphabet);
end;

constructor TBase32Alphabet.CreateCrockford();
begin
  Create('0123456789ABCDEFGHJKMNPQRSTVWXYZ');
  MapAlternate('O', '0');
  MapAlternate('I', '1');
  MapAlternate('L', '1');
end;

destructor TBase32Alphabet.Destroy;
begin
  inherited Destroy;
end;

class function TBase32Alphabet.GetCrockford: IBase32Alphabet;
begin
  result := TBase32Alphabet.CreateCrockford();
end;

class function TBase32Alphabet.GetExtendedHex: IBase32Alphabet;
begin
  result := FExtendedHex;
end;

class function TBase32Alphabet.GetRfc4648: IBase32Alphabet;
begin
  result := FRfc4648;
end;

end.
