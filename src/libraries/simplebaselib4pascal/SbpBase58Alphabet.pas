unit SbpBase58Alphabet;

{$I SimpleBaseLib.inc}

interface

uses
  SbpEncodingAlphabet,
  SbpIBase58Alphabet;

type
  TBase58Alphabet = class sealed(TEncodingAlphabet, IBase58Alphabet)

  strict private

    class var

      FBitCoin, FRipple, FFlickr: IBase58Alphabet;

    class function GetBitCoin: IBase58Alphabet; static; inline;
    class function GetFlickr: IBase58Alphabet; static; inline;
    class function GetRipple: IBase58Alphabet; static; inline;

    class constructor Base58Alphabet();
  public
    class property BitCoin: IBase58Alphabet read GetBitCoin;
    class property Ripple: IBase58Alphabet read GetRipple;
    class property Flickr: IBase58Alphabet read GetFlickr;
    constructor Create(const alphabet: String);
    destructor Destroy; override;
  end;

implementation

{ TBase58Alphabet }

class constructor TBase58Alphabet.Base58Alphabet;
begin
  FBitCoin := TBase58Alphabet.Create
    ('123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz');
  FRipple := TBase58Alphabet.Create
    ('rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz');
  FFlickr := TBase58Alphabet.Create
    ('123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ');
end;

constructor TBase58Alphabet.Create(const alphabet: String);
begin
  Inherited Create(58, alphabet);
end;

destructor TBase58Alphabet.Destroy;
begin
  inherited Destroy;
end;

class function TBase58Alphabet.GetBitCoin: IBase58Alphabet;
begin
  result := FBitCoin;
end;

class function TBase58Alphabet.GetFlickr: IBase58Alphabet;
begin
  result := FFlickr;
end;

class function TBase58Alphabet.GetRipple: IBase58Alphabet;
begin
  result := FRipple;
end;

end.
