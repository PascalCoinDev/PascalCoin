unit SbpEncodingAlphabet;

{$I SimpleBaseLib.inc}

interface

uses
  SbpIEncodingAlphabet,
  SbpSimpleBaseLibTypes;

resourcestring
  SAlphabetNil = '"%s" Cannot Be Nil.';
  SAlphabetParameterMismatch =
    'Required Alphabet Length is "%d" but Provided Alphabet is "%d" Characters Long.';
  SInvalidCharacter = 'Alphabet Contains Character "%s (%d)" Above "%d"';
  SInvalidInput = 'Invalid character value in input "%s"';

type
  TEncodingAlphabet = class abstract(TInterfacedObject, IEncodingAlphabet)

  strict private
  const
    lookupLength = Int32(127);

  var
    FLength: Int32;
    FValue: String;
    /// <summary>
    /// Holds a mapping from character to an actual byte value
    /// The values are held as "value + 1" so a zero would denote "not set"
    /// and would cause an exception.
    /// </summary>
    /// byte[] has no discernible perf impact and saves memory
    FReverseLookupTable: TSimpleBaseLibByteArray;

  strict protected
    constructor Create(length: Int32; const alphabet: String);

    procedure InvalidCharacter(c: Char); inline;
    procedure Map(c: Char; value: Int32); inline;

    function GetLength: Int32; inline;
    function GetValue: String; inline;
    function GetReverseLookupTable: TSimpleBaseLibByteArray; inline;

    property length: Int32 read GetLength;
    property value: String read GetValue;
    property ReverseLookupTable: TSimpleBaseLibByteArray
      read GetReverseLookupTable;

  public
    function ToString(): String; override;

  end;

implementation

{ TEncodingAlphabet }

procedure TEncodingAlphabet.Map(c: Char; value: Int32);
begin
  if (Ord(c) >= lookupLength) then
  begin
    raise EInvalidOperationSimpleBaseLibException.CreateResFmt
      (@SInvalidCharacter, [c, Ord(c), lookupLength]);
  end;
  FReverseLookupTable[Ord(c)] := Byte(value + 1);
end;

constructor TEncodingAlphabet.Create(length: Int32; const alphabet: String);
var
  I, LowPoint: Int32;
begin
  Inherited Create();
  if System.length(alphabet) = 0 then
  begin
    raise EArgumentNilSimpleBaseLibException.CreateResFmt(@SAlphabetNil,
      [alphabet]);
  end;

  if (System.length(alphabet) <> length) then
  begin
    raise EArgumentSimpleBaseLibException.CreateResFmt
      (@SAlphabetParameterMismatch, [length, System.length(alphabet)]);
  end;
  System.SetLength(FReverseLookupTable, lookupLength);
  FLength := length;
  FValue := alphabet;

{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(alphabet);
{$ELSE}
  LowPoint := 1;
{$ENDIF DELPHIXE3_UP}
  for I := LowPoint to length do
  begin
    Map(alphabet[I], I - 1);
  end;
end;

function TEncodingAlphabet.GetLength: Int32;
begin
  result := FLength;
end;

function TEncodingAlphabet.GetReverseLookupTable: TSimpleBaseLibByteArray;
begin
  result := System.Copy(FReverseLookupTable);
end;

function TEncodingAlphabet.GetValue: String;
begin
  result := FValue;
end;

procedure TEncodingAlphabet.InvalidCharacter(c: Char);
begin
  raise EArgumentSimpleBaseLibException.CreateResFmt(@SInvalidInput, [c]);
end;

function TEncodingAlphabet.ToString: String;
begin
  result := value;
end;

end.
