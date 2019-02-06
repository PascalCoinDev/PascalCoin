unit SbpIEncodingAlphabet;

{$I SimpleBaseLib.inc}

interface

uses
  SbpSimpleBaseLibTypes;

type
  IEncodingAlphabet = interface(IInterface)
    ['{EE7F48BB-653B-475D-9BD4-E8BA7DC5A651}']
    function GetLength: Int32;
    function GetValue: String;
    function GetReverseLookupTable: TSimpleBaseLibByteArray;

    procedure InvalidCharacter(c: Char);
    procedure Map(c: Char; value: Int32);

    function ToString(): String;
    property length: Int32 read GetLength;
    property value: String read GetValue;
    property ReverseLookupTable: TSimpleBaseLibByteArray
      read GetReverseLookupTable;

  end;

implementation

end.
