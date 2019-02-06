unit SbpIBase64Alphabet;

{$I SimpleBaseLib.inc}

interface

uses
  SbpIEncodingAlphabet;

type
  IBase64Alphabet = interface(IEncodingAlphabet)
    ['{F8B6C5B1-57F9-4B91-AD50-437B3D3AD0B4}']

    function GetPaddingEnabled: Boolean;
    property PaddingEnabled: Boolean read GetPaddingEnabled;
  end;

implementation

end.
