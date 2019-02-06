unit SbpIBase85Alphabet;

{$I ..\Include\SimpleBaseLib.inc}

interface

uses
  SbpIEncodingAlphabet;

type
  IBase85Alphabet = interface(IEncodingAlphabet)
    ['{25E8C80E-0521-4CD2-9334-8CBC3002FA4B}']

    function GetHasShortcut: Boolean;
    function GetAllSpaceShortcut: Char;
    function GetAllZeroShortcut: Char;

    property HasShortcut: Boolean read GetHasShortcut;
    property AllZeroShortcut: Char read GetAllZeroShortcut;
    property AllSpaceShortcut: Char read GetAllSpaceShortcut;

  end;

implementation

end.
