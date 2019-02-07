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

  end;

implementation

end.
