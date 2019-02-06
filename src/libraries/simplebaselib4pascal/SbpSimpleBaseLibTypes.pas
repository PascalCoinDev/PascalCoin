unit SbpSimpleBaseLibTypes;

{$I SimpleBaseLib.inc}

interface

uses
  SysUtils;

type

  ESimpleBaseLibException = class(Exception);
  EArgumentSimpleBaseLibException = class(ESimpleBaseLibException);
  EArgumentNilSimpleBaseLibException = class(ESimpleBaseLibException);
  EInvalidOperationSimpleBaseLibException = class(ESimpleBaseLibException);

  /// <summary>
  /// Represents a dynamic array of Byte.
  /// </summary>
  TSimpleBaseLibByteArray = TBytes;

{$IFDEF DELPHIXE_UP}
  /// <summary>
  /// Represents a dynamic array of Char.
  /// </summary>
  TSimpleBaseLibCharArray = TArray<Char>;

  /// <summary>
  /// Represents a dynamic array of String.
  /// </summary>
  TSimpleBaseLibStringArray = TArray<String>;

  /// <summary>
  /// Represents a dynamic array of array of byte.
  /// </summary>
  TSimpleBaseLibMatrixByteArray = TArray<TSimpleBaseLibByteArray>;

{$ELSE}
  /// <summary>
  /// Represents a dynamic array of Char.
  /// </summary>
  TSimpleBaseLibCharArray = array of Char;

  /// <summary>
  /// Represents a dynamic array of String.
  /// </summary>
  TSimpleBaseLibStringArray = array of String;

  /// <summary>
  /// Represents a dynamic array of array of byte.
  /// </summary>
  TSimpleBaseLibMatrixByteArray = array of TSimpleBaseLibByteArray;

{$ENDIF DELPHIXE_UP}

implementation

{$IFDEF FPC}

initialization

// Set UTF-8 in AnsiStrings, just like Lazarus
SetMultiByteConversionCodePage(CP_UTF8);
// SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
SetMultiByteRTLFileSystemCodePage(CP_UTF8);
{$ENDIF FPC}

end.
