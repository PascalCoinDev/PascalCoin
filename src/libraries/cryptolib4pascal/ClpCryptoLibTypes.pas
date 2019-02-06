{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpCryptoLibTypes;

{$I CryptoLib.inc}

interface

uses
  SysUtils;

type

  PIInterface = ^IInterface;
{$IFDEF FPC}
  PUInt64 = ^UInt64;
{$ENDIF FPC}
  ECryptoLibException = class(Exception);
  EInvalidCastCryptoLibException = class(EInvalidCast);
  EArithmeticCryptoLibException = class(ECryptoLibException);
  EInvalidOperationCryptoLibException = class(ECryptoLibException);
  EInvalidParameterCryptoLibException = class(ECryptoLibException);
  EIndexOutOfRangeCryptoLibException = class(ECryptoLibException);
  EArgumentCryptoLibException = class(ECryptoLibException);
  EArgumentNilCryptoLibException = class(ECryptoLibException);
  EArgumentOutOfRangeCryptoLibException = class(ECryptoLibException);
  ENullReferenceCryptoLibException = class(ECryptoLibException);
  EUnsupportedTypeCryptoLibException = class(ECryptoLibException);
  EIOCryptoLibException = class(ECryptoLibException);
  EFormatCryptoLibException = class(ECryptoLibException);
  ENotImplementedCryptoLibException = class(ECryptoLibException);
  ENotSupportedCryptoLibException = class(ECryptoLibException);
  EEndOfStreamCryptoLibException = class(EIOCryptoLibException);
  EStreamOverflowCryptoLibException = class(ECryptoLibException);
  EAsn1CryptoLibException = class(ECryptoLibException);
  EAsn1ParsingCryptoLibException = class(ECryptoLibException);
  EInvalidKeyCryptoLibException = class(ECryptoLibException);
  EInvalidCipherTextCryptoLibException = class(ECryptoLibException);
  EStreamCryptoLibException = class(ECryptoLibException);
  ESecurityUtilityCryptoLibException = class(ECryptoLibException);
  EAccessCryptoLibException = class(ECryptoLibException);
  EDataLengthCryptoLibException = class(ECryptoLibException);
  EOutputLengthCryptoLibException = class(ECryptoLibException);
  EBadBlockCryptoLibException = class(ECryptoLibException);

  /// <summary>
  /// Represents a dynamic array of Byte.
  /// </summary>
  TCryptoLibByteArray = TBytes;

  /// <summary>
  /// Represents a dynamic generic array of Type T.
  /// </summary>
  TCryptoLibGenericArray<T> = array of T;

  /// <summary>
  /// Represents a dynamic generic array of array of Type T.
  /// </summary>
  TCryptoLibMatrixGenericArray<T> = array of TCryptoLibGenericArray<T>;

{$IFDEF DELPHIXE_UP}
  /// <summary>
  /// Represents a dynamic array of Boolean.
  /// </summary>
  TCryptoLibBooleanArray = TArray<Boolean>;

  /// <summary>
  /// Represents a dynamic array of ShortInt.
  /// </summary>
  TCryptoLibShortIntArray = TArray<ShortInt>;

  /// <summary>
  /// Represents a dynamic array of Int32.
  /// </summary>
  TCryptoLibInt32Array = TArray<Int32>;

  /// <summary>
  /// Represents a dynamic array of Int64.
  /// </summary>
  TCryptoLibInt64Array = TArray<Int64>;

  /// <summary>
  /// Represents a dynamic array of UInt32.
  /// </summary>
  TCryptoLibUInt32Array = TArray<UInt32>;

  /// <summary>
  /// Represents a dynamic array of UInt64.
  /// </summary>
  TCryptoLibUInt64Array = TArray<UInt64>;

  /// <summary>
  /// Represents a dynamic array of String.
  /// </summary>
  TCryptoLibStringArray = TArray<String>;

  /// <summary>
  /// Represents a dynamic array of Char.
  /// </summary>
  TCryptoLibCharArray = TArray<Char>;

  /// <summary>
  /// Represents a dynamic array of array of ShortInt.
  /// </summary>
  TCryptoLibMatrixShortIntArray = TArray<TCryptoLibShortIntArray>;

  /// <summary>
  /// Represents a dynamic array of array of byte.
  /// </summary>
  TCryptoLibMatrixByteArray = TArray<TCryptoLibByteArray>;

  /// <summary>
  /// Represents a dynamic array of array of Int32.
  /// </summary>
  TCryptoLibMatrixInt32Array = TArray<TCryptoLibInt32Array>;

  /// <summary>
  /// Represents a dynamic array of array of UInt32.
  /// </summary>
  TCryptoLibMatrixUInt32Array = TArray<TCryptoLibUInt32Array>;

  /// <summary>
  /// Represents a dynamic array of array of UInt64.
  /// </summary>
  TCryptoLibMatrixUInt64Array = TArray<TCryptoLibUInt64Array>;

{$ELSE}
  /// <summary>
  /// Represents a dynamic array of Boolean.
  /// </summary>
  TCryptoLibBooleanArray = array of Boolean;

  /// <summary>
  /// Represents a dynamic array of ShortInt.
  /// </summary>
  TCryptoLibShortIntArray = array of ShortInt;

  /// <summary>
  /// Represents a dynamic array of Int32.
  /// </summary>
  TCryptoLibInt32Array = array of Int32;

  /// <summary>
  /// Represents a dynamic array of Int64.
  /// </summary>
  TCryptoLibInt64Array = array of Int64;

  /// <summary>
  /// Represents a dynamic array of UInt32.
  /// </summary>
  TCryptoLibUInt32Array = array of UInt32;

  /// <summary>
  /// Represents a dynamic array of UInt64.
  /// </summary>
  TCryptoLibUInt64Array = array of UInt64;

  /// <summary>
  /// Represents a dynamic array of String.
  /// </summary>
  TCryptoLibStringArray = array of String;

  /// <summary>
  /// Represents a dynamic array of Char.
  /// </summary>
  TCryptoLibCharArray = array of Char;

  /// <summary>
  /// Represents a dynamic array of array of ShortInt.
  /// </summary>
  TCryptoLibMatrixShortIntArray = array of TCryptoLibShortIntArray;

  /// <summary>
  /// Represents a dynamic array of array of byte.
  /// </summary>
  TCryptoLibMatrixByteArray = array of TCryptoLibByteArray;

  /// <summary>
  /// Represents a dynamic array of array of Int32.
  /// </summary>
  TCryptoLibMatrixInt32Array = array of TCryptoLibInt32Array;

  /// <summary>
  /// Represents a dynamic array of array of UInt32.
  /// </summary>
  TCryptoLibMatrixUInt32Array = array of TCryptoLibUInt32Array;

  /// <summary>
  /// Represents a dynamic array of array of UInt64.
  /// </summary>
  TCryptoLibMatrixUInt64Array = array of TCryptoLibUInt64Array;
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
