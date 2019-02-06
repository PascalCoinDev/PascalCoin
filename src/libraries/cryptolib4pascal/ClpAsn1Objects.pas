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

unit ClpAsn1Objects;

{$I CryptoLib.inc}

interface

uses
  Classes,
  Math,
  SyncObjs,
  StrUtils,
  SysUtils,
  Generics.Collections,
  ClpEncoders,
  ClpBits,
  ClpBigInteger,
  ClpArrayUtils,
  ClpStringUtils,
  ClpCryptoLibTypes,
  ClpConverters,
  ClpIAsn1Objects,
  ClpOidTokenizer,
  ClpIOidTokenizer;

resourcestring
  SDataOverflow = 'Data Overflow';
  SCorruptedStreamInvalidTag =
    'Corrupted Stream - Invalid High Tag Number Found';
  SEOFFound = 'EOF Found Inside Tag Value';
  SInvalidEnd = 'EOF Found When Length Expected';
  SInvalidDerLength = 'DER Length More Than 4 Bytes: %d';
  SEndOfStream = 'EOF Found Reading Length';
  SNegativeLength = 'Corrupted Stream - Negative Length Found';
  SOutOfBoundsLength = 'Corrupted stream - Out of Bounds Length Found';
  SUnknownTag = 'Unknown Tag " %d " Encountered';
  SEndOfContent = 'Unexpected End-of-Contents Marker';
  SIndefiniteLength = 'Indefinite Length Primitive Encoding Encountered';
  SUnknownBerObject = 'Unknown BER Object Encountered';
  SCorruptedStream = 'Corrupted Stream Detected: %s';
  SInvalidLength = 'Negative Lengths not Allowed", "Length"';
  SEndOfStreamTwo = 'DEF Length  %d " TObject truncated by " %d';
  SInvalidBufferLength = 'Buffer Length Not Right For Data';
  SMalformedContent = 'Malformed End-of-Contents Marker';

  SExtraData = 'Extra Data Found After Object';
  SUnRecognizedObjectStream = 'Cannot Recognise Object in Stream';
  SUnRecognizedObjectByteArray = 'Cannot Recognise Object in ByteArray';
  SIllegalObject = 'Illegal Object in GetInstance:  %s, "obj"';
  SStrNil = '"Str" Cannot be Nil';
  SProcessingError = 'Error Processing Object : "%s"';
  SInvalidObject = 'Object Implicit - Explicit Expected.';
  SUnknownObject = 'Unknown object in GetInstance:  %s, "obj"';
  SInvalidSequence = '"Failed to Construct Sequence from byte array: " %s';
  SImplicitObject = 'Implicitly Tagged Object';
  SImplicitTag = 'Implicit Tagging for Tag:  %d';
  SUnknownObjectBER = 'Unknown BER Object Encountered: $%x';
  SImplicitTagging = 'Implicit Tagging not Implemented';
  SUnConstructedEncoding =
    'Sequences Must Use Constructed Encoding (see X.690 8.9.1/8.10.1)';
  SUnConstructedEncoding2 =
    'Sets Must Use Constructed Encoding (see X.690 8.11.1/8.12.1)';
  SMalformedObject = 'Malformed Object %s';
  SUnSupportedTag = 'Unsupported Tag Number';
  SConvertError = 'EIOCryptoLibException Converting Stream to Byte Array: %s';
  SEncodingError = 'Encoding Error in GetInstance:  %s  "obj"';
  SDataNil = '"data"';
  SInvalidRange = 'Must be in the Range 0 to 7", "padBits"';
  SPadBitError = 'If "data" is Empty, "padBits" Must be 0';
  SUnalignedData = 'Attempt to Get non-octet Aligned Data from BIT STRING"';
  STruncatedBitString = 'Truncated BIT STRING Detected", "octets"';
  SNotImplemented = 'Not Implemented %s';
  SUnConstructedTag = 'Explicit Tags Must be Constructed (see X.690 8.14.2)';
  SParsingError = '%s';
  SEmptyInput = 'Input Cannot be Empty "astr"';
  SInvalidValue = 'Byte Value Should Have 1 Byte in it'', "val"';
  SInvalidBooleanValue = 'BOOLEAN Value Should Have 1 Byte in it", "Value"';
  SMalformedEnumerated = 'Malformed Enumerated';
  SZeroLength = 'Enumerated has Zero Length, "enc"';
  SInvalidEncoding = 'Invalid Encoding Value: %d';
  SFewObject = 'Too Few Objects in Input Vector, "v"';
  SVectorTooLarge = 'Input Vector too Large", "vector"';
  SNoTaggedObjectFound =
    'No Tagged Object Found in Vector. Structure Doesn ''t Seem to be of Type External, "Vector"';
  SInvalidEncodingValue = 'Invalid Encoding Value';
  SObjectNil = ' "obj" Can''t be Nil';
  SValueNil = ' "value" Can''t be Nil';
  SMalformedInteger = 'Malformed Integer';
  SIdentifierNil = 'Identifier Cannot be Empty';
  SInvalidOID = '"String " %s is " not an OID"';
  SInvalidBranchId = '"String " %s " not a valid OID branch", "branchID"';
  SIllegalCharacters = 'String Contains Illegal Characters "str"';

  // ** Start Stream Operations ** //

type
  TStreamHelper = class helper for TStream

  public

    function ReadByte(): Int32;
    procedure WriteByte(b: Byte); inline;
  end;

type
  TStreamSorter = class sealed(TObject)

  public

    class function Read(input: TStream; var buffer: TCryptoLibByteArray;
      offset, count: Int32): Int32; static;
    class function ReadByte(input: TStream): Int32; static;
  end;

type
  TStreamUtils = class sealed(TObject)

  strict private
  const
    BufferSize = Int32(512);

  public

    class procedure Drain(const inStr: TStream); static;
    class function ReadAll(const inStr: TStream): TCryptoLibByteArray;
      static; inline;
    class function ReadAllLimited(const inStr: TStream; limit: Int32)
      : TCryptoLibByteArray; static; inline;
    class function ReadFully(const inStr: TStream; var buf: TCryptoLibByteArray)
      : Int32; overload; static; inline;
    class function ReadFully(const inStr: TStream; var buf: TCryptoLibByteArray;
      off, len: Int32): Int32; overload; static;
    class procedure PipeAll(const inStr, outStr: TStream); static;
    /// <summary>
    /// Pipe all bytes from <c>inStr</c> to <c>outStr</c>, throwing <c>
    /// EStreamOverflowCryptoLibException</c> if greater than <c>limit</c> bytes in <c>
    /// inStr</c>.
    /// </summary>
    /// <param name="inStr">
    /// Input Stream
    /// </param>
    /// <param name="limit">
    /// Limit
    /// </param>
    /// <param name="outStr">
    /// Output Stream
    /// </param>
    /// <returns>
    /// The number of bytes actually transferred, if not greater than <c>
    /// limit</c>
    /// </returns>
    /// <exception cref="EStreamOverflowCryptoLibException" />
    class function PipeAllLimited(const inStr: TStream; limit: Int64;
      const outStr: TStream): Int64; static;

    class procedure WriteBufTo(const buf: TMemoryStream; const output: TStream);
      overload; static; inline;

    class function WriteBufTo(const buf: TMemoryStream;
      const output: TCryptoLibByteArray; offset: Int32): Int32; overload;
      static; inline;

    class procedure WriteZeroes(const outStr: TStream; count: Int64); static;

  end;

type
  TBaseInputStream = class abstract(TStream)

{$IFDEF DELPHI}
  private

    function GetPosition: Int64; inline;
    procedure SetPosition(const Pos: Int64); inline;
    procedure SetSize64(const NewSize: Int64); inline;
{$ENDIF DELPHI}
  protected

{$IFDEF FPC}
    function GetPosition: Int64; override;
    procedure SetPosition(const Pos: Int64); override;
    procedure SetSize64(const NewSize: Int64); override;
{$ENDIF FPC}
    function GetSize: Int64; override;
    procedure SetSize(NewSize: LongInt); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;

  public
    function ReadByte: Int32; virtual;

    function Read(var buffer; count: LongInt): LongInt; overload; override;
    function Write(const buffer; count: LongInt): LongInt; overload; override;

    function Read(buffer: TCryptoLibByteArray; offset, count: LongInt)
      : LongInt; overload;
{$IFDEF SUPPORT_TSTREAM_READ_BYTEARRAY_OVERLOAD} override {$ELSE} virtual
{$ENDIF SUPPORT_TSTREAM_READ_BYTEARRAY_OVERLOAD};

    function Write(const buffer: TCryptoLibByteArray; offset, count: LongInt)
      : LongInt; overload; {$IFDEF SUPPORT_TSTREAM_WRITE_BYTEARRAY_OVERLOAD} override {$ELSE} virtual
{$ENDIF SUPPORT_TSTREAM_WRITE_BYTEARRAY_OVERLOAD};

    function Seek(offset: LongInt; Origin: Word): LongInt; overload; override;
    function Seek(const offset: Int64; Origin: TSeekOrigin): Int64;
      overload; override;

{$IFNDEF _FIXINSIGHT_}
    property Size: Int64 read GetSize write SetSize64;
{$ENDIF}
    property Position: Int64 read GetPosition write SetPosition;
  end;

type
  TFilterStream = class(TStream)

  protected
  var
    Fs: TStream;

    function GetPosition: Int64; {$IFDEF FPC} override; {$ENDIF FPC}
    procedure SetPosition(const Value: Int64); {$IFDEF FPC} override;
{$ENDIF FPC}
    function GetSize: Int64; override;

  public
    constructor Create(const s: TStream);

    property Size: Int64 read GetSize;
    property Position: Int64 read GetPosition write SetPosition;

    function Seek(const offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var buffer; count: LongInt): LongInt; override;
    function Write(const buffer; count: LongInt): LongInt; override;

    function ReadByte(): Int32;
    procedure WriteByte(Value: Byte);

  end;

type
  TLimitedInputStream = class abstract(TBaseInputStream)

  strict private
  var
    F_limit: Int32;
  strict protected
  var
    F_in: TStream;

    procedure SetParentEofDetect(&on: Boolean);

  public
    constructor Create(inStream: TStream; limit: Int32);
    function GetRemaining(): Int32; virtual;

  end;

type
  TDefiniteLengthInputStream = class(TLimitedInputStream)

  strict private

  var
    F_originalLength, F_remaining: Int32;

    function GetRemaining: Int32; reintroduce; inline;
    class function GetEmptyBytes: TCryptoLibByteArray; static; inline;

  public

    constructor Create(inStream: TStream; length: Int32);

    function ReadByte(): Int32; override;

    function Read(buf: TCryptoLibByteArray; off, len: LongInt)
      : LongInt; override;

    procedure ReadAllIntoByteArray(var buf: TCryptoLibByteArray);

    function ToArray: TCryptoLibByteArray;

    property Remaining: Int32 read GetRemaining;
    class property EmptyBytes: TCryptoLibByteArray read GetEmptyBytes;

  end;

type

  /// <summary>
  /// a general purpose ASN.1 decoder - note: this class differs from the <br />
  /// others in that it returns null after it has read the last object in <br />
  /// the stream. If an ASN.1 Null is encountered a DerBER Null object is <br />
  /// returned. <br />
  /// </summary>
  TAsn1InputStream = class(TFilterStream)

  strict private

  var
    Flimit: Int32;
    FtmpBuffers: TCryptoLibMatrixByteArray;
    FStream: TStream;

    /// <summary>
    /// build an object given its tag and the number of bytes to construct it
    /// from.
    /// </summary>
    function BuildObject(tag, tagNo, length: Int32): IAsn1Object;

  public

    constructor Create(const inputStream: TStream); overload;

    /// <summary>
    /// Create an ASN1InputStream where no DER object will be longer than
    /// limit.
    /// </summary>
    /// <param name="inputStream">
    /// stream containing ASN.1 encoded data.
    /// </param>
    /// <param name="limit">
    /// maximum size of a DER encoded object.
    /// </param>
    constructor Create(const inputStream: TStream; limit: Int32); overload;

    destructor Destroy(); override;

    /// <summary>
    /// the stream is automatically limited to the length of the input array.
    /// </summary>
    /// <param name="input">
    /// array containing ASN.1 encoded data.
    /// </param>
    constructor Create(const input: TCryptoLibByteArray); overload;

    function ReadObject(): IAsn1Object;

    function BuildEncodableVector(): IAsn1EncodableVector;

    function BuildDerEncodableVector(const dIn: TDefiniteLengthInputStream)
      : IAsn1EncodableVector; virtual;

    function CreateDerSequence(const dIn: TDefiniteLengthInputStream)
      : IDerSequence; virtual;

    function CreateDerSet(const dIn: TDefiniteLengthInputStream)
      : IDerSet; virtual;

    class function FindLimit(const input: TStream): Int32; static;

    class function ReadTagNumber(const s: TStream; tag: Int32): Int32; static;

    class function ReadLength(const s: TStream; limit: Int32): Int32; static;

    class function GetBuffer(const defIn: TDefiniteLengthInputStream;
      const tmpBuffers: TCryptoLibMatrixByteArray): TCryptoLibByteArray;
      static; inline;

    class function CreatePrimitiveDerObject(tagNo: Int32;
      const defIn: TDefiniteLengthInputStream;
      const tmpBuffers: TCryptoLibMatrixByteArray): IAsn1Object; static;
  end;

type
  TDerOutputStream = class(TFilterStream)

  strict private
    procedure WriteLength(length: Int32);

  strict protected
    procedure WriteNull();

  public
    constructor Create(const os: TStream);
    procedure WriteEncoded(tag: Int32;
      const bytes: TCryptoLibByteArray); overload;
    procedure WriteEncoded(tag: Int32; first: Byte;
      const bytes: TCryptoLibByteArray); overload;
    procedure WriteEncoded(tag: Int32; const bytes: TCryptoLibByteArray;
      offset, length: Int32); overload;
    procedure WriteEncoded(flags, tagNo: Int32;
      const bytes: TCryptoLibByteArray); overload;
    procedure WriteTag(flags, tagNo: Int32);

    procedure WriteObject(const obj: IAsn1Encodable); overload; virtual;
    procedure WriteObject(const obj: IAsn1Object); overload; virtual;

  end;

type
  TAsn1OutputStream = class sealed(TDerOutputStream)

  public
    constructor Create(os: TStream);

  end;

type
  // TODO Make Obsolete in favour of Asn1OutputStream?
  TBerOutputStream = class sealed(TDerOutputStream)

  public

    constructor Create(os: TStream);

  end;

type
  TConstructedOctetStream = class(TBaseInputStream)

  strict private
  var
    F_parser: IAsn1StreamParser;
    F_first: Boolean;
    F_currentStream: TStream;

  public
    constructor Create(const parser: IAsn1StreamParser);
    function Read(buffer: TCryptoLibByteArray; offset, count: LongInt)
      : LongInt; override;
    function ReadByte(): Int32; override;
  end;

type
  TIndefiniteLengthInputStream = class(TLimitedInputStream)

  strict private
  var
    F_lookAhead: Int32;
    F_eofOn00: Boolean;

    function CheckForEof(): Boolean; inline;
    function RequireByte(): Int32; inline;

  public
    constructor Create(inStream: TStream; limit: Int32);
    procedure SetEofOn00(eofOn00: Boolean);

    function Read(buffer: TCryptoLibByteArray; offset, count: LongInt)
      : LongInt; override;

    function ReadByte(): Int32; override;

  end;

  // ** End Stream Operations ** //

type
  TCollectionUtilities = class sealed(TObject)

  public

    class function ToStructuredString(c: TList<IAsn1Encodable>): String; static;

  end;

type

  TAsn1Encodable = class abstract(TInterfacedObject, IAsn1Encodable,
    IAsn1Convertible)

  public

    const
    Der: String = 'DER';
    Ber: String = 'BER';

    function GetEncoded(): TCryptoLibByteArray; overload;
    function GetEncoded(const encoding: String): TCryptoLibByteArray; overload;

    /// <summary>
    /// Return the DER encoding of the object, null if the DER encoding can
    /// not be made.
    /// </summary>
    /// <returns>
    /// return a DER byte array, null otherwise.
    /// </returns>
    function GetDerEncoded(): TCryptoLibByteArray;

    function Equals(const other: IAsn1Convertible): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    function ToAsn1Object(): IAsn1Object; virtual; abstract;

  end;

type

  TAsn1Object = class abstract(TAsn1Encodable, IAsn1Object)

  strict protected

    function Asn1Equals(const asn1Object: IAsn1Object): Boolean;
      virtual; abstract;

    function Asn1GetHashCode(): Int32; virtual; abstract;

  public
    /// <summary>Create a base ASN.1 object from a byte array.</summary>
    /// <param name="data">The byte array to parse.</param>
    /// <returns>The base ASN.1 object represented by the byte array.</returns>
    /// <exception cref="IOException">
    /// If there is a problem parsing the data, or parsing an object did not exhaust the available data.
    /// </exception>
    class function FromByteArray(const data: TCryptoLibByteArray)
      : IAsn1Object; static;

    /// <summary>Read a base ASN.1 object from a stream.</summary>
    /// <param name="inStr">The stream to parse.</param>
    /// <returns>The base ASN.1 object represented by the byte array.</returns>
    /// <exception cref="IOException">If there is a problem parsing the data.</exception>
    class function FromStream(const inStr: TStream): IAsn1Object; static;

    function ToAsn1Object(): IAsn1Object; override;

    procedure Encode(const derOut: TStream); virtual; abstract;

    function CallAsn1Equals(const obj: IAsn1Object): Boolean;

    function CallAsn1GetHashCode(): Int32;

  end;

type
  TDerObjectIdentifier = class(TAsn1Object, IDerObjectIdentifier)

  strict private

  const
    LONG_LIMIT = Int64((Int64($7FFFFFFFFFFFFFFF) shr 7) - $7F);

  class var

    FLock: TCriticalSection;
    Fcache: array [0 .. 1023] of IDerObjectIdentifier;

  var
    Fidentifier: String;
    Fbody: TCryptoLibByteArray;

    class procedure Boot(); static;
    class constructor CreateDerObjectIdentifier();
    class destructor DestroyDerObjectIdentifier();

    constructor Create(const oid: IDerObjectIdentifier;
      const branchID: String); overload;
    constructor Create(const bytes: TCryptoLibByteArray); overload;

    function GetID: String; inline;

    procedure WriteField(const outputStream: TStream;
      fieldValue: Int64); overload;
    procedure WriteField(const outputStream: TStream;
      const fieldValue: TBigInteger); overload;
    procedure DoOutput(const bOut: TMemoryStream); overload;
    function GetBody(): TCryptoLibByteArray;
    class function IsValidBranchID(const branchID: String; start: Int32)
      : Boolean; static;
    class function IsValidIdentifier(const identifier: String): Boolean; static;
    class function MakeOidStringFromBytes(const bytes: TCryptoLibByteArray)
      : String; static;

  strict protected
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

  public
    // /**
    // * return an Oid from the passed in object
    // *
    // * @exception ArgumentException if the object cannot be converted.
    // */
    class function GetInstance(const obj: TObject): IDerObjectIdentifier;
      overload; static;

    // /**
    // * return an Oid from the passed in byte array
    // */
    class function GetInstance(const obj: TCryptoLibByteArray)
      : IDerObjectIdentifier; overload; static; inline;

    // /**
    // * return an object Identifier from a tagged object.
    // *
    // * @param obj the tagged object holding the object we want
    // * @param explicitly true if the object is meant to be explicitly
    // *              tagged false otherwise.
    // * @exception ArgumentException if the tagged object cannot
    // *               be converted.
    // */
    class function GetInstance(const obj: IAsn1TaggedObject;
      explicitly: Boolean): IDerObjectIdentifier; overload; static; inline;

    class function FromOctetString(const enc: TCryptoLibByteArray)
      : IDerObjectIdentifier; static;

    constructor Create(const identifier: String); overload;

    property ID: String read GetID;

    function Branch(const branchID: String): IDerObjectIdentifier; virtual;

    // /**
    // * Return  true if this oid is an extension of the passed in branch, stem.
    // * @param stem the arc or branch that is a possible parent.
    // * @return  true if the branch is on the passed in stem, false otherwise.
    // */

    function &on(const stem: IDerObjectIdentifier): Boolean; virtual;

    procedure Encode(const derOut: TStream); override;

    function ToString(): String; override;

  end;

type
  TAsn1EncodableVector = class(TInterfacedObject, IAsn1EncodableVector)

  strict private
  var

    Flist: TList<IAsn1Encodable>;

    function GetCount: Int32;
    function GetSelf(Index: Int32): IAsn1Encodable;

  public
    class function FromEnumerable(const e: TList<IAsn1Encodable>)
      : IAsn1EncodableVector; static;

    constructor Create(); overload;
    constructor Create(const v: array of IAsn1Encodable); overload;

    destructor Destroy(); override;

    procedure Add(const objs: array of IAsn1Encodable);

    procedure AddOptional(const objs: array of IAsn1Encodable);

    property Self[Index: Int32]: IAsn1Encodable read GetSelf; default;

    property count: Int32 read GetCount;

    function GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>; virtual;

  end;

type
  TAsn1Generator = class abstract(TInterfacedObject, IAsn1Generator)

  strict private
  var
    F_out: TStream;

  strict protected
    constructor Create(outStream: TStream);
    function GetOut: TStream; inline;
    property &Out: TStream read GetOut;

  public
    procedure AddObject(const obj: IAsn1Encodable); virtual; abstract;

    function GetRawOutputStream(): TStream; virtual; abstract;

    procedure Close(); virtual; abstract;
  end;

type
  /// <summary>
  /// A Null object.
  /// </summary>
  TAsn1Null = class abstract(TAsn1Object, IAsn1Null)

  public

    function ToString(): String; override;

  end;

type
  TAsn1OctetString = class abstract(TAsn1Object, IAsn1OctetString,
    IAsn1OctetStringParser)

  strict private
  var
    FStr: TCryptoLibByteArray;

  strict protected
    function GetStr: TCryptoLibByteArray; inline;
    function GetParser: IAsn1OctetStringParser; inline;
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

  public
    property Str: TCryptoLibByteArray read GetStr;
    property parser: IAsn1OctetStringParser read GetParser;

    /// <summary>
    /// return an Octet string from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want.
    /// </param>
    /// <param name="isExplicit">
    /// explicitly true if the object is meant to be explicitly tagged false
    /// otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IAsn1OctetString; overload; static;
    /// <summary>
    /// return an Octet string from the given object.
    /// </summary>
    /// <param name="obj">
    /// the object we want converted.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IAsn1OctetString;
      overload; static;

    /// <param name="Str">
    /// the octets making up the octet string.
    /// </param>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    constructor Create(const obj: IAsn1Encodable); overload;

    function GetOctetStream(): TStream;

    function GetOctets(): TCryptoLibByteArray; virtual;

    function ToString(): String; override;

  end;

type
  /// <summary>
  /// return an Asn1Sequence from the given object.
  /// </summary>
  TAsn1Sequence = class abstract(TAsn1Object, IAsn1Sequence)

  strict private
  var
    FSeq: TList<IAsn1Encodable>;

  type
    TAsn1SequenceParserImpl = class sealed(TInterfacedObject,
      IAsn1SequenceParserImpl, IAsn1SequenceParser)

    strict private
    var
      Fouter: IAsn1Sequence;
      Fmax, Findex: Int32;

    public
      constructor Create(const outer: IAsn1Sequence);
      function ReadObject(): IAsn1Convertible;
      function ToAsn1Object(): IAsn1Object;

    end;

  strict protected
    function GetCount: Int32; virtual;
    function GetParser: IAsn1SequenceParser; virtual;
    function GetSelf(Index: Integer): IAsn1Encodable; virtual;
    function GetCurrent(const e: IAsn1Encodable): IAsn1Encodable;
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
    procedure AddObject(const obj: IAsn1Encodable); inline;

    constructor Create(capacity: Int32);

  public

    destructor Destroy(); override;

    function ToString(): String; override;

    function GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>; virtual;

    // /**
    // * return the object at the sequence position indicated by index.
    // *
    // * @param index the sequence number (starting at zero) of the object
    // * @return the object at the sequence position indicated by index.
    // */
    property Self[Index: Int32]: IAsn1Encodable read GetSelf; default;

    /// <summary>
    /// return an Asn1Sequence from the given object.
    /// </summary>
    /// <param name="obj">
    /// the object we want converted.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IAsn1Sequence;
      overload; static;

    /// <summary>
    /// return an Asn1Sequence from the given object.
    /// </summary>
    /// <param name="obj">
    /// the byte array we want converted.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TCryptoLibByteArray): IAsn1Sequence;
      overload; static;

    // /**
    // * Return an ASN1 sequence from a tagged object. There is a special
    // * case here, if an object appears to have been explicitly tagged on
    // * reading but we were expecting it to be implicitly tagged in the
    // * normal course of events it indicates that we lost the surrounding
    // * sequence - so we need to add it back (this will happen if the tagged
    // * object is a sequence that contains other sequences). If you are
    // * dealing with implicitly tagged sequences you really <b>should</b>
    // * be using this method.
    // *
    // * @param obj the tagged object.
    // * @param explicitly true if the object is meant to be explicitly tagged,
    // *          false otherwise.
    // * @exception ArgumentException if the tagged object cannot
    // *          be converted.
    // */
    class function GetInstance(const obj: IAsn1TaggedObject;
      explicitly: Boolean): IAsn1Sequence; overload; static;

    property parser: IAsn1SequenceParser read GetParser;
    property count: Int32 read GetCount;

  end;

type
  TDerOctetString = class(TAsn1OctetString, IDerOctetString)

  public
    /// <param name="str">The octets making up the octet string.</param>
    constructor Create(const Str: TCryptoLibByteArray); overload;
    constructor Create(const obj: IAsn1Encodable); overload;

    destructor Destroy(); override;

    procedure Encode(const derOut: TStream); overload; override;
    class procedure Encode(const derOut: TDerOutputStream;
      const bytes: TCryptoLibByteArray; offset, length: Int32); reintroduce;
      overload; static; inline;

  end;

type
  TBerOctetString = class(TDerOctetString, IBerOctetString)

  strict private
  const
    MaxLength = Int32(1000);

  var
    Focts: TList<IDerOctetString>;

    function GenerateOcts(): TList<IDerOctetString>;

    class function ToBytes(octs: TList<IDerOctetString>)
      : TCryptoLibByteArray; static;

  public

    /// <inheritdoc />
    /// <param name="str">The octets making up the octet string.</param>
    constructor Create(const Str: TCryptoLibByteArray); overload;
    constructor Create(const octets: TList<IDerOctetString>); overload;
    constructor Create(const obj: IAsn1Object); overload;
    constructor Create(const obj: IAsn1Encodable); overload;

    destructor Destroy(); override;

    function GetOctets(): TCryptoLibByteArray; override;

    /// <summary>
    /// return the DER octets that make up this string.
    /// </summary>
    function GetEnumerable: TCryptoLibGenericArray<IDerOctetString>; virtual;

    procedure Encode(const derOut: TStream); override;

    class function FromSequence(const seq: IAsn1Sequence)
      : IBerOctetString; static;

  end;

type

  /// <summary>
  /// A Null object.
  /// </summary>
  TDerNull = class(TAsn1Null, IDerNull)

  strict private

    class function GetInstance: IDerNull; static; inline;

  const
    ZeroBytes: TCryptoLibByteArray = Nil;

  strict protected
    constructor Create(dummy: Int32);
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
    function Asn1GetHashCode(): Int32; override;

  public

    procedure Encode(const derOut: TStream); override;
    class property Instance: IDerNull read GetInstance;

  end;

type
  TDerSequence = class(TAsn1Sequence, IDerSequence)

  strict private

    class function GetEmpty: IDerSequence; static; inline;

  public

    class function FromVector(const v: IAsn1EncodableVector)
      : IDerSequence; static;

    /// <summary>
    /// create an empty sequence
    /// </summary>
    constructor Create(); overload;

    /// <summary>
    /// create a sequence containing one object
    /// </summary>
    constructor Create(const obj: IAsn1Encodable); overload;

    constructor Create(const v: array of IAsn1Encodable); overload;

    /// <summary>
    /// create a sequence containing a vector of objects.
    /// </summary>
    constructor Create(const v: IAsn1EncodableVector); overload;

    destructor Destroy(); override;

    /// <summary>
    /// A note on the implementation: <br />As Der requires the constructed,
    /// definite-length model to <br />be used for structured types, this
    /// varies slightly from the <br />ASN.1 descriptions given. Rather than
    /// just outputing Sequence, <br />we also have to specify Constructed,
    /// and the objects length. <br />
    /// </summary>
    procedure Encode(const derOut: TStream); override;

    class property Empty: IDerSequence read GetEmpty;

  end;

type
  TBerSequence = class(TDerSequence, IBerSequence)

  strict private

    class function GetEmpty: IBerSequence; static; inline;

  public

    class function FromVector(const v: IAsn1EncodableVector)
      : IBerSequence; static;

    /// <summary>
    /// create an empty sequence
    /// </summary>
    constructor Create(); overload;

    /// <summary>
    /// create a sequence containing one object
    /// </summary>
    constructor Create(const obj: IAsn1Encodable); overload;

    constructor Create(const v: array of IAsn1Encodable); overload;

    /// <summary>
    /// create a sequence containing a vector of objects.
    /// </summary>
    constructor Create(const v: IAsn1EncodableVector); overload;

    destructor Destroy(); override;

    /// <summary>
    /// A note on the implementation: <br />As Der requires the constructed,
    /// definite-length model to <br />be used for structured types, this
    /// varies slightly from the <br />ASN.1 descriptions given. Rather than
    /// just outputing Sequence, <br />we also have to specify Constructed,
    /// and the objects length. <br />
    /// </summary>
    procedure Encode(const derOut: TStream); override;

    class property Empty: IBerSequence read GetEmpty;

  end;

type
  /// **
  // * ASN.1 TaggedObject - in ASN.1 notation this is any object preceded by
  // * a [n] where n is some number - these are assumed to follow the construction
  // * rules (as with sequences).
  // */
  TAsn1TaggedObject = class abstract(TAsn1Object, IAsn1TaggedObject,
    IAsn1TaggedObjectParser)

  strict private
    FtagNo: Int32;
    Fexplicitly: Boolean;
    Fobj: IAsn1Encodable;

  strict protected
    // /**
    // * @param tagNo the tag number for this object.
    // * @param obj the tagged object.
    // */
    constructor Create(tagNo: Int32; const obj: IAsn1Encodable); overload;
    // /**
    // * @param explicitly true if the object is explicitly tagged.
    // * @param tagNo the tag number for this object.
    // * @param obj the tagged object.
    // */
    constructor Create(explicitly: Boolean; tagNo: Int32;
      const obj: IAsn1Encodable); overload;

    function GetTagNo: Int32; inline;
    function Getexplicitly: Boolean; inline;
    function Getobj: IAsn1Encodable; inline;

    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

    function Asn1GetHashCode(): Int32; override;

  public
    class function IsConstructed(isExplicit: Boolean; const obj: IAsn1Object)
      : Boolean; static;
    class function GetInstance(const obj: IAsn1TaggedObject;
      explicitly: Boolean): IAsn1TaggedObject; overload; static; inline;
    class function GetInstance(obj: TObject): IAsn1TaggedObject; overload;
      static; inline;

    property tagNo: Int32 read GetTagNo;
    property explicitly: Boolean read Getexplicitly;
    property obj: IAsn1Encodable read Getobj;

    // /**
    // * return whether or not the object may be explicitly tagged.
    // * <p>
    // * Note: if the object has been read from an input stream, the only
    // * time you can be sure if isExplicit is returning the true state of
    // * affairs is if it returns false. An implicitly tagged object may appear
    // * to be explicitly tagged, so you need to understand the context under
    // * which the reading was done as well, see GetObject below.</p>
    // */

    function isExplicit(): Boolean; inline;

    function IsEmpty(): Boolean; inline;
    // /**
    // * return whatever was following the tag.
    // * <p>
    // * Note: tagged objects are generally context dependent if you're
    // * trying to extract a tagged object you should be going via the
    // * appropriate GetInstance method.</p>
    // */
    function GetObject(): IAsn1Object; inline;
    // /**
    // * Return the object held in this tagged object as a parser assuming it has
    // * the type of the passed in tag. If the object doesn't have a parser
    // * associated with it, the base object is returned.
    // */
    function GetObjectParser(tag: Int32; isExplicit: Boolean): IAsn1Convertible;

    function ToString(): String; override;

  end;

type
  TAsn1Tags = class sealed(TObject)

  public

    const
    &Boolean = Int32($01);
    &Integer = Int32($02);
    BitString = Int32($03);
    OctetString = Int32($04);
    Null = Int32($05);
    ObjectIdentifier = Int32($06);
    &External = Int32($08);
    Enumerated = Int32($0A);
    Sequence = Int32($10);
    SequenceOf = Int32($10); // for completeness
    &Set = Int32($11);
    SetOf = Int32($11); // for completeness

    NumericString = Int32($12);
    PrintableString = Int32($13);
    T61String = Int32($14);
    VideotexString = Int32($15);
    IA5String = Int32($16);
    UtcTime = Int32($17);
    GeneralizedTime = Int32($18);
    GraphicString = Int32($19);
    VisibleString = Int32($1A);
    GeneralString = Int32($1B);
    UniversalString = Int32($1C);
    BmpString = Int32($1E);
    Utf8String = Int32($0C);

    Constructed = Int32($20);
    Application = Int32($40);
    Tagged = Int32($80);
  end;

type
  /// <summary>
  /// return an Asn1Set from the given object.
  /// </summary>
  TAsn1Set = class abstract(TAsn1Object, IAsn1Set)

  strict private
  var
    F_set: TList<IAsn1Encodable>;
    FisSorted: Boolean;

    /// <summary>
    /// return true if a &lt;= b (arrays are assumed padded with zeros).
    /// </summary>
    function LessThanOrEqual(const a, b: TCryptoLibByteArray): Boolean; inline;

  type
    TAsn1SetParserImpl = class sealed(TInterfacedObject, IAsn1SetParserImpl,
      IAsn1SetParser)

    strict private
      Fouter: IAsn1Set;
      Fmax, Findex: Int32;

    public
      constructor Create(const outer: IAsn1Set);
      function ReadObject(): IAsn1Convertible;
      function ToAsn1Object(): IAsn1Object;

    end;

  strict protected
    function GetCount: Int32; virtual;
    function GetParser: IAsn1SetParser; inline;
    function GetSelf(Index: Integer): IAsn1Encodable; virtual;
    function GetCurrent(const e: IAsn1Encodable): IAsn1Encodable;
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
    procedure AddObject(const obj: IAsn1Encodable); inline;
    procedure Sort();

    constructor Create(capacity: Int32);

  public
    destructor Destroy(); override;

  public

    function ToString(): String; override;

    function ToArray(): TCryptoLibGenericArray<IAsn1Encodable>; virtual;

    function GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>; virtual;

    // /**
    // * return the object at the sequence position indicated by index.
    // *
    // * @param index the sequence number (starting at zero) of the object
    // * @return the object at the sequence position indicated by index.
    // */
    property Self[Index: Int32]: IAsn1Encodable read GetSelf; default;

    /// <summary>
    /// return an ASN1Set from the given object.
    /// </summary>
    /// <param name="obj">
    /// the object we want converted.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IAsn1Set; overload; static;

    /// <summary>
    /// return an Asn1Set from the given object.
    /// </summary>
    /// <param name="obj">
    /// the byte array we want converted.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TCryptoLibByteArray): IAsn1Set;
      overload; static;

    // /**
    // * Return an ASN1 sequence from a tagged object. There is a special
    // * case here, if an object appears to have been explicitly tagged on
    // * reading but we were expecting it to be implicitly tagged in the
    // * normal course of events it indicates that we lost the surrounding
    // * sequence - so we need to add it back (this will happen if the tagged
    // * object is a sequence that contains other sequences). If you are
    // * dealing with implicitly tagged sequences you really <b>should</b>
    // * be using this method.
    // *
    // * @param obj the tagged object.
    // * @param explicitly true if the object is meant to be explicitly tagged,
    // *          false otherwise.
    // * @exception ArgumentException if the tagged object cannot
    // *          be converted.
    // */
    class function GetInstance(const obj: IAsn1TaggedObject;
      explicitly: Boolean): IAsn1Set; overload; static;

    property parser: IAsn1SetParser read GetParser;
    property count: Int32 read GetCount;

  end;

type

  /// <summary>
  /// A Der encoded set object
  /// </summary>
  TDerSet = class(TAsn1Set, IDerSet)

  strict private
    class function GetEmpty: IDerSet; static; inline;

  public

    class function FromVector(const v: IAsn1EncodableVector): IDerSet;
      overload; static;
    class function FromVector(const v: IAsn1EncodableVector;
      needsSorting: Boolean): IDerSet; overload; static;

    /// <summary>
    /// create an empty set
    /// </summary>
    constructor Create(); overload;

    /// <param name="obj">
    /// a single object that makes up the set.
    /// </param>
    constructor Create(const obj: IAsn1Encodable); overload;

    constructor Create(const v: array of IAsn1Encodable); overload;

    /// <param name="v">
    /// a vector of objects making up the set.
    /// </param>
    constructor Create(const v: IAsn1EncodableVector); overload;

    constructor Create(const v: IAsn1EncodableVector;
      needsSorting: Boolean); overload;

    destructor Destroy(); override;

    /// <summary>
    /// A note on the implementation: <br />As Der requires the constructed,
    /// definite-length model to <br />be used for structured types, this
    /// varies slightly from the <br />ASN.1 descriptions given. Rather than
    /// just outputing Set, <br />we also have to specify Constructed, and
    /// the objects length. <br />
    /// </summary>
    procedure Encode(const derOut: TStream); override;

    class property Empty: IDerSet read GetEmpty;

  end;

type
  TAsn1StreamParser = class(TInterfacedObject, IAsn1StreamParser)

  strict private
  var
    F_in: TStream;
    F_limit: Int32;
    FtmpBuffers: TCryptoLibMatrixByteArray;

    procedure Set00Check(enabled: Boolean); inline;

  public
    constructor Create(const inStream: TStream); overload;
    constructor Create(const inStream: TStream; limit: Int32); overload;
    constructor Create(const encoding: TCryptoLibByteArray); overload;

    destructor Destroy; override;

    function ReadIndef(tagValue: Int32): IAsn1Convertible;
    function ReadImplicit(Constructed: Boolean; tag: Int32): IAsn1Convertible;

    function ReadTaggedObject(Constructed: Boolean; tag: Int32): IAsn1Object;

    function ReadObject(): IAsn1Convertible; virtual;

    function ReadVector(): IAsn1EncodableVector; inline;
  end;

type
  TDerSetParser = class(TInterfacedObject, IAsn1SetParser, IAsn1Convertible,
    IDerSetParser)

  strict private
  var
    F_parser: IAsn1StreamParser;

  public

    constructor Create(const parser: IAsn1StreamParser);
    function ReadObject(): IAsn1Convertible; inline;
    function ToAsn1Object(): IAsn1Object; inline;

  end;

type
  TDerSequenceParser = class(TInterfacedObject, IAsn1SequenceParser,
    IAsn1Convertible, IDerSequenceParser)

  strict private
  var
    F_parser: IAsn1StreamParser;

  public

    constructor Create(const parser: IAsn1StreamParser);
    function ReadObject(): IAsn1Convertible; inline;
    function ToAsn1Object(): IAsn1Object; inline;

  end;

type

  /// <summary>
  /// Base class for an application specific object
  /// </summary>
  TDerApplicationSpecific = class(TAsn1Object, IDerApplicationSpecific)

  strict private
  var
    FisConstructed: Boolean;
    Ftag: Int32;
    Foctets: TCryptoLibByteArray;

    class function ReplaceTagNumber(newTag: Int32;
      const input: TCryptoLibByteArray): TCryptoLibByteArray; static;

  strict protected
    function GetApplicationTag: Int32; inline;
    function GetLengthOfHeader(const data: TCryptoLibByteArray): Int32; inline;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
    function Asn1GetHashCode(): Int32; override;

  public
    constructor Create(IsConstructed: Boolean; tag: Int32;
      const octets: TCryptoLibByteArray); overload;
    constructor Create(tag: Int32; const octets: TCryptoLibByteArray); overload;
    constructor Create(tag: Int32; const obj: IAsn1Encodable); overload;
    constructor Create(isExplicit: Boolean; tag: Int32;
      const obj: IAsn1Encodable); overload;
    constructor Create(tagNo: Int32; const vec: IAsn1EncodableVector); overload;

    function IsConstructed(): Boolean; inline;
    function GetContents(): TCryptoLibByteArray; inline;

    /// <summary>
    /// Return the enclosed object assuming explicit tagging.
    /// </summary>
    /// <returns>
    /// the resulting object
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EIOCryptoLibException">
    /// if reconstruction fails.
    /// </exception>
    function GetObject(): IAsn1Object; overload; inline;

    /// <summary>
    /// Return the enclosed object assuming implicit tagging.
    /// </summary>
    /// <param name="derTagNo">
    /// the type tag that should be applied to the object's contents.
    /// </param>
    /// <returns>
    /// the resulting object
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EIOCryptoLibException">
    /// if reconstruction fails.
    /// </exception>
    function GetObject(derTagNo: Int32): IAsn1Object; overload; inline;

    procedure Encode(const derOut: TStream); override;

    property ApplicationTag: Int32 read GetApplicationTag;
  end;

type
  TBerApplicationSpecific = class(TDerApplicationSpecific,
    IBerApplicationSpecific)

  public
    constructor Create(tagNo: Int32; const vec: IAsn1EncodableVector);

  end;

type
  TBerOctetStringParser = class(TInterfacedObject, IAsn1OctetStringParser,
    IAsn1Convertible, IBerOctetStringParser)

  strict private
  var
    F_parser: IAsn1StreamParser;

  public

    constructor Create(const parser: IAsn1StreamParser);
    function GetOctetStream(): TStream; inline;
    function ToAsn1Object(): IAsn1Object;

  end;

type
  TBerApplicationSpecificParser = class(TInterfacedObject,
    IAsn1ApplicationSpecificParser, IAsn1Convertible,
    IBerApplicationSpecificParser)

  strict private
  var
    F_tag: Int32;
    F_parser: IAsn1StreamParser;

  public

    constructor Create(tag: Int32; const parser: IAsn1StreamParser);
    function ReadObject(): IAsn1Convertible; inline;
    function ToAsn1Object(): IAsn1Object; inline;

  end;

type
  TDerStringBase = class abstract(TAsn1Object, IAsn1String, IDerStringBase)

  strict protected
    constructor Create();
    function Asn1GetHashCode(): Int32; override;
  public
    function GetString(): String; virtual; abstract;
    function ToString(): String; override;
  end;

type

  /// <summary>
  /// Der Bit string object.
  /// </summary>
  TDerBitString = class(TDerStringBase, IDerBitString)

  strict private
  const
    FTable: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

  strict protected
  var
    FmData: TCryptoLibByteArray;
    FmPadBits: Int32;

    function GetmPadBits: Int32; inline;
    function GetmData: TCryptoLibByteArray; inline;

    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

    property mPadBits: Int32 read GetmPadBits;
    property mData: TCryptoLibByteArray read GetmData;
  public

    constructor Create(const data: TCryptoLibByteArray;
      padBits: Int32); overload;

    constructor Create(const data: TCryptoLibByteArray); overload;

    constructor Create(namedBits: Int32); overload;

    constructor Create(const obj: IAsn1Encodable); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; virtual;

    function GetBytes(): TCryptoLibByteArray; virtual;

    procedure Encode(const derOut: TStream); override;

    function GetInt32Value: Int32; virtual;
    property Int32Value: Int32 read GetInt32Value;

    /// <summary>
    /// return a Der Bit string from the passed in object
    /// </summary>
    /// <param name="obj">
    /// a Bit string or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a Der Bit string instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerBitString; overload;
      static; inline;

    class function GetInstance(const obj: TCryptoLibByteArray): IDerBitString;
      overload; static;

    /// <summary>
    /// return a Der Bit string from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerBitString; overload; static; inline;

    class function FromAsn1Octets(const octets: TCryptoLibByteArray)
      : IDerBitString; static;

  end;

type
  TBerBitString = class(TDerBitString, IBerBitString)

  public
    constructor Create(const data: TCryptoLibByteArray;
      padBits: Int32); overload;
    constructor Create(const data: TCryptoLibByteArray); overload;
    constructor Create(namedBits: Int32); overload;
    constructor Create(const obj: IAsn1Encodable); overload;

    procedure Encode(const derOut: TStream); override;

  end;

type
  TBerGenerator = class abstract(TAsn1Generator, IBerGenerator)

  strict private
  var
    F_tagged, F_isExplicit: Boolean;
    F_tagNo: Int32;

  strict protected
    constructor Create(outStream: TStream); overload;
    constructor Create(outStream: TStream; tagNo: Int32;
      isExplicit: Boolean); overload;

    procedure WriteHdr(tag: Int32);
    procedure WriteBerHeader(tag: Int32);
    procedure WriteBerBody(contentStream: TStream);
    procedure WriteBerEnd();

  public
    procedure AddObject(const obj: IAsn1Encodable); override;
    function GetRawOutputStream(): TStream; override;
    procedure Close(); override;

  end;

type

  /// <summary>
  /// A BER Null object.
  /// </summary>
  TBerNull = class sealed(TDerNull, IBerNull)

  strict private

    class function GetInstance: IBerNull; static; inline;

    constructor Create(dummy: Int32);

  public

    procedure Encode(const derOut: TStream); override;
    class property Instance: IBerNull read GetInstance;

  end;

type
  TBerSequenceGenerator = class(TBerGenerator, IBerSequenceGenerator)

  public
    constructor Create(outStream: TStream); overload;
    constructor Create(outStream: TStream; tagNo: Int32;
      isExplicit: Boolean); overload;
  end;

type
  TBerSequenceParser = class(TInterfacedObject, IAsn1SequenceParser,
    IAsn1Convertible, IBerSequenceParser)

  strict private
  var
    F_parser: IAsn1StreamParser;

  public

    constructor Create(const parser: IAsn1StreamParser);
    function ReadObject(): IAsn1Convertible; inline;
    function ToAsn1Object(): IAsn1Object; inline;

  end;

type

  /// <summary>
  /// A Ber encoded set object
  /// </summary>
  TBerSet = class sealed(TDerSet, IBerSet)

  strict private
    class function GetEmpty: IBerSet; static; inline;

  public

    class function FromVector(const v: IAsn1EncodableVector): IBerSet;
      overload; static;
    class function FromVector(const v: IAsn1EncodableVector;
      needsSorting: Boolean): IBerSet; overload; static;

    /// <summary>
    /// create an empty set
    /// </summary>
    constructor Create(); overload;

    /// <param name="obj">
    /// a single object that makes up the set.
    /// </param>
    constructor Create(const obj: IAsn1Encodable); overload;

    /// <param name="v">
    /// a vector of objects making up the set.
    /// </param>
    constructor Create(const v: IAsn1EncodableVector); overload;

    constructor Create(const v: IAsn1EncodableVector;
      needsSorting: Boolean); overload;

    destructor Destroy(); override;

    /// <summary>
    /// A note on the implementation: <br />As Ber requires the constructed,
    /// definite-length model to <br />be used for structured types, this
    /// varies slightly from the <br />ASN.1 descriptions given. Rather than
    /// just outputing Set, <br />we also have to specify Constructed, and
    /// the objects length. <br />
    /// </summary>
    procedure Encode(const derOut: TStream); override;

    class property Empty: IBerSet read GetEmpty;

  end;

type
  TBerSetParser = class(TInterfacedObject, IAsn1SetParser, IAsn1Convertible,
    IBerSetParser)

  strict private
  var
    F_parser: IAsn1StreamParser;

  public

    constructor Create(const parser: IAsn1StreamParser);
    function ReadObject(): IAsn1Convertible; inline;
    function ToAsn1Object(): IAsn1Object; inline;

  end;

type

  /// <summary>
  /// DER TaggedObject - in ASN.1 notation this is any object preceded by <br />
  /// a [n] where n is some number - these are assumed to follow the
  /// construction <br />rules (as with sequences). <br />
  /// </summary>
  TDerTaggedObject = class(TAsn1TaggedObject, IDerTaggedObject)

  public

    /// <param name="tagNo">
    /// the tag number for this object.
    /// </param>
    /// <param name="obj">
    /// the tagged object.
    /// </param>
    constructor Create(tagNo: Int32; const obj: IAsn1Encodable); overload;
    /// <param name="explicitly">
    /// true if an explicitly tagged object.
    /// </param>
    /// <param name="tagNo">
    /// the tag number for this object.
    /// </param>
    /// <param name="obj">
    /// the tagged object.
    /// </param>
    constructor Create(explicitly: Boolean; tagNo: Int32;
      const obj: IAsn1Encodable); overload;

    /// <summary>
    /// create an implicitly tagged object that contains a zero length
    /// sequence.
    /// </summary>
    /// <param name="tagNo">
    /// the tag number for this object.
    /// </param>
    constructor Create(tagNo: Int32); overload;

    procedure Encode(const derOut: TStream); override;

  end;

type

  /// <summary>
  /// BER TaggedObject - in ASN.1 notation this is any object preceded by <br />
  /// a [n] where n is some number - these are assumed to follow the
  /// construction <br />rules (as with sequences). <br />
  /// </summary>
  TBerTaggedObject = class(TDerTaggedObject, IBerTaggedObject)

  public

    /// <param name="tagNo">
    /// the tag number for this object.
    /// </param>
    /// <param name="obj">
    /// the tagged object.
    /// </param>
    constructor Create(tagNo: Int32; const obj: IAsn1Encodable); overload;
    /// <param name="explicitly">
    /// true if an explicitly tagged object.
    /// </param>
    /// <param name="tagNo">
    /// the tag number for this object.
    /// </param>
    /// <param name="obj">
    /// the tagged object.
    /// </param>
    constructor Create(explicitly: Boolean; tagNo: Int32;
      const obj: IAsn1Encodable); overload;

    /// <summary>
    /// create an implicitly tagged object that contains a zero length
    /// sequence.
    /// </summary>
    /// <param name="tagNo">
    /// the tag number for this object.
    /// </param>
    constructor Create(tagNo: Int32); overload;

    procedure Encode(const derOut: TStream); override;

  end;

type
  TBerTaggedObjectParser = class(TInterfacedObject, IAsn1TaggedObjectParser,
    IAsn1Convertible, IBerTaggedObjectParser)

  strict private
  var
    F_constructed: Boolean;
    F_tagNumber: Int32;
    F_parser: IAsn1StreamParser;

    function GetIsConstructed: Boolean; inline;
    function GetTagNo: Int32; inline;

  public
    constructor Create(Constructed: Boolean; tagNumber: Int32;
      const parser: IAsn1StreamParser);

    destructor Destroy; override;

    function GetObjectParser(tag: Int32; isExplicit: Boolean)
      : IAsn1Convertible; inline;

    function ToAsn1Object(): IAsn1Object;

    property IsConstructed: Boolean read GetIsConstructed;
    property tagNo: Int32 read GetTagNo;

  end;

type
  TDerBmpString = class(TDerStringBase, IDerBmpString)

  strict private
  var
    FStr: String;

    function GetStr: String; inline;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

  public
    property Str: String read GetStr;

    /// <summary>
    /// basic constructor - byte encoded string.
    /// </summary>
    constructor Create(const astr: TCryptoLibByteArray); overload;

    /// <summary>
    /// basic constructor
    /// </summary>
    constructor Create(const astr: String); overload;

    function GetString(): String; override;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a BMP string from the given object.
    /// </summary>
    /// <param name="obj">
    /// the object we want converted.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerBmpString; overload;
      static; inline;

    /// <summary>
    /// return a BMP string from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerBmpString; overload; static; inline;

  end;

type
  TDerBoolean = class(TAsn1Object, IDerBoolean)

  strict private
  var

    Fvalue: Byte;

    function GetIsTrue: Boolean; inline;

    constructor Create(Value: Boolean); overload;

    class function GetFalse: IDerBoolean; static; inline;
    class function GetTrue: IDerBoolean; static; inline;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
    function Asn1GetHashCode(): Int32; override;

  public

    constructor Create(const val: TCryptoLibByteArray); overload;

    procedure Encode(const derOut: TStream); override;

    function ToString(): String; override;

    property IsTrue: Boolean read GetIsTrue;

    class property True: IDerBoolean read GetTrue;

    class property False: IDerBoolean read GetFalse;

    /// <summary>
    /// return a DerBoolean from the passed in object.
    /// </summary>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerBoolean; overload;
      static; inline;

    /// <summary>
    /// return a DerBoolean from the passed in boolean.
    /// </summary>
    class function GetInstance(Value: Boolean): IDerBoolean; overload;
      static; inline;

    /// <summary>
    /// return a Boolean from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// explicitly true if the object is meant to be explicitly tagged false
    /// otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerBoolean; overload; static; inline;

    class function FromOctetString(const Value: TCryptoLibByteArray)
      : IDerBoolean; static;

  end;

type
  TDerEnumerated = class(TAsn1Object, IDerEnumerated)

  strict private

    class var

      Fcache: array [0 .. 11] of IDerEnumerated;

  var
    Fbytes: TCryptoLibByteArray;

    function GetValue: TBigInteger; inline;
    function GetBytes: TCryptoLibByteArray; inline;

  strict protected

    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
    function Asn1GetHashCode(): Int32; override;

  public

    constructor Create(val: Int32); overload;
    constructor Create(const val: TBigInteger); overload;
    constructor Create(const bytes: TCryptoLibByteArray); overload;

    procedure Encode(const derOut: TStream); override;

    property Value: TBigInteger read GetValue;
    property bytes: TCryptoLibByteArray read GetBytes;

    /// <summary>
    /// return an integer from the passed in object
    /// </summary>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>

    class function GetInstance(const obj: TObject): IDerEnumerated; overload;
      static; inline;

    /// <summary>
    /// return an Enumerated from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerEnumerated; overload; static; inline;

    class function FromOctetString(const enc: TCryptoLibByteArray)
      : IDerEnumerated; static;

  end;

type
  TDerGraphicString = class(TDerStringBase, IDerGraphicString)

  strict private
  var
    FmString: TCryptoLibByteArray;

    function GetmString: TCryptoLibByteArray; inline;

  protected
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

  public
    property mString: TCryptoLibByteArray read GetmString;

    /// <summary>
    /// basic constructor - with bytes.
    /// </summary>
    /// <param name="encoding">
    /// the byte encoding of the characters making up the string.
    /// </param>
    constructor Create(const encoding: TCryptoLibByteArray);

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a Graphic String from the passed in object
    /// </summary>
    /// <param name="obj">
    /// a DerGraphicString or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a DerGraphicString instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerGraphicString; overload;
      static; inline;

    class function GetInstance(const obj: TCryptoLibByteArray)
      : IDerGraphicString; overload; static;

    /// <summary>
    /// return a Graphic string from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerGraphicString; overload; static; inline;

  end;

type

  /// <summary>
  /// Class representing the DER-type External
  /// </summary>
  TDerExternal = class(TAsn1Object, IDerExternal)

  strict private
  var
    FdirectReference: IDerObjectIdentifier;
    FindirectReference: IDerInteger;
    FdataValueDescriptor, FexternalContent: IAsn1Object;
    Fencoding: Int32;

    function GetDataValueDescriptor: IAsn1Object;
    function GetDirectReference: IDerObjectIdentifier;

    /// <summary>
    /// <para>
    /// The encoding of the content. Valid values are
    /// </para>
    /// <para>
    /// &lt;ul&gt; <br />&lt;li&gt;&lt;code&gt;0&lt;/code&gt;
    /// single-ASN1-type&lt;/li&gt; <br />
    /// &lt;li&gt;&lt;code&gt;1&lt;/code&gt; OCTET STRING&lt;/li&gt; <br />
    /// &lt;li&gt;&lt;code&gt;2&lt;/code&gt; BIT STRING&lt;/li&gt; <br />
    /// &lt;/ul&gt;
    /// </para>
    /// </summary>
    function GetEncoding: Int32;
    function GetExternalContent: IAsn1Object;
    function GetIndirectReference: IDerInteger;
    procedure SetDataValueDescriptor(const Value: IAsn1Object);
    procedure SetDirectReference(const Value: IDerObjectIdentifier);
    procedure SetEncoding(const Value: Int32);
    procedure SetExternalContent(const Value: IAsn1Object);
    procedure SetIndirectReference(const Value: IDerInteger);

    class function GetObjFromVector(const v: IAsn1EncodableVector; Index: Int32)
      : IAsn1Object; static; inline;
    class procedure WriteEncodable(ms: TMemoryStream; const e: IAsn1Encodable);
      static; inline;

  strict protected
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

  public
    constructor Create(const vector: IAsn1EncodableVector); overload;

    /// <summary>
    /// Creates a new instance of DerExternal <br />See X.690 for more
    /// informations about the meaning of these parameters
    /// </summary>
    /// <param name="directReference">
    /// The direct reference or &lt;code&gt;null&lt;/code&gt; if not set.
    /// </param>
    /// <param name="indirectReference">
    /// The indirect reference or &lt;code&gt;null&lt;/code&gt; if not set.
    /// </param>
    /// <param name="dataValueDescriptor">
    /// The data value descriptor or &lt;code&gt;null&lt;/code&gt; if not
    /// set.
    /// </param>
    /// <param name="externalData">
    /// The external data in its encoded form.
    /// </param>
    constructor Create(const directReference: IDerObjectIdentifier;
      const indirectReference: IDerInteger;
      const dataValueDescriptor: IAsn1Object;
      const externalData: IDerTaggedObject); overload;

    constructor Create(const directReference: IDerObjectIdentifier;
      const indirectReference: IDerInteger;
      const dataValueDescriptor: IAsn1Object; encoding: Int32;
      const externalData: IAsn1Object); overload;

    procedure Encode(const derOut: TStream); override;

    property dataValueDescriptor: IAsn1Object read GetDataValueDescriptor
      write SetDataValueDescriptor;

    property directReference: IDerObjectIdentifier read GetDirectReference
      write SetDirectReference;

    property encoding: Int32 read GetEncoding write SetEncoding;

    property ExternalContent: IAsn1Object read GetExternalContent
      write SetExternalContent;

    property indirectReference: IDerInteger read GetIndirectReference
      write SetIndirectReference;

  end;

type
  TDerInteger = class sealed(TAsn1Object, IDerInteger)

  strict private
  var
    Fbytes: TCryptoLibByteArray;

    function GetBytes: TCryptoLibByteArray; inline;
    function GetPositiveValue: TBigInteger; inline;
    function GetValue: TBigInteger; inline;
  strict protected
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

  public

    constructor Create(Value: Int32); overload;
    constructor Create(const Value: TBigInteger); overload;
    constructor Create(const bytes: TCryptoLibByteArray); overload;

    property Value: TBigInteger read GetValue;
    property PositiveValue: TBigInteger read GetPositiveValue;
    property bytes: TCryptoLibByteArray read GetBytes;

    procedure Encode(const derOut: TStream); override;

    function ToString(): String; override;

    // /**
    // * return an integer from the passed in object
    // *
    // * @exception ArgumentException if the object cannot be converted.
    // */

    class function GetInstance(const obj: TObject): IDerInteger;
      overload; static;

    // /**
    // * return an Integer from a tagged object.
    // *
    // * @param obj the tagged object holding the object we want
    // * @param isExplicit true if the object is meant to be explicitly
    // *              tagged false otherwise.
    // * @exception ArgumentException if the tagged object cannot
    // *               be converted.
    // */
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerInteger; overload; static; inline;

  end;

type
  TDerExternalParser = class(TAsn1Encodable, IDerExternalParser)

  strict private
  var
    F_parser: IAsn1StreamParser;

  public

    constructor Create(const parser: IAsn1StreamParser);
    function ReadObject(): IAsn1Convertible; inline;
    function ToAsn1Object(): IAsn1Object; override;

  end;

type
  TDerOctetStringParser = class(TInterfacedObject, IAsn1OctetStringParser,
    IAsn1Convertible, IDerOctetStringParser)

  strict private
  var
    FStream: TStream;

  public

    constructor Create(stream: TStream);
    destructor Destroy(); override;
    function GetOctetStream(): TStream; inline;
    function ToAsn1Object(): IAsn1Object;

  end;

type

  TDerGeneralString = class(TDerStringBase, IDerGeneralString)

  strict private
  var
    FStr: String;

    function GetStr: String; inline;

    property Str: String read GetStr;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    constructor Create(const Str: TCryptoLibByteArray); overload;

    constructor Create(const Str: String); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    class function GetInstance(const obj: TObject): IDerGeneralString; overload;
      static; inline;

    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerGeneralString; overload; static; inline;

  end;

type
  TDerGenerator = class abstract(TAsn1Generator, IDerGenerator)

  strict private
  var
    F_tagged, F_isExplicit: Boolean;
    F_tagNo: Int32;

    class procedure WriteLength(const outStr: TStream; length: Int32); static;

  strict protected
    constructor Create(const outStream: TStream); overload;
    constructor Create(const outStream: TStream; tagNo: Int32;
      isExplicit: Boolean); overload;

  public
    procedure WriteDerEncoded(tag: Int32;
      const bytes: TCryptoLibByteArray); overload;
    class procedure WriteDerEncoded(const outStream: TStream; tag: Int32;
      const bytes: TCryptoLibByteArray); overload; static;

    class procedure WriteDerEncoded(const outStr: TStream; tag: Int32;
      const inStr: TStream); overload; static;

  end;

type

  /// <summary>
  /// Der IA5String object - this is an ascii string.
  /// </summary>
  TDerIA5String = class(TDerStringBase, IDerIA5String)

  strict private
  var
    FStr: String;

    function GetStr: String; inline;
    property Str: String read GetStr;

  strict protected
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    /// <summary>
    /// basic constructor - with bytes.
    /// </summary>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    /// <summary>
    /// basic constructor - without validation.
    /// </summary>
    constructor Create(const Str: String); overload;

    /// <summary>
    /// Constructor with optional validation.
    /// </summary>
    /// <param name="Str">
    /// the base string to wrap.
    /// </param>
    /// <param name="validate">
    /// whether or not to check the string.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if validate is true and the string contains characters that should
    /// not be in an IA5String.
    /// </exception>
    constructor Create(const Str: String; validate: Boolean); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a DerIA5String from the passed in object
    /// </summary>
    /// <param name="obj">
    /// a DerIA5String or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a DerIA5String instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerIA5String; overload;
      static; inline;

    /// <summary>
    /// return a DerIA5String from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerIA5String; overload; static; inline;

    /// <summary>
    /// return true if the passed in String can be represented without loss
    /// as an IA5String, false otherwise.
    /// </summary>
    /// <param name="Str">
    /// true if in printable set, false otherwise.
    /// </param>
    class function IsIA5String(const Str: String): Boolean; static; inline;

  end;

type

  /// <summary>
  /// Der NumericString object - this is an ascii string of characters
  /// {0,1,2,3,4,5,6,7,8,9, }.
  /// </summary>
  TDerNumericString = class(TDerStringBase, IDerNumericString)

  strict private
  var
    FStr: String;

    function GetStr: String; inline;
    property Str: String read GetStr;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    /// <summary>
    /// basic constructor - with bytes.
    /// </summary>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    /// <summary>
    /// basic constructor - without validation.
    /// </summary>
    constructor Create(const Str: String); overload;

    /// <summary>
    /// Constructor with optional validation.
    /// </summary>
    /// <param name="Str">
    /// the base string to wrap.
    /// </param>
    /// <param name="validate">
    /// whether or not to check the string.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if validate is true and the string contains characters that should
    /// not be in an IA5String.
    /// </exception>
    constructor Create(const Str: String; validate: Boolean); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a Numeric string from the passed in object
    /// </summary>
    /// <param name="obj">
    /// a DerNumericString or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a DerNumericString instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerNumericString; overload;
      static; inline;

    /// <summary>
    /// return a Numeric String from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerNumericString; overload; static; inline;

    /// <summary>
    /// Return true if the string can be represented as a NumericString
    /// ('0'..'9', ' ')
    /// </summary>
    /// <param name="Str">
    /// string to validate.
    /// </param>
    /// <returns>
    /// true if numeric, false otherwise.
    /// </returns>
    class function IsNumericString(const Str: String): Boolean; static; inline;

  end;

type

  /// <summary>
  /// Der PrintableString object.
  /// </summary>
  TDerPrintableString = class(TDerStringBase, IDerPrintableString)

  strict private
  var
    FStr: String;

    function GetStr: String; inline;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    /// <summary>
    /// basic constructor - with bytes.
    /// </summary>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    /// <summary>
    /// basic constructor - without validation.
    /// </summary>
    constructor Create(const Str: String); overload;

    /// <summary>
    /// Constructor with optional validation.
    /// </summary>
    /// <param name="Str">
    /// the base string to wrap.
    /// </param>
    /// <param name="validate">
    /// whether or not to check the string.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if validate is true and the string contains characters that should
    /// not be in an PrintableString.
    /// </exception>
    constructor Create(const Str: String; validate: Boolean); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    property Str: String read GetStr;

    /// <summary>
    /// return a printable string from the passed in object.
    /// </summary>
    /// <param name="obj">
    /// a DerPrintableString or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a DerPrintableString instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerPrintableString;
      overload; static; inline;

    /// <summary>
    /// return a Printable string from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerPrintableString; overload; static; inline;

    /// <summary>
    /// return true if the passed in String can be represented without loss
    /// as a PrintableString, false otherwise.
    /// </summary>
    /// <param name="Str">
    /// string to validate.
    /// </param>
    /// <returns>
    /// return true if in printable set, false otherwise.
    /// </returns>
    class function IsPrintableString(const Str: String): Boolean;
      static; inline;

  end;

type
  TDerSequenceGenerator = class(TDerGenerator, IDerSequenceGenerator)

  strict private
  var
    F_bOut: TMemoryStream;

  public
    constructor Create(outStream: TStream); overload;
    constructor Create(outStream: TStream; tagNo: Int32;
      isExplicit: Boolean); overload;
    destructor Destroy(); override;
    procedure AddObject(const obj: IAsn1Encodable); override;
    function GetRawOutputStream(): TStream; override;
    procedure Close(); override;
  end;

type

  /// <summary>
  /// Der T61String (also the teletex string) - 8-bit characters
  /// </summary>
  TDerT61String = class(TDerStringBase, IDerT61String)

  strict private

  var
    FStr: String;

    function GetStr: String; inline;
    property Str: String read GetStr;

    class function GetEncoding: TEncoding; static; inline;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    /// <summary>
    /// basic constructor - with bytes.
    /// </summary>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    /// <summary>
    /// basic constructor
    /// </summary>
    constructor Create(const Str: String); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a T61 string from the passed in object.
    /// </summary>
    /// <param name="obj">
    /// a Der T61 string or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a Der T61 string instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerT61String; overload;
      static; inline;

    /// <summary>
    /// return a Der T61 string from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerT61String; overload; static; inline;

  end;

type

  /// <summary>
  /// Der UniversalString object.
  /// </summary>
  TDerUniversalString = class(TDerStringBase, IDerUniversalString)

  strict private
  var
    FStr: TCryptoLibByteArray;

  const
    FTable: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

    function GetStr: TCryptoLibByteArray; inline;
    property Str: TCryptoLibByteArray read GetStr;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    /// <summary>
    /// basic constructor - byte encoded string.
    /// </summary>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a Universal String from the passed in object.
    /// </summary>
    /// <param name="obj">
    /// a Der T61 string or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a Der UniversalString instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerUniversalString;
      overload; static; inline;

    /// <summary>
    /// return a Der UniversalString from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerUniversalString; overload; static; inline;

  end;

type

  /// <summary>
  /// Der UTF8String object.
  /// </summary>
  TDerUtf8String = class(TDerStringBase, IDerUtf8String)

  strict private
  var
    FStr: String;

    function GetStr: String; inline;
    property Str: String read GetStr;

  strict protected
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    /// <summary>
    /// basic constructor - with bytes.
    /// </summary>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    /// <summary>
    /// basic constructor
    /// </summary>
    constructor Create(const Str: String); overload;

    function GetString(): String; override;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return an UTF8 string from the passed in object.
    /// </summary>
    /// <param name="obj">
    /// a Der UTF8String or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a Der UTF8String instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerUtf8String; overload;
      static; inline;

    /// <summary>
    /// return a Der UTF8String from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerUtf8String; overload; static; inline;

  end;

type
  TDerVideotexString = class(TDerStringBase, IDerVideotexString)

  strict private
  var
    FmString: TCryptoLibByteArray;

    function GetmString: TCryptoLibByteArray; inline;

  protected
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;

  public
    property mString: TCryptoLibByteArray read GetmString;

    /// <summary>
    /// basic constructor - with bytes.
    /// </summary>
    /// <param name="encoding">
    /// the byte encoding of the characters making up the string.
    /// </param>
    constructor Create(const encoding: TCryptoLibByteArray);

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a Videotex String from the passed in object
    /// </summary>
    /// <param name="obj">
    /// a DerVideotexString or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a DerVideotexString instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerVideotexString;
      overload; static; inline;

    class function GetInstance(const obj: TCryptoLibByteArray)
      : IDerVideotexString; overload; static;

    /// <summary>
    /// return a Videotex string from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerVideotexString; overload; static; inline;

  end;

type

  /// <summary>
  /// Der VisibleString object.
  /// </summary>
  TDerVisibleString = class(TDerStringBase, IDerVisibleString)

  strict private
  var
    FStr: String;

    function GetStr: String; inline;
    property Str: String read GetStr;

  strict protected
    function Asn1GetHashCode(): Int32; override;
    function Asn1Equals(const asn1Object: IAsn1Object): Boolean; override;
  public

    /// <summary>
    /// basic constructor - byte encoded string.
    /// </summary>
    constructor Create(const Str: TCryptoLibByteArray); overload;

    /// <summary>
    /// basic constructor
    /// </summary>
    constructor Create(const Str: String); overload;

    function GetString(): String; override;

    function GetOctets(): TCryptoLibByteArray; inline;

    procedure Encode(const derOut: TStream); override;

    /// <summary>
    /// return a DerVisibleString from the passed in object
    /// </summary>
    /// <param name="obj">
    /// a DerVisibleString or an object that can be converted into one.
    /// </param>
    /// <returns>
    /// return a DerVisibleString instance, or null.
    /// </returns>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: TObject): IDerVisibleString; overload;
      static; inline;

    /// <summary>
    /// return a DerVisibleString from a tagged object.
    /// </summary>
    /// <param name="obj">
    /// the tagged object holding the object we want
    /// </param>
    /// <param name="isExplicit">
    /// true if the object is meant to be explicitly tagged false otherwise.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the tagged object cannot be converted.
    /// </exception>
    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDerVisibleString; overload; static; inline;

  end;

implementation

{ TStreamHelper }

function TStreamHelper.ReadByte: Int32;
var
  buffer: TCryptoLibByteArray;
begin
  System.SetLength(buffer, 1);
  if (TStreamSorter.Read(Self, buffer, 0, 1) = 0) then
  begin
    result := -1;
  end
  else
  begin
    result := Int32(buffer[0]);
  end;
end;

procedure TStreamHelper.WriteByte(b: Byte);
var
  oneByteArray: TCryptoLibByteArray;
begin
  System.SetLength(oneByteArray, 1);
  oneByteArray[0] := b;
  // Self.Write(oneByteArray, 0, 1);
  Self.Write(oneByteArray[0], 1);
end;

{ TStreamSorter }

class function TStreamSorter.Read(input: TStream;
  var buffer: TCryptoLibByteArray; offset, count: Int32): Int32;
begin
  if input is TIndefiniteLengthInputStream then
  begin
    result := (input as TIndefiniteLengthInputStream).
      Read(buffer, offset, count);
  end
  else if input is TDefiniteLengthInputStream then

  begin
    result := (input as TDefiniteLengthInputStream).Read(buffer, offset, count);
  end
  else if input is TConstructedOctetStream then

  begin
    result := (input as TConstructedOctetStream).Read(buffer, offset, count);
  end
  else
  begin
    result := input.Read(buffer[offset], count);
  end;
end;

class function TStreamSorter.ReadByte(input: TStream): Int32;
begin
  if input is TIndefiniteLengthInputStream then
  begin
    result := (input as TIndefiniteLengthInputStream).ReadByte();
  end
  else if input is TDefiniteLengthInputStream then

  begin
    result := (input as TDefiniteLengthInputStream).ReadByte();
  end
  else if input is TConstructedOctetStream then

  begin
    result := (input as TConstructedOctetStream).ReadByte();
  end
  else
  begin
    result := input.ReadByte();
  end;
end;

{ TStreamUtils }

class procedure TStreamUtils.Drain(const inStr: TStream);
var
  bs: TCryptoLibByteArray;
begin
  System.SetLength(bs, BufferSize);

  while (TStreamSorter.Read(inStr, bs, 0, System.length(bs)) > 0) do
  begin
    // do nothing
  end;
end;

class procedure TStreamUtils.PipeAll(const inStr, outStr: TStream);
var
  numRead: Int32;
  bs: TCryptoLibByteArray;
begin
  System.SetLength(bs, BufferSize);

  numRead := TStreamSorter.Read(inStr, bs, 0, System.length(bs));
  while ((numRead) > 0) do
  begin
    outStr.Write(bs[0], numRead);
    numRead := TStreamSorter.Read(inStr, bs, 0, System.length(bs));

  end;
end;

class function TStreamUtils.PipeAllLimited(const inStr: TStream; limit: Int64;
  const outStr: TStream): Int64;
var
  bs: TCryptoLibByteArray;
  numRead: Int32;
  total: Int64;
begin
  System.SetLength(bs, BufferSize);
  total := 0;

  numRead := TStreamSorter.Read(inStr, bs, 0, System.length(bs));
  while ((numRead) > 0) do
  begin
    if ((limit - total) < numRead) then
    begin
      raise EStreamOverflowCryptoLibException.CreateRes(@SDataOverflow);
    end;
    total := total + numRead;
    outStr.Write(bs[0], numRead);
    numRead := TStreamSorter.Read(inStr, bs, 0, System.length(bs));

  end;
  result := total;
end;

class function TStreamUtils.ReadAll(const inStr: TStream): TCryptoLibByteArray;
var
  buf: TMemoryStream;
begin
  buf := TMemoryStream.Create();
  try
    PipeAll(inStr, buf);
    System.SetLength(result, buf.Size);
    buf.Position := 0;
    buf.Read(result[0], buf.Size);
  finally
    buf.Free;
  end;

end;

class function TStreamUtils.ReadAllLimited(const inStr: TStream; limit: Int32)
  : TCryptoLibByteArray;
var
  buf: TMemoryStream;
begin
  buf := TMemoryStream.Create();
  try
    PipeAllLimited(inStr, limit, buf);
    System.SetLength(result, buf.Size);
    buf.Position := 0;
    buf.Read(result[0], buf.Size);
  finally
    buf.Free;
  end;

end;

class function TStreamUtils.ReadFully(const inStr: TStream;
  var buf: TCryptoLibByteArray; off, len: Int32): Int32;
var
  totalRead, numRead: Int32;
begin
  totalRead := 0;

  while (totalRead < len) do
  begin

    numRead := TStreamSorter.Read(inStr, buf, off + totalRead, len - totalRead);
    if (numRead < 1) then
    begin
      break;
    end;
    totalRead := totalRead + numRead;
  end;
  result := totalRead;
end;

class function TStreamUtils.WriteBufTo(const buf: TMemoryStream;
  const output: TCryptoLibByteArray; offset: Int32): Int32;
var
  bytes: TCryptoLibByteArray;
begin
  buf.Position := 0;
  System.SetLength(bytes, buf.Size);
  buf.Read(bytes[0], buf.Size);
  System.Move(bytes[0], output[offset], System.length(bytes) *
    System.SizeOf(Byte));
  result := System.length(bytes);
end;

class procedure TStreamUtils.WriteZeroes(const outStr: TStream; count: Int64);
var
  zeroes: TCryptoLibByteArray;
begin
  System.SetLength(zeroes, BufferSize);
  while (count > BufferSize) do
  begin
    outStr.Write(zeroes[0], BufferSize);
    count := count - BufferSize;
  end;
  outStr.Write(zeroes[0], Int32(count));
end;

class function TStreamUtils.ReadFully(const inStr: TStream;
  var buf: TCryptoLibByteArray): Int32;
begin
  result := ReadFully(inStr, buf, 0, System.length(buf));
end;

class procedure TStreamUtils.WriteBufTo(const buf: TMemoryStream;
  const output: TStream);
begin
  output.CopyFrom(buf, buf.Size);
end;

{ TBaseInputStream }

function TBaseInputStream.GetPosition: Int64;
begin
  raise ENotSupportedCryptoLibException.Create('');
end;

function TBaseInputStream.GetSize: Int64;
begin
  raise ENotSupportedCryptoLibException.Create('');
end;

{$IFNDEF _FIXINSIGHT_}

function TBaseInputStream.Read(var buffer; count: LongInt): LongInt;
begin
  raise ENotSupportedCryptoLibException.Create('');
end;

function TBaseInputStream.Write(const buffer; count: LongInt): LongInt;
begin
  raise ENotSupportedCryptoLibException.Create('');
end;
{$ENDIF}

function TBaseInputStream.ReadByte: Int32;
var
  buffer: TCryptoLibByteArray;
begin
  System.SetLength(buffer, 1);

  // if (Read(Buffer, 0, 1) = 0) then
  if (TStreamSorter.Read(Self, buffer, 0, 1) = 0) then
  begin
    result := -1;
  end
  else
  begin
    result := Int32(buffer[0]);
  end;
end;

function TBaseInputStream.Seek(offset: LongInt; Origin: Word): LongInt;
begin
  result := Seek(Int64(offset), TSeekOrigin(Origin));
end;

{$IFNDEF _FIXINSIGHT_}

function TBaseInputStream.Seek(const offset: Int64; Origin: TSeekOrigin): Int64;
begin
  raise ENotSupportedCryptoLibException.Create('');
end;

procedure TBaseInputStream.SetPosition(const Pos: Int64);
begin
  raise ENotSupportedCryptoLibException.Create('');
end;

{$ENDIF}

procedure TBaseInputStream.SetSize(const NewSize: Int64);
begin
  SetSize(LongInt(NewSize));
end;

procedure TBaseInputStream.SetSize(NewSize: LongInt);
begin
  raise ENotSupportedCryptoLibException.Create('');
end;

procedure TBaseInputStream.SetSize64(const NewSize: Int64);
begin
  SetSize(NewSize);
end;

function TBaseInputStream.Read(buffer: TCryptoLibByteArray;
  offset, count: LongInt): LongInt;
var
  &pos, endPoint, b: Int32;

begin
  Pos := offset;
  try
    endPoint := offset + count;
    while (Pos < endPoint) do
    begin
      b := ReadByte();
      if (b = -1) then
      begin
        break;
      end;
      buffer[Pos] := Byte(b);
      System.Inc(Pos);
    end;
  except
    on e: EIOCryptoLibException do
    begin
      if (Pos = offset) then
        raise;
    end;

  end;

  result := Pos - offset;
end;

{$IFNDEF _FIXINSIGHT_}

function TBaseInputStream.Write(const buffer: TCryptoLibByteArray;
  offset, count: LongInt): LongInt;
begin
  raise ENotSupportedCryptoLibException.Create('');
end;

{$ENDIF}
{ TFilterStream }

constructor TFilterStream.Create(const s: TStream);
begin
  inherited Create();
  Fs := s;
end;

function TFilterStream.GetPosition: Int64;
begin
  result := Fs.Position;
end;

procedure TFilterStream.SetPosition(const Value: Int64);
begin
  Fs.Position := Value;
end;

function TFilterStream.Write(const buffer; count: LongInt): LongInt;
begin
  result := Fs.Write(PByte(buffer), count);
end;

procedure TFilterStream.WriteByte(Value: Byte);
begin
  Fs.WriteByte(Value);
end;

function TFilterStream.GetSize: Int64;
begin
  result := Fs.Size;
end;

function TFilterStream.Read(var buffer; count: LongInt): LongInt;
begin
  result := Fs.Read(PByte(buffer), count);
end;

function TFilterStream.ReadByte: Int32;
begin

  result := TStreamSorter.ReadByte(Fs);

end;

function TFilterStream.Seek(const offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result := Fs.Seek(offset, Origin);
end;

{ TLimitedInputStream }

constructor TLimitedInputStream.Create(inStream: TStream; limit: Int32);
begin
  Inherited Create();
  F_in := inStream;
  F_limit := limit;
end;

function TLimitedInputStream.GetRemaining: Int32;
begin
  // TODO: maybe one day this can become more accurate
  result := F_limit;
end;

procedure TLimitedInputStream.SetParentEofDetect(&on: Boolean);
var
  indefiniteLengthInputStream: TIndefiniteLengthInputStream;
begin

  if F_in is TIndefiniteLengthInputStream then
  begin
    indefiniteLengthInputStream := F_in as TIndefiniteLengthInputStream;
    indefiniteLengthInputStream.SetEofOn00(&on);
  end;

end;

{ TDefiniteLengthInputStream }

constructor TDefiniteLengthInputStream.Create(inStream: TStream; length: Int32);
begin
  Inherited Create(inStream, length);
  if (length < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidLength);
  end;

  F_originalLength := length;
  F_remaining := length;

  if (length = 0) then
  begin
    SetParentEofDetect(True);
  end;
end;

class function TDefiniteLengthInputStream.GetEmptyBytes: TCryptoLibByteArray;
begin
  result := Nil;
end;

function TDefiniteLengthInputStream.GetRemaining: Int32;
begin
  result := F_remaining;
end;

function TDefiniteLengthInputStream.Read(buf: TCryptoLibByteArray;
  off, len: LongInt): LongInt;
var
  toRead, numRead: Int32;

begin
  if (F_remaining = 0) then
  begin
    result := 0;
    Exit;
  end;

  toRead := Min(len, F_remaining);

  numRead := TStreamSorter.Read(F_in, buf, off, toRead);

  if (numRead < 1) then
  begin
    raise EEndOfStreamCryptoLibException.CreateResFmt(@SEndOfStreamTwo,
      [F_originalLength, F_remaining]);
  end;
  F_remaining := F_remaining - numRead;

  if (F_remaining = 0) then
  begin
    SetParentEofDetect(True);
  end;

  result := numRead;
end;

procedure TDefiniteLengthInputStream.ReadAllIntoByteArray
  (var buf: TCryptoLibByteArray);
begin
  if (F_remaining <> System.length(buf)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidBufferLength);
  end;
  F_remaining := F_remaining - TStreamUtils.ReadFully(F_in, buf);
  if ((F_remaining <> 0)) then
  begin
    raise EEndOfStreamCryptoLibException.CreateResFmt(@SEndOfStreamTwo,
      [F_originalLength, F_remaining]);
  end;
  SetParentEofDetect(True);
end;

function TDefiniteLengthInputStream.ReadByte: Int32;
begin
  if (F_remaining = 0) then
  begin
    result := -1;
    Exit;
  end;

  // result := F_in.ReadByte();
  result := TStreamSorter.ReadByte(F_in);

  if (result < 0) then
  begin
    raise EEndOfStreamCryptoLibException.CreateResFmt(@SEndOfStreamTwo,
      [F_originalLength, F_remaining]);
  end;

  System.Dec(F_remaining);
  if (F_remaining = 0) then
  begin
    SetParentEofDetect(True);
  end;

end;

function TDefiniteLengthInputStream.ToArray: TCryptoLibByteArray;
var
  bytes: TCryptoLibByteArray;
begin
  if (F_remaining = 0) then
  begin
    result := EmptyBytes;
    Exit;
  end;
  System.SetLength(bytes, F_remaining);
  F_remaining := F_remaining - TStreamUtils.ReadFully(F_in, bytes);
  if (F_remaining <> 0) then
  begin
    raise EEndOfStreamCryptoLibException.CreateResFmt(@SEndOfStreamTwo,
      [F_originalLength, F_remaining]);
  end;
  SetParentEofDetect(True);
  result := bytes;
end;

{ TAsn1InputStream }

class function TAsn1InputStream.FindLimit(const input: TStream): Int32;
var
  limitedInputStream: TLimitedInputStream;
  mem: TMemoryStream;
begin
  limitedInputStream := input as TLimitedInputStream;
  if (limitedInputStream <> Nil) then
  begin
    result := limitedInputStream.GetRemaining();
    Exit;
  end
  else if (input is TMemoryStream) then
  begin
    mem := input as TMemoryStream;
    result := Int32(mem.Size - mem.Position);
    Exit;
  end;

  result := System.High(Int32);
end;

class function TAsn1InputStream.GetBuffer(const defIn
  : TDefiniteLengthInputStream; const tmpBuffers: TCryptoLibMatrixByteArray)
  : TCryptoLibByteArray;
var
  len: Int32;
  buf, temp: TCryptoLibByteArray;
begin
  len := defIn.GetRemaining();
  if (len >= System.length(tmpBuffers)) then
  begin
    result := defIn.ToArray();
    Exit;
  end;

  buf := tmpBuffers[len];
  if (buf = Nil) then
  begin
    System.SetLength(temp, len);
    tmpBuffers[len] := temp;
    buf := tmpBuffers[len];
  end;

  defIn.ReadAllIntoByteArray(buf);

  result := buf;
end;

class function TAsn1InputStream.CreatePrimitiveDerObject(tagNo: Int32;
  const defIn: TDefiniteLengthInputStream;
  const tmpBuffers: TCryptoLibMatrixByteArray): IAsn1Object;
var
  bytes: TCryptoLibByteArray;
begin
  case tagNo of
    TAsn1Tags.Boolean:
      begin
        result := TDerBoolean.FromOctetString(GetBuffer(defIn, tmpBuffers));
        Exit;
      end;
    TAsn1Tags.Enumerated:
      begin
        result := TDerEnumerated.FromOctetString(GetBuffer(defIn, tmpBuffers));
        Exit;
      end;
    TAsn1Tags.ObjectIdentifier:
      begin
        result := TDerObjectIdentifier.FromOctetString
          (GetBuffer(defIn, tmpBuffers));
        Exit;
      end;

  end;

  bytes := defIn.ToArray();

  case tagNo of

    TAsn1Tags.BitString:
      begin
        result := TDerBitString.FromAsn1Octets(bytes);
        Exit;
      end;
    TAsn1Tags.BmpString:
      begin
        result := TDerBmpString.Create(bytes);
        Exit;
      end;
    // TAsn1Tags.GeneralizedTime:
    // begin
    // result := TDerGeneralizedTime.Create(bytes);
    // Exit;
    // end;
    TAsn1Tags.GeneralString:
      begin
        result := TDerGeneralString.Create(bytes);
        Exit;
      end;
    TAsn1Tags.GraphicString:
      begin
        result := TDerGraphicString.Create(bytes);
        Exit;
      end;
    TAsn1Tags.IA5String:
      begin
        result := TDerIA5String.Create(bytes);
        Exit;
      end;
    TAsn1Tags.Integer:
      begin
        result := TDerInteger.Create(bytes);
        Exit;
      end;
    TAsn1Tags.Null:
      begin
        // actual content is ignored (enforce 0 length?)
        result := TDerNull.Instance;
        Exit;
      end;
    TAsn1Tags.NumericString:
      begin
        result := TDerNumericString.Create(bytes);
        Exit;
      end;
    TAsn1Tags.OctetString:
      begin
        result := TDerOctetString.Create(bytes);
        Exit;
      end;
    TAsn1Tags.PrintableString:
      begin
        result := TDerPrintableString.Create(bytes);
        Exit;
      end;
    TAsn1Tags.T61String:
      begin
        result := TDerT61String.Create(bytes);
        Exit;
      end;
    TAsn1Tags.UniversalString:
      begin
        result := TDerUniversalString.Create(bytes);
        Exit;
      end;
    // TAsn1Tags.UtcTime:
    // begin
    // result := TDerUtcTime.Create(bytes);
    // Exit;
    // end;
    TAsn1Tags.Utf8String:
      begin
        result := TDerUtf8String.Create(bytes);
        Exit;
      end;
    TAsn1Tags.VideotexString:
      begin
        result := TDerVideotexString.Create(bytes);
        Exit;
      end;
    TAsn1Tags.VisibleString:
      begin
        result := TDerVisibleString.Create(bytes);
        Exit;
      end;
  else
    begin
      raise EIOCryptoLibException.CreateResFmt(@SUnknownTag, [tagNo]);
    end;

  end;
end;

destructor TAsn1InputStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

constructor TAsn1InputStream.Create(const inputStream: TStream; limit: Int32);
begin
  Inherited Create(inputStream);
  Flimit := limit;
  System.SetLength(FtmpBuffers, 16);
end;

constructor TAsn1InputStream.Create(const inputStream: TStream);
begin
  Create(inputStream, FindLimit(inputStream));
end;

constructor TAsn1InputStream.Create(const input: TCryptoLibByteArray);
begin
  // used TBytesStream here for one pass creation and population with byte array :)
  FStream := TBytesStream.Create(input);
  Create(FStream, System.length(input));

end;

class function TAsn1InputStream.ReadLength(const s: TStream;
  limit: Int32): Int32;
var
  &length, Size, next, I: Int32;
begin

  length := TStreamSorter.ReadByte(s);

  if (length < 0) then
  begin
    raise EEndOfStreamCryptoLibException.CreateRes(@SInvalidEnd);
  end;

  if (length = $80) then
  begin
    result := -1; // indefinite-length encoding
    Exit;
  end;

  if (length > 127) then
  begin
    Size := length and $7F;

    // Note: The invalid long form "$ff" (see X.690 8.1.3.5c) will be caught here
    if (Size > 4) then
    begin
      raise EIOCryptoLibException.CreateResFmt(@SInvalidDerLength, [Size]);
    end;

    length := 0;
    I := 0;
    while I < Size do
    begin

      next := TStreamSorter.ReadByte(s);

      if (next < 0) then
      begin
        raise EEndOfStreamCryptoLibException.CreateRes(@SEndOfStream);
      end;

      length := (length shl 8) + next;

      System.Inc(I);
    end;

    if (length < 0) then
    begin
      raise EIOCryptoLibException.CreateRes(@SNegativeLength);
    end;

    if (length >= limit) then // after all we must have read at least 1 byte
    begin
      raise EIOCryptoLibException.CreateRes(@SOutOfBoundsLength);
    end;
  end;

  result := length;
end;

function TAsn1InputStream.ReadObject: IAsn1Object;
var
  tag, tagNo, &length: Int32;
  IsConstructed: Boolean;
  indIn: TIndefiniteLengthInputStream;
  sp: IAsn1StreamParser;
begin

  tag := ReadByte();

  if (tag <= 0) then
  begin
    if (tag = 0) then
    begin
      raise EIOCryptoLibException.CreateRes(@SEndOfContent);
    end;

    result := Nil;
    Exit;
  end;

  //
  // calculate tag number
  //
  tagNo := ReadTagNumber(Fs, tag);

  IsConstructed := (tag and TAsn1Tags.Constructed) <> 0;

  //
  // calculate length
  //
  length := ReadLength(Fs, Flimit);

  if (length < 0) then // indefinite length method
  begin
    if (not IsConstructed) then
    begin

      raise EIOCryptoLibException.CreateRes(@SIndefiniteLength);

    end;

    indIn := TIndefiniteLengthInputStream.Create(Fs, Flimit);
    sp := TAsn1StreamParser.Create(indIn, Flimit);

    if ((tag and TAsn1Tags.Application) <> 0) then
    begin
      result := (TBerApplicationSpecificParser.Create(tagNo, sp)
        as IBerApplicationSpecificParser).ToAsn1Object();
      Exit;
    end;

    if ((tag and TAsn1Tags.Tagged) <> 0) then
    begin
      result := (TBerTaggedObjectParser.Create(True, tagNo, sp)
        as IBerTaggedObjectParser).ToAsn1Object();
      Exit;
    end;

    // TODO There are other tags that may be constructed (e.g. BitString)

    case tagNo of
      TAsn1Tags.OctetString:
        begin
          result := (TBerOctetStringParser.Create(sp) as IBerOctetStringParser)
            .ToAsn1Object();
          Exit;
        end;
      TAsn1Tags.Sequence:
        begin
          result := (TBerSequenceParser.Create(sp) as IBerSequenceParser)
            .ToAsn1Object();
          Exit;
        end;
      TAsn1Tags.&Set:
        begin
          result := (TBerSetParser.Create(sp) as IBerSetParser).ToAsn1Object();
          Exit;
        end;
      TAsn1Tags.External:
        begin
          result := (TDerExternalParser.Create(sp) as IDerExternalParser)
            .ToAsn1Object();
          Exit;
        end;
    else
      begin
        raise EIOCryptoLibException.CreateRes(@SUnknownBerObject);
      end;
    end;

  end
  else
  begin
    try
      result := BuildObject(tag, tagNo, length);
    except
      on e: EArgumentCryptoLibException do
      begin
        raise EAsn1CryptoLibException.CreateResFmt(@SCorruptedStream,
          [e.Message]);
      end;
    end;
  end;
end;

function TAsn1InputStream.BuildDerEncodableVector
  (const dIn: TDefiniteLengthInputStream): IAsn1EncodableVector;
var
  res: TAsn1InputStream;
begin
  res := TAsn1InputStream.Create(dIn);
  try
    result := res.BuildEncodableVector();
  finally
    res.Free;
  end;
end;

function TAsn1InputStream.BuildEncodableVector: IAsn1EncodableVector;
var
  v: IAsn1EncodableVector;
  o: IAsn1Object;
begin
  v := TAsn1EncodableVector.Create();

  o := ReadObject();
  while (o <> Nil) do
  begin
    v.Add([o]);
    o := ReadObject();
  end;

  result := v;
end;

function TAsn1InputStream.BuildObject(tag, tagNo, length: Int32): IAsn1Object;
var
  IsConstructed: Boolean;
  defIn: TDefiniteLengthInputStream;
  v: IAsn1EncodableVector;
  strings: TList<IDerOctetString>;
  I: Int32;
begin
  IsConstructed := (tag and TAsn1Tags.Constructed) <> 0;
  defIn := TDefiniteLengthInputStream.Create(Fs, length);

  if ((tag and TAsn1Tags.Application) <> 0) then
  begin
    try
      result := TDerApplicationSpecific.Create(IsConstructed, tagNo,
        defIn.ToArray());
      Exit;
    finally
      defIn.Free;
    end;
  end;

  if ((tag and TAsn1Tags.Tagged) <> 0) then
  begin

    result := (TAsn1StreamParser.Create(defIn) as IAsn1StreamParser)
      .ReadTaggedObject(IsConstructed, tagNo);
    Exit;

  end;

  if (IsConstructed) then
  begin
    // TODO There are other tags that may be constructed (e.g. BitString)
    case (tagNo) of

      TAsn1Tags.OctetString:
        //
        // yes, people actually do this...
        //
        begin
          try
            v := BuildDerEncodableVector(defIn);
            strings := TList<IDerOctetString>.Create;
            strings.capacity := v.count;

            I := 0;
            while (I <> v.count) do
            begin
              strings.Add(v[I] as IDerOctetString);
            end;

            result := TBerOctetString.Create(strings);
            Exit;
          finally
            defIn.Free;
          end;
        end;
      TAsn1Tags.Sequence:
        begin
          try
            result := CreateDerSequence(defIn);
            Exit;
          finally
            defIn.Free;
          end;
        end;
      TAsn1Tags.&Set:
        begin
          try
            result := CreateDerSet(defIn);
            Exit;
          finally
            defIn.Free;
          end;
        end;
      TAsn1Tags.External:
        begin
          try
            result := TDerExternal.Create(BuildDerEncodableVector(defIn));
            Exit;
          finally
            defIn.Free;
          end;
        end;
    else
      begin
        defIn.Free; // free the stream incase an unsupported tag is encountered.
        raise EIOCryptoLibException.CreateResFmt(@SUnknownTag, [tagNo]);
      end;

    end;

  end;

  try
    result := CreatePrimitiveDerObject(tagNo, defIn, FtmpBuffers);
  finally
    defIn.Free;
  end;

end;

function TAsn1InputStream.CreateDerSequence
  (const dIn: TDefiniteLengthInputStream): IDerSequence;
begin
  result := TDerSequence.FromVector(BuildDerEncodableVector(dIn));
end;

function TAsn1InputStream.CreateDerSet(const dIn
  : TDefiniteLengthInputStream): IDerSet;
begin
  result := TDerSet.FromVector(BuildDerEncodableVector(dIn), False);
end;

class function TAsn1InputStream.ReadTagNumber(const s: TStream;
  tag: Int32): Int32;
var
  tagNo, b: Int32;
begin
  tagNo := tag and $1F;

  //
  // with tagged object tag number is bottom 5 bits, or stored at the start of the content
  //
  if (tagNo = $1F) then
  begin
    tagNo := 0;

    b := TStreamSorter.ReadByte(s);

    // X.690-0207 8.1.2.4.2
    // "c) bits 7 to 1 of the first subsequent octet shall not all be zero."
    if ((b and $7F) = 0) then // Note: -1 will pass
    begin
      raise EIOCryptoLibException.CreateRes(@SCorruptedStreamInvalidTag);
    end;

    while ((b >= 0) and ((b and $80) <> 0)) do
    begin
      tagNo := tagNo or (b and $7F);
      tagNo := tagNo shl 7;

      b := TStreamSorter.ReadByte(s);

    end;

    if (b < 0) then
    begin
      raise EEndOfStreamCryptoLibException.CreateRes(@SEOFFound);
    end;
    tagNo := tagNo or (b and $7F);
  end;

  result := tagNo;
end;

{ TDerOutputStream }

constructor TDerOutputStream.Create(const os: TStream);
begin
  Inherited Create(os);
end;

procedure TDerOutputStream.WriteEncoded(tag: Int32; first: Byte;
  const bytes: TCryptoLibByteArray);
begin
  WriteByte(Byte(tag));
  WriteLength(System.length(bytes) + 1);
  WriteByte(first);
  Write(bytes[0], System.length(bytes));
end;

procedure TDerOutputStream.WriteEncoded(tag: Int32;
  const bytes: TCryptoLibByteArray);
begin
  WriteByte(Byte(tag));
  WriteLength(System.length(bytes));
  if bytes <> Nil then
  begin
    Write(bytes[0], System.length(bytes));
  end;
end;

procedure TDerOutputStream.WriteEncoded(flags, tagNo: Int32;
  const bytes: TCryptoLibByteArray);
begin
  WriteTag(flags, tagNo);
  WriteLength(System.length(bytes));
  Write(bytes[0], System.length(bytes));
end;

procedure TDerOutputStream.WriteEncoded(tag: Int32;
  const bytes: TCryptoLibByteArray; offset, length: Int32);
begin
  WriteByte(Byte(tag));
  WriteLength(length);
  Write(bytes[offset], length);
end;

procedure TDerOutputStream.WriteLength(length: Int32);
var
  Size, I: Int32;
  val: UInt32;
begin
  if (length > 127) then
  begin
    Size := 1;
    val := UInt32(length);
    val := val shr 8;
    while (val <> 0) do
    begin
      System.Inc(Size);
      val := val shr 8;
    end;

    WriteByte(Byte(Size or $80));

    I := (Size - 1) * 8;

    while I >= 0 do
    begin
      WriteByte(Byte(TBits.Asr32(length, I)));
      System.Dec(I, 8);
    end;

  end
  else
  begin
    WriteByte(Byte(length));
  end;
end;

procedure TDerOutputStream.WriteNull;
begin
  WriteByte(TAsn1Tags.Null);
  WriteByte($00);
end;

procedure TDerOutputStream.WriteObject(const obj: IAsn1Encodable);
var
  asn1: IAsn1Object;
begin
  if (obj = Nil) then
  begin
    WriteNull();
  end
  else
  begin
    asn1 := obj.ToAsn1Object();
    asn1.Encode(Self);
  end;
end;

procedure TDerOutputStream.WriteObject(const obj: IAsn1Object);
begin
  if (obj = Nil) then
  begin
    WriteNull();
  end
  else
  begin
    obj.Encode(Self);
  end;
end;

procedure TDerOutputStream.WriteTag(flags, tagNo: Int32);
var
  stack: TCryptoLibByteArray;
  Pos: Int32;
begin
  if (tagNo < 31) then
  begin
    WriteByte(Byte(flags or tagNo));
  end
  else
  begin
    WriteByte(Byte(flags or $1F));
    if (tagNo < 128) then
    begin
      WriteByte(Byte(tagNo));
    end
    else
    begin
      System.SetLength(stack, 5);
      Pos := System.length(stack);

      System.Dec(Pos);
      stack[Pos] := Byte(tagNo and $7F);

      repeat
        tagNo := TBits.Asr32(tagNo, 7);
        System.Dec(Pos);
        stack[Pos] := Byte(tagNo and $7F or $80);
      until (not(tagNo > 127));

      Write(stack[Pos], System.length(stack) - Pos);
    end;
  end;
end;

{ TAsn1OutputStream }

constructor TAsn1OutputStream.Create(os: TStream);
begin
  Inherited Create(os);
end;

{ TBerOutputStream }

constructor TBerOutputStream.Create(os: TStream);
begin
  Inherited Create(os);
end;

{ TConstructedOctetStream }

constructor TConstructedOctetStream.Create(const parser: IAsn1StreamParser);
begin
  Inherited Create();
  F_parser := parser;
  F_first := True;
end;

function TConstructedOctetStream.Read(buffer: TCryptoLibByteArray;
  offset, count: LongInt): LongInt;
var
  s, aos: IAsn1OctetStringParser;
  totalRead, numRead: Int32;
begin
  if (F_currentStream = Nil) then
  begin
    if (not F_first) then
    begin
      result := 0;
      Exit;
    end;

    if (not Supports(F_parser.ReadObject(), IAsn1OctetStringParser, s)) then
    begin
      result := 0;
      Exit;
    end;

    F_first := False;
    F_currentStream := s.GetOctetStream();
  end;

  totalRead := 0;

  while True do

  begin

    numRead := TStreamSorter.Read(F_currentStream, buffer, offset + totalRead,
      count - totalRead);

    if (numRead > 0) then
    begin
      totalRead := totalRead + numRead;

      if (totalRead = count) then
      begin
        result := totalRead;
        Exit;
      end;
    end
    else
    begin

      if (not Supports(F_parser.ReadObject(), IAsn1OctetStringParser, aos)) then
      begin
        F_currentStream := Nil;
        result := totalRead;
        Exit;
      end;

      F_currentStream := aos.GetOctetStream();
    end
  end;
  result := 0;
end;

function TConstructedOctetStream.ReadByte: Int32;
var
  s, aos: IAsn1OctetStringParser;
  b: Int32;
begin
  if (F_currentStream = Nil) then
  begin
    if (not F_first) then
    begin
      result := 0;
      Exit;
    end;

    if (not Supports(F_parser.ReadObject(), IAsn1OctetStringParser, s)) then
    begin
      result := 0;
      Exit;
    end;

    F_first := False;
    F_currentStream := s.GetOctetStream();
  end;

  while True do

  begin

    // b := F_currentStream.ReadByte();
    b := TStreamSorter.ReadByte(F_currentStream);

    if (b >= 0) then
    begin
      result := b;
      Exit;
    end;

    if (not Supports(F_parser.ReadObject(), IAsn1OctetStringParser, aos)) then
    begin
      F_currentStream := Nil;
      result := -1;
      Exit;
    end;

    F_currentStream := aos.GetOctetStream();
  end;

  result := 0;
end;

{ TIndefiniteLengthInputStream }

function TIndefiniteLengthInputStream.RequireByte: Int32;
begin

  // result := F_in.ReadByte();
  result := TStreamSorter.ReadByte(F_in);

  if (result < 0) then
  begin
    // Corrupted stream
    raise EEndOfStreamCryptoLibException.Create('');
  end;
end;

function TIndefiniteLengthInputStream.CheckForEof: Boolean;
var
  extra: Int32;
begin
  if (F_lookAhead = $00) then
  begin
    extra := RequireByte();
    if (extra <> 0) then
    begin
      raise EIOCryptoLibException.CreateRes(@SMalformedContent);
    end;

    F_lookAhead := -1;
    SetParentEofDetect(True);
    result := True;
    Exit;
  end;
  result := F_lookAhead < 0;
end;

constructor TIndefiniteLengthInputStream.Create(inStream: TStream;
  limit: Int32);
begin
  Inherited Create(inStream, limit);
  F_lookAhead := RequireByte();
  CheckForEof();
end;

function TIndefiniteLengthInputStream.Read(buffer: TCryptoLibByteArray;
  offset, count: LongInt): LongInt;
var
  numRead: Int32;
begin
  // Only use this optimisation if we aren't checking for 00
  if ((F_eofOn00) or (count <= 1)) then
  begin
    result := (Inherited Read(buffer, offset, count));
    Exit;
  end;

  if (F_lookAhead < 0) then
  begin
    result := 0;
    Exit;
  end;

  numRead := TStreamSorter.Read(F_in, buffer, offset + 1, count - 1);

  if (numRead <= 0) then
  begin
    // Corrupted stream
    raise EEndOfStreamCryptoLibException.Create('');
  end;

  buffer[offset] := Byte(F_lookAhead);
  F_lookAhead := RequireByte();

  result := numRead + 1;
end;

function TIndefiniteLengthInputStream.ReadByte: Int32;
begin
  if (F_eofOn00 and CheckForEof()) then
  begin
    result := -1;
    Exit;
  end;

  result := F_lookAhead;
  F_lookAhead := RequireByte();

end;

procedure TIndefiniteLengthInputStream.SetEofOn00(eofOn00: Boolean);
begin
  F_eofOn00 := eofOn00;
  if (F_eofOn00) then
  begin
    CheckForEof();
  end;
end;

{ TCollectionUtilities }

class function TCollectionUtilities.ToStructuredString
  (c: TList<IAsn1Encodable>): String;
var
  sl: TStringList;
  idx: Int32;
begin

  sl := TStringList.Create();
  sl.LineBreak := '';
  try
    sl.Add('[');

    if (c.count <> 0) then
    begin
      sl.Add((c[0] as TAsn1Encodable).ClassName);
      if c.count > 1 then
      begin
        for idx := 1 to c.count - 2 do
        begin
          sl.Add(', ');
          sl.Add((c[idx] as TAsn1Encodable).ClassName);
        end;
      end;
    end;

    sl.Add(']');
    result := sl.Text;
  finally
    sl.Free;
  end;

end;

{ TAsn1Encodable }

function TAsn1Encodable.Equals(const other: IAsn1Convertible): Boolean;
var
  o1, o2: IAsn1Object;
begin

  if (other = Self as IAsn1Convertible) then
  begin
    result := True;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := False;
    Exit;
  end;
  o1 := ToAsn1Object();
  o2 := other.ToAsn1Object();

  result := ((o1 = o2) or o1.CallAsn1Equals(o2));
end;

function TAsn1Encodable.GetDerEncoded: TCryptoLibByteArray;
begin

  try
    result := GetEncoded(Der);
  except
    on e: EIOCryptoLibException do
    begin
      result := Nil;
    end;
  end;
end;

function TAsn1Encodable.GetEncoded: TCryptoLibByteArray;
var
  bOut: TMemoryStream;
  aOut: TAsn1OutputStream;
begin

  bOut := TMemoryStream.Create();
  aOut := TAsn1OutputStream.Create(bOut);
  try
    aOut.WriteObject(Self as IAsn1Encodable);
    System.SetLength(result, bOut.Size);
    bOut.Position := 0;
    bOut.Read(result[0], System.length(result));

  finally
    bOut.Free;
    aOut.Free;
  end;

end;

function TAsn1Encodable.GetEncoded(const encoding: String): TCryptoLibByteArray;
var
  bOut: TMemoryStream;
  dOut: TDerOutputStream;
begin
  if (encoding = Der) then
  begin
    bOut := TMemoryStream.Create();
    dOut := TDerOutputStream.Create(bOut);
    try
      dOut.WriteObject(Self as IAsn1Encodable);
      System.SetLength(result, bOut.Size);
      bOut.Position := 0;
      bOut.Read(result[0], System.length(result));

    finally
      bOut.Free;
      dOut.Free;
    end;
    Exit;
  end;

  result := GetEncoded();
end;

function TAsn1Encodable.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := ToAsn1Object().CallAsn1GetHashCode();
end;

{ TAsn1Object }

function TAsn1Object.CallAsn1Equals(const obj: IAsn1Object): Boolean;
begin
  result := Asn1Equals(obj);
end;

function TAsn1Object.CallAsn1GetHashCode: Int32;
begin
  result := Asn1GetHashCode();
end;

class function TAsn1Object.FromByteArray(const data: TCryptoLibByteArray)
  : IAsn1Object;
var
  asn1: TAsn1InputStream;
  input: TBytesStream;
begin
  try
    // used TBytesStream here for one pass creation and population with byte array :)
    input := TBytesStream.Create(data);
    try

      asn1 := TAsn1InputStream.Create(input, System.length(data));

      try
        result := asn1.ReadObject();
      finally
        asn1.Free;
      end;
      if (input.Position <> input.Size) then
      begin
        raise EIOCryptoLibException.CreateRes(@SExtraData);
      end;
    finally
      input.Free;
    end;
  except
    on e: EInvalidCastCryptoLibException do
    begin
      raise EIOCryptoLibException.CreateRes(@SUnRecognizedObjectByteArray);
    end;
  end;
end;

class function TAsn1Object.FromStream(const inStr: TStream): IAsn1Object;
var
  asn1Stream: TAsn1InputStream;
begin
  asn1Stream := TAsn1InputStream.Create(inStr);
  try
    try
      result := asn1Stream.ReadObject();
    except
      on e: EInvalidCastCryptoLibException do
      begin
        raise EIOCryptoLibException.CreateRes(@SUnRecognizedObjectStream);
      end;
    end;
  finally
    asn1Stream.Free;
  end;
end;

function TAsn1Object.ToAsn1Object: IAsn1Object;
begin
  result := Self as IAsn1Object;
end;

{ TDerObjectIdentifier }

function TDerObjectIdentifier.GetID: String;
begin
  result := Fidentifier;
end;

function TDerObjectIdentifier.Asn1Equals(const asn1Object: IAsn1Object)
  : Boolean;
var
  other: IDerObjectIdentifier;
begin
  if (not Supports(asn1Object, IDerObjectIdentifier, other)) then
  begin
    result := False;
    Exit;
  end;

  result := ID = other.ID;
end;

function TDerObjectIdentifier.Asn1GetHashCode: Int32;
begin
  result := TStringUtils.GetStringHashCode(Fidentifier);
end;

class procedure TDerObjectIdentifier.Boot;
begin
  if FLock = Nil then
  begin
    FLock := TCriticalSection.Create;
  end;
end;

function TDerObjectIdentifier.Branch(const branchID: String)
  : IDerObjectIdentifier;
begin
  result := TDerObjectIdentifier.Create(Self as IDerObjectIdentifier, branchID);
end;

constructor TDerObjectIdentifier.Create(const oid: IDerObjectIdentifier;
  const branchID: String);
begin
  Inherited Create();
  if (not(IsValidBranchID(branchID, 1))) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidBranchId,
      [branchID]);
  end;

  Fidentifier := oid.ID + '.' + branchID;
end;

constructor TDerObjectIdentifier.Create(const identifier: String);
begin
  Inherited Create();
  if (identifier = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SIdentifierNil);
  end;
  if (not(IsValidIdentifier(identifier))) then
  begin
    raise EFormatCryptoLibException.CreateResFmt(@SInvalidOID, [identifier]);
  end;

  Fidentifier := identifier;
end;

constructor TDerObjectIdentifier.Create(const bytes: TCryptoLibByteArray);
begin
  Inherited Create();
  Fidentifier := MakeOidStringFromBytes(bytes);
  Fbody := System.Copy(bytes);
end;

function TDerObjectIdentifier.&on(const stem: IDerObjectIdentifier): Boolean;
var
  LocalId, stemId: String;
begin
  LocalId := ID;
  stemId := stem.ID;
  result := (System.length(LocalId) > System.length(stemId)) and
    (LocalId[System.length(stemId) + 1] = '.') and
    (AnsiStartsStr(stemId, LocalId));
end;

class constructor TDerObjectIdentifier.CreateDerObjectIdentifier;
begin
  TDerObjectIdentifier.Boot;
end;

class destructor TDerObjectIdentifier.DestroyDerObjectIdentifier;
begin
  FLock.Free;
end;

procedure TDerObjectIdentifier.DoOutput(const bOut: TMemoryStream);
var
  tok: IOidTokenizer;
  token: String;
  first: Int32;
begin
  tok := TOidTokenizer.Create(Fidentifier);
  token := tok.NextToken();
  first := StrToInt(token) * 40;
  token := tok.NextToken();
  if (System.length(token) <= 18) then
  begin
    WriteField(bOut, Int64(first + StrToInt64(token)));
  end
  else
  begin
    WriteField(bOut, TBigInteger.Create(token).Add(TBigInteger.ValueOf(first)));
  end;

  while (tok.HasMoreTokens) do
  begin
    token := tok.NextToken();
    if (System.length(token) <= 18) then
    begin
      WriteField(bOut, StrToInt64(token));
    end
    else
    begin
      WriteField(bOut, TBigInteger.Create(token));
    end;
  end;
end;

procedure TDerObjectIdentifier.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.ObjectIdentifier,
    GetBody());
end;

class function TDerObjectIdentifier.FromOctetString
  (const enc: TCryptoLibByteArray): IDerObjectIdentifier;
var
  HashCode, first: Int32;
  entry: IDerObjectIdentifier;
begin

  HashCode := TArrayUtils.GetArrayHashCode(enc);
  first := HashCode and 1023;

  FLock.Acquire;
  try
    entry := Fcache[first];
    if ((entry <> Nil) and (TArrayUtils.AreEqual(enc, entry.GetBody()))) then
    begin
      result := entry;
      Exit;
    end;

    Fcache[first] := TDerObjectIdentifier.Create(enc);
    result := Fcache[first];

  finally
    FLock.Release;
  end;

end;

function TDerObjectIdentifier.GetBody: TCryptoLibByteArray;
var
  bOut: TMemoryStream;
begin

  FLock.Acquire;
  try
    if (Fbody = Nil) then
    begin
      bOut := TMemoryStream.Create();
      try
        DoOutput(bOut);
        System.SetLength(Fbody, bOut.Size);
        bOut.Position := 0;
        bOut.Read(Fbody[0], System.length(Fbody));
      finally
        bOut.Free;
      end;
    end;

  finally
    FLock.Release;
  end;

  result := Fbody;
end;

class function TDerObjectIdentifier.GetInstance(const obj: IAsn1TaggedObject;
  explicitly: Boolean): IDerObjectIdentifier;
var
  o: IAsn1Object;
begin

  o := obj.GetObject();

  if ((explicitly) or (Supports(o, IDerObjectIdentifier))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := FromOctetString(TAsn1OctetString.GetInstance(o as TAsn1Object)
    .GetOctets());
end;

class function TDerObjectIdentifier.GetInstance(const obj: TObject)
  : IDerObjectIdentifier;
begin
  if ((obj = Nil) or (obj is TDerObjectIdentifier)) then
  begin
    result := obj as TDerObjectIdentifier;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerObjectIdentifier.GetInstance(const obj: TCryptoLibByteArray)
  : IDerObjectIdentifier;
begin
  result := FromOctetString(obj);
end;

class function TDerObjectIdentifier.IsValidBranchID(const branchID: String;
  start: Int32): Boolean;
var
  periodAllowed: Boolean;
  Pos: Int32;
  ch: Char;
begin
  periodAllowed := False;

  Pos := System.length(branchID) + 1;
  System.Dec(Pos);
  while (Pos >= start) do
  begin
    ch := branchID[Pos];

    // TODO Leading zeroes?
    // if (('0' <= ch) and (ch <= '9')) then
    // begin
    // periodAllowed := true;
    // continue;
    // end;

    // TODO Leading zeroes?
    if (CharInSet(ch, ['0' .. '9'])) then
    begin
      periodAllowed := True;
      System.Dec(Pos);
      continue;
    end;

    if (ch = '.') then
    begin
      if (not(periodAllowed)) then
      begin
        result := False;
        Exit;
      end;

      periodAllowed := False;
      System.Dec(Pos);
      continue;
    end;

    result := False;
    Exit;
  end;

  result := periodAllowed;
end;

class function TDerObjectIdentifier.IsValidIdentifier(const identifier
  : String): Boolean;
var
  first: Char;
begin
  if ((System.length(identifier) < 3) or (identifier[2] <> '.')) then
  begin
    result := False;
    Exit;
  end;

  first := identifier[1];
  // if ((first < '0') or (first > '2')) then
  // begin
  // result := false;
  // Exit;
  // end;
  if (not CharInSet(first, ['0' .. '2'])) then
  begin
    result := False;
    Exit;
  end;

  result := IsValidBranchID(identifier, 3);
end;

class function TDerObjectIdentifier.MakeOidStringFromBytes
  (const bytes: TCryptoLibByteArray): String;
var
  objId: TStringList;
  Value: Int64;
  bigValue: TBigInteger;
  first: Boolean;
  I, b: Int32;
begin
  Value := 0;
  bigValue := Default (TBigInteger);
  first := True;
  objId := TStringList.Create();
  objId.LineBreak := '';
  try
    I := 0;
    while I <> System.length(bytes) do
    begin
      b := Int32(bytes[I]);

      if (Value <= LONG_LIMIT) then
      begin
        Value := Value + (b and $7F);
        if ((b and $80) = 0) then // end of number reached
        begin
          if (first) then
          begin
            if (Value < 40) then
            begin
              objId.Add('0');
            end
            else if (Value < 80) then
            begin
              objId.Add('1');
              Value := Value - 40;
            end
            else
            begin
              objId.Add('2');
              Value := Value - 80;
            end;
            first := False;
          end;

          objId.Add('.');
          objId.Add(IntToStr(Value));
          Value := 0;
        end
        else
        begin
          Value := Value shl 7;
        end;
      end
      else
      begin
        if (not bigValue.IsInitialized) then
        begin
          bigValue := TBigInteger.ValueOf(Value);
        end;
        bigValue := bigValue.&Or(TBigInteger.ValueOf(b and $7F));
        if ((b and $80) = 0) then
        begin
          if (first) then
          begin
            objId.Add('2');
            bigValue := bigValue.Subtract(TBigInteger.ValueOf(80));
            first := False;
          end;

          objId.Add('.');
          objId.Add(bigValue.ToString());
          bigValue := Default (TBigInteger);
          Value := 0;
        end
        else
        begin
          bigValue := bigValue.ShiftLeft(7);
        end
      end;

      System.Inc(I);
    end;

    result := objId.Text;

  finally
    objId.Free;
  end;

end;

function TDerObjectIdentifier.ToString: String;
begin
  result := ID;
end;

procedure TDerObjectIdentifier.WriteField(const outputStream: TStream;
  const fieldValue: TBigInteger);
var
  byteCount, I: Int32;
  tmpValue: TBigInteger;
  tmp: TCryptoLibByteArray;
begin
  byteCount := (fieldValue.BitLength + 6) div 7;
  if (byteCount = 0) then
  begin
    outputStream.WriteByte(0);
  end
  else
  begin
    tmpValue := fieldValue;
    System.SetLength(tmp, byteCount);

    I := byteCount - 1;

    while I >= 0 do
    begin
      tmp[I] := Byte((tmpValue.Int32Value and $7F) or $80);
      tmpValue := tmpValue.ShiftRight(7);
      System.Dec(I);
    end;

    tmp[byteCount - 1] := tmp[byteCount - 1] and $7F;
    outputStream.Write(tmp[0], System.length(tmp));
  end;
end;

procedure TDerObjectIdentifier.WriteField(const outputStream: TStream;
  fieldValue: Int64);
var
  tempRes: TCryptoLibByteArray;
  Pos: Int32;
begin
  System.SetLength(tempRes, 9);
  Pos := 8;
  tempRes[Pos] := Byte(fieldValue and $7F);
  while (fieldValue >= (Int64(1) shl 7)) do
  begin
    fieldValue := TBits.Asr64(fieldValue, 7);
    System.Dec(Pos);
    tempRes[Pos] := Byte((fieldValue and $7F) or $80);
  end;
  outputStream.Write(tempRes[Pos], 9 - Pos);
end;

{ TAsn1EncodableVector }

procedure TAsn1EncodableVector.Add(const objs: array of IAsn1Encodable);
var
  obj: IAsn1Encodable;
begin
  for obj in objs do
  begin
    Flist.Add(obj);
  end;
end;

procedure TAsn1EncodableVector.AddOptional(const objs: array of IAsn1Encodable);
var
  obj: IAsn1Encodable;
begin
  if (System.length(objs) <> 0) then
  begin
    for obj in objs do
    begin
      if (obj <> Nil) then
      begin
        Flist.Add(obj);
      end;
    end;
  end;
end;

constructor TAsn1EncodableVector.Create(const v: array of IAsn1Encodable);
begin
  inherited Create();
  Flist := TList<IAsn1Encodable>.Create();
  Add(v);
end;

constructor TAsn1EncodableVector.Create();
begin
  inherited Create();
  Flist := TList<IAsn1Encodable>.Create();
end;

destructor TAsn1EncodableVector.Destroy;
begin
  Flist.Free;
  inherited Destroy;
end;

class function TAsn1EncodableVector.FromEnumerable
  (const e: TList<IAsn1Encodable>): IAsn1EncodableVector;
var
  v: IAsn1EncodableVector;
  obj: IAsn1Encodable;
begin
  v := TAsn1EncodableVector.Create();
  for obj in e do
  begin
    v.Add(obj);
  end;
  result := v;
end;

function TAsn1EncodableVector.GetCount: Int32;
begin
  result := Flist.count;
end;

function TAsn1EncodableVector.GetEnumerable
  : TCryptoLibGenericArray<IAsn1Encodable>;
begin
  result := Flist.ToArray;
end;

function TAsn1EncodableVector.GetSelf(Index: Int32): IAsn1Encodable;
begin
  result := Flist[index];
end;

{ TAsn1Generator }

constructor TAsn1Generator.Create(outStream: TStream);
begin
  F_out := outStream;
end;

function TAsn1Generator.GetOut: TStream;
begin
  result := F_out;
end;

{ TAsn1Null }

function TAsn1Null.ToString: String;
begin
  result := 'NULL';
end;

{ TAsn1OctetString }

function TAsn1OctetString.GetStr: TCryptoLibByteArray;
begin
  result := FStr;
end;

function TAsn1OctetString.GetParser: IAsn1OctetStringParser;
begin
  result := Self as IAsn1OctetStringParser;
end;

constructor TAsn1OctetString.Create(const Str: TCryptoLibByteArray);
begin
  Inherited Create();
  if (Str = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;

  FStr := Str;
end;

function TAsn1OctetString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerOctetString;
begin

  if (not Supports(asn1Object, IDerOctetString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := TArrayUtils.AreEqual(GetOctets(), other.GetOctets());
end;

function TAsn1OctetString.Asn1GetHashCode: Int32;
begin
  result := TArrayUtils.GetArrayHashCode(GetOctets());
end;

constructor TAsn1OctetString.Create(const obj: IAsn1Encodable);
begin
  Inherited Create();
  try
    FStr := obj.GetEncoded(TAsn1Encodable.Der);
  except
    on e: EIOCryptoLibException do
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SProcessingError,
        [e.Message]);
    end;
  end;
end;

class function TAsn1OctetString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IAsn1OctetString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IAsn1OctetString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TBerOctetString.FromSequence
    (TAsn1Sequence.GetInstance(o as TAsn1Object));
end;

class function TAsn1OctetString.GetInstance(const obj: TObject)
  : IAsn1OctetString;
var
  asn1TaggedObject: IAsn1TaggedObject;
begin
  if ((obj = Nil) or (obj is TAsn1OctetString)) then
  begin
    result := obj as TAsn1OctetString;
    Exit;
  end;

  // TODO: this needs to be deleted in V2
  if Supports(obj, IAsn1TaggedObject, asn1TaggedObject) then
  begin
    result := GetInstance(asn1TaggedObject.GetObject() as TAsn1Object);
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

function TAsn1OctetString.GetOctets: TCryptoLibByteArray;
begin
  result := Str;
end;

function TAsn1OctetString.GetOctetStream: TStream;
begin
  // used TBytesStream here for one pass creation and population with byte array :)
  result := TBytesStream.Create(Str);
end;

function TAsn1OctetString.ToString: String;
begin
  result := '#' + THex.Encode(Str);
end;

{ TAsn1Sequence }

function TAsn1Sequence.GetCurrent(const e: IAsn1Encodable): IAsn1Encodable;
var
  encObj: IAsn1Encodable;
begin
  encObj := e;

  // unfortunately null was allowed as a substitute for DER null
  if (encObj = Nil) then
  begin
    result := TDerNull.Instance;
    Exit;
  end;

  result := encObj;
end;

procedure TAsn1Sequence.AddObject(const obj: IAsn1Encodable);
begin
  FSeq.Add(obj);
end;

function TAsn1Sequence.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IAsn1Sequence;
  l1, l2: TCryptoLibGenericArray<IAsn1Encodable>;
  o1, o2: IAsn1Object;
  idx: Int32;
begin

  if (not Supports(asn1Object, IAsn1Sequence, other)) then
  begin
    result := False;
    Exit;
  end;

  if (count <> other.count) then
  begin
    result := False;
    Exit;
  end;

  l1 := GetEnumerable;
  l2 := other.GetEnumerable;

  for idx := System.Low(l1) to System.High(l1) do
  begin
    o1 := GetCurrent(l1[idx]).ToAsn1Object();
    o2 := GetCurrent(l2[idx]).ToAsn1Object();

    if (not(o1.Equals(o2))) then
    begin
      result := False;
      Exit;
    end;
  end;

  result := True;
end;

function TAsn1Sequence.Asn1GetHashCode: Int32;
var
  hc: Int32;
  o: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  hc := count;

  LListAsn1Encodable := Self.GetEnumerable;
  for o in LListAsn1Encodable do
  begin
    hc := hc * 17;
    if (o = Nil) then
    begin
      hc := hc xor TDerNull.Instance.GetHashCode();
    end
    else
    begin
      hc := hc xor o.GetHashCode();
    end;
  end;

  result := hc;
end;

constructor TAsn1Sequence.Create(capacity: Int32);
begin
  inherited Create();
  FSeq := TList<IAsn1Encodable>.Create();
  FSeq.capacity := capacity;
end;

destructor TAsn1Sequence.Destroy;
begin
  FSeq.Free;
  inherited Destroy;
end;

function TAsn1Sequence.GetCount: Int32;
begin
  result := FSeq.count;
end;

function TAsn1Sequence.GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  result := FSeq.ToArray;
end;

class function TAsn1Sequence.GetInstance(const obj: TObject): IAsn1Sequence;
var
  primitive: IAsn1Object;
  Sequence: IAsn1Sequence;
  res: IAsn1SequenceParser;
begin
  if ((obj = Nil) or (obj is TAsn1Sequence)) then
  begin
    result := obj as TAsn1Sequence;
    Exit;
  end;

  if (Supports(obj, IAsn1SequenceParser, res)) then
  begin
    result := TAsn1Sequence.GetInstance(res.ToAsn1Object() as TAsn1Object);
    Exit;

  end;

  if (obj is TAsn1Encodable) then
  begin
    primitive := (obj as TAsn1Encodable).ToAsn1Object();

    if (Supports(primitive, IAsn1Sequence, Sequence)) then
    begin
      result := Sequence;
      Exit;
    end;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SUnknownObject,
    [obj.ClassName]);

end;

class function TAsn1Sequence.GetInstance(const obj: TCryptoLibByteArray)
  : IAsn1Sequence;
begin
  try
    result := TAsn1Sequence.GetInstance(FromByteArray(obj) as TAsn1Object);
  except
    on e: EIOCryptoLibException do
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SInvalidSequence,
        [e.Message]);
    end;
  end;
end;

class function TAsn1Sequence.GetInstance(const obj: IAsn1TaggedObject;
  explicitly: Boolean): IAsn1Sequence;
var
  inner: IAsn1Object;
  Sequence: IAsn1Sequence;
begin
  inner := obj.GetObject();

  if (explicitly) then
  begin
    if (not(obj.isExplicit())) then
      raise EArgumentCryptoLibException.CreateRes(@SInvalidObject);

    result := inner as IAsn1Sequence;
    Exit;
  end;

  //
  // constructed object which appears to be explicitly tagged
  // when it should be implicit means we have to add the
  // surrounding sequence.
  //
  if (obj.isExplicit()) then
  begin
    if (Supports(obj, IBerTaggedObject)) then
    begin
      result := TBerSequence.Create(inner);
      Exit;
    end;

    result := TDerSequence.Create(inner);
    Exit;
  end;

  if (Supports(inner, IAsn1Sequence, Sequence)) then
  begin
    result := Sequence;
    Exit;
  end;
  raise EArgumentCryptoLibException.CreateResFmt(@SUnknownObject,
    [(obj as TAsn1TaggedObject).ClassName]);

end;

function TAsn1Sequence.GetParser: IAsn1SequenceParser;
begin
  result := TAsn1SequenceParserImpl.Create(Self as IAsn1Sequence);
end;

function TAsn1Sequence.GetSelf(Index: Integer): IAsn1Encodable;
begin
  result := FSeq[index];
end;

function TAsn1Sequence.ToString: String;
begin
  result := TCollectionUtilities.ToStructuredString(FSeq);
end;

{ TAsn1Sequence.TAsn1SequenceParserImpl }

constructor TAsn1Sequence.TAsn1SequenceParserImpl.Create
  (const outer: IAsn1Sequence);
begin
  inherited Create();
  Fouter := outer;
  Fmax := outer.count;
end;

function TAsn1Sequence.TAsn1SequenceParserImpl.ReadObject: IAsn1Convertible;
var
  obj: IAsn1Encodable;
  Sequence: IAsn1Sequence;
  asn1Set: IAsn1Set;
begin
  if (Findex = Fmax) then
  begin
    result := Nil;
    Exit;
  end;

  obj := Fouter[Findex];
  System.Inc(Findex);

  if (Supports(obj, IAsn1Sequence, Sequence)) then
  begin
    result := Sequence.parser;
    Exit;
  end;

  if (Supports(obj, IAsn1Set, asn1Set)) then
  begin
    result := asn1Set.parser;
    Exit;
  end;

  // NB: Asn1OctetString implements Asn1OctetStringParser directly
  // if (obj is Asn1OctetString)
  // return ((Asn1OctetString)obj).Parser;

  result := obj;
end;

function TAsn1Sequence.TAsn1SequenceParserImpl.ToAsn1Object: IAsn1Object;
begin
  result := Fouter;
end;

{ TDerOctetString }

constructor TDerOctetString.Create(const Str: TCryptoLibByteArray);
begin
  Inherited Create(Str);
end;

constructor TDerOctetString.Create(const obj: IAsn1Encodable);
begin
  Inherited Create(obj);
end;

destructor TDerOctetString.Destroy;
begin
  inherited Destroy;
end;

procedure TDerOctetString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.OctetString, Str);
end;

class procedure TDerOctetString.Encode(const derOut: TDerOutputStream;
  const bytes: TCryptoLibByteArray; offset, length: Int32);
begin
  derOut.WriteEncoded(TAsn1Tags.OctetString, bytes, offset, length);
end;

{ TBerOctetString }

constructor TBerOctetString.Create(const octets: TList<IDerOctetString>);
begin
  Inherited Create(ToBytes(octets));
  Focts := octets;
end;

constructor TBerOctetString.Create(const Str: TCryptoLibByteArray);
begin
  Inherited Create(Str);
end;

constructor TBerOctetString.Create(const obj: IAsn1Encodable);
begin
  Inherited Create(obj.ToAsn1Object());
end;

destructor TBerOctetString.Destroy;
begin
  Focts.Free;
  inherited Destroy;
end;

constructor TBerOctetString.Create(const obj: IAsn1Object);
begin
  Inherited Create(obj);
end;

procedure TBerOctetString.Encode(const derOut: TStream);
var
  oct: IDerOctetString;
  LListIDerOctetString: TCryptoLibGenericArray<IDerOctetString>;
begin
  if ((derOut is TAsn1OutputStream) or (derOut is TBerOutputStream)) then
  begin
    (derOut as TDerOutputStream).WriteByte(TAsn1Tags.Constructed or
      TAsn1Tags.OctetString);

    (derOut as TDerOutputStream).WriteByte($80);

    //
    // write out the octet array
    //
    LListIDerOctetString := Self.GetEnumerable;
    for oct in LListIDerOctetString do
    begin
      (derOut as TDerOutputStream).WriteObject(oct);
    end;

    (derOut as TDerOutputStream).WriteByte($00);
    (derOut as TDerOutputStream).WriteByte($00);
  end
  else
  begin
    (Inherited Encode(derOut));
  end;
end;

class function TBerOctetString.FromSequence(const seq: IAsn1Sequence)
  : IBerOctetString;
var
  v: TList<IDerOctetString>;
  obj: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  v := TList<IDerOctetString>.Create();

  LListAsn1Encodable := seq.GetEnumerable;
  for obj in LListAsn1Encodable do
  begin
    v.Add(obj as IDerOctetString);
  end;

  result := TBerOctetString.Create(v);

end;

function TBerOctetString.GenerateOcts: TList<IDerOctetString>;
var
  I, endPoint: Int32;
  nStr: TCryptoLibByteArray;
begin
  result := TList<IDerOctetString>.Create();
  I := 0;
  while I < System.length(Str) do
  begin
    endPoint := Min(System.length(Str), I + MaxLength);

    System.SetLength(nStr, endPoint - I);

    System.Move(Str[I], nStr[0], System.length(nStr) * System.SizeOf(Byte));
    result.Add(TDerOctetString.Create(nStr) as IDerOctetString);
    System.Inc(I, MaxLength);
  end;
end;

function TBerOctetString.GetEnumerable: TCryptoLibGenericArray<IDerOctetString>;
var
  LList: TList<IDerOctetString>;
begin

  if (Focts = Nil) then
  begin
    LList := GenerateOcts();
    try
      result := LList.ToArray;
      Exit;
    finally
      LList.Free;
    end;
  end;

  result := Focts.ToArray;

end;

function TBerOctetString.GetOctets: TCryptoLibByteArray;
begin
  result := Str;
end;

class function TBerOctetString.ToBytes(octs: TList<IDerOctetString>)
  : TCryptoLibByteArray;
var
  bOut: TMemoryStream;
  o: IDerOctetString;
  octets: TCryptoLibByteArray;
begin
  bOut := TMemoryStream.Create();
  try
    for o in octs do
    begin
      octets := o.GetOctets();
      bOut.Write(octets[0], System.length(octets));
    end;

    System.SetLength(result, bOut.Size);
    bOut.Position := 0;
    bOut.Read(result[0], bOut.Size);
  finally
    bOut.Free;
  end;
end;

{ TDerNull }

function TDerNull.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
begin
  result := Supports(asn1Object, IDerNull);
end;

function TDerNull.Asn1GetHashCode: Int32;
begin
  result := -1;
end;

{$IFNDEF _FIXINSIGHT_}

constructor TDerNull.Create(dummy: Int32);
begin
  Inherited Create();
end;
{$ENDIF}

procedure TDerNull.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Null, ZeroBytes);
end;

class function TDerNull.GetInstance: IDerNull;
begin
  result := TDerNull.Create(0);
end;

{ TDerSequence }

class function TDerSequence.GetEmpty: IDerSequence;
begin
  result := TDerSequence.Create();
end;

constructor TDerSequence.Create(const obj: IAsn1Encodable);
begin
  Inherited Create(1);
  AddObject(obj);
end;

constructor TDerSequence.Create;
begin
  Inherited Create(0);
end;

constructor TDerSequence.Create(const v: IAsn1EncodableVector);
var
  ae: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  Inherited Create(v.count);
  LListAsn1Encodable := v.GetEnumerable;
  for ae in LListAsn1Encodable do
  begin
    AddObject(ae);
  end;
end;

constructor TDerSequence.Create(const v: array of IAsn1Encodable);
var
  ae: IAsn1Encodable;
begin
  Inherited Create(System.length(v));
  for ae in v do
  begin
    AddObject(ae);
  end;
end;

destructor TDerSequence.Destroy;
begin
  inherited Destroy;
end;

procedure TDerSequence.Encode(const derOut: TStream);
var
  bOut: TMemoryStream;
  dOut: TDerOutputStream;
  obj: IAsn1Encodable;
  bytes: TCryptoLibByteArray;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  // TODO Intermediate buffer could be avoided if we could calculate expected length
  bOut := TMemoryStream.Create();
  dOut := TDerOutputStream.Create(bOut);
  try

    LListAsn1Encodable := Self.GetEnumerable;
    for obj in LListAsn1Encodable do
    begin
      dOut.WriteObject(obj);
    end;

    System.SetLength(bytes, bOut.Size);
    bOut.Position := 0;
    bOut.Read(bytes[0], bOut.Size);
  finally
    bOut.Free;
    dOut.Free;
  end;

  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Sequence or
    TAsn1Tags.Constructed, bytes);
end;

class function TDerSequence.FromVector(const v: IAsn1EncodableVector)
  : IDerSequence;
begin
  if v.count < 1 then
  begin
    result := Empty;
  end
  else
  begin
    result := TDerSequence.Create(v);
  end;

end;

{ TBerSequence }

class function TBerSequence.GetEmpty: IBerSequence;
begin
  result := TBerSequence.Create();
end;

constructor TBerSequence.Create(const obj: IAsn1Encodable);
begin
  Inherited Create(obj);
end;

constructor TBerSequence.Create;
begin
  Inherited Create();
end;

constructor TBerSequence.Create(const v: IAsn1EncodableVector);
begin
  Inherited Create(v);
end;

destructor TBerSequence.Destroy;
begin
  inherited Destroy;
end;

constructor TBerSequence.Create(const v: array of IAsn1Encodable);
begin
  Inherited Create(v);
end;

procedure TBerSequence.Encode(const derOut: TStream);
var
  o: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin

  if ((derOut is TAsn1OutputStream) or (derOut is TBerOutputStream)) then
  begin
    (derOut as TDerOutputStream).WriteByte(TAsn1Tags.Sequence or
      TAsn1Tags.Constructed);
    (derOut as TDerOutputStream).WriteByte($80);

    LListAsn1Encodable := Self.GetEnumerable;
    for o in LListAsn1Encodable do
    begin
      (derOut as TDerOutputStream).WriteObject(o);
    end;

    (derOut as TDerOutputStream).WriteByte($00);
    (derOut as TDerOutputStream).WriteByte($00);
  end
  else
  begin
    (Inherited Encode(derOut));
  end;

end;

class function TBerSequence.FromVector(const v: IAsn1EncodableVector)
  : IBerSequence;
begin
  if v.count < 1 then
  begin
    result := Empty;
  end
  else
  begin
    result := TBerSequence.Create(v);
  end;

end;

{ TAsn1TaggedObject }

function TAsn1TaggedObject.GetObject: IAsn1Object;
begin
  if (Fobj <> Nil) then
  begin
    result := Fobj.ToAsn1Object();
    Exit;
  end;

  result := Nil;
end;

function TAsn1TaggedObject.GetTagNo: Int32;
begin
  result := FtagNo;
end;

function TAsn1TaggedObject.Getexplicitly: Boolean;
begin
  result := Fexplicitly;
end;

function TAsn1TaggedObject.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IAsn1TaggedObject;
begin

  if (not Supports(asn1Object, IAsn1TaggedObject, other)) then
  begin
    result := False;
    Exit;
  end;

  result := ((tagNo = other.tagNo) and
    // TODO Should this be part of equality?
    (explicitly = other.explicitly)) and
    (GetObject().Equals(other.GetObject()));
end;

function TAsn1TaggedObject.Asn1GetHashCode: Int32;
var
  code: Int32;
begin
  code := Abs(tagNo);

  // TODO: actually this is wrong - the problem is that a re-encoded
  // object may end up with a different hashCode due to implicit
  // tagging. As implicit tagging is ambiguous if a sequence is involved
  // it seems the only correct method for both equals and hashCode is to
  // compare the encodings...
  // code := code xor explicitly.GetHashCode();

  if (Fobj <> Nil) then
  begin
    code := code xor Fobj.GetHashCode();
  end;

  result := code;
end;

constructor TAsn1TaggedObject.Create(tagNo: Int32; const obj: IAsn1Encodable);
begin
  Inherited Create();
  Fexplicitly := True;
  FtagNo := tagNo;
  Fobj := obj;
end;

constructor TAsn1TaggedObject.Create(explicitly: Boolean; tagNo: Int32;
  const obj: IAsn1Encodable);
begin
  Inherited Create();
  // IAsn1Choice marker interface 'insists' on explicit tagging
  Fexplicitly := explicitly or (Supports(obj, IAsn1Choice));
  FtagNo := tagNo;
  Fobj := obj;
end;

class function TAsn1TaggedObject.GetInstance(obj: TObject): IAsn1TaggedObject;
begin
  if ((obj = Nil) or (obj is TAsn1TaggedObject)) then
  begin
    result := obj as TAsn1TaggedObject;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SUnknownObject,
    [obj.ClassName]);
end;

function TAsn1TaggedObject.Getobj: IAsn1Encodable;
begin
  result := Fobj;
end;

class function TAsn1TaggedObject.GetInstance(const obj: IAsn1TaggedObject;
  explicitly: Boolean): IAsn1TaggedObject;
begin
  if (explicitly) then
  begin
    result := obj.GetObject() as IAsn1TaggedObject;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateRes(@SImplicitObject);
end;

function TAsn1TaggedObject.GetObjectParser(tag: Int32; isExplicit: Boolean)
  : IAsn1Convertible;
begin
  case tag of

    TAsn1Tags.&Set:
      begin
        result := TAsn1Set.GetInstance(Self as IAsn1TaggedObject,
          isExplicit).parser;
        Exit;
      end;
    TAsn1Tags.Sequence:
      begin
        result := TAsn1Sequence.GetInstance(Self as IAsn1TaggedObject,
          isExplicit).parser;
        Exit;
      end;
    TAsn1Tags.OctetString:
      begin
        result := TAsn1OctetString.GetInstance(Self as IAsn1TaggedObject,
          isExplicit).parser;
        Exit;
      end;
  end;

  if (isExplicit) then
  begin
    result := GetObject();
    Exit;
  end;

  raise ENotImplementedCryptoLibException.CreateResFmt(@SImplicitTag, [tag]);

end;

class function TAsn1TaggedObject.IsConstructed(isExplicit: Boolean;
  const obj: IAsn1Object): Boolean;
var
  Tagged: IAsn1TaggedObject;
begin
  if ((isExplicit) or (Supports(obj, IAsn1Sequence)) or
    (Supports(obj, IAsn1Set))) then
  begin
    result := True;
    Exit;
  end;

  if (not Supports(obj, IAsn1TaggedObject, Tagged)) then
  begin
    result := False;
    Exit;
  end;
  result := IsConstructed(Tagged.isExplicit(), Tagged.GetObject());
end;

function TAsn1TaggedObject.IsEmpty: Boolean;
begin
  result := False; // empty;
end;

function TAsn1TaggedObject.isExplicit: Boolean;
begin
  result := Fexplicitly;
end;

function TAsn1TaggedObject.ToString: String;
begin
  result := '[' + IntToStr(tagNo) + ']' + (Fobj as TAsn1Encodable).ClassName;
end;

{ TAsn1Set }

procedure TAsn1Set.AddObject(const obj: IAsn1Encodable);
begin
  F_set.Add(obj);
end;

function TAsn1Set.GetCurrent(const e: IAsn1Encodable): IAsn1Encodable;
var
  encObj: IAsn1Encodable;
begin
  encObj := e;

  // unfortunately null was allowed as a substitute for DER null
  if (encObj = Nil) then
  begin
    result := TDerNull.Instance;
    Exit;
  end;

  result := encObj;
end;

function TAsn1Set.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IAsn1Set;
  l1, l2: TCryptoLibGenericArray<IAsn1Encodable>;
  o1, o2: IAsn1Object;
  idx: Int32;
begin

  if (not Supports(asn1Object, IAsn1Set, other)) then
  begin
    result := False;
    Exit;
  end;

  if (count <> other.count) then
  begin
    result := False;
    Exit;
  end;

  l1 := GetEnumerable;
  l2 := other.GetEnumerable;

  for idx := System.Low(l1) to System.High(l1) do
  begin
    o1 := GetCurrent(l1[idx]).ToAsn1Object();
    o2 := GetCurrent(l2[idx]).ToAsn1Object();

    if (not(o1.Equals(o2))) then
    begin
      result := False;
      Exit;
    end;
  end;

  result := True;
end;

function TAsn1Set.Asn1GetHashCode: Int32;
var
  hc: Int32;
  o: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  hc := count;

  LListAsn1Encodable := Self.GetEnumerable;
  for o in LListAsn1Encodable do
  begin
    hc := hc * 17;
    if (o = Nil) then
    begin
      hc := hc xor TDerNull.Instance.GetHashCode();
    end
    else
    begin
      hc := hc xor o.GetHashCode();
    end;
  end;

  result := hc;
end;

constructor TAsn1Set.Create(capacity: Int32);
begin
  Inherited Create();
  F_set := TList<IAsn1Encodable>.Create();
  F_set.capacity := capacity;
  FisSorted := False;
end;

destructor TAsn1Set.Destroy;
begin
  F_set.Free;
  inherited Destroy;
end;

function TAsn1Set.GetCount: Int32;
begin
  result := F_set.count;
end;

function TAsn1Set.GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  result := F_set.ToArray;
end;

class function TAsn1Set.GetInstance(const obj: TCryptoLibByteArray): IAsn1Set;
begin
  try
    result := TAsn1Set.GetInstance(FromByteArray(obj) as TAsn1Object);
  except
    on e: EIOCryptoLibException do
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SInvalidSequence,
        [e.Message]);
    end;
  end;
end;

class function TAsn1Set.GetInstance(const obj: IAsn1TaggedObject;
  explicitly: Boolean): IAsn1Set;
var
  inner: IAsn1Object;
  asn1Set: IAsn1Set;
  asn1Sequence: IAsn1Sequence;
  v: IAsn1EncodableVector;
  ae: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  inner := obj.GetObject();

  if (explicitly) then
  begin
    if (not(obj.isExplicit())) then
      raise EArgumentCryptoLibException.CreateRes(@SInvalidObject);

    result := inner as IAsn1Set;
    Exit;
  end;

  //
  // constructed object which appears to be explicitly tagged
  // when it should be implicit means we have to add the
  // surrounding sequence.
  //
  if (obj.isExplicit()) then
  begin

    result := TDerSet.Create(inner);
    Exit;
  end;

  if (Supports(inner, IAsn1Set, asn1Set)) then
  begin
    result := asn1Set;
    Exit;
  end;
  //
  // in this case the parser returns a sequence, convert it
  // into a set.
  //

  if (Supports(inner, IAsn1Sequence, asn1Sequence)) then
  begin
    v := TAsn1EncodableVector.Create();

    LListAsn1Encodable := asn1Sequence.GetEnumerable;
    for ae in LListAsn1Encodable do
    begin
      v.Add(ae);
    end;

    // TODO Should be able to construct set directly from sequence?
    result := TDerSet.Create(v, False);
    Exit;
  end;
  raise EArgumentCryptoLibException.CreateResFmt(@SUnknownObject,
    [(obj as TAsn1TaggedObject).ClassName]);

end;

class function TAsn1Set.GetInstance(const obj: TObject): IAsn1Set;
var
  primitive: IAsn1Object;
  asn1Set: IAsn1Set;
  res: IAsn1SetParser;
begin
  if ((obj = Nil) or (obj is TAsn1Set)) then
  begin
    result := obj as TAsn1Set;
    Exit;
  end;

  if (Supports(obj, IAsn1SetParser, res)) then
  begin
    result := TAsn1Set.GetInstance(res.ToAsn1Object() as TAsn1Object);
    Exit;

  end;

  if (obj is TAsn1Encodable) then
  begin
    primitive := (obj as TAsn1Encodable).ToAsn1Object();

    if (Supports(primitive, IAsn1Set, asn1Set)) then
    begin
      result := asn1Set;
      Exit;
    end;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SUnknownObject,
    [obj.ClassName]);

end;

function TAsn1Set.GetParser: IAsn1SetParser;
begin
  result := TAsn1SetParserImpl.Create(Self as IAsn1Set);
end;

function TAsn1Set.GetSelf(Index: Integer): IAsn1Encodable;
begin
  result := F_set[index];
end;

function TAsn1Set.LessThanOrEqual(const a, b: TCryptoLibByteArray): Boolean;
var
  len, I: Int32;
begin
  len := Math.Min(System.length(a), System.length(b));

  I := 0;
  while I <> len do
  begin

    if (a[I] <> b[I]) then
    begin

      result := (a[I]) < (b[I]);
      Exit;
    end;
    System.Inc(I);
  end;

  result := len = System.length(a);
end;

procedure TAsn1Set.Sort;
var
  swapped: Boolean;
  lastSwap, Index, swapIndex: Int32;
  a, b: TCryptoLibByteArray;
  temp: IAsn1Encodable;

begin
  if (not FisSorted) then
  begin
    FisSorted := True;
    if (F_set.count > 1) then
    begin
      swapped := True;
      lastSwap := F_set.count - 1;

      while (swapped) do
      begin
        index := 0;
        swapIndex := 0;
        a := F_set[0].GetEncoded(TAsn1Encodable.Der);

        swapped := False;

        while (index <> lastSwap) do
        begin
          b := F_set[index + 1].GetEncoded(TAsn1Encodable.Der);

          if (LessThanOrEqual(a, b)) then
          begin
            a := b;
          end
          else
          begin
            temp := F_set[index];
            // Review being picky for copy
            // temp := System.Copy(F_set.List, Index, 1)[0];
            F_set[index] := F_set[index + 1];
            F_set[index + 1] := temp;

            swapped := True;
            swapIndex := index;
          end;

          System.Inc(index);
        end;

        lastSwap := swapIndex;
      end;
    end;
  end;
end;

function TAsn1Set.ToArray: TCryptoLibGenericArray<IAsn1Encodable>;
var
  values: TCryptoLibGenericArray<IAsn1Encodable>;
  I: Int32;
begin
  System.SetLength(values, count);
  for I := 0 to System.Pred(count) do
  begin
    values[I] := Self[I];
  end;

  result := values;
end;

function TAsn1Set.ToString: String;
begin
  result := TCollectionUtilities.ToStructuredString(F_set);
end;

{ TAsn1Set.TAsn1SetParserImpl }

constructor TAsn1Set.TAsn1SetParserImpl.Create(const outer: IAsn1Set);
begin
  Inherited Create();
  Fouter := outer;
  Fmax := outer.count;
end;

function TAsn1Set.TAsn1SetParserImpl.ReadObject: IAsn1Convertible;
var
  obj: IAsn1Encodable;
  Sequence: IAsn1Sequence;
  asn1Set: IAsn1Set;
begin
  if (Findex = Fmax) then
  begin
    result := Nil;
    Exit;
  end;

  obj := Fouter[Findex];
  System.Inc(Findex);

  if (Supports(obj, IAsn1Sequence, Sequence)) then
  begin
    result := Sequence.parser;
    Exit;
  end;

  if (Supports(obj, IAsn1Set, asn1Set)) then
  begin
    result := asn1Set.parser;
    Exit;
  end;

  // NB: Asn1OctetString implements Asn1OctetStringParser directly
  // if (obj is Asn1OctetString)
  // return ((Asn1OctetString)obj).Parser;

  result := obj;
end;

function TAsn1Set.TAsn1SetParserImpl.ToAsn1Object: IAsn1Object;
begin
  result := Fouter;
end;

{ TDerSet }

class function TDerSet.GetEmpty: IDerSet;
begin
  result := TDerSet.Create();
end;

constructor TDerSet.Create(const v: array of IAsn1Encodable);
var
  o: IAsn1Encodable;
begin
  Inherited Create(System.length(v));
  for o in v do
  begin
    AddObject(o);
  end;

  Sort();
end;

constructor TDerSet.Create;
begin
  Inherited Create(0);
end;

constructor TDerSet.Create(const v: IAsn1EncodableVector;
  needsSorting: Boolean);
var
  o: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  Inherited Create(v.count);
  LListAsn1Encodable := v.GetEnumerable;
  for o in LListAsn1Encodable do
  begin
    AddObject(o);
  end;

  if (needsSorting) then
  begin
    Sort();
  end;
end;

constructor TDerSet.Create(const obj: IAsn1Encodable);
begin
  Inherited Create(1);
  AddObject(obj);
end;

constructor TDerSet.Create(const v: IAsn1EncodableVector);
begin
  Create(v, True);
end;

destructor TDerSet.Destroy;
begin
  inherited Destroy;
end;

procedure TDerSet.Encode(const derOut: TStream);
var
  bOut: TMemoryStream;
  dOut: TDerOutputStream;
  obj: IAsn1Encodable;
  bytes: TCryptoLibByteArray;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  // TODO Intermediate buffer could be avoided if we could calculate expected length
  bOut := TMemoryStream.Create();
  dOut := TDerOutputStream.Create(bOut);

  try
    LListAsn1Encodable := Self.GetEnumerable;
    for obj in LListAsn1Encodable do
    begin
      dOut.WriteObject(obj);
    end;

    System.SetLength(bytes, bOut.Size);
    bOut.Position := 0;
    bOut.Read(bytes[0], bOut.Size);
  finally
    bOut.Free;
    dOut.Free;
  end;

  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.&Set or
    TAsn1Tags.Constructed, bytes);
end;

class function TDerSet.FromVector(const v: IAsn1EncodableVector;
  needsSorting: Boolean): IDerSet;
begin
  if v.count < 1 then
  begin
    result := Empty;
  end
  else
  begin
    result := TDerSet.Create(v, needsSorting);
  end;
end;

class function TDerSet.FromVector(const v: IAsn1EncodableVector): IDerSet;
begin
  if v.count < 1 then
  begin
    result := Empty;
  end
  else
  begin
    result := TDerSet.Create(v);
  end;
end;

{ TAsn1StreamParser }

procedure TAsn1StreamParser.Set00Check(enabled: Boolean);
var
  indefiniteLengthInputStream: TIndefiniteLengthInputStream;
begin
  if (F_in is TIndefiniteLengthInputStream) then
  begin
    indefiniteLengthInputStream := F_in as TIndefiniteLengthInputStream;
    indefiniteLengthInputStream.SetEofOn00(enabled);
  end;
end;

constructor TAsn1StreamParser.Create(const inStream: TStream);
begin
  Create(inStream, TAsn1InputStream.FindLimit(inStream));
end;

constructor TAsn1StreamParser.Create(const inStream: TStream; limit: Int32);
begin
  Inherited Create();
  F_in := inStream;
  F_limit := limit;
  System.SetLength(FtmpBuffers, 16);
end;

constructor TAsn1StreamParser.Create(const encoding: TCryptoLibByteArray);
begin
  // used TBytesStream here for one pass creation and population with byte array :)
  Create(TBytesStream.Create(encoding), System.length(encoding));

end;

destructor TAsn1StreamParser.Destroy;
begin
  F_in.Free;
  inherited Destroy;
end;

function TAsn1StreamParser.ReadVector: IAsn1EncodableVector;
var
  v: IAsn1EncodableVector;
  obj: IAsn1Convertible;
begin
  v := TAsn1EncodableVector.Create();

  obj := ReadObject();
  while (obj <> Nil) do
  begin
    v.Add([obj.ToAsn1Object()]);
    obj := ReadObject();
  end;

  result := v;
end;

function TAsn1StreamParser.ReadImplicit(Constructed: Boolean; tag: Int32)
  : IAsn1Convertible;
begin
  if (F_in is TIndefiniteLengthInputStream) then
  begin
    if (not Constructed) then
    begin
      raise EIOCryptoLibException.CreateRes(@SIndefiniteLength);
    end;

    result := ReadIndef(tag);
    Exit;
  end;

  if (Constructed) then
  begin
    case tag of

      TAsn1Tags.&Set:
        begin
          result := TDerSetParser.Create(Self as IAsn1StreamParser);
          Exit;
        end;
      TAsn1Tags.Sequence:
        begin
          result := TDerSequenceParser.Create(Self as IAsn1StreamParser);
          Exit;
        end;
      TAsn1Tags.OctetString:
        begin
          result := TBerOctetStringParser.Create(Self as IAsn1StreamParser);
          Exit;
        end;

    end;
  end
  else
  begin
    case tag of

      TAsn1Tags.&Set:
        begin
          raise EAsn1CryptoLibException.CreateRes(@SUnConstructedEncoding);
        end;
      TAsn1Tags.Sequence:
        begin
          raise EAsn1CryptoLibException.CreateRes(@SUnConstructedEncoding2);
        end;
      TAsn1Tags.OctetString:
        begin
          result := TDerOctetStringParser.Create
            (F_in as TDefiniteLengthInputStream);
          Exit;
        end;
    end;

  end;

  raise EAsn1CryptoLibException.CreateRes(@SImplicitTagging);

end;

function TAsn1StreamParser.ReadIndef(tagValue: Int32): IAsn1Convertible;
begin
  // Note: INDEF => CONSTRUCTED

  // TODO There are other tags that may be constructed (e.g. BIT_STRING)
  case tagValue of
    TAsn1Tags.External:
      begin
        result := TDerExternalParser.Create(Self as IAsn1StreamParser);
        Exit;
      end;

    TAsn1Tags.OctetString:
      begin
        result := TBerOctetStringParser.Create(Self as IAsn1StreamParser);
        Exit;
      end;

    TAsn1Tags.Sequence:
      begin
        result := TBerSequenceParser.Create(Self as IAsn1StreamParser);
        Exit;
      end;

    TAsn1Tags.&Set:
      begin
        result := TBerSetParser.Create(Self as IAsn1StreamParser);
        Exit;
      end;

  else
    begin
      raise EAsn1CryptoLibException.CreateResFmt(@SUnknownObjectBER,
        [tagValue]);
    end;

  end;
end;

function TAsn1StreamParser.ReadObject: IAsn1Convertible;
var
  tag, tagNo, &length: Int32;
  IsConstructed: Boolean;
  indIn: TIndefiniteLengthInputStream;
  sp: IAsn1StreamParser;
  defIn: TDefiniteLengthInputStream;
begin
  tag := TStreamSorter.ReadByte(F_in);

  if (tag = -1) then
  begin
    result := Nil;
    Exit;
  end;

  // turn off looking for "00" while we resolve the tag
  Set00Check(False);

  //
  // calculate tag number
  //
  tagNo := TAsn1InputStream.ReadTagNumber(F_in, tag);

  IsConstructed := (tag and TAsn1Tags.Constructed) <> 0;

  //
  // calculate length
  //
  length := TAsn1InputStream.ReadLength(F_in, F_limit);

  if (length < 0) then // indefinite length method
  begin
    if (not IsConstructed) then
    begin
      raise EIOCryptoLibException.CreateRes(@SIndefiniteLength);
    end;

    indIn := TIndefiniteLengthInputStream.Create(F_in, F_limit);

    sp := TAsn1StreamParser.Create(indIn, F_limit);

    if ((tag and TAsn1Tags.Application) <> 0) then
    begin

      result := TBerApplicationSpecificParser.Create(tagNo, sp);
      Exit;

    end;

    if ((tag and TAsn1Tags.Tagged) <> 0) then
    begin

      result := TBerTaggedObjectParser.Create(True, tagNo, sp);
      Exit;

    end;

    result := sp.ReadIndef(tagNo);
    Exit;

  end;

  defIn := TDefiniteLengthInputStream.Create(F_in, length);

  if ((tag and TAsn1Tags.Application) <> 0) then
  begin
    try
      result := TDerApplicationSpecific.Create(IsConstructed, tagNo,
        defIn.ToArray());
      Exit;
    finally
      defIn.Free;
    end;
  end;

  if ((tag and TAsn1Tags.Tagged) <> 0) then
  begin
    result := TBerTaggedObjectParser.Create(IsConstructed, tagNo,
      TAsn1StreamParser.Create(defIn) as IAsn1StreamParser);
    Exit;

  end;

  if (IsConstructed) then
  begin
    // TODO There are other tags that may be constructed (e.g. BitString)
    case tagNo of

      TAsn1Tags.OctetString:
        begin
          //
          // yes, people actually do this...
          //

          result := TBerOctetStringParser.Create(TAsn1StreamParser.Create(defIn)
            as IAsn1StreamParser);
          Exit;

        end;
      TAsn1Tags.Sequence:
        begin

          result := TDerSequenceParser.Create(TAsn1StreamParser.Create(defIn)
            as IAsn1StreamParser);
          Exit;

        end;
      TAsn1Tags.&Set:
        begin

          result := TDerSetParser.Create(TAsn1StreamParser.Create(defIn)
            as IAsn1StreamParser);
          Exit;

        end;

      TAsn1Tags.External:
        begin

          result := TDerExternalParser.Create(TAsn1StreamParser.Create(defIn)
            as IAsn1StreamParser);
          Exit;

        end;
    else
      begin
        defIn.Free; // free the stream incase an unsupported tag is encountered.
        raise EIOCryptoLibException.CreateResFmt(@SUnknownTag, [tagNo]);
      end;

    end;
  end;

  // Some primitive encodings can be handled by parsers too...
  case tagNo of
    TAsn1Tags.OctetString:
      begin
        result := TDerOctetStringParser.Create(defIn);
        Exit;
      end;
  end;

  try
    try
      result := TAsn1InputStream.CreatePrimitiveDerObject(tagNo, defIn,
        FtmpBuffers);
      Exit;

    except

      on e: EArgumentCryptoLibException do
      begin
        raise EAsn1CryptoLibException.CreateResFmt(@SCorruptedStream,
          [e.Message]);
      end;

    end;
  finally
    defIn.Free;
  end;

end;

function TAsn1StreamParser.ReadTaggedObject(Constructed: Boolean; tag: Int32)
  : IAsn1Object;
var
  defIn: TDefiniteLengthInputStream;
  v: IAsn1EncodableVector;
begin
  if (not Constructed) then
  begin
    // Note: !CONSTRUCTED => IMPLICIT
    defIn := F_in as TDefiniteLengthInputStream;
    result := TDerTaggedObject.Create(False, tag,
      TDerOctetString.Create(defIn.ToArray()));
    Exit;
  end;

  v := ReadVector();

  if (F_in is TIndefiniteLengthInputStream) then
  begin
    if v.count = 1 then
    begin
      result := TBerTaggedObject.Create(True, tag, v[0]);
      Exit;
    end
    else
    begin
      result := TBerTaggedObject.Create(False, tag, TBerSequence.FromVector(v));
      Exit;
    end;

  end;

  if v.count = 1 then
  begin
    result := TDerTaggedObject.Create(True, tag, v[0]);
    Exit;
  end
  else
  begin
    result := TDerTaggedObject.Create(False, tag, TDerSequence.FromVector(v));
    Exit;
  end;

end;

{ TDerSetParser }

constructor TDerSetParser.Create(const parser: IAsn1StreamParser);
begin
  F_parser := parser;
end;

function TDerSetParser.ReadObject: IAsn1Convertible;
begin
  result := F_parser.ReadObject();
end;

function TDerSetParser.ToAsn1Object: IAsn1Object;
begin
  result := TDerSet.Create(F_parser.ReadVector(), False);
end;

{ TDerSequenceParser }

constructor TDerSequenceParser.Create(const parser: IAsn1StreamParser);
begin
  F_parser := parser;
end;

function TDerSequenceParser.ReadObject: IAsn1Convertible;
begin
  result := F_parser.ReadObject();
end;

function TDerSequenceParser.ToAsn1Object: IAsn1Object;
begin
  result := TDerSequence.Create(F_parser.ReadVector());
end;

{ TDerApplicationSpecific }

function TDerApplicationSpecific.GetApplicationTag: Int32;
begin
  result := Ftag;
end;

function TDerApplicationSpecific.GetContents: TCryptoLibByteArray;
begin
  result := Foctets;
end;

function TDerApplicationSpecific.IsConstructed: Boolean;
begin
  result := FisConstructed;
end;

function TDerApplicationSpecific.GetLengthOfHeader
  (const data: TCryptoLibByteArray): Int32;
var
  &length, Size: Int32;
begin
  length := data[1]; // TODO: assumes 1 byte tag

  if (length = $80) then
  begin
    result := 2; // indefinite-length encoding
    Exit;
  end;

  if (length > 127) then
  begin
    Size := length and $7F;

    // Note: The invalid long form "0xff" (see X.690 8.1.3.5c) will be caught here
    if (Size > 4) then
    begin
      raise EInvalidOperationCryptoLibException.CreateResFmt
        (@SInvalidDerLength, [Size]);
    end;

    result := Size + 2;
    Exit;
  end;

  result := 2;
end;

constructor TDerApplicationSpecific.Create(tag: Int32;
  const obj: IAsn1Encodable);
begin
  Create(True, tag, obj);
end;

constructor TDerApplicationSpecific.Create(tag: Int32;
  const octets: TCryptoLibByteArray);
begin
  Create(False, tag, octets);
end;

constructor TDerApplicationSpecific.Create(IsConstructed: Boolean; tag: Int32;
  const octets: TCryptoLibByteArray);
begin
  Inherited Create();
  FisConstructed := IsConstructed;
  Ftag := tag;
  Foctets := octets;
end;

function TDerApplicationSpecific.Asn1Equals(const asn1Object
  : IAsn1Object): Boolean;
var
  other: IDerApplicationSpecific;
begin

  if (not Supports(asn1Object, IDerApplicationSpecific, other)) then
  begin
    result := False;
    Exit;
  end;

  result := (IsConstructed = other.IsConstructed) and
    (ApplicationTag = other.ApplicationTag) and
    TArrayUtils.AreEqual(GetContents, other.GetContents);
end;

function TDerApplicationSpecific.Asn1GetHashCode: Int32;
var
  HashCode: Int32;
begin
  case IsConstructed of
    True:
      HashCode := 1;
    False:
      HashCode := 0;
  end;
  result := HashCode xor Ftag xor TArrayUtils.GetArrayHashCode(Foctets);
end;

constructor TDerApplicationSpecific.Create(tagNo: Int32;
  const vec: IAsn1EncodableVector);
var
  bOut: TMemoryStream;
  bs: TCryptoLibByteArray;
  I: Int32;
  val: IAsn1Encodable;
begin
  Inherited Create();
  Ftag := tagNo;
  FisConstructed := True;

  bOut := TMemoryStream.Create();
  try
    I := 0;
    while I <> vec.count do

    begin
      try
        val := vec[I];
        bs := val.GetDerEncoded();
        bOut.Write(bs[0], System.length(bs));
      except
        on e: EIOCryptoLibException do
        begin
          raise EInvalidOperationCryptoLibException.CreateResFmt
            (@SMalformedObject, [e.Message]);
        end;
      end;
      System.Inc(I);
    end;

    System.SetLength(Foctets, bOut.Size);
    bOut.Position := 0;
    bOut.Read(Foctets[0], bOut.Size);

  finally
    bOut.Free;
  end;

end;

procedure TDerApplicationSpecific.Encode(const derOut: TStream);
var
  classBits: Int32;
begin
  classBits := TAsn1Tags.Application;
  if (IsConstructed) then
  begin
    classBits := classBits or TAsn1Tags.Constructed;
  end;

  (derOut as TDerOutputStream).WriteEncoded(classBits, Ftag, Foctets);
end;

constructor TDerApplicationSpecific.Create(isExplicit: Boolean; tag: Int32;
  const obj: IAsn1Encodable);
var
  asn1Obj: IAsn1Object;
  data, tmp: TCryptoLibByteArray;
  lenBytes: Int32;
begin
  Inherited Create();
  asn1Obj := obj.ToAsn1Object();

  data := asn1Obj.GetDerEncoded();

  FisConstructed := TAsn1TaggedObject.IsConstructed(isExplicit, asn1Obj);
  Ftag := tag;

  if (isExplicit) then
  begin
    Foctets := data;
  end
  else
  begin
    lenBytes := GetLengthOfHeader(data);
    System.SetLength(tmp, System.length(data) - lenBytes);
    System.Move(data[lenBytes], tmp[0], System.length(tmp) *
      System.SizeOf(Byte));
    Foctets := tmp;
  end;
end;

function TDerApplicationSpecific.GetObject: IAsn1Object;
begin
  result := FromByteArray(GetContents());
end;

function TDerApplicationSpecific.GetObject(derTagNo: Int32): IAsn1Object;
var
  orig, tmp: TCryptoLibByteArray;
begin
  if (derTagNo >= $1F) then
  begin
    raise EIOCryptoLibException.CreateRes(@SUnSupportedTag);
  end;

  orig := GetEncoded();
  tmp := ReplaceTagNumber(derTagNo, orig);

  if ((orig[0] and TAsn1Tags.Constructed) <> 0) then
  begin
    tmp[0] := tmp[0] or TAsn1Tags.Constructed;
  end;

  result := FromByteArray(tmp);
end;

class function TDerApplicationSpecific.ReplaceTagNumber(newTag: Int32;
  const input: TCryptoLibByteArray): TCryptoLibByteArray;
var
  tagNo, Index, b, Remaining: Int32;
  tmp: TCryptoLibByteArray;
begin
  tagNo := input[0] and $1F;
  index := 1;
  //
  // with tagged object tag number is bottom 5 bits, or stored at the start of the content
  //
  if (tagNo = $1F) then
  begin

    b := input[index];
    System.Inc(index);

    // X.690-0207 8.1.2.4.2
    // "c) bits 7 to 1 of the first subsequent octet shall not all be zero."
    if ((b and $7F) = 0) then // Note: -1 will pass
    begin
      raise EIOCryptoLibException.CreateRes(@SCorruptedStreamInvalidTag);
    end;

    while ((b and $80) <> 0) do
    begin
      b := input[index];
      System.Inc(index);
    end;

  end;

  Remaining := System.length(input) - index;
  System.SetLength(tmp, 1 + Remaining);
  tmp[0] := Byte(newTag);
  System.Move(input[index], tmp[1], Remaining * System.SizeOf(Byte));

  result := tmp;
end;

{ TBerApplicationSpecific }

constructor TBerApplicationSpecific.Create(tagNo: Int32;
  const vec: IAsn1EncodableVector);
begin
  inherited Create(tagNo, vec);
end;

{ TBerOctetStringParser }

constructor TBerOctetStringParser.Create(const parser: IAsn1StreamParser);
begin
  Inherited Create();
  F_parser := parser;
end;

function TBerOctetStringParser.GetOctetStream: TStream;
begin
  result := TConstructedOctetStream.Create(F_parser);
end;

function TBerOctetStringParser.ToAsn1Object: IAsn1Object;
var
  LStream: TStream;
begin
  try
    LStream := GetOctetStream();

    try
      result := TBerOctetString.Create(TStreamUtils.ReadAll(LStream));
    finally
      LStream.Free;
    end;

  except
    on e: EIOCryptoLibException do
    begin
      raise EAsn1ParsingCryptoLibException.CreateResFmt(@SConvertError,
        [e.Message]);
    end;

  end;
end;

{ TBerApplicationSpecificParser }

constructor TBerApplicationSpecificParser.Create(tag: Int32;
  const parser: IAsn1StreamParser);
begin
  F_tag := tag;
  F_parser := parser;
end;

function TBerApplicationSpecificParser.ReadObject: IAsn1Convertible;
begin
  result := F_parser.ReadObject();
end;

function TBerApplicationSpecificParser.ToAsn1Object: IAsn1Object;
begin
  result := TBerApplicationSpecific.Create(F_tag, F_parser.ReadVector());
end;

{ TDerStringBase }

function TDerStringBase.Asn1GetHashCode: Int32;
begin
  result := TStringUtils.GetStringHashCode(GetString());
end;

constructor TDerStringBase.Create;
begin
  Inherited Create();
end;

function TDerStringBase.ToString: String;
begin
  result := GetString();
end;

{ TDerBitString }

class function TDerBitString.GetInstance(const obj: TCryptoLibByteArray)
  : IDerBitString;
begin
  try
    result := FromByteArray(obj) as IDerBitString;
  except
    on e: Exception do
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SEncodingError,
        [e.Message]);
    end;

  end;
end;

function TDerBitString.GetmData: TCryptoLibByteArray;
begin
  result := FmData;
end;

function TDerBitString.GetmPadBits: Int32;
begin
  result := FmPadBits;
end;

function TDerBitString.GetOctets: TCryptoLibByteArray;
begin
  if (mPadBits <> 0) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SUnalignedData);
  end;
  result := System.Copy(mData);
end;

function TDerBitString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerBitString;
begin

  if (not Supports(asn1Object, IDerBitString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := (mPadBits = other.mPadBits) and
    (TArrayUtils.AreEqual(mData, other.mData));
end;

constructor TDerBitString.Create(const data: TCryptoLibByteArray;
  padBits: Int32);
begin
  Inherited Create();
  if (data = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SDataNil);
  end;

  if ((padBits < 0) or (padBits > 7)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidRange);
  end;

  if ((System.length(data) = 0) and (padBits <> 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SPadBitError);
  end;

  FmData := System.Copy(data);
  FmPadBits := padBits;

end;

constructor TDerBitString.Create(const data: TCryptoLibByteArray);
begin
  Create(data, 0);
end;

constructor TDerBitString.Create(namedBits: Int32);
var
  bits, bytes, I, padBits: Int32;
  data: TCryptoLibByteArray;
begin
  Inherited Create();
  if (namedBits = 0) then
  begin
    System.SetLength(FmData, 0);
    FmPadBits := 0;
    Exit;
  end;
  bits := TBigInteger.BitLen(namedBits);
  bytes := (bits + 7) div 8;

{$IFDEF DEBUG}
  System.Assert((0 < bytes) and (bytes <= 4));
{$ENDIF DEBUG}
  System.SetLength(data, bytes);

  System.Dec(bytes);

  for I := 0 to System.Pred(bytes) do
  begin
    data[I] := Byte(namedBits);
    namedBits := TBits.Asr32(namedBits, 8);
  end;

{$IFDEF DEBUG}
  System.Assert((namedBits and $FF) <> 0);
{$ENDIF DEBUG}
  data[bytes] := Byte(namedBits);

  padBits := 0;
  while ((namedBits and (1 shl padBits)) = 0) do
  begin
    System.Inc(padBits);
  end;

{$IFDEF DEBUG}
  System.Assert(padBits < 8);
{$ENDIF DEBUG}
  FmData := data;
  FmPadBits := padBits;
end;

procedure TDerBitString.Encode(const derOut: TStream);
var
  last, mask, unusedBits: Int32;
  contents: TCryptoLibByteArray;
begin
  if (mPadBits > 0) then
  begin
    last := mData[System.length(mData) - 1];
    mask := (1 shl mPadBits) - 1;
    unusedBits := last and mask;

    if (unusedBits <> 0) then
    begin
      contents := TArrayUtils.Prepend(mData, Byte(mPadBits));

      // /*
      // * X.690-0207 11.2.1: Each unused bit in the final octet of the encoding of a bit string value shall be set to zero.
      // */
      contents[System.length(contents) - 1] := Byte(last xor unusedBits);

      (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.BitString, contents);
      Exit;
    end;
  end;

  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.BitString,
    Byte(mPadBits), mData);
end;

class function TDerBitString.FromAsn1Octets(const octets: TCryptoLibByteArray)
  : IDerBitString;
var
  padBits, last, mask: Int32;
  data: TCryptoLibByteArray;
begin
  if (System.length(octets) < 1) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@STruncatedBitString);
  end;

  padBits := octets[0];
  data := TArrayUtils.CopyOfRange(octets, 1, System.length(octets));

  if ((padBits > 0) and (padBits < 8) and (System.length(data) > 0)) then
  begin
    last := data[System.length(data) - 1];
    mask := (1 shl padBits) - 1;

    if ((last and mask) <> 0) then
    begin
      result := TBerBitString.Create(data, padBits);
      Exit;
    end;
  end;

  result := TDerBitString.Create(data, padBits);
end;

function TDerBitString.GetBytes: TCryptoLibByteArray;
begin
  result := System.Copy(mData);

  // DER requires pad bits be zero
  if (mPadBits > 0) then
  begin
    result[System.length(result) - 1] := result[System.length(result) - 1] and
      Byte($FF shl mPadBits);
  end;

end;

class function TDerBitString.GetInstance(const obj: TObject): IDerBitString;
begin
  if ((obj = Nil) or (obj is TDerBitString)) then
  begin
    result := obj as TDerBitString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerBitString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerBitString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerBitString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := FromAsn1Octets((o as IAsn1OctetString).GetOctets());
end;

function TDerBitString.GetInt32Value: Int32;
var
  Value, &length, I, mask: Int32;
begin
  Value := 0;
  length := Min(4, System.length(mData));
  for I := 0 to System.Pred(length) do
  begin
    Value := Value or (Int32(mData[I]) shl (8 * I));
  end;

  if ((mPadBits > 0) and (length = System.length(mData))) then
  begin
    mask := (1 shl mPadBits) - 1;
    Value := Value and (not(mask shl (8 * (length - 1))));
  end;
  result := Value;
end;

function TDerBitString.GetString: String;
var
  buffer: TStringList;
  I: Int32;
  Str: TCryptoLibByteArray;
  ubyte: UInt32;
begin
  buffer := TStringList.Create();
  buffer.LineBreak := '';
  Str := GetDerEncoded();
  buffer.Add('#');
  I := 0;
  try
    while I <> System.length(Str) do
    begin
      ubyte := Str[I];
      buffer.Add(FTable[(ubyte shr 4) and $F]);
      buffer.Add(FTable[Str[I] and $F]);
      System.Inc(I);
    end;
    result := buffer.Text;
  finally
    buffer.Free;
  end;
end;

function TDerBitString.Asn1GetHashCode: Int32;
begin
  result := mPadBits xor TArrayUtils.GetArrayHashCode(mData);
end;

constructor TDerBitString.Create(const obj: IAsn1Encodable);
begin
  Create(obj.GetDerEncoded());
end;

{ TBerBitString }

constructor TBerBitString.Create(const data: TCryptoLibByteArray);
begin
  Inherited Create(data);
end;

constructor TBerBitString.Create(const data: TCryptoLibByteArray;
  padBits: Int32);
begin
  Inherited Create(data, padBits);
end;

constructor TBerBitString.Create(const obj: IAsn1Encodable);
begin
  Inherited Create(obj);
end;

constructor TBerBitString.Create(namedBits: Int32);
begin
  Inherited Create(namedBits);
end;

procedure TBerBitString.Encode(const derOut: TStream);
begin
  if ((derOut is TAsn1OutputStream) or (derOut is TBerOutputStream)) then
  begin
    (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.BitString,
      Byte(mPadBits), mData);
  end
  else
  begin
    Inherited Encode(derOut);
  end;
end;

{ TBerGenerator }

constructor TBerGenerator.Create(outStream: TStream);
begin
  Inherited Create(outStream);
end;

procedure TBerGenerator.AddObject(const obj: IAsn1Encodable);
var
  temp: TBerOutputStream;
begin
  temp := TBerOutputStream.Create(&Out);
  try
    temp.WriteObject(obj);
  finally
    temp.Free;
  end;
end;

procedure TBerGenerator.Close;
begin
  WriteBerEnd();
end;

constructor TBerGenerator.Create(outStream: TStream; tagNo: Int32;
  isExplicit: Boolean);
begin
  Inherited Create(outStream);
  F_tagged := True;
  F_isExplicit := isExplicit;
  F_tagNo := tagNo;
end;

function TBerGenerator.GetRawOutputStream: TStream;
begin
  result := &Out;
end;

procedure TBerGenerator.WriteBerBody(contentStream: TStream);
begin
  TStreamUtils.PipeAll(contentStream, &Out);
end;

procedure TBerGenerator.WriteBerEnd;
begin
  &Out.WriteByte($00);
  &Out.WriteByte($00);

  if (F_tagged and F_isExplicit) then // write extra end for tag header
  begin
    &Out.WriteByte($00);
    &Out.WriteByte($00);
  end;
end;

procedure TBerGenerator.WriteBerHeader(tag: Int32);
var
  tagNum: Int32;
begin
  if (F_tagged) then
  begin
    tagNum := F_tagNo or TAsn1Tags.Tagged;

    if (F_isExplicit) then
    begin
      WriteHdr(tagNum or TAsn1Tags.Constructed);
      WriteHdr(tag);
    end
    else
    begin
      if ((tag and TAsn1Tags.Constructed) <> 0) then
      begin
        WriteHdr(tagNum or TAsn1Tags.Constructed);
      end
      else
      begin
        WriteHdr(tagNum);
      end;
    end
  end
  else
  begin
    WriteHdr(tag);
  end;
end;

procedure TBerGenerator.WriteHdr(tag: Int32);
begin
  &Out.WriteByte(Byte(tag));
  &Out.WriteByte($80);
end;

{ TBerNull }

constructor TBerNull.Create(dummy: Int32);
begin
  Inherited Create(dummy);
end;

procedure TBerNull.Encode(const derOut: TStream);
begin

  if ((derOut is TAsn1OutputStream) or (derOut is TBerOutputStream)) then
  begin
    (derOut as TDerOutputStream).WriteByte(TAsn1Tags.Null);
  end
  else
  begin
    Inherited Encode(derOut);
  end;
end;

class function TBerNull.GetInstance: IBerNull;
begin
  result := TBerNull.Create(0);
end;

{ TBerSequenceGenerator }

constructor TBerSequenceGenerator.Create(outStream: TStream);
begin
  Inherited Create(outStream);
  WriteBerHeader(TAsn1Tags.Constructed or TAsn1Tags.Sequence);
end;

constructor TBerSequenceGenerator.Create(outStream: TStream; tagNo: Int32;
  isExplicit: Boolean);
begin
  Inherited Create(outStream, tagNo, isExplicit);
  WriteBerHeader(TAsn1Tags.Constructed or TAsn1Tags.Sequence);
end;

{ TBerSequenceParser }

constructor TBerSequenceParser.Create(const parser: IAsn1StreamParser);
begin
  F_parser := parser;
end;

function TBerSequenceParser.ReadObject: IAsn1Convertible;
begin
  result := F_parser.ReadObject();
end;

function TBerSequenceParser.ToAsn1Object: IAsn1Object;
begin
  result := TBerSequence.Create(F_parser.ReadVector());
end;

{ TBerSet }

class function TBerSet.GetEmpty: IBerSet;
begin
  result := TBerSet.Create();
end;

constructor TBerSet.Create;
begin
  Inherited Create();
end;

constructor TBerSet.Create(const v: IAsn1EncodableVector;
  needsSorting: Boolean);
begin
  Inherited Create(v, needsSorting);
end;

destructor TBerSet.Destroy;
begin
  inherited Destroy;
end;

constructor TBerSet.Create(const obj: IAsn1Encodable);
begin
  Inherited Create(obj);
end;

constructor TBerSet.Create(const v: IAsn1EncodableVector);
begin
  Inherited Create(v, False);
end;

procedure TBerSet.Encode(const derOut: TStream);
var
  o: IAsn1Encodable;
  LListAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
begin
  if ((derOut is TAsn1OutputStream) or (derOut is TBerOutputStream)) then
  begin
    (derOut as TDerOutputStream).WriteByte(TAsn1Tags.&Set or
      TAsn1Tags.Constructed);

    (derOut as TDerOutputStream).WriteByte($80);

    LListAsn1Encodable := Self.GetEnumerable;
    for o in LListAsn1Encodable do
    begin
      (derOut as TDerOutputStream).WriteObject(o);
    end;

    (derOut as TDerOutputStream).WriteByte($00);
    (derOut as TDerOutputStream).WriteByte($00);
  end
  else
  begin
    (Inherited Encode(derOut));
  end;
end;

class function TBerSet.FromVector(const v: IAsn1EncodableVector;
  needsSorting: Boolean): IBerSet;
begin
  if v.count < 1 then
  begin
    result := Empty;
  end
  else
  begin
    result := TBerSet.Create(v, needsSorting);
  end;
end;

class function TBerSet.FromVector(const v: IAsn1EncodableVector): IBerSet;
begin
  if v.count < 1 then
  begin
    result := Empty;
  end
  else
  begin
    result := TBerSet.Create(v);
  end;
end;

{ TBerSetParser }

constructor TBerSetParser.Create(const parser: IAsn1StreamParser);
begin
  F_parser := parser;
end;

function TBerSetParser.ReadObject: IAsn1Convertible;
begin
  result := F_parser.ReadObject();
end;

function TBerSetParser.ToAsn1Object: IAsn1Object;
begin
  result := TBerSet.Create(F_parser.ReadVector(), False);
end;

{ TDerTaggedObject }

constructor TDerTaggedObject.Create(tagNo: Int32; const obj: IAsn1Encodable);
begin
  Inherited Create(tagNo, obj);
end;

constructor TDerTaggedObject.Create(explicitly: Boolean; tagNo: Int32;
  const obj: IAsn1Encodable);
begin
  Inherited Create(explicitly, tagNo, obj)
end;

constructor TDerTaggedObject.Create(tagNo: Int32);
begin
  Inherited Create(False, tagNo, TDerSequence.Empty)
end;

procedure TDerTaggedObject.Encode(const derOut: TStream);
var
  bytes: TCryptoLibByteArray;
  flags: Int32;
begin
  if (not IsEmpty()) then
  begin
    bytes := obj.GetDerEncoded();

    if (explicitly) then
    begin
      (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Constructed or
        TAsn1Tags.Tagged, tagNo, bytes);
    end
    else
    begin
      //
      // need to mark constructed types... (preserve Constructed tag)
      //
      flags := (bytes[0] and TAsn1Tags.Constructed) or TAsn1Tags.Tagged;
      (derOut as TDerOutputStream).WriteTag(flags, tagNo);
      derOut.Write(bytes[1], System.length(bytes) - 1);
    end
  end
  else
  begin
    (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Constructed or
      TAsn1Tags.Tagged, tagNo, Nil);
  end;
end;

{ TBerTaggedObject }

constructor TBerTaggedObject.Create(tagNo: Int32; const obj: IAsn1Encodable);
begin
  Inherited Create(tagNo, obj);
end;

constructor TBerTaggedObject.Create(explicitly: Boolean; tagNo: Int32;
  const obj: IAsn1Encodable);
begin
  Inherited Create(explicitly, tagNo, obj)
end;

constructor TBerTaggedObject.Create(tagNo: Int32);
begin
  Inherited Create(False, tagNo, TBerSequence.Empty)
end;

procedure TBerTaggedObject.Encode(const derOut: TStream);
var
  eObj: TList<IAsn1Encodable>;
  LListIDerOctetString: TCryptoLibGenericArray<IDerOctetString>;
  LListIAsn1Encodable: TCryptoLibGenericArray<IAsn1Encodable>;
  asn1OctetString: IAsn1OctetString;
  berOctetString: IBerOctetString;
  derOctetString: IDerOctetString;
  asn1Sequence: IAsn1Sequence;
  asn1Set: IAsn1Set;
  o: IAsn1Encodable;
begin
  eObj := TList<IAsn1Encodable>.Create();
  try
    if ((derOut is TAsn1OutputStream) or (derOut is TBerOutputStream)) then
    begin
      (derOut as TDerOutputStream)
        .WriteTag(Byte(TAsn1Tags.Constructed or TAsn1Tags.Tagged), tagNo);
      (derOut as TDerOutputStream).WriteByte($80);

      if (not IsEmpty()) then
      begin
        if (not explicitly) then
        begin
          if (Supports(obj, IAsn1OctetString, asn1OctetString)) then
          begin
            if (Supports(asn1OctetString, IBerOctetString, berOctetString)) then
            begin
              LListIDerOctetString := berOctetString.GetEnumerable;
              for derOctetString in LListIDerOctetString do
              begin
                eObj.Add(derOctetString as IAsn1Encodable);
              end;
            end
            else
            begin
              berOctetString := TBerOctetString.Create
                (asn1OctetString.GetOctets());
              LListIDerOctetString := berOctetString.GetEnumerable;
              for derOctetString in LListIDerOctetString do
              begin
                eObj.Add(derOctetString as IAsn1Encodable);
              end;
            end
          end
          else if Supports(obj, IAsn1Sequence, asn1Sequence) then
          begin
            LListIAsn1Encodable := asn1Sequence.GetEnumerable;
            for o in LListIAsn1Encodable do
            begin
              eObj.Add(o);
            end;
          end
          else if Supports(obj, IAsn1Set, asn1Set) then
          begin
            LListIAsn1Encodable := asn1Set.GetEnumerable;
            for o in LListIAsn1Encodable do
            begin
              eObj.Add(o);
            end;
          end
          else
          begin
            raise ENotImplementedCryptoLibException.CreateResFmt
              (@SNotImplemented, [(obj as TAsn1Encodable).ClassName]);
          end;

          for o in eObj do
          begin
            (derOut as TDerOutputStream).WriteObject(o);
          end;
        end
        else
        begin
          (derOut as TDerOutputStream).WriteObject(obj);
        end;
      end;

      (derOut as TDerOutputStream).WriteByte($00);
      (derOut as TDerOutputStream).WriteByte($00);
    end
    else
    begin
      (Inherited Encode(derOut));
    end
  finally
    eObj.Free;
  end;

end;

{ TBerTaggedObjectParser }

constructor TBerTaggedObjectParser.Create(Constructed: Boolean;
  tagNumber: Int32; const parser: IAsn1StreamParser);
begin
  F_constructed := Constructed;
  F_tagNumber := tagNumber;
  F_parser := parser;
end;

destructor TBerTaggedObjectParser.Destroy;
begin
  F_parser := Nil;
  inherited Destroy;
end;

function TBerTaggedObjectParser.GetIsConstructed: Boolean;
begin
  result := F_constructed;
end;

function TBerTaggedObjectParser.GetObjectParser(tag: Int32; isExplicit: Boolean)
  : IAsn1Convertible;
begin
  if (isExplicit) then
  begin
    if (not F_constructed) then
    begin
      raise EIOCryptoLibException.CreateRes(@SUnConstructedTag);
    end;

    result := F_parser.ReadObject();
    Exit;
  end;

  result := F_parser.ReadImplicit(F_constructed, tag);
end;

function TBerTaggedObjectParser.GetTagNo: Int32;
begin
  result := F_tagNumber;
end;

function TBerTaggedObjectParser.ToAsn1Object: IAsn1Object;
begin
  try
    result := F_parser.ReadTaggedObject(F_constructed, F_tagNumber);
  except
    on e: EIOCryptoLibException do
    begin
      raise EAsn1ParsingCryptoLibException.CreateResFmt(@SParsingError,
        [e.Message]);
    end;

  end;
end;

{ TDerBmpString }

function TDerBmpString.GetStr: String;
begin
  result := FStr;
end;

function TDerBmpString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerBmpString;
begin

  if (not Supports(asn1Object, IDerBmpString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

constructor TDerBmpString.Create(const astr: TCryptoLibByteArray);
var
  cs: TCryptoLibCharArray;
  I: Int32;
begin
  Inherited Create();
  if (astr = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SEmptyInput);
  end;

  System.SetLength(cs, System.length(astr) shr 1);

  I := 0;

  while I <> System.length(cs) do
  begin
    cs[I] := Char((astr[2 * I] shl 8) or (astr[2 * I + 1] and $FF));
    System.Inc(I);
  end;

  System.SetString(FStr, PChar(@cs[0]), System.length(cs));
end;

constructor TDerBmpString.Create(const astr: String);
begin
  Inherited Create();
  if (astr = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SEmptyInput);
  end;

  FStr := astr;
end;

procedure TDerBmpString.Encode(const derOut: TStream);
var
  c: TCryptoLibCharArray;
  b: TCryptoLibByteArray;
  I, LowPoint, HighPoint: Int32;
begin
  System.SetLength(c, System.length(Str));

  // had to use this loop because somehow, StrPLCopy causes memory leak in FPC v3.0.5
{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(Str);
  HighPoint := System.High(Str);
{$ELSE}
  LowPoint := 1;
  HighPoint := System.length(Str);
{$ENDIF DELPHIXE3_UP}
  for I := LowPoint to HighPoint do
  begin
    c[I - 1] := Str[I];
  end;
  System.SetLength(b, System.length(c) * 2);

  I := 0;

  while I <> System.length(c) do
  begin
    b[2 * I] := Byte(Ord(c[I]) shr 8);
    b[2 * I + 1] := Byte(c[I]);
    System.Inc(I);
  end;

  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.BmpString, b);

end;

class function TDerBmpString.GetInstance(const obj: TObject): IDerBmpString;
begin
  if ((obj = Nil) or (obj is TDerBmpString)) then
  begin
    result := obj as TDerBmpString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerBmpString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerBmpString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerBmpString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerBmpString.Create(TAsn1OctetString.GetInstance(o as TAsn1Object)
    .GetOctets());
end;

function TDerBmpString.GetString: String;
begin
  result := FStr;
end;

{ TDerBoolean }

function TDerBoolean.GetIsTrue: Boolean;
begin
  result := Fvalue <> 0;
end;

function TDerBoolean.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerBoolean;
begin

  if (not Supports(asn1Object, IDerBoolean, other)) then
  begin
    result := System.False;
    Exit;
  end;

  result := IsTrue = other.IsTrue;
end;

function TDerBoolean.Asn1GetHashCode: Int32;
begin
  result := Ord(IsTrue);
end;

constructor TDerBoolean.Create(const val: TCryptoLibByteArray);
begin
  Inherited Create();
  if (System.length(val) <> 1) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidValue);
  end;

  // TODO Are there any constraints on the possible byte values?
  Fvalue := val[0];
end;

constructor TDerBoolean.Create(Value: Boolean);
begin
  Inherited Create();
  if Value then
  begin
    Fvalue := Byte($FF)
  end
  else
  begin
    Fvalue := Byte(0)
  end;
end;

procedure TDerBoolean.Encode(const derOut: TStream);
begin
  // TODO Should we make sure the byte value is one of '0' or '0xff' here?
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Boolean,
    TCryptoLibByteArray.Create(Fvalue));
end;

class function TDerBoolean.FromOctetString(const Value: TCryptoLibByteArray)
  : IDerBoolean;
var
  b: Byte;
begin
  if (System.length(Value) <> 1) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidBooleanValue);
  end;

  b := Value[0];

  case b of
    0:
      result := TDerBoolean.False;
    $FF:
      result := TDerBoolean.True
  else
    begin
      result := TDerBoolean.Create(Value);
    end;
  end;

end;

class function TDerBoolean.GetInstance(Value: Boolean): IDerBoolean;
begin
  if Value then
  begin
    result := TDerBoolean.True;
  end
  else
  begin
    result := TDerBoolean.False;
  end;
end;

class function TDerBoolean.GetInstance(const obj: TObject): IDerBoolean;
begin
  if ((obj = Nil) or (obj is TDerBoolean)) then
  begin
    Supports(obj, IDerBoolean, result);
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerBoolean.GetFalse: IDerBoolean;
begin
  result := TDerBoolean.Create(System.False);
end;

class function TDerBoolean.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerBoolean;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerBoolean))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := FromOctetString((o as IAsn1OctetString).GetOctets());
end;

class function TDerBoolean.GetTrue: IDerBoolean;
begin
  result := TDerBoolean.Create(System.True);
end;

function TDerBoolean.ToString: String;
begin
  result := BoolToStr(IsTrue, System.True);
end;

{ TDerEnumerated }

function TDerEnumerated.GetBytes: TCryptoLibByteArray;
begin
  result := Fbytes;
end;

function TDerEnumerated.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerEnumerated;
begin

  if (not Supports(asn1Object, IDerEnumerated, other)) then
  begin
    result := False;
    Exit;
  end;

  result := TArrayUtils.AreEqual(bytes, other.bytes);
end;

function TDerEnumerated.Asn1GetHashCode: Int32;
begin
  result := TArrayUtils.GetArrayHashCode(bytes);
end;

constructor TDerEnumerated.Create(val: Int32);
begin
  Inherited Create();
  Fbytes := TBigInteger.ValueOf(val).ToByteArray();
end;

constructor TDerEnumerated.Create(const val: TBigInteger);
begin
  Inherited Create();
  Fbytes := val.ToByteArray();
end;

constructor TDerEnumerated.Create(const bytes: TCryptoLibByteArray);
begin
  Inherited Create();
  if (System.length(bytes) > 1) then
  begin
    if ((bytes[0] = 0) and ((bytes[1] and $80) = 0)) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SMalformedEnumerated);
    end;
    if ((bytes[0] = Byte($FF)) and ((bytes[1] and $80) <> 0)) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SMalformedEnumerated);
    end;
  end;
  Fbytes := System.Copy(bytes);
end;

procedure TDerEnumerated.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Enumerated, Fbytes);
end;

class function TDerEnumerated.FromOctetString(const enc: TCryptoLibByteArray)
  : IDerEnumerated;
var
  LValue: Int32;
  cached: IDerEnumerated;
begin
  if (System.length(enc) = 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SZeroLength);
  end;

  if (System.length(enc) = 1) then
  begin
    LValue := enc[0];
    if (LValue < System.length(Fcache)) then
    begin
      cached := Fcache[LValue];
      if (cached <> Nil) then
      begin
        result := cached;
        Exit;
      end;
      Fcache[LValue] := TDerEnumerated.Create(System.Copy(enc));
      result := Fcache[LValue];
      Exit;
    end;
  end;

  result := TDerEnumerated.Create(System.Copy(enc));
end;

class function TDerEnumerated.GetInstance(const obj: TObject): IDerEnumerated;
begin
  if ((obj = Nil) or (obj is TDerEnumerated)) then
  begin
    result := obj as TDerEnumerated;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerEnumerated.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerEnumerated;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if (isExplicit or (Supports(o, IDerEnumerated))) then
  begin
    result := GetInstance(o as TObject);
    Exit;
  end;

  result := FromOctetString((o as IAsn1OctetString).GetOctets());
end;

function TDerEnumerated.GetValue: TBigInteger;
begin
  result := TBigInteger.Create(Fbytes);
end;

{ TDerGraphicString }

function TDerGraphicString.GetmString: TCryptoLibByteArray;
begin
  result := FmString;
end;

function TDerGraphicString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerGraphicString;
begin

  if (not Supports(asn1Object, IDerGraphicString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := TArrayUtils.AreEqual(mString, other.mString);
end;

function TDerGraphicString.Asn1GetHashCode: Int32;
begin
  result := TArrayUtils.GetArrayHashCode(mString);
end;

constructor TDerGraphicString.Create(const encoding: TCryptoLibByteArray);
begin
  Inherited Create();
  FmString := System.Copy(encoding);
end;

procedure TDerGraphicString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.GraphicString, mString);
end;

class function TDerGraphicString.GetInstance(const obj: TObject)
  : IDerGraphicString;
begin
  if ((obj = Nil) or (obj is TDerGraphicString)) then
  begin
    result := obj as TDerGraphicString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerGraphicString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerGraphicString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerGraphicString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerGraphicString.Create
    (TAsn1OctetString.GetInstance(o as TAsn1Object).GetOctets());
end;

class function TDerGraphicString.GetInstance(const obj: TCryptoLibByteArray)
  : IDerGraphicString;
begin
  try
    result := FromByteArray(obj) as IDerGraphicString;
  except
    on e: Exception do
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SEncodingError,
        [e.Message]);
    end;

  end;
end;

function TDerGraphicString.GetOctets: TCryptoLibByteArray;
begin
  result := System.Copy(mString);
end;

function TDerGraphicString.GetString: String;
begin
  result := TConverters.ConvertBytesToString(mString, TEncoding.ANSI);
end;

{ TDerExternal }

class function TDerExternal.GetObjFromVector(const v: IAsn1EncodableVector;
  Index: Int32): IAsn1Object;
var
  val: IAsn1Encodable;
begin
  if (v.count <= index) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SFewObject);
  end;

  val := v[index];
  result := val.ToAsn1Object();
end;

class procedure TDerExternal.WriteEncodable(ms: TMemoryStream;
  const e: IAsn1Encodable);
var
  bs: TCryptoLibByteArray;
begin
  if (e <> Nil) then
  begin
    bs := e.GetDerEncoded();
    ms.Write(bs[0], System.length(bs));
  end;
end;

function TDerExternal.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerExternal;
begin
  if (Self.Equals(asn1Object)) then
  begin
    result := True;
    Exit;
  end;

  if (not Supports(asn1Object, IDerExternal, other)) then
  begin
    result := False;
    Exit;
  end;

  result := directReference.Equals(other.directReference) and
    indirectReference.Equals(other.indirectReference) and
    dataValueDescriptor.Equals(other.dataValueDescriptor) and
    ExternalContent.Equals(other.ExternalContent);
end;

function TDerExternal.Asn1GetHashCode: Int32;
var
  ret: Int32;
begin
  ret := ExternalContent.GetHashCode();
  if (directReference <> Nil) then
  begin
    ret := ret xor directReference.GetHashCode();
  end;
  if (indirectReference <> Nil) then
  begin
    ret := ret xor indirectReference.GetHashCode();
  end;
  if (dataValueDescriptor <> Nil) then
  begin
    ret := ret xor dataValueDescriptor.GetHashCode();
  end;
  result := ret;
end;

constructor TDerExternal.Create(const directReference: IDerObjectIdentifier;
  const indirectReference: IDerInteger; const dataValueDescriptor: IAsn1Object;
  encoding: Int32; const externalData: IAsn1Object);
begin
  Inherited Create();
  FdirectReference := directReference;
  FindirectReference := indirectReference;
  FdataValueDescriptor := dataValueDescriptor;
  Fencoding := encoding;
  FexternalContent := externalData.ToAsn1Object();
end;

constructor TDerExternal.Create(const vector: IAsn1EncodableVector);
var
  offset: Int32;
  enc: IAsn1Object;
  derObjectIdentifier: IDerObjectIdentifier;
  derInteger: IDerInteger;
  obj: IAsn1TaggedObject;
begin
  Inherited Create();
  offset := 0;
  enc := GetObjFromVector(vector, offset);

  if (Supports(enc, IDerObjectIdentifier, derObjectIdentifier)) then
  begin
    directReference := derObjectIdentifier;
    System.Inc(offset);
    enc := GetObjFromVector(vector, offset);
  end;

  if (Supports(enc, IDerInteger, derInteger)) then
  begin
    indirectReference := derInteger;
    System.Inc(offset);
    enc := GetObjFromVector(vector, offset);
  end;
  if (not(Supports(enc, IAsn1TaggedObject))) then
  begin
    dataValueDescriptor := enc;
    System.Inc(offset);
    enc := GetObjFromVector(vector, offset);
  end;

  if (vector.count <> (offset + 1)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SVectorTooLarge);
  end;

  if (not(Supports(enc, IAsn1TaggedObject, obj))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNoTaggedObjectFound);
  end;

  // Use property accessor to include check on value
  encoding := obj.tagNo;

  if ((Fencoding < 0) or (Fencoding > 2)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SInvalidEncodingValue);
  end;

  FexternalContent := obj.GetObject();
end;

constructor TDerExternal.Create(const directReference: IDerObjectIdentifier;
  const indirectReference: IDerInteger; const dataValueDescriptor: IAsn1Object;
  const externalData: IDerTaggedObject);
begin
  Create(directReference, indirectReference, dataValueDescriptor,
    externalData.tagNo, externalData.ToAsn1Object());
end;

procedure TDerExternal.Encode(const derOut: TStream);
var
  ms: TMemoryStream;
  buffer: TCryptoLibByteArray;
begin
  ms := TMemoryStream.Create();
  try
    WriteEncodable(ms, directReference);
    WriteEncodable(ms, indirectReference);
    WriteEncodable(ms, dataValueDescriptor);
    WriteEncodable(ms, TDerTaggedObject.Create(TAsn1Tags.External,
      ExternalContent));

    System.SetLength(buffer, ms.Size);
    ms.Position := 0;
    ms.Read(buffer[0], ms.Size);
    (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Constructed,
      TAsn1Tags.External, buffer);
  finally
    ms.Free;
  end;
end;

function TDerExternal.GetDataValueDescriptor: IAsn1Object;
begin
  result := FdataValueDescriptor;
end;

function TDerExternal.GetDirectReference: IDerObjectIdentifier;
begin
  result := FdirectReference;
end;

function TDerExternal.GetEncoding: Int32;
begin
  result := Fencoding;
end;

function TDerExternal.GetExternalContent: IAsn1Object;
begin
  result := FexternalContent;
end;

function TDerExternal.GetIndirectReference: IDerInteger;
begin
  result := FindirectReference;
end;

procedure TDerExternal.SetDataValueDescriptor(const Value: IAsn1Object);
begin
  FdataValueDescriptor := Value;
end;

procedure TDerExternal.SetDirectReference(const Value: IDerObjectIdentifier);
begin
  FdirectReference := Value;
end;

procedure TDerExternal.SetEncoding(const Value: Int32);
begin
  if ((Fencoding < 0) or (Fencoding > 2)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateResFmt
      (@SInvalidEncoding, [Value]);
  end;

  Fencoding := Value;
end;

procedure TDerExternal.SetExternalContent(const Value: IAsn1Object);
begin
  FexternalContent := Value;
end;

procedure TDerExternal.SetIndirectReference(const Value: IDerInteger);
begin
  FindirectReference := Value;
end;

{ TDerInteger }

function TDerInteger.GetBytes: TCryptoLibByteArray;
begin
  result := Fbytes;
end;

class function TDerInteger.GetInstance(const obj: TObject): IDerInteger;
begin
  if ((obj = Nil) or (obj is TDerInteger)) then
  begin
    result := obj as TDerInteger;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);

end;

function TDerInteger.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerInteger;
begin

  if (not Supports(asn1Object, IDerInteger, other)) then
  begin
    result := False;
    Exit;
  end;

  result := TArrayUtils.AreEqual(bytes, other.bytes);
end;

function TDerInteger.Asn1GetHashCode: Int32;
begin
  result := TArrayUtils.GetArrayHashCode(Fbytes);
end;

constructor TDerInteger.Create(const Value: TBigInteger);
begin
  inherited Create();
  if (not Value.IsInitialized) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SValueNil);
  end;

  Fbytes := Value.ToByteArray();
end;

constructor TDerInteger.Create(Value: Int32);
begin
  inherited Create();
  Fbytes := TBigInteger.ValueOf(Value).ToByteArray();
end;

constructor TDerInteger.Create(const bytes: TCryptoLibByteArray);
begin
  inherited Create();
  if (System.length(bytes) > 1) then
  begin
    if ((bytes[0] = 0) and ((bytes[1] and $80) = 0)) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SMalformedInteger);
    end;
    if ((bytes[0] = Byte($FF)) and ((bytes[1] and $80) <> 0)) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SMalformedInteger);
    end;
  end;
  Fbytes := System.Copy(bytes);
end;

procedure TDerInteger.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Integer, Fbytes);
end;

class function TDerInteger.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerInteger;
var
  o: IAsn1Object;
begin
  if (obj = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SObjectNil);

  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerInteger))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerInteger.Create(TAsn1OctetString.GetInstance(o as TAsn1Object)
    .GetOctets());

end;

function TDerInteger.GetPositiveValue: TBigInteger;
begin
  result := TBigInteger.Create(1, Fbytes);
end;

function TDerInteger.GetValue: TBigInteger;
begin
  result := TBigInteger.Create(Fbytes);
end;

function TDerInteger.ToString: String;
begin
  result := Value.ToString();
end;

{ TDerExternalParser }

constructor TDerExternalParser.Create(const parser: IAsn1StreamParser);
begin
  Inherited Create();
  F_parser := parser;
end;

function TDerExternalParser.ReadObject: IAsn1Convertible;
begin
  result := F_parser.ReadObject();
end;

function TDerExternalParser.ToAsn1Object: IAsn1Object;
begin
  result := TDerExternal.Create(F_parser.ReadVector());
end;

{ TDerOctetStringParser }

constructor TDerOctetStringParser.Create(stream: TStream);
begin
  FStream := stream;
end;

destructor TDerOctetStringParser.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TDerOctetStringParser.GetOctetStream: TStream;
begin
  result := FStream;
end;

function TDerOctetStringParser.ToAsn1Object: IAsn1Object;
begin
  try
    result := TDerOctetString.Create((FStream as TDefiniteLengthInputStream)
      .ToArray());
  except
    on e: EIOCryptoLibException do
    begin
      raise EInvalidOperationCryptoLibException.CreateResFmt(@SConvertError,
        [e.Message]);
    end;
  end;
end;

{ TDerGeneralString }

function TDerGeneralString.GetStr: String;
begin
  result := FStr;
end;

function TDerGeneralString.GetOctets: TCryptoLibByteArray;
begin
  result := TConverters.ConvertStringToBytes(Str, TEncoding.ASCII);
end;

function TDerGeneralString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerGeneralString;
begin

  if (not Supports(asn1Object, IDerGeneralString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

constructor TDerGeneralString.Create(const Str: TCryptoLibByteArray);
begin
  Create(TConverters.ConvertBytesToString(Str, TEncoding.ASCII));
end;

constructor TDerGeneralString.Create(const Str: String);
begin
  Inherited Create();
  if (Str = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;

  FStr := Str;
end;

procedure TDerGeneralString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.GeneralString,
    GetOctets());
end;

class function TDerGeneralString.GetInstance(const obj: TObject)
  : IDerGeneralString;
begin
  if ((obj = Nil) or (obj is TDerGeneralString)) then
  begin
    result := obj as TDerGeneralString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerGeneralString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerGeneralString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerGeneralString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerGeneralString.Create
    (TAsn1OctetString.GetInstance(o as TAsn1Object).GetOctets());
end;

function TDerGeneralString.GetString: String;
begin
  result := Str;
end;

{ TDerGenerator }

constructor TDerGenerator.Create(const outStream: TStream);
begin
  Inherited Create(outStream);
end;

constructor TDerGenerator.Create(const outStream: TStream; tagNo: Int32;
  isExplicit: Boolean);
begin
  Inherited Create(outStream);
  F_tagged := True;
  F_isExplicit := isExplicit;
  F_tagNo := tagNo;
end;

class procedure TDerGenerator.WriteDerEncoded(const outStream: TStream;
  tag: Int32; const bytes: TCryptoLibByteArray);
begin
  outStream.WriteByte(Byte(tag));
  WriteLength(outStream, System.length(bytes));
  outStream.Write(bytes[0], System.length(bytes));
end;

procedure TDerGenerator.WriteDerEncoded(tag: Int32;
  const bytes: TCryptoLibByteArray);
var
  tagNum, newTag: Int32;
  bOut: TMemoryStream;
  temp: TCryptoLibByteArray;
begin
  if (F_tagged) then
  begin
    tagNum := F_tagNo or TAsn1Tags.Tagged;

    if (F_isExplicit) then
    begin
      newTag := F_tagNo or TAsn1Tags.Constructed or TAsn1Tags.Tagged;
      bOut := TMemoryStream.Create();
      try
        WriteDerEncoded(bOut, tag, bytes);
        bOut.Position := 0;
        System.SetLength(temp, bOut.Size);
        bOut.Read(temp[0], bOut.Size);
        WriteDerEncoded(&Out, newTag, temp);
      finally
        bOut.Free;
      end;
    end
    else
    begin
      if ((tag and TAsn1Tags.Constructed) <> 0) then
      begin
        tagNum := tagNum or TAsn1Tags.Constructed;
      end;

      WriteDerEncoded(&Out, tagNum, bytes);
    end;
  end
  else
  begin
    WriteDerEncoded(&Out, tag, bytes);
  end;
end;

class procedure TDerGenerator.WriteDerEncoded(const outStr: TStream; tag: Int32;
  const inStr: TStream);
begin
  WriteDerEncoded(outStr, tag, TStreamUtils.ReadAll(inStr));
end;

class procedure TDerGenerator.WriteLength(const outStr: TStream; length: Int32);
var
  Size, val, I: Int32;
begin
  if (length > 127) then
  begin
    Size := 1;
    val := length;

    val := TBits.Asr32(val, 8);
    while (val <> 0) do
    begin
      System.Inc(Size);
      val := TBits.Asr32(val, 8);
    end;

    outStr.WriteByte(Byte(Size or $80));

    I := (Size - 1) * 8;

    while I >= 0 do
    begin
      outStr.WriteByte(Byte(TBits.Asr32(length, I)));
      System.Dec(I, 8);
    end;
  end
  else
  begin
    outStr.WriteByte(Byte(length));
  end;
end;

{ TDerIA5String }

function TDerIA5String.GetStr: String;
begin
  result := FStr;
end;

function TDerIA5String.GetOctets: TCryptoLibByteArray;
begin
  result := TConverters.ConvertStringToBytes(Str, TEncoding.ASCII);
end;

class function TDerIA5String.IsIA5String(const Str: String): Boolean;
var
  ch: Char;
begin
  for ch in Str do
  begin
    if (Ord(ch) > $007F) then
    begin
      result := False;
      Exit;
    end;
  end;

  result := True;
end;

function TDerIA5String.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerIA5String;
begin

  if (not Supports(asn1Object, IDerIA5String, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

function TDerIA5String.Asn1GetHashCode: Int32;
begin
  result := TStringUtils.GetStringHashCode(FStr);
end;

constructor TDerIA5String.Create(const Str: TCryptoLibByteArray);
begin
  Create(TConverters.ConvertBytesToString(Str, TEncoding.ASCII), False);
end;

constructor TDerIA5String.Create(const Str: String);
begin
  Create(Str, False);
end;

constructor TDerIA5String.Create(const Str: String; validate: Boolean);
begin
  Inherited Create();
  if (Str = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;
  if (validate and (not IsIA5String(Str))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SIllegalCharacters);
  end;

  FStr := Str;
end;

procedure TDerIA5String.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.IA5String, GetOctets());
end;

class function TDerIA5String.GetInstance(const obj: TObject): IDerIA5String;
begin
  if ((obj = Nil) or (obj is TDerIA5String)) then
  begin
    result := obj as TDerIA5String;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerIA5String.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerIA5String;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerIA5String))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerIA5String.Create(TAsn1OctetString.GetInstance(o as TAsn1Object)
    .GetOctets());
end;

function TDerIA5String.GetString: String;
begin
  result := Str;
end;

{ TDerNumericString }

function TDerNumericString.GetStr: String;
begin
  result := FStr;
end;

function TDerNumericString.GetOctets: TCryptoLibByteArray;
begin
  result := TConverters.ConvertStringToBytes(Str, TEncoding.ASCII);
end;

class function TDerNumericString.IsNumericString(const Str: String): Boolean;
var
  ch: Char;
begin
  for ch in Str do
  begin
    // char.IsDigit(ch)
    if ((Ord(ch) > $007F) or ((ch <> ' ') and (not CharInSet(ch, ['0' .. '9']))))
    then
    begin
      result := False;
      Exit;
    end;
  end;

  result := True;
end;

function TDerNumericString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerNumericString;
begin

  if (not Supports(asn1Object, IDerNumericString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

constructor TDerNumericString.Create(const Str: TCryptoLibByteArray);
begin
  Create(TConverters.ConvertBytesToString(Str, TEncoding.ASCII), False);
end;

constructor TDerNumericString.Create(const Str: String);
begin
  Create(Str, False);
end;

constructor TDerNumericString.Create(const Str: String; validate: Boolean);
begin
  Inherited Create();
  if (Str = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;
  if (validate and (not IsNumericString(Str))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SIllegalCharacters);
  end;

  FStr := Str;
end;

procedure TDerNumericString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.NumericString,
    GetOctets());
end;

class function TDerNumericString.GetInstance(const obj: TObject)
  : IDerNumericString;
begin
  if ((obj = Nil) or (obj is TDerNumericString)) then
  begin
    result := obj as TDerNumericString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerNumericString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerNumericString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerNumericString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerNumericString.Create
    (TAsn1OctetString.GetInstance(o as TAsn1Object).GetOctets());
end;

function TDerNumericString.GetString: String;
begin
  result := Str;
end;

{ TDerPrintableString }

function TDerPrintableString.GetStr: String;
begin
  result := FStr;
end;

function TDerPrintableString.GetString: String;
begin
  result := Str;
end;

function TDerPrintableString.GetOctets: TCryptoLibByteArray;
begin
  result := TConverters.ConvertStringToBytes(Str, TEncoding.ASCII);
end;

class function TDerPrintableString.IsPrintableString(const Str: String)
  : Boolean;
var
  ch: Char;
begin
  for ch in Str do
  begin

    if ((Ord(ch) > $007F)) then
    begin
      result := False;
      Exit;
    end;

    // if (char.IsLetterOrDigit(ch))
    if CharInSet(ch, ['a' .. 'z', 'A' .. 'Z', '0' .. '9']) then
    begin
      continue;
    end;

    case IndexStr(UnicodeString(ch), [''' ''', '\', '(', ')', '+', '-', '.',
      ':', '=', '?', '/', ',']) of
      0 .. 11:
        begin
          continue;
        end;
    end;

    result := False;
    Exit;
  end;

  result := True;
end;

function TDerPrintableString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerPrintableString;
begin

  if (not Supports(asn1Object, IDerPrintableString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

constructor TDerPrintableString.Create(const Str: TCryptoLibByteArray);
begin
  Create(TConverters.ConvertBytesToString(Str, TEncoding.ASCII), False);
end;

constructor TDerPrintableString.Create(const Str: String);
begin
  Create(Str, False);
end;

constructor TDerPrintableString.Create(const Str: String; validate: Boolean);
begin
  Inherited Create();
  if (Str = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;
  if (validate and (not IsPrintableString(Str))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SIllegalCharacters);
  end;

  FStr := Str;
end;

procedure TDerPrintableString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.PrintableString,
    GetOctets());
end;

class function TDerPrintableString.GetInstance(const obj: TObject)
  : IDerPrintableString;
begin
  if ((obj = Nil) or (obj is TDerPrintableString)) then
  begin
    result := obj as TDerPrintableString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerPrintableString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerPrintableString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerPrintableString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerPrintableString.Create
    (TAsn1OctetString.GetInstance(o as TAsn1Object).GetOctets());
end;

{ TDerSequenceGenerator }

procedure TDerSequenceGenerator.AddObject(const obj: IAsn1Encodable);
var
  temp: TDerOutputStream;
begin
  temp := TDerOutputStream.Create(F_bOut);
  try
    temp.WriteObject(obj);
  finally
    temp.Free;
  end;
end;

procedure TDerSequenceGenerator.Close;
var
  temp: TCryptoLibByteArray;
begin
  F_bOut.Position := 0;
  System.SetLength(temp, F_bOut.Size);
  F_bOut.Read(temp[0], F_bOut.Size);
  WriteDerEncoded(TAsn1Tags.Constructed or TAsn1Tags.Sequence, temp);
end;

constructor TDerSequenceGenerator.Create(outStream: TStream);
begin
  Inherited Create(outStream);
  F_bOut := TMemoryStream.Create();
end;

constructor TDerSequenceGenerator.Create(outStream: TStream; tagNo: Int32;
  isExplicit: Boolean);
begin
  Inherited Create(outStream, tagNo, isExplicit);
  F_bOut := TMemoryStream.Create();
end;

destructor TDerSequenceGenerator.Destroy;
begin
  F_bOut.Free;
  inherited Destroy;
end;

function TDerSequenceGenerator.GetRawOutputStream: TStream;
begin
  result := F_bOut;
end;

{ TDerT61String }

class function TDerT61String.GetEncoding: TEncoding;
begin
  result := TEncoding.GetEncoding('iso-8859-1');
end;

function TDerT61String.GetStr: String;
begin
  result := FStr;
end;

function TDerT61String.GetOctets: TCryptoLibByteArray;
var
  LEncoding: TEncoding;
begin
  LEncoding := TDerT61String.GetEncoding();
  try
    result := TConverters.ConvertStringToBytes(Str, LEncoding);
  finally
    LEncoding.Free;
  end;
end;

function TDerT61String.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerT61String;
begin

  if (not Supports(asn1Object, IDerT61String, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

constructor TDerT61String.Create(const Str: TCryptoLibByteArray);
var
  LEncoding: TEncoding;
begin
  Inherited Create();
  LEncoding := TDerT61String.GetEncoding();
  try
    Create(TConverters.ConvertBytesToString(Str, LEncoding));
  finally
    LEncoding.Free;
  end;
end;

constructor TDerT61String.Create(const Str: String);
begin
  Inherited Create();
  if (Str = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;

  FStr := Str;
end;

procedure TDerT61String.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.T61String, GetOctets());
end;

class function TDerT61String.GetInstance(const obj: TObject): IDerT61String;
begin
  if ((obj = Nil) or (obj is TDerT61String)) then
  begin
    result := obj as TDerT61String;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerT61String.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerT61String;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerT61String))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerT61String.Create(TAsn1OctetString.GetInstance(o as TAsn1Object)
    .GetOctets());
end;

function TDerT61String.GetString: String;
begin
  result := Str;
end;

{ TDerUniversalString }

function TDerUniversalString.GetStr: TCryptoLibByteArray;
begin
  result := FStr;
end;

function TDerUniversalString.GetOctets: TCryptoLibByteArray;
begin
  result := System.Copy(Str);
end;

function TDerUniversalString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerUniversalString;
begin

  if (not Supports(asn1Object, IDerUniversalString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := TArrayUtils.AreEqual(Str, other.Str);
end;

constructor TDerUniversalString.Create(const Str: TCryptoLibByteArray);
begin
  Inherited Create();
  if (Str = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;

  FStr := Str;
end;

procedure TDerUniversalString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.UniversalString, Str);
end;

class function TDerUniversalString.GetInstance(const obj: TObject)
  : IDerUniversalString;
begin
  if ((obj = Nil) or (obj is TDerUniversalString)) then
  begin
    result := obj as TDerUniversalString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerUniversalString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerUniversalString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerUniversalString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerUniversalString.Create
    (TAsn1OctetString.GetInstance(o as TAsn1Object).GetOctets());
end;

function TDerUniversalString.GetString: String;
var
  buffer: TStringList;
  I: Int32;
  enc: TCryptoLibByteArray;
  ubyte: UInt32;
begin
  buffer := TStringList.Create();
  buffer.LineBreak := '';
  enc := GetDerEncoded();
  buffer.Add('#');
  I := 0;
  try
    while I <> System.length(enc) do
    begin
      ubyte := enc[I];
      buffer.Add(FTable[(ubyte shr 4) and $F]);
      buffer.Add(FTable[enc[I] and $F]);
      System.Inc(I);
    end;
    result := buffer.Text;
  finally
    buffer.Free;
  end;
end;

{ TDerUtf8String }

function TDerUtf8String.GetStr: String;
begin
  result := FStr;
end;

function TDerUtf8String.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerUtf8String;
begin

  if (not Supports(asn1Object, IDerUtf8String, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

constructor TDerUtf8String.Create(const Str: TCryptoLibByteArray);
begin
  Create(TConverters.ConvertBytesToString(Str, TEncoding.UTF8));
end;

constructor TDerUtf8String.Create(const Str: String);
begin
  Inherited Create();
  if (Str = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;

  FStr := Str;
end;

procedure TDerUtf8String.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.Utf8String,
    TConverters.ConvertStringToBytes(Str, TEncoding.UTF8));
end;

class function TDerUtf8String.GetInstance(const obj: TObject): IDerUtf8String;
begin
  if ((obj = Nil) or (obj is TDerUtf8String)) then
  begin
    result := obj as TDerUtf8String;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerUtf8String.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerUtf8String;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerUtf8String))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerUtf8String.Create(TAsn1OctetString.GetInstance(o as TAsn1Object)
    .GetOctets());
end;

function TDerUtf8String.GetString: String;
begin
  result := Str;
end;

{ TDerVideotexString }

function TDerVideotexString.GetmString: TCryptoLibByteArray;
begin
  result := FmString;
end;

function TDerVideotexString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerVideotexString;
begin

  if (not Supports(asn1Object, IDerVideotexString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := TArrayUtils.AreEqual(mString, other.mString);
end;

function TDerVideotexString.Asn1GetHashCode: Int32;
begin
  result := TArrayUtils.GetArrayHashCode(mString);
end;

constructor TDerVideotexString.Create(const encoding: TCryptoLibByteArray);
begin
  Inherited Create();
  FmString := System.Copy(encoding);
end;

procedure TDerVideotexString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.VideotexString, mString);
end;

class function TDerVideotexString.GetInstance(const obj: TObject)
  : IDerVideotexString;
begin
  if ((obj = Nil) or (obj is TDerVideotexString)) then
  begin
    result := obj as TDerVideotexString;
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

class function TDerVideotexString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerVideotexString;
var
  o: IAsn1Object;
begin
  o := obj.GetObject();

  if ((isExplicit) or (Supports(o, IDerVideotexString))) then
  begin
    result := GetInstance(o as TAsn1Object);
    Exit;
  end;

  result := TDerVideotexString.Create
    (TAsn1OctetString.GetInstance(o as TAsn1Object).GetOctets());
end;

class function TDerVideotexString.GetInstance(const obj: TCryptoLibByteArray)
  : IDerVideotexString;
begin
  try
    result := FromByteArray(obj) as IDerVideotexString;
  except
    on e: Exception do
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SEncodingError,
        [e.Message]);
    end;

  end;
end;

function TDerVideotexString.GetOctets: TCryptoLibByteArray;
begin
  result := System.Copy(mString);
end;

function TDerVideotexString.GetString: String;
begin
  result := TConverters.ConvertBytesToString(mString, TEncoding.ANSI)
end;

{ TDerVisibleString }

function TDerVisibleString.GetStr: String;
begin
  result := FStr;
end;

function TDerVisibleString.GetOctets: TCryptoLibByteArray;
begin
  result := TConverters.ConvertStringToBytes(Str, TEncoding.ASCII);
end;

function TDerVisibleString.Asn1Equals(const asn1Object: IAsn1Object): Boolean;
var
  other: IDerVisibleString;
begin

  if (not Supports(asn1Object, IDerVisibleString, other)) then
  begin
    result := False;
    Exit;
  end;

  result := Str = other.Str;
end;

function TDerVisibleString.Asn1GetHashCode: Int32;
begin
  result := TStringUtils.GetStringHashCode(FStr);
end;

constructor TDerVisibleString.Create(const Str: TCryptoLibByteArray);
begin
  Create(TConverters.ConvertBytesToString(Str, TEncoding.ASCII));
end;

constructor TDerVisibleString.Create(const Str: String);
begin
  Inherited Create();
  if (Str = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SStrNil);
  end;

  FStr := Str;
end;

procedure TDerVisibleString.Encode(const derOut: TStream);
begin
  (derOut as TDerOutputStream).WriteEncoded(TAsn1Tags.VisibleString,
    GetOctets());
end;

class function TDerVisibleString.GetInstance(const obj: TObject)
  : IDerVisibleString;
var
  asn1OctetString: IAsn1OctetString;
  asn1TaggedObject: IAsn1TaggedObject;
begin
  if ((obj = Nil) or (obj is TDerVisibleString)) then
  begin
    result := obj as TDerVisibleString;
    Exit;
  end;

  if Supports(obj, IAsn1OctetString, asn1OctetString) then
  begin
    result := TDerVisibleString.Create(asn1OctetString.GetOctets());
    Exit;
  end;

  if Supports(obj, IAsn1TaggedObject, asn1TaggedObject) then
  begin
    result := GetInstance(asn1TaggedObject.GetObject() as TAsn1Object);
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SIllegalObject,
    [obj.ClassName]);
end;

{$IFNDEF _FIXINSIGHT_}

class function TDerVisibleString.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDerVisibleString;
begin
  result := GetInstance(obj.GetObject() as TAsn1Object);
end;
{$ENDIF}

function TDerVisibleString.GetString: String;
begin
  result := Str;
end;

end.
