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

unit ClpIAsn1Objects;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpBigInteger,
  Generics.Collections,
  ClpCryptoLibTypes;

type
  IAsn1Object = interface;

  IAsn1Convertible = interface(IInterface)
    ['{13104D9E-9DF1-4CCE-B48C-1ACC2AC362B1}']

    function ToAsn1Object(): IAsn1Object;
  end;

  IAsn1Encodable = interface(IAsn1Convertible)

    ['{1B2D1F84-4E8F-442E-86F8-B75C9942F1AB}']

    function GetEncoded(): TCryptoLibByteArray; overload;
    function GetEncoded(const encoding: String): TCryptoLibByteArray; overload;

    function GetDerEncoded(): TCryptoLibByteArray;

    function Equals(const obj: IAsn1Convertible): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
  end;

  IAsn1Object = interface(IAsn1Encodable)

    ['{83A52A0F-570B-43BB-9B98-8E5351FDA996}']

    function Asn1Equals(const asn1Object: IAsn1Object): Boolean;

    function Asn1GetHashCode(): Int32;

    procedure Encode(const derOut: TStream);

    function CallAsn1Equals(const obj: IAsn1Object): Boolean;

    function CallAsn1GetHashCode(): Int32;

  end;

type
  IDerObjectIdentifier = interface(IAsn1Object)

    ['{8626051F-828D-419B-94E8-65CC0752CCA1}']

    function GetID: String;
    property ID: String read GetID;

    procedure WriteField(const outputStream: TStream;
      fieldValue: Int64); overload;
    procedure WriteField(const outputStream: TStream;
      const fieldValue: TBigInteger); overload;
    procedure DoOutput(const bOut: TMemoryStream); overload;
    function GetBody(): TCryptoLibByteArray;

    function Branch(const branchID: String): IDerObjectIdentifier;

    function &On(const stem: IDerObjectIdentifier): Boolean;

    function ToString(): String;

  end;

type
  IAsn1EncodableVector = interface(IInterface)
    ['{A78E22EB-DB67-472E-A55F-CD710BCBDBFA}']

    function GetCount: Int32;
    function GetSelf(Index: Int32): IAsn1Encodable;

    procedure Add(const objs: array of IAsn1Encodable);

    procedure AddOptional(const objs: array of IAsn1Encodable);

    property Self[Index: Int32]: IAsn1Encodable read GetSelf; default;

    property Count: Int32 read GetCount;

    function GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>;

  end;

type
  IAsn1Generator = interface(IInterface)
    ['{40EEDADB-1F59-46F7-9AB9-17C4761C6C66}']

    function GetOut: TStream;

    property &Out: TStream read GetOut;

  end;

type
  IAsn1Null = interface(IAsn1Object)

    ['{5BA79253-4596-4384-9B67-1F3589BD9D24}']

    function ToString(): String;
  end;

type
  IAsn1OctetStringParser = interface(IAsn1Convertible)
    ['{8C24599C-B47F-4824-B110-2F10BE8B8377}']

    function GetOctetStream(): TStream;
  end;

type
  IAsn1OctetString = interface(IAsn1Object)
    ['{7F7FE981-DD88-4076-8A99-F24DA1005475}']

    function GetStr: TCryptoLibByteArray;
    function GetParser: IAsn1OctetStringParser;

    property Str: TCryptoLibByteArray read GetStr;
    property Parser: IAsn1OctetStringParser read GetParser;

    function GetOctetStream(): TStream;

    function GetOctets(): TCryptoLibByteArray;

    function ToString(): String;

  end;

type
  IAsn1TaggedObject = interface(IAsn1Object)
    ['{2EBA68BE-CEC6-4030-BB9C-E8310C9B7D5F}']

    function GetTagNo: Int32;
    function Getexplicitly: Boolean;
    function Getobj: IAsn1Encodable;

    property tagNo: Int32 read GetTagNo;
    property explicitly: Boolean read Getexplicitly;
    property obj: IAsn1Encodable read Getobj;

    function isExplicit(): Boolean;

    function IsEmpty(): Boolean;

    function GetObject(): IAsn1Object;

    function GetObjectParser(tag: Int32; isExplicit: Boolean): IAsn1Convertible;

    function ToString(): String;

  end;

type
  IAsn1TaggedObjectParser = interface(IAsn1Convertible)

    ['{AB221E2D-A78A-46F7-85AE-B642D904B705}']

    function GetTagNo: Int32;
    property tagNo: Int32 read GetTagNo;
    function GetObjectParser(tag: Int32; isExplicit: Boolean): IAsn1Convertible;
  end;

type
  IDerOctetString = interface(IAsn1OctetString)

    ['{0FFF8858-026C-49B5-A600-7F746FE6BCF7}']

  end;

type
  IAsn1SequenceParser = interface(IAsn1Convertible)

    ['{FAB73E91-6375-450C-938A-5C862D186857}']

    function ReadObject(): IAsn1Convertible;
  end;

type
  IAsn1Sequence = interface(IAsn1Object)

    ['{37A263B7-6724-422B-9B4C-08EBA272045F}']

    function GetCount: Int32;
    function GetParser: IAsn1SequenceParser;
    function GetSelf(Index: Integer): IAsn1Encodable;
    function GetCurrent(const e: IAsn1Encodable): IAsn1Encodable;

    procedure AddObject(const obj: IAsn1Encodable);

    function ToString(): String;

    function GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>;

    property Self[Index: Int32]: IAsn1Encodable read GetSelf; default;

    property Parser: IAsn1SequenceParser read GetParser;
    property Count: Int32 read GetCount;

  end;

type
  IAsn1SequenceParserImpl = interface(IAsn1SequenceParser)

    ['{B986EDD8-A7F3-4E9C-9D5B-2FF9120D9A91}']

  end;

type
  IBerOctetString = interface(IDerOctetString)

    ['{B9D96DA7-623C-491C-9304-7B67A6DBCFA6}']

    function GenerateOcts(): TList<IDerOctetString>;

    /// <summary>
    /// return the DER octets that make up this string.
    /// </summary>

    function GetEnumerable: TCryptoLibGenericArray<IDerOctetString>;

  end;

type
  IDerNull = interface(IAsn1Null)

    ['{0B4ABCBF-DF52-4934-8A43-218D948E1841}']

  end;

type
  IDerTaggedObject = interface(IAsn1TaggedObject)

    ['{CC77CFAB-8FCF-43E6-8FE4-95EFD99B9731}']

  end;

type
  IBerTaggedObject = interface(IDerTaggedObject)

    ['{DD6A102D-70DC-4428-8199-62D88276BDBA}']

  end;

type
  IDerSequence = interface(IAsn1Sequence)

    ['{ED1E13E2-6604-4FDE-BDB2-862704A9C90C}']

  end;

type
  IBerSequence = interface(IDerSequence)

    ['{B78E91BF-DB39-4033-8A7A-F0D024C5322A}']

  end;

type
  IAsn1SetParser = interface(IAsn1Convertible)

    ['{79248C62-59F2-4ACF-B62B-826FCCCF2AE1}']

    function ReadObject(): IAsn1Convertible;
  end;

type
  IAsn1Set = interface(IAsn1Object)
    ['{0BA9633A-73D2-4F5E-A1C0-0FCF2623847C}']

    function GetCount: Int32;
    function GetParser: IAsn1SetParser;
    function GetSelf(Index: Integer): IAsn1Encodable;
    function GetCurrent(const e: IAsn1Encodable): IAsn1Encodable;

    function ToString(): String;

    function ToArray(): TCryptoLibGenericArray<IAsn1Encodable>;

    function GetEnumerable: TCryptoLibGenericArray<IAsn1Encodable>;

    property Self[Index: Int32]: IAsn1Encodable read GetSelf; default;

    property Parser: IAsn1SetParser read GetParser;
    property Count: Int32 read GetCount;

  end;

type
  IAsn1SetParserImpl = interface(IAsn1SetParser)

    ['{23EAFC37-244E-42D7-89A8-1740B12656C1}']

  end;

type

  IDerSet = interface(IAsn1Set)

    ['{592C8E57-5B00-4927-AD34-EA4481D436BE}']

  end;

type
  /// **
  // * Marker interface for CHOICE objects - if you implement this in a roll-your-own
  // * object, any attempt to tag the object implicitly will convert the tag to an
  // * explicit one as the encoding rules require.
  // * <p>
  // * If you use this interface your class should also implement the GetInstance
  // * pattern which takes a tag object and the tagging mode used.
  // * </p>
  // */
  IAsn1Choice = interface(IInterface)
    // marker interface
    ['{9C12BE01-9579-48F2-A5B0-4FA5DD807B32}']
  end;

type
  IAsn1StreamParser = interface(IInterface)
    ['{D6FF970C-F5B0-4B62-8585-F5F499A18597}']

    procedure Set00Check(enabled: Boolean);

    function ReadIndef(tagValue: Int32): IAsn1Convertible;
    function ReadImplicit(constructed: Boolean; tag: Int32): IAsn1Convertible;

    function ReadTaggedObject(constructed: Boolean; tag: Int32): IAsn1Object;

    function ReadObject(): IAsn1Convertible;

    function ReadVector(): IAsn1EncodableVector;

  end;

type
  IDerSetParser = interface(IAsn1SetParser)

    ['{C9F565EE-4324-4F67-8DC9-E2A305AAD715}']

  end;

type
  IDerSequenceParser = interface(IAsn1SequenceParser)

    ['{C6B7A49D-C2CC-43A1-9820-7B36AD4FD3B6}']

  end;

type
  IDerApplicationSpecific = interface(IAsn1Object)
    ['{87EE7482-8D8B-4593-85B7-B5B286B43195}']

    function GetApplicationTag: Int32;
    function GetLengthOfHeader(const data: TCryptoLibByteArray): Int32;

    function isConstructed(): Boolean;
    function GetContents(): TCryptoLibByteArray;

    function GetObject(): IAsn1Object; overload;

    function GetObject(derTagNo: Int32): IAsn1Object; overload;

    property ApplicationTag: Int32 read GetApplicationTag;

  end;

type
  IBerApplicationSpecific = interface(IDerApplicationSpecific)
    ['{C7414497-904B-4CE8-8B5F-E252739F5B5C}']
  end;

type
  IBerOctetStringParser = interface(IAsn1OctetStringParser)
    ['{27698DDF-3584-45F6-8B6D-0AD85AA63F10}']

  end;

type
  IAsn1ApplicationSpecificParser = interface(IAsn1Convertible)

    ['{25C8291B-BE36-4916-8BD0-1F8629ACEE7F}']

    function ReadObject(): IAsn1Convertible;
  end;

type
  IBerApplicationSpecificParser = interface(IAsn1ApplicationSpecificParser)

    ['{60363C2D-CE20-467C-BC1D-38DFCFDFDFFA}']

  end;

type
  IDerStringBase = interface(IAsn1Object)
    ['{710E6D65-28DE-4BFE-9C62-3CFC0E909DD9}']

    function GetString(): String;
    function ToString(): String;
  end;

type
  IDerBitString = interface(IDerStringBase)

    ['{2EBCCC24-BF14-4EB1-BADA-C521439682BE}']

    function GetmData: TCryptoLibByteArray;
    property mData: TCryptoLibByteArray read GetmData;

    function GetmPadBits: Int32;
    property mPadBits: Int32 read GetmPadBits;

    function GetOctets(): TCryptoLibByteArray;

    function GetBytes(): TCryptoLibByteArray;

    function GetInt32Value: Int32;
    property Int32Value: Int32 read GetInt32Value;

  end;

type

  /// <summary>
  /// basic interface for Der string objects.
  /// </summary>
  IAsn1String = interface(IInterface)
    ['{D86B1260-C45A-42BE-A5B6-33F4C2DE4DF4}']

    function GetString(): String;
  end;

type
  IBerBitString = interface(IDerBitString)

    ['{6AB7DC7E-CD61-4D34-AE15-99E036688C77}']

  end;

type
  IBerGenerator = interface(IAsn1Generator)
    ['{9ED01B6B-EF67-46B2-8EF6-023CA3967345}']

    procedure WriteHdr(tag: Int32);

    procedure WriteBerHeader(tag: Int32);
    procedure WriteBerBody(contentStream: TStream);
    procedure WriteBerEnd();

    procedure AddObject(const obj: IAsn1Encodable);
    function GetRawOutputStream(): TStream;
    procedure Close();

  end;

type
  IBerNull = interface(IDerNull)
    ['{16AF74E8-26E1-466D-BE93-38A06853DA98}']

  end;

type
  IBerSequenceGenerator = interface(IBerGenerator)
    ['{D3EC8215-D932-40B3-B3C3-462C1558B88A}']

  end;

type
  IBerSequenceParser = interface(IAsn1SequenceParser)

    ['{9383F917-78CD-418C-B9B5-7972B907E8BE}']

  end;

type
  IBerSet = interface(IDerSet)
    ['{FD8838BB-8905-409A-AB93-136EEC6A05E4}']

  end;

type
  IBerSetParser = interface(IAsn1SetParser)

    ['{258A83DA-0C41-444D-9111-9CF7EAAEA54A}']

  end;

type
  IBerTaggedObjectParser = interface(IAsn1TaggedObjectParser)

    ['{F1E974C6-6C98-448D-945B-077E63ACB66F}']

    function GetIsConstructed: Boolean;

    property isConstructed: Boolean read GetIsConstructed;

  end;

type
  IDerBmpString = interface(IDerStringBase)

    ['{D100D2D5-5B11-47DC-99A4-A968EF31F5FC}']

    function GetStr: String;

    property Str: String read GetStr;

  end;

type
  IDerBoolean = interface(IAsn1Object)
    ['{1AD2B466-E3D6-4CD4-A66B-660FB63F2FFE}']

    function GetIsTrue: Boolean;

    function ToString(): String;

    property IsTrue: Boolean read GetIsTrue;

  end;

type
  IDerEnumerated = interface(IAsn1Object)
    ['{876D2762-5B34-4CC0-815A-D10DF1318D12}']

    function GetValue: TBigInteger;
    function GetBytes: TCryptoLibByteArray;

    property Value: TBigInteger read GetValue;
    property bytes: TCryptoLibByteArray read GetBytes;

  end;

type
  IDerGraphicString = interface(IDerStringBase)
    ['{582F6117-BFE1-47A2-BFD5-DC5C83E43985}']

    function GetmString: TCryptoLibByteArray;

    property mString: TCryptoLibByteArray read GetmString;

    function GetOctets(): TCryptoLibByteArray;

  end;

type

  IDerInteger = interface(IAsn1Object)
    ['{B968152A-5A16-4C1D-95E1-3B5F416D2C75}']

    function GetBytes: TCryptoLibByteArray;
    function GetPositiveValue: TBigInteger;
    function GetValue: TBigInteger;

    function ToString(): String;

    property Value: TBigInteger read GetValue;
    property PositiveValue: TBigInteger read GetPositiveValue;
    property bytes: TCryptoLibByteArray read GetBytes;

  end;

type
  IDerExternal = interface(IAsn1Object)

    ['{9AC333C2-0F64-4A5F-BE0A-EBCC2A4E2A00}']

    function GetDataValueDescriptor: IAsn1Object;
    function GetDirectReference: IDerObjectIdentifier;

    function GetEncoding: Int32;
    function GetExternalContent: IAsn1Object;
    function GetIndirectReference: IDerInteger;
    procedure SetDataValueDescriptor(const Value: IAsn1Object);
    procedure SetDirectReference(const Value: IDerObjectIdentifier);
    procedure SetEncoding(const Value: Int32);
    procedure SetExternalContent(const Value: IAsn1Object);
    procedure SetIndirectReference(const Value: IDerInteger);

    property DataValueDescriptor: IAsn1Object read GetDataValueDescriptor
      write SetDataValueDescriptor;

    property DirectReference: IDerObjectIdentifier read GetDirectReference
      write SetDirectReference;

    property encoding: Int32 read GetEncoding write SetEncoding;

    property ExternalContent: IAsn1Object read GetExternalContent
      write SetExternalContent;

    property IndirectReference: IDerInteger read GetIndirectReference
      write SetIndirectReference;

  end;

type
  IDerExternalParser = interface(IAsn1Encodable)
    ['{6BF2AB32-0307-4E49-BC4C-844ADCD884E0}']

    function ReadObject(): IAsn1Convertible;
  end;

type
  IDerOctetStringParser = interface(IAsn1OctetStringParser)
    ['{49664C03-CD81-423F-A93D-025D3116B066}']

  end;

type
  IDerGeneralString = interface(IDerStringBase)

    ['{E1C5097C-2FA1-4236-85D9-30784E6AA46D}']

    function GetStr: String;

    property Str: String read GetStr;

    function GetOctets(): TCryptoLibByteArray;

  end;

type
  IDerGenerator = interface(IAsn1Generator)
    ['{2A2A6C30-6CF7-41D2-9421-923A4A82E52B}']

    procedure WriteDerEncoded(tag: Int32; const bytes: TCryptoLibByteArray);

  end;

type
  IDerIA5String = interface(IDerStringBase)

    ['{F7BAC857-74F7-4660-95E1-F849B5D77F6C}']

    function GetStr: String;

    property Str: String read GetStr;

    function GetOctets(): TCryptoLibByteArray;

  end;

type
  IDerNumericString = interface(IDerStringBase)

    ['{58BB62CA-16C5-4696-AC0B-E83628182740}']

    function GetStr: String;

    property Str: String read GetStr;

    function GetOctets(): TCryptoLibByteArray;

  end;

type
  IDerPrintableString = interface(IDerStringBase)

    ['{119C220A-2672-48E3-A24A-11128B5F599B}']

    function GetStr: String;

    property Str: String read GetStr;

    function GetOctets(): TCryptoLibByteArray;

  end;

type
  IDerSequenceGenerator = interface(IDerGenerator)
    ['{1E0E4FD7-84CA-4D02-AB9E-5AF8461270DE}']
    procedure AddObject(const obj: IAsn1Encodable);
    function GetRawOutputStream(): TStream;
    procedure Close();
  end;

type
  IDerT61String = interface(IDerStringBase)

    ['{A3B8C316-F349-4A5A-96CB-4655581A0308}']

    function GetStr: String;

    property Str: String read GetStr;

    function GetOctets(): TCryptoLibByteArray;

  end;

type
  IDerUniversalString = interface(IDerStringBase)

    ['{60EC8C9A-B672-44E4-9C5B-B4022D937002}']

    function GetStr: TCryptoLibByteArray;

    property Str: TCryptoLibByteArray read GetStr;

    function GetOctets(): TCryptoLibByteArray;

  end;

type
  IDerUtf8String = interface(IDerStringBase)

    ['{C4ACD432-807D-4A27-B3FC-0694000EB995}']

    function GetStr: String;

    property Str: String read GetStr;

  end;

type
  IDerVideotexString = interface(IDerStringBase)
    ['{9484AEC5-5667-4C14-8441-4447B0B29F1B}']

    function GetmString: TCryptoLibByteArray;

    property mString: TCryptoLibByteArray read GetmString;

    function GetOctets(): TCryptoLibByteArray;

  end;

type
  IDerVisibleString = interface(IDerStringBase)

    ['{0540C649-B50B-45DA-9FC9-1248BD3F73F1}']

    function GetStr: String;

    property Str: String read GetStr;

    function GetOctets(): TCryptoLibByteArray;

  end;

implementation

end.
