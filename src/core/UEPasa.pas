unit UEPasa;

{ Copyright (c) 2020 by Herman Schoenfeld

  PIP-0027: E-PASA Reference Implementation
  See: https://github.com/PascalCoin/PascalCoin/blob/master/PIP/PIP-0027.md

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF FPC}

interface

uses
  SysUtils,
  TypInfo,
  uregexpr,
  UCommon,
  UCrypto,
  UEncoding,
  SyncObjs;

type

  EPascalCoinException = class(Exception);

  EPasaErrorCode = (
    Success, BadFormat, BadChecksum, InvalidAccountNumber,
    AccountChecksumInvalid, InvalidAccountName, MismatchedPayloadEncoding,
    PayloadTooLarge, MissingPassword, UnusedPassword, InvalidPassword,
    BadExtendedChecksum
  );

  { TPayloadTrait }

  TPayloadTrait = (
    ptNonDeterministic = 0,      // Payload encryption and encoding method not specified.
    ptPublic = 1,                // Unencrypted, public payload.
    ptRecipientKeyEncrypted = 2, // ECIES encrypted using recipient accounts public key.
    ptSenderKeyEncrypted = 3,    // ECIES encrypted using sender accounts public key.
    ptPasswordEncrypted = 4,     // AES encrypted using pwd param
    ptAsciiFormatted = 5,        // Payload data encoded in ASCII
    ptHexFormatted = 6,          // Payload data encoded in HEX
    ptBase58Formatted = 7,       // Payload data encoded in Base58
    ptAddressedByName = 8        // E-PASA addressed by account name (not number).
  );

  { TPayloadTraitHelper }

  TPayloadTraitHelper = record helper for TPayloadTrait
  public
    function ToProtocolValue: byte;
  end;

  { TPayloadType }

  TPayloadType = set of TPayloadTrait;

  { TPayloadTypesHelper }

  TPayloadTypeHelper = record helper for TPayloadType
  public
    function HasTrait(APayloadTrait: TPayloadTrait): Boolean; inline;
    function ToProtocolValue : byte;
    function IsValid : Boolean;
  end;

  { TEPasa }


  TEPasa = record
    strict private var
      FAccount, FAccountChecksum: TNullable<UInt32>;
      FAccountName, FPayload, FPassword, FExtendedChecksum: String;
      FPayloadType: TPayloadType;

      function GetAccount: TNullable<UInt32>; inline;
      procedure SetAccount(const AValue: TNullable<UInt32>); inline;
      function GetAccountChecksum: TNullable<UInt32>; inline;
      procedure SetAccountChecksum(const AValue: TNullable<UInt32>); inline;
      function GetPayloadType: TPayloadType; inline;
      function GetAccountName: String; inline;
      procedure SetAccountName(const AValue: String); inline;
      procedure SetPayloadType(const AValue: TPayloadType); inline;
      function GetExtendedChecksum: String; inline;
      procedure SetExtendedChecksum(const AValue: String); inline;
      function GetPassword: String; inline;
      procedure SetPassword(const AValue: String); inline;
      function GetPayload: String; inline;
      procedure SetPayload(const AValue: String); inline;
      function GetHasPayload: Boolean; inline;
      function GetIsStandard: Boolean; inline;
      function GetIsPayToKey: Boolean; inline;
      function GetIsAddressedByName : Boolean; inline;
      function GetIsAddressedByNumber : Boolean; inline;
      class function GetEmptyValue : TEPasa; static;
    public
      property Account: TNullable<UInt32> read GetAccount write SetAccount;
      property AccountChecksum: TNullable<UInt32> read GetAccountChecksum write SetAccountChecksum;
      property AccountName: String read GetAccountName write SetAccountName;
      property PayloadType: TPayloadType read GetPayloadType write SetPayloadType;
      property Payload: String read GetPayload write SetPayload;
      property Password: String read GetPassword write SetPassword;
      property ExtendedChecksum: String read GetExtendedChecksum write SetExtendedChecksum;
      property IsAddressedByNumber: boolean read GetIsAddressedByNumber;
      property IsAddressedByName: boolean read GetIsAddressedByName;
      property IsPayToKey: boolean read GetIsPayToKey;
      property IsClassicPASA: boolean read GetIsStandard;
      property HasPayload: boolean read GetHasPayload;
      class property Empty : TEPasa read GetEmptyValue;

      function GetRawPayloadBytes(): TBytes; inline;

      function ToClassicPASAString(): String; overload;
      function ToString(): String; overload;
      function ToString(AOmitExtendedChecksum: Boolean): String; overload;

      class function TryParse(const AEPasaText: String; out AEPasa: TEPasa) : Boolean; static;
      class function Parse(const AEPasaText: String): TEPasa; static;

      class function CalculateAccountChecksum(AAccNo: UInt32): Byte; static; inline;
      procedure Clear;
  end;



  { TEPasaParser }

  TEPasaParser = class
    strict private
      class var FEPasaRegex: TCustomRegex;
      class var FEPasaLocker : TCriticalSection;
      class constructor CreateRegexEPasaParser();
      class destructor DestroyRegexEPasaParser();

  public
    const
      // note: regex syntax escapes following chars [\^$.|?*+(){}
      // note: epasa syntax escapes following chars: :\"[]()<>(){}
      // note: c-sharp syntax verbatim strings escape: " as ""
      IntegerPattern = '(0|[1-9]\d*)';
      AccountNamePattern = '(?P<AccountName>' + TPascal64Encoding.StringPattern + ')';
      AccountChecksumPattern = '(?:(?P<ChecksumDelim>-)(?P<Checksum>\d{2}))?';
      AccountNumberPattern = '(?P<AccountNumber>' + IntegerPattern + ')' + AccountChecksumPattern;
      PasaPattern = '(' + AccountNumberPattern + '|' + AccountNamePattern + ')';
      ASCIIContentPattern = '"' + TPascalAsciiEncoding.StringPattern + '"';
      HexContentPattern = '0x' + THexEncoding.SubStringPattern;
      Base58ContentPattern = TPascalBase58Encoding.SubStringPattern;
      PayloadPasswordPattern = '(?:(?P<PayloadPasswordDelim>' + ':){1}(?P<PayloadPassword>' + TPascalAsciiEncoding.StringPattern + ')?)?';
      PayloadStartCharPattern = '(?P<PayloadStartChar>[\[\(<\{])';
      PayloadEndCharPattern = '(?P<PayloadEndChar>[]\)>\}])';
      PayloadContentPattern = '(?P<PayloadContent>' + ASCIIContentPattern + '|' + HexContentPattern + '|' + Base58ContentPattern + ')?';
      PayloadPattern = '(?:' + PayloadStartCharPattern + PayloadContentPattern + PayloadPasswordPattern + PayloadEndCharPattern + ')?';
      ExtendedChecksumPattern = '(?:' + '(?P<ExtendedChecksumDelim>:)' + '(?P<ExtendedChecksum>' + THexEncoding.BytePattern + THexEncoding.BytePattern + '))?';
      EPasaPattern = PasaPattern + PayloadPattern + ExtendedChecksumPattern;

      function Parse(const AEPasaText: String): TEPasa;
      function TryParse(const AEPasaText: String; out AEPasa: TEPasa): Boolean; overload;
      function TryParse(const AEPasaText: String; out AEPasa: TEPasa; out AErrorCode: EPasaErrorCode): Boolean; overload;
  end;

  { TEPasaComp }

  TEPasaComp = class sealed(TObject)

    strict private
      class function ReadUInt16AsBytesLE(AValue: UInt16): TArray<Byte>; static;

    public
      const
        MaxPublicAsciiContentLength: Int32 = 255;
        MaxECIESAsciiContentLength: Int32 = 144;
        MaxAESAsciiContentLength: Int32 = 223;
        MaxPublicHexContentLength: Int32 = 510 + 2;
        MaxECIESHexContentLength: Int32 = 288 + 2;
        MaxAESHexContentLength: Int32 = 446 + 2;
        MaxPublicBase58ContentLength: Int32 = 348;
        MaxECIESBase58ContentLength: Int32 = 196;
        MaxAESBase58ContentLength: Int32 = 304;
        ExtendedChecksumMurMur3Seed: UInt32 = 0;

      class function ComputeExtendedChecksum(const AText: String): String; static;
      class function IsValidExtendedChecksum(const AText: String; const AChecksum: String): Boolean; static;
      class function IsValidPayloadLength(APayloadType: TPayloadType; const APayloadContent: String): Boolean; static;
      class function IsValidPasswordLength(const APasswordValue: String) : Boolean; static;

      class function GetPayloadTypeProtocolByte(const APayloadType : TPayloadType) : Byte;
      class function GetPayloadTypeFromProtocolByte(AByte : Byte) : TPayloadType;
      class function FromProtocolValue(AVal : Byte) : TPayloadType;
  end;

resourcestring
  SUnknownPayloadEncoding = 'Unknown payload encoding.';
  SInvalidEPasaFormat = 'Invalid E-PASA format, %s';
  SInvalidEPasa = 'Invalid EPASA "%s": %s';
  SInvalidHexString = 'Invalid hex-formatted string, %s';
  SInvalidBase58String = 'Invalid Base58-formatted string, %s';
  SBase58EncodeError = 'Error Encoding to Base58';
  SUnRecognizedStartCharacter = 'Unrecognized start character, %s';
  SInvalidPASCQuantity = 'Invalid PASC quantity string, %s';

implementation

uses
  HlpHashFactory,
  HlpIHashInfo,
  HlpConverters,
  UMemory;

var
  EmptyEPasa : TEPasa;

{ TPayloadTraitHelper }

function TPayloadTraitHelper.ToProtocolValue: Byte;
begin
  case Self of
    ptNonDeterministic: Exit(0);
    ptPublic: Exit(BYTE_BIT_0);
    ptRecipientKeyEncrypted: Exit(BYTE_BIT_1);
    ptSenderKeyEncrypted: Exit(BYTE_BIT_2);
    ptPasswordEncrypted: Exit(BYTE_BIT_3);
    ptAsciiFormatted: Exit(BYTE_BIT_4);
    ptHexFormatted: Exit(BYTE_BIT_5);
    ptBase58Formatted: Exit(BYTE_BIT_6);
    ptAddressedByName: Exit(BYTE_BIT_7);
  end;
  raise Exception.Create('Internal Error 2faed11a-1b0f-447a-87d1-2e1735ac4ca2');
end;

{ TPayloadTypeHelper }

function TPayloadTypeHelper.HasTrait(APayloadTrait : TPayloadTrait) : Boolean;
begin
  Result := APayloadTrait in Self;
end;

function TPayloadTypeHelper.IsValid: Boolean;
var LValue, LEncryptedBits, LFormattedBits : Byte;
begin
  { As described on PIP-0027 E-PASA:
    Bits 0..3 describe how payload is encrypted. 1 option (and only 1) must be selected
    Bits 4..6 describe how is data encoded: String, Hexa or Base58. 1 option (and 1 only 1) must be selected

    IsValid = 1 bit from 0..3 and 1 bit from 4..6 must be selected
  }
  LValue := Self.ToProtocolValue;
  LEncryptedBits := (LValue and $0F); // 0000 1111
  LFormattedBits := (LValue and $70); // 0111 0000
  Result :=
      (
         ((LEncryptedBits and BYTE_BIT_0)=BYTE_BIT_0)
      or ((LEncryptedBits and BYTE_BIT_1)=BYTE_BIT_1)
      or ((LEncryptedBits and BYTE_BIT_2)=BYTE_BIT_2)
      or ((LEncryptedBits and BYTE_BIT_3)=BYTE_BIT_3)
      )
      and
      (
         ((LFormattedBits and BYTE_BIT_4)=BYTE_BIT_4)
      or ((LFormattedBits and BYTE_BIT_5)=BYTE_BIT_5)
      or ((LFormattedBits and BYTE_BIT_6)=BYTE_BIT_6)
      );
end;

function TPayloadTypeHelper.ToProtocolValue : Byte;
begin
  Result := TEPasaComp.GetPayloadTypeProtocolByte(Self);
end;

{ TEPasa }

procedure TEPasa.Clear;
begin
  Self.FAccount.Clear;
  Self.FAccountChecksum.Clear;
  Self.FAccountName:='';
  Self.FPayload:='';
  Self.FPassword:='';
  Self.FExtendedChecksum:='';
  Self.FPayloadType:=[];
end;

function TEPasa.GetAccount: TNullable<UInt32>;
begin
  Result := FAccount;
end;

function TEPasa.GetAccountChecksum: TNullable<UInt32>;
begin
  Result := FAccountChecksum;
end;

function TEPasa.GetAccountName: String;
begin
  Result := FAccountName;
end;

function TEPasa.GetExtendedChecksum: String;
begin
  Result := FExtendedChecksum;
end;

function TEPasa.GetPassword: String;
begin
  Result := FPassword;
end;

function TEPasa.GetPayload: String;
begin
  Result := FPayload;
end;

function TEPasa.GetPayloadType: TPayloadType;
begin
  Result := FPayloadType;
end;

procedure TEPasa.SetAccount(const AValue: TNullable<UInt32>);
begin
  FAccount := AValue;
end;

procedure TEPasa.SetAccountChecksum(const AValue: TNullable<UInt32>);
begin
  FAccountChecksum := AValue;
end;

procedure TEPasa.SetAccountName(const AValue: String);
begin
  FAccountName := AValue;
end;

procedure TEPasa.SetExtendedChecksum(const AValue: String);
begin
  FExtendedChecksum := AValue;
end;

procedure TEPasa.SetPassword(const AValue: String);
begin
  FPassword := AValue;
end;

procedure TEPasa.SetPayload(const AValue: String);
begin
  FPayload := AValue;
end;

procedure TEPasa.SetPayloadType(const AValue: TPayloadType);
begin
  FPayloadType := AValue;
end;

function TEPasa.GetIsAddressedByNumber : Boolean;
begin
  Result := NOT PayloadType.HasTrait(ptAddressedByName);
end;

function TEPasa.GetIsAddressedByName : Boolean;
begin
  Result := (NOT IsPayToKey) AND PayloadType.HasTrait(ptAddressedByName);
end;

function TEPasa.GetIsPayToKey: Boolean;
begin
  Result :=
    (AccountName = '@') and
    (PayloadType.HasTrait(ptAddressedByName) and
    PayloadType.HasTrait(ptPublic) and
    PayloadType.HasTrait(ptBase58Formatted));
end;

function TEPasa.GetIsStandard: Boolean;
begin
  Result := (NOT PayloadType.HasTrait(ptAddressedByName)) AND (NOT HasPayload);
end;

function TEPasa.GetHasPayload: Boolean;
begin
  Result := PayloadType.HasTrait(ptPublic) OR PayloadType.HasTrait(ptRecipientKeyEncrypted) OR PayloadType.HasTrait(ptSenderKeyEncrypted);
end;

function TEPasa.GetRawPayloadBytes: TBytes;
begin
  if (PayloadType.HasTrait(ptAsciiFormatted)) then
    Exit(TEncoding.ASCII.GetBytes(Payload));

  if (PayloadType.HasTrait(ptBase58Formatted)) then
    Exit(TPascalBase58Encoding.Decode(Payload));

  if (PayloadType.HasTrait(ptHexFormatted)) then
    Exit(THexEncoding.Decode(Payload));

  raise EPascalCoinException.CreateRes(@SUnknownPayloadEncoding);
end;

function TEPasa.ToClassicPASAString : String;
begin
  Result := ToString(True);
end;

function TEPasa.ToString: String;
begin
  Result := ToString(False);
end;

function TEPasa.ToString(AOmitExtendedChecksum: Boolean): String;
var
  LPayloadContent: String;
begin
  Result := string.Empty;
  if PayloadType.HasTrait(ptNonDeterministic) then Exit;

  if (PayloadType.HasTrait(ptAddressedByName)) then begin
    Result := Result + TPascal64Encoding.Escape(AccountName);
  end else begin
    if (Not Account.HasValue) then Exit;
    Result := Result + Account.Value.ToString();
    if (AccountChecksum.HasValue) then begin
      Result := Result + String.Format('-%u', [AccountChecksum.Value]);
    end;
  end;

  if (PayloadType.HasTrait(ptAsciiFormatted)) then begin
    LPayloadContent := String.Format('"%s"', [TPascalAsciiEncoding.Escape(Payload)]);
  end else if (PayloadType.HasTrait(ptHexFormatted)) then begin
    LPayloadContent := string.Format('0x%s', [Payload]);
  end else if (PayloadType.HasTrait(ptBase58Formatted)) then begin
    LPayloadContent := string.Format('%s', [Payload]);
  end else begin
    // it is non-deterministic, so payload content is ignored
    LPayloadContent := string.Empty;
  end;

  if (PayloadType.HasTrait(ptPublic)) then begin
    Result := Result + string.Format('[%s]', [LPayloadContent]);
  end else if (PayloadType.HasTrait(ptRecipientKeyEncrypted)) then begin
    Result := Result + string.Format('(%s)', [LPayloadContent]);
  end else if (PayloadType.HasTrait(ptSenderKeyEncrypted)) then begin
    Result := Result + string.Format('<%s>', [LPayloadContent]);
  end else if (PayloadType.HasTrait(ptPasswordEncrypted)) then begin
    Result := Result + string.Format('{%s:%s}', [LPayloadContent, TPascalAsciiEncoding.Escape(Password)]);
  end else begin
    // it is non-deterministic, so payload omitted entirely
  end;

  if (not AOmitExtendedChecksum) then begin
    if (ExtendedChecksum='') then begin
      // Need to compute:
      ExtendedChecksum := TEPasaComp.ComputeExtendedChecksum(Result);
    end;
    Result := Result + string.Format(':%s', [ExtendedChecksum]);
  end;
end;



class function TEPasa.TryParse(const AEPasaText: String; out AEPasa: TEPasa): Boolean;
var
  LParser: TEPasaParser;
  LDisposables : TDisposables;
begin
  LParser := LDisposables.AddObject( TEPasaParser.Create() ) as TEPasaParser;
  Result := LParser.TryParse(AEPasaText, AEPasa);
end;

class function TEPasa.Parse(const AEPasaText: String): TEPasa;
begin
  if (TryParse(AEPasaText, Result)) then
    Exit(Result);
  raise EArgumentException.CreateResFmt(@SInvalidEPasaFormat, ['AEPasaText']);
end;

class function TEPasa.CalculateAccountChecksum(AAccNo: UInt32): Byte;
begin
  Result := Byte(((UInt64(AAccNo) * 101) mod 89) + 10);
end;


class function TEPasa.GetEmptyValue : TEPasa;
begin
  Result := EmptyEPasa;
end;

{ TEPasaParser }

class constructor TEPasaParser.CreateRegexEPasaParser;
begin
  FEPasaRegex := TCustomRegex.Create(EPasaPattern);
  FEPasaLocker := TCriticalSection.Create;
end;

class destructor TEPasaParser.DestroyRegexEPasaParser;
begin
  FEPasaRegex.Free;
  FEPasaLocker.Free;
end;

function TEPasaParser.Parse(const AEPasaText: String): TEPasa;
var
  LErrorCode: EPasaErrorCode;
begin
  if (not TryParse(AEPasaText, Result, LErrorCode)) then
    raise EArgumentException.CreateResFmt(@SInvalidEPasa,
      ['AEPasaText', GetEnumName(TypeInfo(EPasaErrorCode), Ord(LErrorCode))]);
  Exit(Result);
end;

function TEPasaParser.TryParse(const AEPasaText: String; out AEPasa: TEPasa): Boolean;
var
  LErrorCode: EPasaErrorCode;
begin
  Result := TryParse(AEPasaText, AEPasa, LErrorCode);
end;

function TEPasaParser.TryParse(const AEPasaText: String; out AEPasa: TEPasa; out AErrorCode: EPasaErrorCode): Boolean;
var
  LChecksumDelim, LAccountNumber, LAccountChecksum, LAccountName, LPayloadStartChar,
  LPayloadEndChar, LPayloadContent, LPayloadPasswordDelim, LPayloadPassword,
  LExtendedChecksumDelim, LExtendedChecksum, LActualChecksum: String;
  LAccNo, LAccChecksum: UInt32;
  LActualAccountChecksum: Byte;
begin
  AErrorCode := EPasaErrorCode.Success;
  AEPasa.Clear;
  if (string.IsNullOrEmpty(AEPasaText)) then begin
    AErrorCode := EPasaErrorCode.BadFormat;
    Exit(False);
  end;

  FEPasaLocker.Acquire; // Protect against multithread
  Try

  FEPasaRegex.Match(AEPasaText);

  LChecksumDelim := FEPasaRegex.GetMatchFromName('ChecksumDelim');
  LAccountNumber := FEPasaRegex.GetMatchFromName('AccountNumber');
  LAccountChecksum := FEPasaRegex.GetMatchFromName('Checksum');
  LAccountName := FEPasaRegex.GetMatchFromName('AccountName');
  LPayloadStartChar := FEPasaRegex.GetMatchFromName('PayloadStartChar');
  LPayloadEndChar := FEPasaRegex.GetMatchFromName('PayloadEndChar');
  LPayloadContent := FEPasaRegex.GetMatchFromName('PayloadContent');
  LPayloadPasswordDelim := FEPasaRegex.GetMatchFromName('PayloadPasswordDelim');
  LPayloadPassword := FEPasaRegex.GetMatchFromName('PayloadPassword');
  LExtendedChecksumDelim := FEPasaRegex.GetMatchFromName('ExtendedChecksumDelim');
  LExtendedChecksum := FEPasaRegex.GetMatchFromName('ExtendedChecksum');

  // Check parsed completely
  if (AEPasaText <> FEPasaRegex.Value) then begin
    AErrorCode := EPasaErrorCode.BadFormat;
    Exit(False);
  end;

  Finally
    FEPasaLocker.Release;
  End;

  if (LAccountName <> #0) then begin
    // Account Name
    if (string.IsNullOrEmpty(LAccountName)) then begin
      AErrorCode := EPasaErrorCode.BadFormat;
      Exit(False);
    end;

    // KeyNote
    // when multiple enums are OR'ed in C#, they are combined and
    // if any of the enums numeric value is zero, it is excluded.
    // in our case,"PayloadType.NonDeterministic" is always zero so we exclude it from our set.
    AEPasa.PayloadType := AEPasa.PayloadType + [ptAddressedByName] -[ptNonDeterministic];
    AEPasa.AccountName := TPascal64Encoding.Unescape(LAccountName);
    AEPasa.Account := Nil;
    AEPasa.AccountChecksum := Nil;
  end else begin
    // Account Number
    if (not UInt32.TryParse(LAccountNumber, LAccNo)) then begin
      AErrorCode := EPasaErrorCode.InvalidAccountNumber;
      Exit(False);
    end;
    AEPasa.Account := LAccNo;
    LActualAccountChecksum := TEPasa.CalculateAccountChecksum(LAccNo);

    if (LChecksumDelim <> #0) then begin
      // validate account checksum
      if (not UInt32.TryParse(LAccountChecksum, LAccChecksum)) then begin
        AErrorCode := EPasaErrorCode.AccountChecksumInvalid;
        Exit(False);
      end;
      if (LAccChecksum <> LActualAccountChecksum) then begin
        AErrorCode := EPasaErrorCode.BadChecksum;
        Exit(False);
      end;
    end;

    AEPasa.AccountChecksum := LActualAccountChecksum;
  end;

  // Encryption type
  case LPayloadStartChar[1] of
    #0: begin
        // do nothing
      end;
    '[': begin
        if (LPayloadEndChar <> ']') then begin
          AErrorCode := EPasaErrorCode.MismatchedPayloadEncoding;
          Exit(False);
        end;
        AEPasa.PayloadType := AEPasa.PayloadType + [ptPublic] -
          [ptNonDeterministic];
      end;
     '(': begin
        if (LPayloadEndChar <> ')') then begin
          AErrorCode := EPasaErrorCode.MismatchedPayloadEncoding;
          Exit(False);
        end;
        AEPasa.PayloadType := AEPasa.PayloadType + [ptRecipientKeyEncrypted] - [ptNonDeterministic];
      end;
    '<': begin
        if (LPayloadEndChar <> '>') then begin
          AErrorCode := EPasaErrorCode.MismatchedPayloadEncoding;
          Exit(False);
        end;
        AEPasa.PayloadType := AEPasa.PayloadType +
          [ptSenderKeyEncrypted] - [ptNonDeterministic];
      end;

    '{': begin
        if (LPayloadEndChar <> '}') then begin
          AErrorCode := EPasaErrorCode.MismatchedPayloadEncoding;
          Exit(False);
        end;
        AEPasa.PayloadType := AEPasa.PayloadType + [ptPasswordEncrypted] - [ptNonDeterministic];
      end
      else raise ENotSupportedException.CreateResFmt(@SUnRecognizedStartCharacter, [LPayloadStartChar]);
  end;

  // Password
  if (AEPasa.PayloadType.HasTrait(ptPasswordEncrypted)) then begin
    if (LPayloadPasswordDelim = #0) then begin
      AErrorCode := EPasaErrorCode.MissingPassword;
      Exit(False);
    end;

    AEPasa.Password := TPascalAsciiEncoding.UnEscape(IIF(LPayloadPassword = #0, '', LPayloadPassword));
  end else if (LPayloadPasswordDelim <> #0) then begin
    AErrorCode := EPasaErrorCode.UnusedPassword;
    Exit(False);
  end;

  // Payload
  if (LPayloadStartChar <> #0) then begin
    if (LPayloadContent = #0) then begin
      AEPasa.Payload := string.Empty;
    end else if (LPayloadContent.StartsWith('"')) then begin
      AEPasa.PayloadType := AEPasa.PayloadType + [ptAsciiFormatted] - [ptNonDeterministic];
      AEPasa.Payload := TPascalAsciiEncoding.UnEscape(LPayloadContent.Trim(['"']));
    end else if (LPayloadContent.StartsWith('0x')) then begin
      AEPasa.PayloadType := AEPasa.PayloadType + [ptHexFormatted] - [ptNonDeterministic];
      AEPasa.Payload := System.Copy(LPayloadContent, 3, LPayloadContent.Length - 2);
    end else begin
      AEPasa.PayloadType := AEPasa.PayloadType + [ptBase58Formatted] - [ptNonDeterministic];
      AEPasa.Payload := LPayloadContent;
    end;
  end;

  // Payload Lengths
  if (not TEPasaComp.IsValidPayloadLength(AEPasa.PayloadType, AEPasa.Payload)) then begin
    AErrorCode := EPasaErrorCode.PayloadTooLarge;
    Exit(False);
  end;

  // Extended Checksum
  LActualChecksum := TEPasaComp.ComputeExtendedChecksum(AEPasa.ToString(True));
  if (LExtendedChecksumDelim <> #0) then begin
    if (LExtendedChecksum <> LActualChecksum) then begin
      AErrorCode := EPasaErrorCode.BadExtendedChecksum;
      Exit(False);
    end;
  end;
  AEPasa.ExtendedChecksum := LActualChecksum;

  Result := True;
end;


{ TEPasaComp }

class function TEPasaComp.ReadUInt16AsBytesLE(AValue: UInt16): TArray<Byte>;
begin
  System.SetLength(Result, System.SizeOf(UInt16));
  Result[0] := Byte(AValue);
  Result[1] := Byte(AValue shr 8);
end;

class function TEPasaComp.ComputeExtendedChecksum(const AText: String): String;
var
  LHashInstance: IHashWithKey;
  LChecksum: UInt16;
begin
  LHashInstance := THashFactory.THash32.CreateMurmurHash3_x86_32();
  LHashInstance.Key := TConverters.ReadUInt32AsBytesLE(ExtendedChecksumMurMur3Seed);
  LChecksum := UInt16(LHashInstance.ComputeBytes(TEncoding.ASCII.GetBytes(AText)).GetUInt32() mod 65536);
  Result := THexEncoding.Encode(ReadUInt16AsBytesLE(LChecksum), True);
end;

class function TEPasaComp.IsValidExtendedChecksum(const AText, AChecksum: String): Boolean;
begin
  Result := ComputeExtendedChecksum(AText) = AChecksum;
end;

class function TEPasaComp.IsValidPayloadLength(APayloadType: TPayloadType; const APayloadContent: String): Boolean;
begin
  if (string.IsNullOrEmpty(APayloadContent)) then
    Exit(True);

  if (APayloadType.HasTrait(ptPublic)) then begin

    if (APayloadType.HasTrait(ptAsciiFormatted)) then
      Exit(TPascalAsciiEncoding.UnEscape(APayloadContent).Length <= MaxPublicAsciiContentLength);

    if (APayloadType.HasTrait(ptHexFormatted)) then
      Exit(APayloadContent.Length <= MaxPublicHexContentLength);

    if (APayloadType.HasTrait(ptBase58Formatted)) then
      Exit(APayloadContent.Length <= MaxPublicBase58ContentLength);

    // unknown encoding format
    Result := False;
  end;

  if (APayloadType.HasTrait(ptSenderKeyEncrypted) or APayloadType.HasTrait(ptRecipientKeyEncrypted)) then begin

    if (APayloadType.HasTrait(ptAsciiFormatted)) then
      Exit(TPascalAsciiEncoding.UnEscape(APayloadContent).Length <= MaxECIESAsciiContentLength);

    if (APayloadType.HasTrait(ptHexFormatted)) then
      Exit(APayloadContent.Length <= MaxECIESHexContentLength);

    if (APayloadType.HasTrait(ptBase58Formatted)) then
      Exit(APayloadContent.Length <= MaxECIESBase58ContentLength);
    // unknown encoding format
    Result := False;
  end;

  if (APayloadType.HasTrait(ptPasswordEncrypted)) then begin
    if (APayloadType.HasTrait(ptAsciiFormatted)) then
      Exit(TPascalAsciiEncoding.UnEscape(APayloadContent).Length <= MaxAESAsciiContentLength);

    if (APayloadType.HasTrait(ptHexFormatted)) then
      Exit(APayloadContent.Length <= MaxAESHexContentLength);

    if (APayloadType.HasTrait(ptBase58Formatted)) then
      Exit(APayloadContent.Length <= MaxAESBase58ContentLength);

    // unknown encoding format
    Result := False;
  end;

  // unknown encryption format
  Result := False;
end;

class function TEPasaComp.IsValidPasswordLength(const APasswordValue : String): Boolean;
begin
  // no password length policy established (only client-side concern)
  Result := True;
end;

class function TEPasaComp.GetPayloadTypeProtocolByte(const APayloadType : TPayloadType) : Byte;
var
 LPayloadType : TPayloadTrait;
begin
  Result := 0; // NonDeterministic by default
  for LPayloadType := Low(TPayloadTrait) to High(TPayloadTrait) do
    if APayloadType.HasTrait(LPayloadType) then
      Result := Result OR LPayloadType.ToProtocolValue;
end;

class function TEPasaComp.GetPayloadTypeFromProtocolByte(AByte: Byte) : TPayloadType;
var
 LPayloadType : TPayloadTrait;
 LPayloadTypeByte : byte;
begin
  if AByte = 0 then
    Exit([ptNonDeterministic]);

  Result := [];
  for LPayloadType := Low(TPayloadTrait) to High(TPayloadTrait) do begin
    LPayloadTypeByte := LPayloadType.ToProtocolValue;
    if (AByte AND LPayloadTypeByte) = LPayloadTypeByte then
      Result := Result + [LPayloadType];
  end;
end;

class function TEPasaComp.FromProtocolValue(AVal : Byte) : TPayloadType;
begin
  if AVal = 0 then begin
    Exit([ptNonDeterministic]);
  end;
  Result := [];
  if AVal AND BYTE_BIT_0 <> 0 then Result := Result + [ptPublic];
  if AVal AND BYTE_BIT_1 <> 0 then Result := Result + [ptRecipientKeyEncrypted];
  if AVal AND BYTE_BIT_2 <> 0 then Result := Result + [ptSenderKeyEncrypted];
  if AVal AND BYTE_BIT_3 <> 0 then Result := Result + [ptPasswordEncrypted];
  if AVal AND BYTE_BIT_4 <> 0 then Result := Result + [ptAsciiFormatted];
  if AVal AND BYTE_BIT_5 <> 0 then Result := Result + [ptHexFormatted];
  if AVal AND BYTE_BIT_6 <> 0 then Result := Result + [ptBase58Formatted];
  if AVal AND BYTE_BIT_7 <> 0 then Result := Result + [ptAddressedByName];
end;




initialization
{$IFDEF FPC}
FillChar(EmptyEPasa, SizeOf(EmptyEPASA), 0);
{$ELSE}
EmptyEPasa := Default(TEPasa);
{$ENDIF}
end.
