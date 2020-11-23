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
  UAccounts,
  UCommon,
  UCrypto,
  UEncoding;

type

  EPascalCoinException = class(Exception);

  EPasaErrorCode = (
    Success, BadFormat, BadChecksum, InvalidAccountNumber,
    AccountChecksumInvalid, InvalidAccountName, MismatchedPayloadEncoding,
    PayloadTooLarge, MissingPassword, UnusedPassword, InvalidPassword,
    BadExtendedChecksum
  );

  { PayloadType}

  PayloadType = (
    NonDeterministic = 0,      // Payload encryption and encoding method not specified.
    &Public = 1,               // Unencrypted, public payload.
    RecipientKeyEncrypted = 2, // ECIES encrypted using recipient accounts public key.
    SenderKeyEncrypted = 3,    // ECIES encrypted using sender accounts public key.
    PasswordEncrypted = 4,     // AES encrypted using pwd param
    AsciiFormatted = 5,        // Payload data encoded in ASCII
    HexFormatted = 6,          // Payload data encoded in HEX
    Base58Formatted = 7,       // Payload data encoded in Base58
    AddressedByName = 8        // E-PASA addressed by account name (not number).
  );

  { PayloadTypeHelper }

  PayloadTypeHelper = record helper for PayloadType
  public
    function Value: Int32;
  end;

  { PayloadTypes }

  PayloadTypes = set of PayloadType;

  { PayloadTypesHelper }

  PayloadTypesHelper = record helper for PayloadTypes
  public
    function IsPayloadTypeInSet(APayloadType: PayloadType): Boolean; inline;
  end;


  { TEPasa }

  TEPasa = record
    strict private
      var
        FAccount, FAccountChecksum: TNullable<UInt32>;
        FAccountName, FPayload, FPassword, FExtendedChecksum: String;
        FPayloadTypes: PayloadTypes;

      function GetAccount: TNullable<UInt32>; inline;
      procedure SetAccount(const AValue: TNullable<UInt32>); inline;
      function GetAccountChecksum: TNullable<UInt32>; inline;
      procedure SetAccountChecksum(const AValue: TNullable<UInt32>); inline;
      function GetPayloadTypes: PayloadTypes; inline;
      function GetAccountName: String; inline;
      procedure SetAccountName(const AValue: String); inline;
      procedure SetPayloadTypes(const AValue: PayloadTypes); inline;
      function GetExtendedChecksum: String; inline;
      procedure SetExtendedChecksum(const AValue: String); inline;
      function GetPassword: String; inline;
      procedure SetPassword(const AValue: String); inline;
      function GetPayload: String; inline;
      procedure SetPayload(const AValue: String); inline;

    public
      function IsPayToKey: Boolean; inline;
      function GetRawPayloadBytes(): TArray<Byte>; inline;
      function ToString(): String; overload;
      function ToString(AOmitExtendedChecksum: Boolean): String; reintroduce; overload;

      property Account: TNullable<UInt32> read GetAccount write SetAccount;
      property AccountChecksum: TNullable<UInt32> read GetAccountChecksum write SetAccountChecksum;
      property AccountName: String read GetAccountName write SetAccountName;
      property PayloadTypes: PayloadTypes read GetPayloadTypes write SetPayloadTypes;
      property Payload: String read GetPayload write SetPayload;
      property Password: String read GetPassword write SetPassword;
      property ExtendedChecksum: String read GetExtendedChecksum write SetExtendedChecksum;

      class function TryParse(const AEPasaText: String; out AEPasa: TEPasa) : Boolean; static;
      class function Parse(const AEPasaText: String): TEPasa; static;

      class function CalculateAccountChecksum(AAccNo: UInt32): Byte; static; inline;

  end;

  { TEPasaParser }

  TEPasaParser = class
    strict private
      class var FEPasaRegex: TCustomRegex;
      class constructor CreateRegexEPasaParser();
      class destructor DestroyRegexEPasaParser();

  public
    const
      // note: regex syntax escapes following chars [\^$.|?*+(){}
      // note: epasa syntax escapes following chars: :\"[]()<>(){}
      // note: c-sharp syntax verbatim strings escape: " as ""
      IntegerPattern = '(0|[1-9]\d+)';
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

  { TEPasaHelper }

  TEPasaHelper = class sealed(TObject)

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
      class function IsValidPayloadLength(APayloadType: PayloadTypes; const APayloadContent: String): Boolean; static;
      class function IsValidPasswordLength(const APasswordValue: String) : Boolean; static;

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

{ PayloadTypeHelper }

function PayloadTypeHelper.Value: Int32;
begin
  case Self of
    NonDeterministic: Result := $00000000;
    &Public: Result := $00000001;
    RecipientKeyEncrypted: Result := $00000010;
    SenderKeyEncrypted: Result := $00000100;
    PasswordEncrypted: Result := $00001000;
    AsciiFormatted: Result := $00010000;
    HexFormatted: Result := $00100000;
    Base58Formatted: Result := $01000000;
    AddressedByName: Result := $10000000;
  end;
end;

{ PayloadTypesHelper }

function PayloadTypesHelper.IsPayloadTypeInSet(APayloadType : PayloadType) : Boolean;
begin
  Result := APayloadType in Self;
end;

{ TEPasa }

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

function TEPasa.GetPayloadTypes: PayloadTypes;
begin
  Result := FPayloadTypes;
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

procedure TEPasa.SetPayloadTypes(const AValue: PayloadTypes);
begin
  FPayloadTypes := AValue;
end;

function TEPasa.IsPayToKey: Boolean;
begin
  Result :=
    (AccountName = '@') and
    (PayloadTypes.IsPayloadTypeInSet(PayloadType.AddressedByName) and
    PayloadTypes.IsPayloadTypeInSet(PayloadType.Public) and
    PayloadTypes.IsPayloadTypeInSet(PayloadType.Base58Formatted));
end;

function TEPasa.GetRawPayloadBytes: TArray<Byte>;
begin
  if (PayloadTypes.IsPayloadTypeInSet(PayloadType.AsciiFormatted)) then
    Exit(TEncoding.ASCII.GetBytes(Payload));

  if (PayloadTypes.IsPayloadTypeInSet(PayloadType.Base58Formatted)) then
    Exit(TPascalBase58Encoding.Decode(Payload));

  if (PayloadTypes.IsPayloadTypeInSet(PayloadType.HexFormatted)) then
    Exit(THexEncoding.Decode(Payload));

  raise EPascalCoinException.CreateRes(@SUnknownPayloadEncoding);
end;

function TEPasa.ToString(AOmitExtendedChecksum: Boolean): String;
var
  LPayloadContent: String;
begin
  Result := string.Empty;
  if (PayloadTypes.IsPayloadTypeInSet(PayloadType.AddressedByName)) then begin
    Result := Result + TPascal64Encoding.Escape(AccountName);
  end else begin
    Result := Result + Account.Value.ToString();
    if (AccountChecksum.HasValue) then begin
      Result := Result + String.Format('-%u', [AccountChecksum.Value]);
    end;
  end;

  if (PayloadTypes.IsPayloadTypeInSet(PayloadType.AsciiFormatted)) then begin
    LPayloadContent := String.Format('"%s"', [TPascalAsciiEncoding.Escape(Payload)]);
  end else if (PayloadTypes.IsPayloadTypeInSet(PayloadType.HexFormatted)) then begin
    LPayloadContent := string.Format('0x%s', [Payload]);
  end else if (PayloadTypes.IsPayloadTypeInSet(PayloadType.Base58Formatted)) then begin
    LPayloadContent := string.Format('%s', [Payload]);
  end else begin
    // it is non-deterministic, so payload content is ignored
    LPayloadContent := string.Empty;
  end;

  if (PayloadTypes.IsPayloadTypeInSet(PayloadType.Public)) then begin
    Result := Result + string.Format('[%s]', [LPayloadContent]);
  end else if (PayloadTypes.IsPayloadTypeInSet(PayloadType.RecipientKeyEncrypted)) then begin
    Result := Result + string.Format('(%s)', [LPayloadContent]);
  end else if (PayloadTypes.IsPayloadTypeInSet(PayloadType.SenderKeyEncrypted)) then begin
    Result := Result + string.Format('<%s>', [LPayloadContent]);
  end else if (PayloadTypes.IsPayloadTypeInSet(PayloadType.PasswordEncrypted)) then begin
    Result := Result + string.Format('{%s:%s}', [LPayloadContent, TPascalAsciiEncoding.Escape(Password)]);
  end else begin
    // it is non-deterministic, so payload omitted entirely
  end;

  if (not AOmitExtendedChecksum) then begin
    Result := Result + string.Format(':%s', [ExtendedChecksum]);
  end;
end;

function TEPasa.ToString: String;
begin
  Result := ToString(False);
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


{ TEPasaParser }

class constructor TEPasaParser.CreateRegexEPasaParser;
begin
  FEPasaRegex := TCustomRegex.Create(EPasaPattern);
end;

class destructor TEPasaParser.DestroyRegexEPasaParser;
begin
  FEPasaRegex.Free;
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
  LEPasa : TEPasa;
begin
  AErrorCode := EPasaErrorCode.Success;
  AEPasa := LEPasa;
  if (string.IsNullOrEmpty(AEPasaText)) then begin
    AErrorCode := EPasaErrorCode.BadFormat;
    Exit(False);
  end;

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
    AEPasa.PayloadTypes := AEPasa.PayloadTypes + [PayloadType.AddressedByName] -
      [PayloadType.NonDeterministic];
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
        AEPasa.PayloadTypes := AEPasa.PayloadTypes + [PayloadType.Public] -
          [PayloadType.NonDeterministic];
      end;
     '(': begin
        if (LPayloadEndChar <> ')') then begin
          AErrorCode := EPasaErrorCode.MismatchedPayloadEncoding;
          Exit(False);
        end;
        AEPasa.PayloadTypes := AEPasa.PayloadTypes + [PayloadType.RecipientKeyEncrypted] - [PayloadType.NonDeterministic];
      end;
    '<': begin
        if (LPayloadEndChar <> '>') then begin
          AErrorCode := EPasaErrorCode.MismatchedPayloadEncoding;
          Exit(False);
        end;
        AEPasa.PayloadTypes := AEPasa.PayloadTypes +
          [PayloadType.SenderKeyEncrypted] - [PayloadType.NonDeterministic];
      end;

    '{': begin
        if (LPayloadEndChar <> '}') then begin
          AErrorCode := EPasaErrorCode.MismatchedPayloadEncoding;
          Exit(False);
        end;
        AEPasa.PayloadTypes := AEPasa.PayloadTypes + [PayloadType.PasswordEncrypted] - [PayloadType.NonDeterministic];
      end
      else raise ENotSupportedException.CreateResFmt(@SUnRecognizedStartCharacter, [LPayloadStartChar]);
  end;

  // Password
  if (AEPasa.PayloadTypes.IsPayloadTypeInSet(PayloadType.PasswordEncrypted)) then begin
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
      AEPasa.PayloadTypes := AEPasa.PayloadTypes + [PayloadType.AsciiFormatted] - [PayloadType.NonDeterministic];
      AEPasa.Payload := TPascalAsciiEncoding.UnEscape(LPayloadContent.Trim(['"']));
    end else if (LPayloadContent.StartsWith('0x')) then begin
      AEPasa.PayloadTypes := AEPasa.PayloadTypes + [PayloadType.HexFormatted] - [PayloadType.NonDeterministic];
      AEPasa.Payload := System.Copy(LPayloadContent, 3, LPayloadContent.Length - 2);
    end else begin
      AEPasa.PayloadTypes := AEPasa.PayloadTypes + [PayloadType.Base58Formatted] - [PayloadType.NonDeterministic];
      AEPasa.Payload := LPayloadContent;
    end;
  end;

  // Payload Lengths
  if (not TEPasaHelper.IsValidPayloadLength(AEPasa.PayloadTypes, AEPasa.Payload)) then begin
    AErrorCode := EPasaErrorCode.PayloadTooLarge;
    Exit(False);
  end;

  // Extended Checksum
  LActualChecksum := TEPasaHelper.ComputeExtendedChecksum(AEPasa.ToString(True));
  if (LExtendedChecksumDelim <> #0) then begin
    if (LExtendedChecksum <> LActualChecksum) then begin
      AErrorCode := EPasaErrorCode.BadExtendedChecksum;
      Exit(False);
    end;
  end;
  AEPasa.ExtendedChecksum := LActualChecksum;

  Result := True;
end;


{ TEPasaHelper }

class function TEPasaHelper.ReadUInt16AsBytesLE(AValue: UInt16): TArray<Byte>;
begin
  System.SetLength(Result, System.SizeOf(UInt16));
  Result[0] := Byte(AValue);
  Result[1] := Byte(AValue shr 8);
end;

class function TEPasaHelper.ComputeExtendedChecksum(const AText: String): String;
var
  LHashInstance: IHashWithKey;
  LChecksum: UInt16;
begin
  LHashInstance := THashFactory.THash32.CreateMurmurHash3_x86_32();
  LHashInstance.Key := TConverters.ReadUInt32AsBytesLE(ExtendedChecksumMurMur3Seed);
  LChecksum := UInt16(LHashInstance.ComputeBytes(TEncoding.ASCII.GetBytes(AText)).GetUInt32() mod 65536);
  Result := THexEncoding.Encode(ReadUInt16AsBytesLE(LChecksum), True);
end;

class function TEPasaHelper.IsValidExtendedChecksum(const AText, AChecksum: String): Boolean;
begin
  Result := ComputeExtendedChecksum(AText) = AChecksum;
end;

class function TEPasaHelper.IsValidPayloadLength(APayloadType: PayloadTypes; const APayloadContent: String): Boolean;
begin
  if (string.IsNullOrEmpty(APayloadContent)) then
    Exit(True);

  if (APayloadType.IsPayloadTypeInSet(PayloadType.Public)) then begin

    if (APayloadType.IsPayloadTypeInSet(PayloadType.AsciiFormatted)) then
      Exit(TPascalAsciiEncoding.UnEscape(APayloadContent).Length <= MaxPublicAsciiContentLength);

    if (APayloadType.IsPayloadTypeInSet(PayloadType.HexFormatted)) then
      Exit(APayloadContent.Length <= MaxPublicHexContentLength);

    if (APayloadType.IsPayloadTypeInSet(PayloadType.Base58Formatted)) then
      Exit(APayloadContent.Length <= MaxPublicBase58ContentLength);

    // unknown encoding format
    Result := False;
  end;

  if (APayloadType.IsPayloadTypeInSet(PayloadType.SenderKeyEncrypted) or APayloadType.IsPayloadTypeInSet(PayloadType.RecipientKeyEncrypted)) then begin

    if (APayloadType.IsPayloadTypeInSet(PayloadType.AsciiFormatted)) then
      Exit(TPascalAsciiEncoding.UnEscape(APayloadContent).Length <= MaxECIESAsciiContentLength);

    if (APayloadType.IsPayloadTypeInSet(PayloadType.HexFormatted)) then
      Exit(APayloadContent.Length <= MaxECIESHexContentLength);

    if (APayloadType.IsPayloadTypeInSet(PayloadType.Base58Formatted)) then
      Exit(APayloadContent.Length <= MaxECIESBase58ContentLength);
    // unknown encoding format
    Result := False;
  end;

  if (APayloadType.IsPayloadTypeInSet(PayloadType.PasswordEncrypted)) then begin
    if (APayloadType.IsPayloadTypeInSet(PayloadType.AsciiFormatted)) then
      Exit(TPascalAsciiEncoding.UnEscape(APayloadContent).Length <= MaxAESAsciiContentLength);

    if (APayloadType.IsPayloadTypeInSet(PayloadType.HexFormatted)) then
      Exit(APayloadContent.Length <= MaxAESHexContentLength);

    if (APayloadType.IsPayloadTypeInSet(PayloadType.Base58Formatted)) then
      Exit(APayloadContent.Length <= MaxAESBase58ContentLength);

    // unknown encoding format
    Result := False;
  end;

  // unknown encryption format
  Result := False;
end;

class function TEPasaHelper.IsValidPasswordLength(const APasswordValue : String): Boolean;
begin
  // no password length policy established (only client-side concern)
  Result := True;
end;

end.
