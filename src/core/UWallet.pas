unit UWallet;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, USettings, UBlockChain, UAccounts, UCrypto, UCommon;

Type
  TWalletKey = Record
    Name : AnsiString;
    AccountKey : TAccountKey;
    CryptedKey : TRawBytes;
    PrivateKey : TECPrivateKey;
    SearchableAccountKey : TRawBytes;
    function HasPrivateKey : boolean;
  End;

  TWalletKeys = Class(TComponent)
  private
    FSearchableKeys : TList;
    FFileName: AnsiString;
    FWalletPassword: AnsiString;
    FWalletFileStream : TFileStream;
    FIsValidPassword: Boolean;
    FWalletFileName: AnsiString;
    FIsReadingStream : Boolean;
    FOnChanged: TNotifyManyEvent;
    function GetHasPassword : boolean;
    function GetKey(index: Integer): TWalletKey;
    procedure SetWalletPassword(const Value: AnsiString);
    Procedure GeneratePrivateKeysFromPassword;
    procedure SetWalletFileName(const Value: AnsiString);
    Function Find(Const AccountKey: TAccountKey; var Index: Integer): Boolean;
  public
    Property Key[index : Integer] : TWalletKey read GetKey; default;
    Constructor Create(AOwner : TComponent); override;
    Destructor destroy; override;
    Procedure LoadFromStream(Stream : TStream);
    Procedure SaveToStream(Stream : TStream);
    Property IsValidPassword : Boolean read FIsValidPassword;
    property HasPassword : boolean read GetHasPassword;
    Property WalletPassword : AnsiString read FWalletPassword write SetWalletPassword;
    Function AddPrivateKey(Const Name : AnsiString; ECPrivateKey : TECPrivateKey) : Integer; virtual;
    Function AddPublicKey(Const Name : AnsiString; ECDSA_Public : TECDSA_Public) : Integer; virtual;
    Function IndexOfAccountKey(AccountKey : TAccountKey) : Integer;
    Procedure Delete(index : Integer); virtual;
    Procedure Clear; virtual;
    Function Count : Integer;
    Property WalletFileName : AnsiString read FWalletFileName write SetWalletFileName;
    Property OnChanged : TNotifyManyEvent read FOnChanged write FOnChanged;
    Procedure SetName(index : Integer; Const newName : AnsiString);
    Function LockWallet : Boolean;
  End;

  TWalletKeysExt = Class(TWalletKeys)
  private
    FOrderedAccountKeysList : TOrderedAccountKeysList;
    procedure SetSafeBox(const Value: TPCSafeBox);
    function GetSafeBox: TPCSafeBox;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor destroy; override;
    Function AddPrivateKey(Const Name : AnsiString; ECPrivateKey : TECPrivateKey) : Integer; override;
    Function AddPublicKey(Const Name : AnsiString; ECDSA_Public : TECDSA_Public) : Integer; override;
    Procedure Delete(index : Integer); override;
    Procedure Clear; override;
    Property AccountsKeyList : TOrderedAccountKeysList read FOrderedAccountKeysList;
    Property SafeBox : TPCSafeBox read GetSafeBox write SetSafeBox;
  End;

  TRestoreWalletResult = record
    TotalKeysFound : Integer;
    ImportedPrivateKeys : Integer;
    ImportedPublicKeys : Integer;
    Duplicates : Integer;
    Success : boolean;
  end;

  TWallet = class
    private
      FKeys : TWalletKeysExt; static;
      class function GetKeys : TWalletKeysExt; static;
      class function GetMiningKey : TAccountKey; static;
      class procedure CheckLoaded;
      class procedure CheckUnlocked;
    public
      class property Keys : TWalletKeysExt read GetKeys;
      class property MiningKey : TAccountKey read GetMiningKey;
      class procedure Load;
      class function HasKey(const AKey: TWalletKey) : boolean;
      class procedure DeleteKey(const AKey: TWalletKey);
      class procedure GenerateNewKey(const AName: string; AEncryptionTypeNID : Word);
      class function ExportPublicKey(const AKey: TWalletKey) : string;
      class function ExportPrivateKey(const AKey: TWalletKey; const APassword: string) : string;
      class procedure ImportPrivateKey(const AName, AKeyImportText, APassword: string);
      class procedure ImportPublicKey(Const AName, AKeyImportText : string);
      class function RestoreWallet(const AFileName, APassword: string) : TRestoreWalletResult;
      class procedure BackupWallet(const AFileName: string);
      class function TryDecryptPrivateKey(const AEncryptedKeyText, APassword:string; out APrivateKey : TECPrivateKey; out AMessage : string) : boolean;
      class function TryParseEncryptedKey(const AKeyText, AKeyPassword : string; out AKey : TECPrivateKey) : boolean;
      class function TryParseRawKey(const ARawBytes : TRawBytes; AEncryptionTypeNID : Word; out AKey : TECPrivateKey) : boolean;
      class function TryParseHexKey(const AHexString : string; AEncryptionTypeNID : Word; out AKey : TECPrivateKey) : boolean;
  end;

Const CT_TWalletKey_NUL  : TWalletKey = (Name:'';AccountKey:(EC_OpenSSL_NID:0;x:'';y:'');CryptedKey:'';PrivateKey:Nil;SearchableAccountKey:'');

implementation

uses
  SysUtils, UConst, ULog, UAES, UFolderHelper;

Const
  CT_PrivateKeyFile_Magic = 'TWalletKeys';
  CT_PrivateKeyFile_Version = 100;

{ TWalletKey }

function TWalletKey.HasPrivateKey : boolean;
begin
  Result := Length(Self.CryptedKey) > 0;
end;

{ TWalletKeys }

Type PWalletKey = ^TWalletKey;

function TWalletKeys.GetHasPassword : boolean;
begin
  Result := NOT ((IsValidPassword = True) AND (WalletPassword = ''));
end;

function TWalletKeys.AddPrivateKey(Const Name : AnsiString; ECPrivateKey: TECPrivateKey): Integer;
Var P : PWalletKey;
  s : AnsiString;
begin
  if Not Find(ECPrivateKey.PublicKey,Result) then begin
    // Result is new position
    New(P);
    P^ := CT_TWalletKey_NUL;
    P^.Name := Name;
    P^.AccountKey := ECPrivateKey.PublicKey;
    P^.CryptedKey := TAESComp.EVP_Encrypt_AES256(TCrypto.PrivateKey2Hexa(ECPrivateKey.PrivateKey),WalletPassword);
    P^.PrivateKey := TECPrivateKey.Create;
    P^.PrivateKey.SetPrivateKeyFromHexa(ECPrivateKey.EC_OpenSSL_NID, TCrypto.PrivateKey2Hexa(ECPrivateKey.PrivateKey));
    P^.SearchableAccountKey := TAccountComp.AccountKey2RawString(ECPrivateKey.PublicKey);
    FSearchableKeys.Insert(Result,P);
  end else begin
    P := FSearchableKeys[Result];
    P^.Name := Name;
    if NOT P^.HasPrivateKey then begin
      // overriding watch-only public key with full private/public key data, so double-check private key matches
      if P^.AccountKey <> ECPrivateKey.PublicKey then
        raise Exception.Create('[UWallet.pas] TWalletKeys.AddPrivateKey - consistency check failed when overriding watch-only key');
      P^.CryptedKey := TAESComp.EVP_Encrypt_AES256(TCrypto.PrivateKey2Hexa(ECPrivateKey.PrivateKey), WalletPassword);
      P^.PrivateKey := TECPrivateKey.Create;
      P^.PrivateKey.SetPrivateKeyFromHexa(ECPrivateKey.EC_OpenSSL_NID, TCrypto.PrivateKey2Hexa(ECPrivateKey.PrivateKey));
    end;
  end;
  if Not FIsReadingStream then SaveToStream(FWalletFileStream);
  FOnChanged.Invoke(Self);
end;

function TWalletKeys.AddPublicKey(const Name: AnsiString; ECDSA_Public: TECDSA_Public): Integer;
Var P : PWalletKey;
begin
  if Not Find(ECDSA_Public,Result) then begin
    // Result is new position
    New(P);
    P^ := CT_TWalletKey_NUL;
    P^.Name := Name;
    P^.AccountKey := ECDSA_Public;
    P^.CryptedKey := '';
    P^.PrivateKey := Nil;
    P^.SearchableAccountKey := TAccountComp.AccountKey2RawString(ECDSA_Public);
    FSearchableKeys.Insert(Result,P);
  end else begin
    P := FSearchableKeys[Result];
    P^.Name := Name;
  end;
  if Not FIsReadingStream then SaveToStream(FWalletFileStream);
  FOnChanged.Invoke(Self);
end;

procedure TWalletKeys.Clear;
Var P : PWalletKey;
  i : Integer;
begin
  for i := FSearchableKeys.Count-1 downto 0 do begin
    P := FSearchableKeys[i];
    FreeAndNil(P^.PrivateKey);
    Dispose(P);
  end;
  FSearchableKeys.Clear;
  FIsValidPassword := true;
end;

function TWalletKeys.Count: Integer;
begin
  Result := FSearchableKeys.Count;
end;

constructor TWalletKeys.Create(AOwner : TComponent);
begin
  inherited;
  FIsValidPassword := false;
  FWalletFileStream := Nil;
  FWalletPassword := '';
  FSearchableKeys := TList.Create;
  FIsReadingStream := false;
  FOnChanged := Nil;
end;

procedure TWalletKeys.Delete(index: Integer);
Var P : PWalletKey;
begin
  P := FSearchableKeys[index];
  FreeAndNil(P^.PrivateKey);
  Dispose(P);
  FSearchableKeys.Delete(index);
  SaveToStream(FWalletFileStream);
  FOnChanged.Invoke(Self);
end;

destructor TWalletKeys.destroy;
begin
  FOnChanged := Nil;
  FreeAndNil(FWalletFileStream);
  Clear;
  FreeAndNil(FSearchableKeys);
  inherited;
end;

function TWalletKeys.Find(const AccountKey: TAccountKey; var Index: Integer): Boolean;
var L, H, I, C: Integer;
  rak : TRawBytes;
begin
  Result := False;
  rak := TAccountComp.AccountKey2RawString(AccountKey);
  L := 0;
  H := FSearchableKeys.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr( PWalletKey(FSearchableKeys[I]).SearchableAccountKey, rak );
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TWalletKeys.GeneratePrivateKeysFromPassword;
Var i : Integer;
 P : PWalletKey;
 s : TRawBytes;
 isOk : Boolean;
begin
  FIsValidPassword := false;
  isOk := true;
  for i := 0 to FSearchableKeys.Count - 1 do begin
    P := FSearchableKeys[i];
    FreeAndNil(P^.PrivateKey);
  end;
  // try to unencrypt
  for i := 0 to FSearchableKeys.Count - 1 do begin
    P := FSearchableKeys[i];
    if P^.CryptedKey<>'' then begin
      isOk := TAESComp.EVP_Decrypt_AES256( P^.CryptedKey, FWalletPassword, s );
      If isOk then begin
        P^.PrivateKey := TECPrivateKey.Create;
        try
          P^.PrivateKey.SetPrivateKeyFromHexa(P^.AccountKey.EC_OpenSSL_NID,s);
        except on E: Exception do begin
            P^.PrivateKey.Free;
            P^.PrivateKey := Nil;
            isOk := false;
            TLog.NewLog(lterror,ClassName,Format('Fatal error when generating EC private key %d/%d: %s',[i+1,FSearchableKeys.Count,E.Message]));
            //disabled exit... continue: exit;
          end;
        end;
      end;
    end;
  end;
  FIsValidPassword := isOk;
end;

function TWalletKeys.GetKey(index: Integer): TWalletKey;
begin
  Result := PWalletKey(FSearchableKeys[index])^;
end;

function TWalletKeys.IndexOfAccountKey(AccountKey: TAccountKey): Integer;
begin
  if Not find(AccountKey,Result) then Result := -1;
end;

procedure TWalletKeys.LoadFromStream(Stream: TStream);
Var fileversion,i,l,j : Integer;
  s : AnsiString;
  P : PWalletKey;
  wk : TWalletKey;
begin
  Clear;
  FIsValidPassword := false;
  FIsReadingStream := true;
  try
    if Stream.Size - Stream.Position > 0 then begin
      TStreamOp.ReadAnsiString(Stream,s);
      if Not AnsiSameStr(s,CT_PrivateKeyFile_Magic) then raise Exception.Create('Invalid '+Classname+' stream');
      // Read version:
      Stream.Read(fileversion,4);
      if (fileversion<>CT_PrivateKeyFile_Version) then begin
        // Old version
        Stream.Position := Stream.Position-4;
        TLog.NewLog(lterror,ClassName,'Invalid PrivateKeys file version: '+Inttostr(fileversion));
      end;
      Stream.Read(l,4);
      for i := 0 to l - 1 do begin
        wk := CT_TWalletKey_NUL;
        TStreamOp.ReadAnsiString(Stream,wk.Name);
        Stream.Read(wk.AccountKey.EC_OpenSSL_NID,sizeof(wk.AccountKey.EC_OpenSSL_NID));
        TStreamOp.ReadAnsiString(Stream,wk.AccountKey.x);
        TStreamOp.ReadAnsiString(Stream,wk.AccountKey.y);
        TStreamOp.ReadAnsiString(Stream,wk.CryptedKey);
        wk.PrivateKey := Nil;
        j :=AddPublicKey(wk.Name,wk.AccountKey);
        P := PWalletKey(FSearchableKeys[j]);
        P^.CryptedKey := wk.CryptedKey; // Adding encrypted data
      end;
    end;
    GeneratePrivateKeysFromPassword;
  finally
    FIsReadingStream := false;
  end;
  FOnChanged.Invoke(Self);
end;

function TWalletKeys.LockWallet: Boolean;
begin
  // Return true when wallet has a password, locking it. False if there password is empty string
  FWalletPassword := '';
  GeneratePrivateKeysFromPassword;
  Result := Not IsValidPassword;
  FOnChanged.Invoke(Self);
end;

procedure TWalletKeys.SaveToStream(Stream: TStream);
var i : Integer;
  P : PWalletKey;
begin
  if FIsReadingStream then exit;
  if Not Assigned(Stream) then exit;
  Stream.Size := 0;
  Stream.Position:=0;
  TStreamOp.WriteAnsiString(Stream,CT_PrivateKeyFile_Magic);
  i := CT_PrivateKeyFile_Version;
  Stream.Write(i,4);
  i := FSearchableKeys.Count;
  Stream.Write(i,4);
  for i := 0 to FSearchableKeys.Count - 1 do begin
    P := FSearchableKeys[i];
    TStreamOp.WriteAnsiString(Stream,P^.Name);
    Stream.Write(P^.AccountKey.EC_OpenSSL_NID,sizeof(P^.AccountKey.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream,P^.AccountKey.x);
    TStreamOp.WriteAnsiString(Stream,P^.AccountKey.y);
    TStreamOp.WriteAnsiString(Stream,P^.CryptedKey);
  end;
end;

procedure TWalletKeys.SetName(index: Integer; const newName: AnsiString);
begin
  if PWalletKey(FSearchableKeys[index])^.Name=newName then exit;
  PWalletKey(FSearchableKeys[index])^.Name := newName;
  SaveToStream(FWalletFileStream);
  FOnChanged.Invoke(Self);
end;

procedure TWalletKeys.SetWalletFileName(const Value: AnsiString);
var fm : Word;
begin
  if FWalletFileName = Value then exit;
  FWalletFileName := Value;
  if Assigned(FWalletFileStream) then FWalletFileStream.Free;
  FWalletFileStream := Nil;
  if Value<>'' then begin
    if FileExists(Value) then fm := fmOpenReadWrite
    else fm := fmCreate;
    FWalletFileStream := TFileStream.Create(WalletfileName,fm+fmShareDenyWrite);
    FWalletFileStream.Position:=0;
    LoadFromStream(FWalletFileStream);
  end;
end;

procedure TWalletKeys.SetWalletPassword(const Value: AnsiString);
Var i : Integer;
  P : PWalletKey;
begin
  if FWalletPassword=Value then exit;
  FWalletPassword := Value;
  for i := 0 to FSearchableKeys.Count - 1 do begin
    P := FSearchableKeys[i];
    If Assigned(P^.PrivateKey) then begin
      P^.CryptedKey := TAESComp.EVP_Encrypt_AES256(TCrypto.PrivateKey2Hexa(P^.PrivateKey.PrivateKey),FWalletPassword);
    end else begin
      if FIsValidPassword then begin
        TLog.NewLog(lterror,Classname,Format('Fatal error: Private key not found %d/%d',[i+1,FSearchableKeys.Count]));
      end;
      FIsValidPassword := false;
    end;
  end;
  // Try if password is Ok
  GeneratePrivateKeysFromPassword;
  if FIsValidPassword then SaveToStream(FWalletFileStream);
  FOnChanged.Invoke(Self);
end;

{ TWalletKeysExt }

function TWalletKeysExt.AddPrivateKey(const Name: AnsiString;
  ECPrivateKey: TECPrivateKey): Integer;
begin
  Result := inherited AddPrivateKey(Name,ECPrivateKey);
  if Assigned(FOrderedAccountKeysList) then begin
    FOrderedAccountKeysList.AddAccountKey(ECPrivateKey.PublicKey);
  end;
end;

function TWalletKeysExt.AddPublicKey(const Name: AnsiString;
  ECDSA_Public: TECDSA_Public): Integer;
begin
  Result := inherited AddPublicKey(Name,ECDSA_Public);
  if Assigned(FOrderedAccountKeysList) then begin
    FOrderedAccountKeysList.AddAccountKey(ECDSA_Public);
  end;
end;

procedure TWalletKeysExt.Clear;
begin
  inherited;
  if Assigned(FOrderedAccountKeysList) then begin
    FOrderedAccountKeysList.Clear;
  end;
end;

constructor TWalletKeysExt.Create(AOwner: TComponent);
begin
  inherited;
  FOrderedAccountKeysList := Nil;
end;

procedure TWalletKeysExt.Delete(index: Integer);
begin
  if Assigned(FOrderedAccountKeysList) then begin
    FOrderedAccountKeysList.RemoveAccountKey( Key[index].AccountKey );
  end;
  inherited;
end;

destructor TWalletKeysExt.destroy;
begin
  FreeAndnil(FOrderedAccountKeysList);
  inherited;
end;

function TWalletKeysExt.GetSafeBox: TPCSafeBox;
begin
  Result := Nil;
  if Assigned(FOrderedAccountKeysList) then begin
    Result := FOrderedAccountKeysList.SafeBox;
  end;
end;

procedure TWalletKeysExt.SetSafeBox(const Value: TPCSafeBox);
Var i : Integer;
begin
  if Assigned(FOrderedAccountKeysList) then begin
    if FOrderedAccountKeysList.SafeBox<>Value then FreeAndNil(FOrderedAccountKeysList)
    else exit;
  end;
  if Assigned(Value) then begin
    // Initialize
    FOrderedAccountKeysList := TOrderedAccountKeysList.Create(Value,false);
    for i := 0 to Count - 1 do begin
      FOrderedAccountKeysList.AddAccountKey(Key[i].AccountKey);
    end;
  end;
end;

{ TWallet }

class function TWallet.GetMiningKey: TAccountKey;
Var PK : TECPrivateKey;
  i : Integer;
  PublicK : TECDSA_Public;
begin
  CheckLoaded;
  Result := CT_TECDSA_Public_Nul;
  case TSettings.MinerPrivateKeyType of
    mpk_NewEachTime: PublicK := CT_TECDSA_Public_Nul;
    mpk_Selected: PublicK := TAccountComp.RawString2Accountkey(TSettings.MinerSelectedPrivateKey);
    mpk_Random: begin
      PublicK := CT_TECDSA_Public_Nul;
      if FKeys.Count>0 then PublicK := FKeys.Key[Random(FKeys.Count)].AccountKey;
    end;
  end;
  i := FKeys.IndexOfAccountKey(PublicK);
  if i>=0 then begin
    if (FKeys.Key[i].CryptedKey='') then i:=-1;
  end else begin
    // Generate a new key
    PK := TECPrivateKey.Create;
    try
      PK.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
      FKeys.AddPrivateKey('User Key '+FormatDateTime('YYYY-MM-DD hh:nn' ,Now), PK);
      PublicK := PK.PublicKey;
    finally
      PK.Free;
    end;
  end;
  Result := PublicK;
end;

class function TWallet.GetKeys : TWalletKeysExt; inline;
begin
  CheckLoaded;
  Result := FKeys;
end;

class procedure TWallet.Load;
begin
  try
    if not Assigned(FKeys) then
      FKeys := TWalletKeysExt.Create(nil);
    FKeys.WalletFileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'WalletKeys.dat';
  except
    on E:Exception do begin
      E.Message := 'Cannot open your wallet... Perhaps another instance of Pascal Coin is active!'+#10+#10+E.Message;
      Raise;
    end;
  end;
end;

class function TWallet.HasKey(const AKey: TWalletKey) : boolean;
begin
  Result := FKeys.IndexOfAccountKey(AKey.AccountKey) >= 0;
end;

class procedure TWallet.DeleteKey(const AKey: TWalletKey);
var
  i : Integer;
begin
  CheckLoaded;
  i := FKeys.IndexOfAccountKey(AKey.AccountKey);
  if i >= 0 then
    FKeys.Delete(i)
  else
    raise Exception.Create('Key not found');
end;

class procedure TWallet.GenerateNewKey(const AName: string; AEncryptionTypeNID : Word);
var
  privateKey : TECPrivateKey;
begin
  privateKey := TECPrivateKey.Create;
  try
    privateKey.GenerateRandomPrivateKey(AEncryptionTypeNID);
    FKeys.AddPrivateKey(AName, privateKey);
  finally
    privateKey.Free;
  end;
end;

class function TWallet.ExportPublicKey(const AKey: TWalletKey) : string;
begin
  Result := TAccountComp.AccountPublicKeyExport(AKey.AccountKey);
end;

class function TWallet.ExportPrivateKey(const AKey: TWalletKey; const APassword: string) : string;
begin
  Result := TCrypto.ToHexaString(TAESComp.EVP_Encrypt_AES256(AKey.PrivateKey.ExportToRaw, APassword));
end;

class procedure TWallet.ImportPrivateKey(const AName, AKeyImportText, APassword: string);
var
 message : String;
 EC : TECPrivateKey;
 i : Integer;
begin
  CheckLoaded;
  CheckUnlocked;
  try
    if NOT TryDecryptPrivateKey(AKeyImportText, APassword, EC, message) then
      raise Exception.Create(message);
    i := FKeys.IndexOfAccountKey(EC.PublicKey);
    if  i>=0  then
      if FKeys.Key[i].HasPrivateKey then
        raise Exception.Create('This key is already in your wallet!');
    i := FKeys.AddPrivateKey(AName,EC);
  finally
    if Assigned(EC) then
      EC.Free;
  end;
end;

class procedure TWallet.ImportPublicKey(Const AName, AKeyImportText : string);
var
  raw, errors : AnsiString;
  accountKey : TAccountKey;
begin
  CheckLoaded;
  CheckUnlocked;
  If not TAccountComp.AccountPublicKeyImport(AKeyImportText, accountKey, errors) then begin
    raw := TCrypto.HexaToRaw(AKeyImportText);
    if trim(raw)='' then
      raise Exception.Create('Invalid public key value (Not hexa or not an imported format)'+#10+errors);
    accountKey := TAccountComp.RawString2Accountkey(raw);
  end;
  If not TAccountComp.IsValidAccountKey(accountKey,errors) then
    raise Exception.Create('This data is not a valid public key'+#10+errors);
  if FKeys.IndexOfAccountKey(accountKey)>=0 then
    raise exception.Create('This key exists on your wallet');
  FKeys.AddPublicKey(AName, accountKey);
end;

class function TWallet.RestoreWallet(const AFileName, APassword: string) : TRestoreWalletResult;
var
  wki : TWalletKeys;
  i, j : Integer;
begin
  CheckLoaded;
  if NOT FileExists(AFileName) then
    raise Exception.Create('File not found: ' + AFilename);

  wki := TWalletKeys.Create(nil);
  try
    wki.WalletFileName := AFileName;
    if wki.Count<=0 then
      raise Exception.Create('Wallet file has no valid data');
    Result.ImportedPrivateKeys := 0;
    Result.ImportedPublicKeys := 0;
    Result.Success := false;

    // If password required, set it
    if NOT wki.IsValidPassword then begin
      wki.WalletPassword := APassword;
      if NOT wki.IsValidPassword then
        exit;
    end;

    // Import the keys
    Result.TotalKeysFound:= wki.Count;
    for i := 0 to wki.Count - 1 do begin
      if NOT HasKey(wki.Key[i]) then begin
        if wki.Key[i].HasPrivateKey then begin
          TWallet.Keys.AddPrivateKey(wki.Key[i].Name, wki.Key[i].PrivateKey);
          inc(Result.ImportedPrivateKeys);
        end else begin
          TWallet.Keys.AddPublicKey(wki.Key[i].Name, wki.Key[i].AccountKey);
          inc(Result.ImportedPublicKeys);
        end;
      end else begin
        j := FKeys.IndexOfAccountKey(wki[i].AccountKey);
        // case: existing wallet has public key but import has private key
        if (NOT FKeys[j].HasPrivateKey) AND (wki.Key[i].HasPrivateKey) then begin
          TWallet.Keys.AddPrivateKey(wki.Key[i].Name, wki.Key[i].PrivateKey);
          inc(Result.ImportedPrivateKeys);
        end else Inc(Result.Duplicates);
      end;
    end;
    result.Success := true;
  finally
    wki.Free;
  end;
end;

class procedure TWallet.BackupWallet(const AFileName : string);
var
  fs : TFileStream;
begin
  CheckLoaded;
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    fs.Size := 0;
    TWallet.Keys.SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

class function TWallet.TryDecryptPrivateKey(const AEncryptedKeyText, APassword:string; out APrivateKey : TECPrivateKey; out AMessage : string) : boolean;
var
 parseResult : Boolean;
begin
  APrivateKey := nil;
  AMessage := '';
  if NOT TCrypto.IsHexString(AEncryptedKeyText) then begin
    Result := false;
    AMessage := 'Invalid key text. You must enter a hexadecimal value ("0".."9" or "A".."F").';
    exit;
  end;
  case Length(AEncryptedKeyText) div 2 of
    32: parseResult := TryParseRawKey(AEncryptedKeyText, CT_NID_secp256k1, APrivateKey);
    35,36: parseResult := TryParseRawKey(AEncryptedKeyText, CT_NID_sect283k1, APrivateKey);
    48: parseResult := TryParseRawKey(AEncryptedKeyText, CT_NID_secp384r1, APrivateKey);
    65,66: parseResult := TryParseRawKey(AEncryptedKeyText, CT_NID_secp521r1, APrivateKey);
    64, 80, 96: parseResult := TryParseEncryptedKey(AEncryptedKeyText, APassword, APrivateKey);
    else begin
      result := false;
      AMessage := 'Invalidly formatted private key string. Ensure it is an encrypted private key export or raw private key hexstring.';
      exit;
    end;
  end;
  if NOT parseResult then begin
    Result := false;
    if Length(AEncryptedKeyText) div 2 in [64, 80, 96] then
      AMessage := 'Incorrect password'
    else
      AMessage := 'Unencrypted key data is invalid or corrupted';
    if Assigned(APrivateKey) then FreeAndNil(APrivateKey);
    exit;
  end;
  if Not Assigned(APrivateKey) then begin
    Result := false;
    AMessage := '[UWallet.pas] TWallet.ImportPrivateKey - expected non-null private key';
    exit;
  end;
  Result := true;
end;

class function TWallet.TryParseEncryptedKey(const AKeyText, AKeyPassword : string; out AKey : TECPrivateKey) : boolean;
var
 decrypt : string;
begin
  AKey := nil;
  if NOT TCrypto.IsHexString(AKeyText) then begin
    Result := false;
    exit;
  end;
  decrypt := '';
  If (TAESComp.EVP_Decrypt_AES256(TCrypto.HexaToRaw(AKeyText),AKeyPassword,decrypt)) AND (decrypt<>'') then begin
    AKey := TECPrivateKey.ImportFromRaw(decrypt);
    Result := true;
  end else begin
    Result := false;
  end;
end;

class function TWallet.TryParseRawKey(const ARawBytes : TRawBytes; AEncryptionTypeNID : Word; out AKey : TECPrivateKey) : boolean;
begin
  Result := TryParseHexKey(TCrypto.ToHexaString(ARawBytes), AEncryptionTypeNID, AKey);
end;

class function TWallet.TryParseHexKey(const AHexString : string; AEncryptionTypeNID : Word; out AKey : TECPrivateKey) : boolean;
begin
  AKey := TECPrivateKey.Create;
  Try
    AKey.SetPrivateKeyFromHexa(AEncryptionTypeNID, AHexString);
    Result := True;
  Except
    On E:Exception do begin
      FreeAndNil(AKey);
      Result := false;
    end;
  end;
end;

class procedure TWallet.CheckLoaded;
begin
  if not Assigned(FKeys) then
    raise Exception.Create('Wallet has not been loaded');
end;

class procedure TWallet.CheckUnlocked;
begin
  if NOT FKeys.IsValidPassword then
    raise Exception.Create('Wallet is locked.');
end;

initialization
  TWallet.FKeys := nil;

finalization
  if Assigned(TWallet.FKeys) then
    FreeAndNil(TWallet.FKeys);

end.
