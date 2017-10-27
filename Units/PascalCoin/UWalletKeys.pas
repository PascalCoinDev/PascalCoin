unit UWalletKeys;

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
  Classes, UBlockChain, UAccounts, UCrypto, UCommon;

Type
  TWalletKey = Record
    Name : AnsiString;
    AccountKey : TAccountKey;
    CryptedKey : TRawBytes;
    PrivateKey : TECPrivateKey;
    SearchableAccountKey : TRawBytes;
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
    //
    Property AccountsKeyList : TOrderedAccountKeysList read FOrderedAccountKeysList;
    Property SafeBox : TPCSafeBox read GetSafeBox write SetSafeBox;
  End;


Const CT_TWalletKey_NUL  : TWalletKey = (Name:'';AccountKey:(EC_OpenSSL_NID:0;x:'';y:'');CryptedKey:'';PrivateKey:Nil;SearchableAccountKey:'');

implementation

uses
  SysUtils, UConst, ULog, UAES;

Const
  CT_PrivateKeyFile_Magic = 'TWalletKeys';
  CT_PrivateKeyFile_Version = 100;

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
  FWalletPassword := ''; //AntonB - this clear Password on lock - is block lock
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

end.
