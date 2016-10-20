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
  Classes, UBlockChain, UAccounts, UCrypto;

Type
  TWalletKey = Record
    Name : AnsiString;
    AccountKey : TAccountKey;
    CryptedKey : TRawBytes;
    PrivateKey : TECPrivateKey;
  End;

  TWalletKeys = Class(TComponent)
  private
    FKeys : TList;
    FFileName: AnsiString;
    FWalletPassword: AnsiString;
    FWalletFileStream : TFileStream;
    FIsValidPassword: Boolean;
    FWalletFileName: AnsiString;
    FIsReadingStream : Boolean;
    FOnChanged: TNotifyEvent;
    function GetKey(index: Integer): TWalletKey;
    procedure SetWalletPassword(const Value: AnsiString);
    Procedure GeneratePrivateKeysFromPassword;
    procedure SetWalletFileName(const Value: AnsiString);
  public
    Property Key[index : Integer] : TWalletKey read GetKey; default;
    Constructor Create(AOwner : TComponent); override;
    Destructor destroy; override;
    Procedure LoadFromStream(Stream : TStream);
    Procedure SaveToStream(Stream : TStream);
    Property IsValidPassword : Boolean read FIsValidPassword;
    Property WalletPassword : AnsiString read FWalletPassword write SetWalletPassword;
    Function AddPrivateKey(Const Name : AnsiString; ECPrivateKey : TECPrivateKey) : Integer;
    Function AddPublicKey(Const Name : AnsiString; ECDSA_Public : TECDSA_Public) : Integer;
    Function IndexOfAccountKey(AccountKey : TAccountKey) : Integer;
    Procedure Delete(index : Integer);
    Procedure Clear;
    Function Count : Integer;
    Property WalletFileName : AnsiString read FWalletFileName write SetWalletFileName;
    Property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
    Procedure SetName(index : Integer; Const newName : AnsiString);
  End;

Const CT_TWalletKey_NUL  : TWalletKey = (Name:'';AccountKey:(EC_OpenSSL_NID:0;x:'';y:'');CryptedKey:'';PrivateKey:Nil);

implementation

uses
  SysUtils, UConst, ULog, UAES;

Const
  CT_PrivateKeyFile_Magic = 'TWalletKeys';
  CT_PrivateKeyFile_Version = 100;

{ TWalletKeys }

Type PWalletKey = ^TWalletKey;

function TWalletKeys.AddPrivateKey(Const Name : AnsiString; ECPrivateKey: TECPrivateKey): Integer;
Var P : PWalletKey;
  s : AnsiString;
begin
  Result := IndexOfAccountKey(ECPrivateKey.PublicKey);
  if Result<0 then begin
    New(P);
    P^ := CT_TWalletKey_NUL;
    P^.Name := Name;
    P^.AccountKey := ECPrivateKey.PublicKey;
    P^.CryptedKey := TAESComp.EVP_Encrypt_AES256(TCrypto.PrivateKey2Hexa(ECPrivateKey.PrivateKey),WalletPassword);
    P^.PrivateKey := TECPrivateKey.Create;
    P^.PrivateKey.SetPrivateKeyFromHexa(ECPrivateKey.EC_OpenSSL_NID, TCrypto.PrivateKey2Hexa(ECPrivateKey.PrivateKey));
    Result := FKeys.Add(P);
  end else begin
    P := Fkeys[Result];
    P^.Name := Name;
  end;
  if Not FIsReadingStream then SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then  FOnChanged(Self);
end;

function TWalletKeys.AddPublicKey(const Name: AnsiString; ECDSA_Public: TECDSA_Public): Integer;
Var P : PWalletKey;
begin
  Result := IndexOfAccountKey(ECDSA_Public);
  if Result<0 then begin
    New(P);
    P^ := CT_TWalletKey_NUL;
    P^.Name := Name;
    P^.AccountKey := ECDSA_Public;
    P^.PrivateKey := Nil;
    Result := FKeys.Add(P);
  end else begin
    P := Fkeys[Result];
    P^.Name := Name;
  end;
  if Not FIsReadingStream then SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then  FOnChanged(Self);
end;

procedure TWalletKeys.Clear;
Var P : PWalletKey;
  i : Integer;
begin
  for i := FKeys.Count - 1 downto 0 do begin
    P := FKeys[i];
    P^.PrivateKey.Free;
    Dispose(P);
  end;
  FKeys.Clear;
  FIsValidPassword := true;
end;

function TWalletKeys.Count: Integer;
begin
  Result := FKeys.Count;
end;

constructor TWalletKeys.Create(AOwner : TComponent);
begin
  inherited;
  FIsValidPassword := false;
  FWalletFileStream := Nil;
  FWalletPassword := '';
  FKeys := TList.Create;
  FIsReadingStream := false;
  FOnChanged := Nil;
end;

procedure TWalletKeys.Delete(index: Integer);
Var P : PWalletKey;
begin
  P := FKeys[index];
  P^.PrivateKey.Free;
  Dispose(P);
  FKeys.Delete(index);
  SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

destructor TWalletKeys.destroy;
begin
  FOnChanged := Nil;
  FreeAndNil(FWalletFileStream);
  Clear;
  FKeys.Free;
  inherited;
end;

procedure TWalletKeys.GeneratePrivateKeysFromPassword;
Var i : Integer;
 P : PWalletKey;
 s : TRawBytes;
 isOk : Boolean;
begin
  FIsValidPassword := false;
  isOk := true;
  for i := 0 to FKeys.Count - 1 do begin
    P := FKeys[i];
    FreeAndNil(P^.PrivateKey);
  end;
  // try to unencrypt
  for i := 0 to FKeys.Count - 1 do begin
    P := FKeys[i];
    if P^.CryptedKey<>'' then begin
      isOk := TAESComp.EVP_Decrypt_AES256( P^.CryptedKey, FWalletPassword, s );
      If isOk then begin
        P^.PrivateKey := TECPrivateKey.Create;
        try
          P^.PrivateKey.SetPrivateKeyFromHexa(P^.AccountKey.EC_OpenSSL_NID,s);
        except on E: Exception do begin
            P^.PrivateKey.Free;
            P^.PrivateKey := Nil;
            TLog.NewLog(lterror,ClassName,Format('Fatal error when generating EC private key %d/%d: %s',[i+1,FKeys.Count,E.Message]));
            exit;
          end;
        end;
      end;
    end;
  end;
  FIsValidPassword := isOk;
end;

function TWalletKeys.GetKey(index: Integer): TWalletKey;
begin
  Result := PWalletKey(FKeys[index])^;
end;

function TWalletKeys.IndexOfAccountKey(AccountKey: TAccountKey): Integer;
begin
  for result := 0 to FKeys.Count - 1 do begin
    if TAccountComp.equal( PWalletKey(FKeys[result])^.AccountKey, AccountKey) then exit;
  end;
  Result := -1;
end;

procedure TWalletKeys.LoadFromStream(Stream: TStream);
Var fileversion,i,l : Integer;
  s : AnsiString;
  P : PWalletKey;
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
        New(P);
        P^ := CT_TWalletKey_NUL;
        TStreamOp.ReadAnsiString(Stream,P^.Name);
        Stream.Read(P^.AccountKey.EC_OpenSSL_NID,sizeof(P^.AccountKey.EC_OpenSSL_NID));
        TStreamOp.ReadAnsiString(Stream,P^.AccountKey.x);
        TStreamOp.ReadAnsiString(Stream,P^.AccountKey.y);
        TStreamOp.ReadAnsiString(Stream,P^.CryptedKey);
        P^.PrivateKey := Nil;
        FKeys.Add(P);
      end;
    end;
    GeneratePrivateKeysFromPassword;
  finally
    FIsReadingStream := false;
  end;
  if Assigned(FOnChanged) then FOnChanged(Self);
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
  i := FKeys.Count;
  Stream.Write(i,4);
  for i := 0 to FKeys.Count - 1 do begin
    P := FKeys[i];
    TStreamOp.WriteAnsiString(Stream,P^.Name);
    Stream.Write(P^.AccountKey.EC_OpenSSL_NID,sizeof(P^.AccountKey.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream,P^.AccountKey.x);
    TStreamOp.WriteAnsiString(Stream,P^.AccountKey.y);
    TStreamOp.WriteAnsiString(Stream,P^.CryptedKey);
  end;
end;

procedure TWalletKeys.SetName(index: Integer; const newName: AnsiString);
begin
  if PWalletKey(FKeys[index])^.Name=newName then exit;
  PWalletKey(FKeys[index])^.Name := newName;
  SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then  FOnChanged(Self);
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
  for i := 0 to FKeys.Count - 1 do begin
    P := FKeys[i];
    If Assigned(P^.PrivateKey) then begin
      P^.CryptedKey := TAESComp.EVP_Encrypt_AES256(TCrypto.PrivateKey2Hexa(P^.PrivateKey.PrivateKey),FWalletPassword);
    end else begin
      if FIsValidPassword then begin
        TLog.NewLog(lterror,Classname,Format('Fatal error: Private key not found %d/%d',[i+1,FKeys.Count]));
      end;
      FIsValidPassword := false;
    end;
  end;
  // Try if password is Ok
  GeneratePrivateKeysFromPassword;
  if FIsValidPassword then SaveToStream(FWalletFileStream);
end;

end.
