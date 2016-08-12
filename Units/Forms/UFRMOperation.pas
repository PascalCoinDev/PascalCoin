unit UFRMOperation;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UNode, UWalletKeys, UCrypto, Buttons, UBlockChain,
  UAccounts, ActnList, ComCtrls;

type
  TFRMOperation = class(TForm)
    Label1: TLabel;
    bbExecute: TBitBtn;
    bbCancel: TBitBtn;
    lblAccountBalance: TLabel;
    Label2: TLabel;
    ebSenderAccount: TEdit;
    PageControl: TPageControl;
    tsOperation: TTabSheet;
    gbOperation: TGroupBox;
    lblDestAccount: TLabel;
    lblAmount: TLabel;
    lblNewPrivateKey: TLabel;
    lblTransactionErrors: TLabel;
    lblChangeKeyErrors: TLabel;
    lblNewOwnerPublicKey: TLabel;
    lblNewOwnerErrors: TLabel;
    lblFee: TLabel;
    rbTransaction: TRadioButton;
    rbChangeKey: TRadioButton;
    ebDestAccount: TEdit;
    ebAmount: TEdit;
    cbNewPrivateKey: TComboBox;
    rbTransferToANewOwner: TRadioButton;
    ebNewPublicKey: TEdit;
    ebFee: TEdit;
    bbKeys: TBitBtn;
    gbPayload: TGroupBox;
    lblEncryptPassword: TLabel;
    Label4: TLabel;
    lblEncryptionErrors: TLabel;
    lblPayloadLength: TLabel;
    rbEncryptedWithEC: TRadioButton;
    rbEncrptedWithPassword: TRadioButton;
    rbNotEncrypted: TRadioButton;
    ebEncryptPassword: TEdit;
    memoPayload: TMemo;
    rbEncryptedWithOldEC: TRadioButton;
    ActionList1: TActionList;
    actExecute: TAction;
    tsGlobalError: TTabSheet;
    lblGlobalErrors: TLabel;
    bbPassword: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure rbTransactionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure memoPayloadClick(Sender: TObject);
    procedure ebDestAccountChange(Sender: TObject);
    procedure cbNewPrivateKeyChange(Sender: TObject);
    procedure ebNewPublicKeyChange(Sender: TObject);
    procedure ebEncryptPasswordChange(Sender: TObject);
    procedure ebFeeExit(Sender: TObject);
    procedure bbKeysClick(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure ebSenderAccountExit(Sender: TObject);
    procedure ebSenderAccountKeyPress(Sender: TObject; var Key: Char);
    procedure bbPasswordClick(Sender: TObject);
    procedure ebDestAccountExit(Sender: TObject);
  private
    FSenderAccount: Cardinal;
    FNode : TNode;
    FWalletKeys: TWalletKeys;
    FPAccount : PAccount;
    FOperation : TPCOperation;
    FFee: Int64;
    FEncodedPayload : TRawBytes;
    FDisabled : Boolean;
    //
    FTxDestAccount : Cardinal;
    FTxAmount : Int64;
    FNewAccountPublicKey : TAccountKey;
    procedure SetSenderAccount(const Value: Cardinal);
    procedure SetWalletKeys(const Value: TWalletKeys);
    { Private declarations }
    Function UpdateOperationOptions(var errors : AnsiString) : Boolean;
    Function UpdatePayload(var errors : AnsiString) : Boolean;
    Function GetSenderAcccount: PAccount;
    procedure SetFee(const Value: Int64);
  public
    { Public declarations }
    Property SenderAccount : Cardinal read FSenderAccount write SetSenderAccount;
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
    Property Fee : Int64 read FFee write SetFee;
  end;

implementation

uses
  UECIES, UConst, UOpTransaction, UFRMNewPrivateKeyType, UAES, UFRMWalletKeys;

{$R *.dfm}

{ TFRMOperation }

procedure TFRMOperation.actExecuteExecute(Sender: TObject);
Var errors : AnsiString;
  P : PAccount;
  i : Integer;
  wk : TWalletKey;
  npk : TECPrivateKey;
  FRM : TFRMNewPrivateKeyType;
begin
  P := GetSenderAcccount;
  if Not Assigned(P) then raise Exception.Create('Invalid sender account');
  if Not Assigned(WalletKeys) then raise Exception.Create('No wallet keys');
  i := WalletKeys.IndexOfAccountKey(P^.accountkey);
  if (i<0) then begin
    Raise Exception.Create('Sender account private key not found in Wallet');
  end;
  wk := WalletKeys.Key[i];
  If Not UpdateOperationOptions(errors) then raise Exception.Create(errors);
  If Not UpdatePayload(errors) then raise Exception.Create(errors);
  //
  FreeAndNil(FOperation);
  if rbTransaction.Checked then begin
    FOperation := TOpTransaction.Create(SenderAccount,P^.n_operation+1,FTxDestAccount,wk.PrivateKey,FTxAmount,fee,FEncodedPayload);
  end else if rbChangeKey.Checked then begin
    i := Integer(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex]);
    if i=-1 then begin
      // New key
      FRM := TFRMNewPrivateKeyType.Create(Self);
      try
        FRM.WalletKeys := WalletKeys;
        if FRM.ShowModal=MrOk then begin
          npk := FRM.GeneratedPrivateKey;
          FNewAccountPublicKey := npk.PublicKey;
        end else begin
          raise Exception.Create('No new key generated');
        end;
      finally
        FRM.Free;
      end;
      // Note: Regenerate Payload because we have created a new Private Key...
      If Not UpdatePayload(errors) then raise Exception.Create(errors);
    end else begin
      FNewAccountPublicKey := WalletKeys.Key[i].AccountKey;
    end;
    FOperation := TOpChangeKey.Create(SenderAccount,P^.n_operation+1,wk.PrivateKey,FNewAccountPublicKey,fee,FEncodedPayload);
  end else if rbTransferToANewOwner.Checked then begin
    FOperation := TOpChangeKey.Create(SenderAccount,P^.n_operation+1,wk.PrivateKey,FNewAccountPublicKey,fee,FEncodedPayload);
  end else begin
    raise Exception.Create('No operation selected');
  end;
  if Application.MessageBox(PChar('Execute this operation:'+#10+#10+FOperation.ToString+#10+#10+'Note: This operation will be transmitted to the network!'),
     PChar(Application.Title),MB_YESNO+MB_ICONINFORMATION+MB_DEFBUTTON2)<>IdYes then exit;

  // Execute operation
  If FNode.AddOperation(nil,FOperation,errors) then begin
    Application.MessageBox(PChar('Executed opreation:'+#10+FOperation.ToString),PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
    ModalResult := MrOk;
  end else begin
    raise Exception.Create(errors);
  end;
end;

procedure TFRMOperation.bbKeysClick(Sender: TObject);
Var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(Self);
  Try
    FRM.WalletKeys := WalletKeys;
    FRM.ShowModal;
    rbChangeKey.Checked := true;
    cbNewPrivateKey.SetFocus;
    SetWalletKeys(WalletKeys);
  Finally
    FRM.Free;
  End;
end;

procedure TFRMOperation.bbPasswordClick(Sender: TObject);
Var s : String;
  errors : AnsiString;
begin
  if FWalletKeys.IsValidPassword then begin
  end else begin
    s := '';
    Repeat
      if Not InputQuery('Wallet Password','Insert Wallet Password',s) then exit;
      FWalletKeys.WalletPassword := s;
    Until FWalletKeys.IsValidPassword;
    UpdateOperationOptions(errors);
  end;
end;

procedure TFRMOperation.cbNewPrivateKeyChange(Sender: TObject);
begin
  If FDisabled then exit;
  rbChangeKey.Checked := true;
  rbTransactionClick(Nil);
end;

procedure TFRMOperation.ebDestAccountChange(Sender: TObject);
begin
  if FDisabled then exit;
  rbTransaction.Checked := true;
  rbTransactionClick(Nil);
end;

procedure TFRMOperation.ebDestAccountExit(Sender: TObject);
Var an : Cardinal;
begin
  If TAccountComp.AccountTxtNumberToAccountNumber(ebDestAccount.Text,an) then begin
    ebDestAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(an);
  end else begin
    ebDestAccount.Text := '';
  end;
end;

procedure TFRMOperation.ebEncryptPasswordChange(Sender: TObject);
begin
  if FDisabled then exit;
  rbEncrptedWithPassword.Checked := true;
  memoPayloadClick(Nil);
end;

procedure TFRMOperation.ebFeeExit(Sender: TObject);
Var l : boolean;
begin
  l := FDisabled;
  FDisabled := true;
  try
    ebFee.Text := TAccountComp.FormatMoney(Fee);
    ebAmount.Text := TAccountComp.FormatMoney(FTxAmount);
  finally
    FDisabled := l;
  end;
end;

procedure TFRMOperation.ebNewPublicKeyChange(Sender: TObject);
begin
  if FDisabled then exit;
  rbTransferToANewOwner.Checked := true;
  rbTransactionClick(Nil);
end;

procedure TFRMOperation.ebSenderAccountExit(Sender: TObject);
Var an : Cardinal;
begin
  If TAccountComp.AccountTxtNumberToAccountNumber(ebSenderAccount.Text,an) then begin
    SenderAccount := an;
    ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(an);
  end else begin
    ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(SenderAccount);
  end;
end;

procedure TFRMOperation.ebSenderAccountKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then ebSenderAccountExit(Nil);
end;

procedure TFRMOperation.FormCreate(Sender: TObject);
begin
  FDisabled := true;
  FNode := TNode.Node;
  FOperation := Nil;
  FPAccount := Nil;
  ebDestAccount.Text := '';
  ebAmount.Text := TAccountComp.FormatMoney(0);
  ebEncryptPassword.Text := '';
  ebNewPublicKey.Text := '';
  ebFee.Text := TAccountComp.FormatMoney(0);
  memoPayload.Lines.Text := '';
  lblTransactionErrors.Caption := '';
  lblChangeKeyErrors.Caption := '';
  lblEncryptionErrors.Caption := '';
  lblNewOwnerErrors.Caption := '';
  FTxDestAccount := 0;
  FTxAmount := 0;
  FSenderAccount := MaxInt;
  FNewAccountPublicKey := CT_Account_NUL.accountkey;
  FDisabled := false;
  lblAccountBalance.Caption := '';
end;

procedure TFRMOperation.FormDestroy(Sender: TObject);
begin
  if Assigned(FPAccount) then Dispose(FPAccount);
  if assigned(FOperation) then FOperation.Free;
end;

function TFRMOperation.GetSenderAcccount: PAccount;
begin
  if Not Assigned(FPAccount) then begin
    if (Assigned(FNode)) And (FSenderAccount>=0) And (FSenderAccount<FNode.Bank.AccountsCount) then begin
      New(FPAccount);
      FPAccount^ := FNode.Operations.SafeBoxTransaction.Account(FSenderAccount);
    end;
  end;
  Result := FPAccount;
  if Assigned(FPAccount) then begin
    lblAccountBalance.Caption := 'Balance: '+TAccountComp.FormatMoney(FPAccount.balance);
  end else lblAccountBalance.Caption := '';
end;

procedure TFRMOperation.memoPayloadClick(Sender: TObject);
Var errors : AnsiString;
begin
  UpdatePayload(errors);
end;

procedure TFRMOperation.rbTransactionClick(Sender: TObject);
Var errors : AnsiString;
begin
  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.SetFee(const Value: Int64);
var wd : Boolean;
begin
  if FFee = Value then exit;
  wd := FDisabled;
  try
    FDisabled := true;
    FFee := Value;
    ebFee.Text := TAccountComp.FormatMoney(value);
  finally
    FDisabled := wd;
  end;
end;

procedure TFRMOperation.SetSenderAccount(const Value: Cardinal);
begin
  if FSenderAccount=Value then exit;

  FSenderAccount := Value;
  if Assigned(FPAccount) then begin
    Dispose(FPAccount);
    FPAccount := Nil;
  end;
  ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(Value);
  //
  rbTransactionClick(Nil);
end;

procedure TFRMOperation.SetWalletKeys(const Value: TWalletKeys);
Var i : Integer;
  wk : TWalletKey;
  s : String;
begin
  FWalletKeys := Value;
  cbNewPrivateKey.items.BeginUpdate;
  Try
    cbNewPrivateKey.Items.Clear;
    cbNewPrivateKey.Items.AddObject('Generate a new Private Key',TObject(-1));
    For i:=0 to FWalletKeys.Count-1 do begin
      wk := FWalletKeys.Key[i];
      if (wk.Name='') then begin
        s := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(wk.AccountKey));
      end else begin
        s := wk.Name;
      end;
      if Not Assigned(wk.PrivateKey) then s := s + '(*)';
      cbNewPrivateKey.Items.AddObject(s,TObject(i));
    end;
  Finally
    cbNewPrivateKey.Items.EndUpdate;
  End;
  rbTransactionClick(Nil);
  memoPayloadClick(Nil);
end;

function TFRMOperation.UpdateOperationOptions(var errors : AnsiString) : Boolean;
Var P : PAccount;
  i : Integer;
  wk : TWalletKey;
  e : AnsiString;
begin
  Result := false;
  errors := '';
  FreeAndNil(FOperation);
  rbEncryptedWithOldEC.Enabled := rbChangeKey.Checked;
  lblDestAccount.Enabled := rbTransaction.Checked;
  lblAmount.Enabled := rbTransaction.Checked;
  lblFee.Enabled := rbTransaction.Checked;
  lblNewPrivateKey.Enabled := rbChangeKey.Checked;
  lblNewOwnerPublicKey.Enabled := rbTransferToANewOwner.Checked;
  try
    Try
      P := GetSenderAcccount;
      bbPassword.Visible := false;
      bbPassword.Enabled := false;
      if Not Assigned(P) then begin
        errors := 'Invalid sender account';
        lblGlobalErrors.Caption := errors;
        exit;
      end;
      if Not Assigned(WalletKeys) then begin
        errors := 'No wallet keys';
        lblGlobalErrors.Caption := errors;
        exit;
      end;
      i := WalletKeys.IndexOfAccountKey(P^.accountkey);
      if (i<0) then begin
        errors := 'Private Key not found in Wallet... You cannot operate with this account';
        lblGlobalErrors.Caption := errors;
        exit;
      end;
      wk := WalletKeys.Key[i];
      if not assigned(wk.PrivateKey) then begin
        if wk.CryptedKey<>'' then begin
          errors := 'Wallet is password protected. Need password';
          bbPassword.Visible := true;
          bbPassword.Enabled := true;
        end else begin
          errors := 'Private Key not found in Wallet, only public key. You cannot operate with this account';
        end;
        lblGlobalErrors.Caption := errors;
        exit;
      end;
      lblGlobalErrors.Caption := '';
    Finally
      tsGlobalError.TabVisible := false;
      tsOperation.TabVisible := false;
      if lblGlobalErrors.Caption<>'' then begin
        PageControl.ActivePage := tsGlobalError;
      end else begin
        PageControl.ActivePage := tsOperation;
      end;
    End;
    if rbTransaction.Checked then begin
      rbTransaction.Font.Style := [fsBold];
      rbChangeKey.ParentFont := true;
      rbTransferToANewOwner.ParentFont := true;
      lblChangeKeyErrors.Caption := '';
      lblNewOwnerErrors.Caption := '';
      rbEncryptedWithOldEC.Checked := false;
      rbEncryptedWithEC.Caption := 'Encrypted with dest account public key';
      ebDestAccount.ParentFont := true;
      ebAmount.ParentFont := true;
      ebFee.ParentFont := true;
      cbNewPrivateKey.Font.Color := clGrayText;
      ebNewPublicKey.Font.Color := clGrayText;
      if not (TAccountComp.AccountTxtNumberToAccountNumber(ebDestAccount.Text,FTxDestAccount)) then begin
        errors := 'Invalid dest account ('+ebDestAccount.Text+')';
        lblTransactionErrors.Caption := errors;
        exit;
      end;
      if not TAccountComp.TxtToMoney(ebAmount.Text,FTxAmount) then begin
        errors := 'Invalid amount ('+ebAmount.Text+')';
        lblTransactionErrors.Caption := errors;
        exit;
      end;
      if not TAccountComp.TxtToMoney(ebFee.Text,FFee) then begin
        errors := 'Invalid fee ('+ebFee.Text+')';
        lblTransactionErrors.Caption := errors;
        exit;
      end;
      if (P^.balance<(FTxAmount+FFee)) then begin
        errors := 'Insufficient founds';
        lblTransactionErrors.Caption := errors;
        exit;
      end;

      lblTransactionErrors.Caption := '';
      result := true;
    end else if rbChangeKey.Checked then begin
      rbTransaction.ParentFont := true;
      rbChangeKey.Font.Style := [fsBold];
      rbTransferToANewOwner.ParentFont := true;
      lblTransactionErrors.Caption := '';
      lblNewOwnerErrors.Caption := '';
      rbEncryptedWithEC.Caption := 'Encrypted with new public key';
      ebDestAccount.Font.Color := clGrayText;
      ebAmount.Font.Color := clGrayText;
      ebFee.Font.Color := clGrayText;
      cbNewPrivateKey.ParentFont := true;
      ebNewPublicKey.Font.Color := clGrayText;
      //
      if cbNewPrivateKey.ItemIndex<0 then begin
        errors := 'Must select a new Private key';
        lblChangeKeyErrors.Caption := errors;
        exit;

      end;
      lblChangeKeyErrors.Caption := '';
      Result := true;
    end else if rbTransferToANewOwner.Checked then begin
      rbTransaction.ParentFont := true;
      rbChangeKey.ParentFont := true;
      rbTransferToANewOwner.Font.Style := [fsBold];
      lblTransactionErrors.Caption := '';
      lblChangeKeyErrors.Caption := '';
      lblNewOwnerErrors.Caption := '';
      ebDestAccount.Font.Color := clGrayText;
      ebAmount.Font.Color := clGrayText;
      ebFee.Font.Color := clGrayText;
      cbNewPrivateKey.Font.Color := clGrayText;
      ebNewPublicKey.ParentFont := true;
      If Not TAccountComp.AccountKeyFromImport(ebNewPublicKey.Text,FNewAccountPublicKey,errors) then begin
//        errors := 'Invalid new owner public key';
        lblNewOwnerErrors.Caption := errors;
        exit;
      end;
      Result := true;
    end else begin
      rbTransaction.ParentFont := true;
      rbChangeKey.ParentFont := true;
      rbTransferToANewOwner.ParentFont := true;
      ebDestAccount.Font.Color := clGrayText;
      ebAmount.Font.Color := clGrayText;
      ebFee.Font.Color := clGrayText;
      cbNewPrivateKey.Font.Color := clGrayText;
      lblTransactionErrors.Caption := '';
      lblChangeKeyErrors.Caption := '';
      lblNewOwnerErrors.Caption := '';
      errors := 'Must select an operation';
    end;
  finally

  end;
  //
  UpdatePayload(e);
end;

function TFRMOperation.UpdatePayload(var errors : AnsiString) : Boolean;
Var payload_u : AnsiString;
  payload_encrypted : TRawBytes;
  account : TAccount;
  dest_account_number : Cardinal;
  i : Integer;
  valid : Boolean;
  wk : TWalletKey;
begin
  valid := false;
  payload_encrypted := '';
  FEncodedPayload := '';
  errors := 'Unknown error';
  payload_u := memoPayload.Lines.Text;
  try
    if (payload_u='') then begin
      valid := true;
      exit;
    end;
    if (rbEncryptedWithOldEC.Checked) then begin
      errors := 'Error encrypting';
      account := FNode.Node.Operations.SafeBoxTransaction.Account(SenderAccount);
      payload_encrypted := ECIESEncrypt(account.accountkey,payload_u);
      valid := payload_encrypted<>'';
    end else if (rbEncryptedWithEC.Checked) then begin
      errors := 'Error encrypting';
      if rbTransaction.Checked then begin
        // With dest public key
        If Not TAccountComp.AccountTxtNumberToAccountNumber(ebDestAccount.Text,dest_account_number) then begin
          errors := 'Invalid dest account number';
          exit;
        end;
        account := FNode.Node.Operations.SafeBoxTransaction.Account(dest_account_number);
        payload_encrypted := ECIESEncrypt(account.accountkey,payload_u);
        valid := payload_encrypted<>'';
      end else if rbChangeKey.Checked then begin
        // With new key generated
        if (cbNewPrivateKey.ItemIndex>=0) then begin
          i := Integer(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex]);
          if (i>=0) then FNewAccountPublicKey := WalletKeys.Key[i].AccountKey;
        end else begin
          valid := false;
          errors := 'Must select a private key';
          exit;
        end;
        if FNewAccountPublicKey.EC_OpenSSL_NID<>CT_Account_NUL.accountkey.EC_OpenSSL_NID then begin
          payload_encrypted := ECIESEncrypt(FNewAccountPublicKey,payload_u);
          valid := payload_encrypted<>'';
        end else begin
          valid := false;
          errors := 'Selected private key is not valid to encode';
          exit;
        end;
      end else if rbTransferToANewOwner.Checked then begin
        //
        if FNewAccountPublicKey.EC_OpenSSL_NID<>CT_Account_NUL.accountkey.EC_OpenSSL_NID then begin
          payload_encrypted := ECIESEncrypt(FNewAccountPublicKey,payload_u);
          valid := payload_encrypted<>'';
        end else begin
          valid := false;
          errors := 'New owner public key not valid';
        end;
      end else begin
        errors := 'Must select operation type to encrypt payload';
        valid := false;
      end;
    end else if (rbEncrptedWithPassword.Checked) then begin
      payload_encrypted := TAESComp.EVP_Encrypt_AES256(payload_u,ebEncryptPassword.Text);
      valid := payload_encrypted<>'';
    end else if (rbNotEncrypted.Checked) then begin
      payload_encrypted := payload_u;
      valid := true;
    end else begin
      errors := 'Must select an encryption option for payload';
    end;
  finally
    if valid then begin
      if length(payload_encrypted)>CT_MaxPayloadSize then begin
        valid := false;
        errors := 'Payload size is bigger than '+inttostr(CT_MaxPayloadSize)+' ('+Inttostr(length(payload_encrypted))+')';
      end;
    end;
    if valid then begin
      lblEncryptionErrors.Caption := '';
      lblPayloadLength.Caption := Format('(%db -> %db)',[length(payload_u),length(payload_encrypted)]);
    end else begin
      lblEncryptionErrors.Caption := errors;
      lblPayloadLength.Caption := Format('(%db -> ?)',[length(payload_u)]);
    end;
    FEncodedPayload := payload_encrypted;
    Result := valid;
  end;
end;

end.
