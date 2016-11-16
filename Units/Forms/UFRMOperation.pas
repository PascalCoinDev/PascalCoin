unit UFRMOperation;

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
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UNode, UWalletKeys, UCrypto, Buttons, UBlockChain,
  UAccounts, ActnList, ComCtrls, Types;

type

  { TFRMOperation }

  TFRMOperation = class(TForm)
    lblAccountCaption: TLabel;
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
    memoAccounts: TMemo;
    lblAccountsCount: TLabel;
    procedure ebAmountChange(Sender: TObject);
    procedure ebFeeChange(Sender: TObject);
    procedure ebNewPublicKeyExit(Sender: TObject);
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
    FNode : TNode;
    FWalletKeys: TWalletKeys;
    FFee: Int64;
    FEncodedPayload : TRawBytes;
    FDisabled : Boolean;
    //
    FTxDestAccount : Cardinal;
    FTxAmount : Int64;
    FNewAccountPublicKey : TAccountKey;
    FSenderAccounts: TOrderedCardinalList;
    procedure SetWalletKeys(const Value: TWalletKeys);
    { Private declarations }
    Procedure UpdateAccountsInfo;
    Function UpdateOperationOptions(var errors : AnsiString) : Boolean;
    Function UpdatePayload(Const SenderAccount : TAccount; var errors : AnsiString) : Boolean;
    procedure SetFee(const Value: Int64);
    Procedure OnSenderAccountsChanged(Sender : TObject);
  public
    { Public declarations }
    Property SenderAccounts : TOrderedCardinalList read FSenderAccounts;
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
    Property Fee : Int64 read FFee write SetFee;
  end;

implementation

uses
  UECIES, UConst, UOpTransaction, UFRMNewPrivateKeyType, UAES, UFRMWalletKeys;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TFRMOperation }

procedure TFRMOperation.actExecuteExecute(Sender: TObject);
Var errors : AnsiString;
  P : PAccount;
  i,iAcc : Integer;
  wk : TWalletKey;
  ops : TOperationsHashTree;
  op : TPCOperation;
  account : TAccount;
  operation_to_string, operationstxt, auxs : String;
  _amount,_fee, _totalamount, _totalfee : Int64;
  dooperation : Boolean;
begin
  if Not Assigned(WalletKeys) then raise Exception.Create('No wallet keys');
  If Not UpdateOperationOptions(errors) then raise Exception.Create(errors);
  ops := TOperationsHashTree.Create;
  Try
    _totalamount := 0;
    _totalfee := 0;
    operationstxt := '';
    operation_to_string := '';
    for iAcc := 0 to FSenderAccounts.Count - 1 do begin
      op := Nil;
      account := FNode.Operations.SafeBoxTransaction.Account(FSenderAccounts.Get(iAcc));
      If Not UpdatePayload(account, errors) then
        raise Exception.Create('Error encoding payload of sender account '+TAccountComp.AccountNumberToAccountTxtNumber(account.account)+': '+errors);
      i := WalletKeys.IndexOfAccountKey(account.accountkey);
      if i<0 then begin
        Raise Exception.Create('Sender account private key not found in Wallet');
      end;

      wk := WalletKeys.Key[i];
      dooperation := true;
      //
      if rbTransaction.Checked then begin
        // Amount
        _amount := 0;
        _fee := 0;
        if FSenderAccounts.Count>1 then begin
          if account.balance>0 then begin
            if account.balance>fee then begin
              _amount := account.balance - fee;
              _fee := fee;
            end else begin
              _amount := account.balance;
              _fee := 0;
            end;
          end else dooperation := false;
        end else begin
          _amount := FTxAmount;
          _fee := fee;
        end;
        if dooperation then begin
          op := TOpTransaction.Create(account.account,account.n_operation+1,FTxDestAccount,wk.PrivateKey,_amount,_fee,FEncodedPayload);
          inc(_totalamount,_amount);
          inc(_totalfee,_fee);
        end;
        operationstxt := 'Transaction to '+TAccountComp.AccountNumberToAccountTxtNumber(FTxDestAccount);
      end else if rbChangeKey.Checked then begin
        i := PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex]);
        if (i<0) Or (i>=WalletKeys.Count) then raise Exception.Create('Invalid selected key');
        FNewAccountPublicKey := WalletKeys.Key[i].AccountKey;
        if account.balance>fee then _fee := fee
        else _fee := 0;
        op := TOpChangeKey.Create(account.account,account.n_operation+1,wk.PrivateKey,FNewAccountPublicKey,_fee,FEncodedPayload);
        inc(_totalfee,_fee);
        operationstxt := 'Change private key to '+wk.Name;
      end else if rbTransferToANewOwner.Checked then begin
        if account.balance>fee then _fee := fee
        else _fee := 0;
        op := TOpChangeKey.Create(account.account,account.n_operation+1,wk.PrivateKey,FNewAccountPublicKey,_fee,FEncodedPayload);
        operationstxt := 'Transfer to a new owner with key type '+TAccountComp.GetECInfoTxt(FNewAccountPublicKey.EC_OpenSSL_NID);
        inc(_totalfee,_fee);
      end else begin
        raise Exception.Create('No operation selected');
      end;
      if Assigned(op) And (dooperation) then begin
        ops.AddOperationToHashTree(op);
        if operation_to_string<>'' then operation_to_string := operation_to_string + #10;
        operation_to_string := operation_to_string + op.ToString;
      end;
      FreeAndNil(op);
    end;
    if (ops.OperationsCount=0) then raise Exception.Create('No valid operation to execute');

    if (FSenderAccounts.Count>1) then begin
      if rbTransaction.Checked then auxs := 'Total amount that dest will receive: '+TAccountComp.FormatMoney(_totalamount)+#10
      else auxs:='';
      if Application.MessageBox(PChar('Execute '+Inttostr(FSenderAccounts.Count)+' operations?'+#10+
        'Operation: '+operationstxt+#10+
        auxs+
        'Total fee: '+TAccountComp.FormatMoney(_totalfee)+#10+#10+'Note: This operation will be transmitted to the network!'),
        PChar(Application.Title),MB_YESNO+MB_ICONINFORMATION+MB_DEFBUTTON2)<>IdYes then exit;
    end else begin
      if Application.MessageBox(PChar('Execute this operation:'+#10+#10+operation_to_string+#10+#10+'Note: This operation will be transmitted to the network!'),
        PChar(Application.Title),MB_YESNO+MB_ICONINFORMATION+MB_DEFBUTTON2)<>IdYes then exit;
    end;
    i := FNode.AddOperations(nil,ops,errors);
    if (i=ops.OperationsCount) then begin
      Application.MessageBox(PChar('Successfully executed '+inttostr(i)+' operations!'+#10+#10+operation_to_string),PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
      ModalResult := MrOk;
    end else if (i>0) then begin
      Application.MessageBox(PChar('One or more of your operations has not been executed:'+#10+
        'Errors:'+#10+
        errors+#10+#10+
        'Total successfully executed operations: '+inttostr(i)),PChar(Application.Title),MB_OK+MB_ICONWARNING);
      ModalResult := MrOk;
    end else begin
      raise Exception.Create(errors);
    end;
  Finally
    ops.Free;
  End;
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
      if Not InputQuery('Wallet password','Enter wallet password',s) then exit;
      FWalletKeys.WalletPassword := s;
    Until FWalletKeys.IsValidPassword;
    SetWalletKeys(WalletKeys);
    UpdateOperationOptions(errors);
  end;
end;

procedure TFRMOperation.cbNewPrivateKeyChange(Sender: TObject);
begin
  If FDisabled then exit;
  If not rbChangeKey.Checked then begin
    rbChangeKey.Checked := true;
    rbTransactionClick(Nil);
  end;
end;

procedure TFRMOperation.ebDestAccountChange(Sender: TObject);
begin
  if FDisabled then exit;
  If not rbTransaction.Checked then begin
    rbTransaction.Checked := true;
    rbTransactionClick(Nil);
  end;
end;

procedure TFRMOperation.ebDestAccountExit(Sender: TObject);
Var an : Cardinal;
  errors : AnsiString;
begin
  If TAccountComp.AccountTxtNumberToAccountNumber(ebDestAccount.Text,an) then begin
    ebDestAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(an);
  end else begin
    ebDestAccount.Text := '';
  end;
  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.ebEncryptPasswordChange(Sender: TObject);
begin
  if FDisabled then exit;
  rbEncrptedWithPassword.Checked := true;
  memoPayloadClick(Nil);
end;

procedure TFRMOperation.ebFeeExit(Sender: TObject);
Var l : boolean;
  errors : AnsiString;
begin
  l := FDisabled;
  FDisabled := true;
  try
    UpdateOperationOptions(errors);
    ebFee.Text := TAccountComp.FormatMoney(Fee);
    if SenderAccounts.Count<=1 then begin
      ebAmount.Text := TAccountComp.FormatMoney(FTxAmount);
    end;
  finally
    FDisabled := l;
  end;
end;

procedure TFRMOperation.ebNewPublicKeyChange(Sender: TObject);
begin
  if FDisabled then exit;
  if not rbTransferToANewOwner.Checked then begin
    rbTransferToANewOwner.Checked := true;
    rbTransactionClick(Nil);
  end;
end;

procedure TFRMOperation.ebSenderAccountExit(Sender: TObject);
Var an : Cardinal;
begin
  If TAccountComp.AccountTxtNumberToAccountNumber(ebSenderAccount.Text,an) then begin
    SenderAccounts.Disable;
    try
      SenderAccounts.Clear;
      SenderAccounts.Add(an);
    finally
      SenderAccounts.Enable;
    end;
    ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(an);
  end else begin
    if SenderAccounts.Count=1 then begin
      ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(SenderAccounts.Get(0));
    end else begin
      ebSenderAccount.Text := '';
    end;
  end;
end;

procedure TFRMOperation.ebSenderAccountKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then ebSenderAccountExit(Nil);
end;

procedure TFRMOperation.FormCreate(Sender: TObject);
begin
  FSenderAccounts := TOrderedCardinalList.Create;
  FSenderAccounts.OnListChanged := OnSenderAccountsChanged;
  FDisabled := true;
  FNode := TNode.Node;
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
  FNewAccountPublicKey := CT_Account_NUL.accountkey;
  FDisabled := false;
  lblAccountBalance.Caption := '';
  memoAccounts.Lines.Clear;
end;

procedure TFRMOperation.ebAmountChange(Sender: TObject);
begin
  If FDisabled then exit;
  TAccountComp.TxtToMoney(ebAmount.text,FTxAmount);
  If not rbTransaction.Checked then begin
    rbTransaction.Checked := true;
    rbTransactionClick(Nil);
  end;
end;

procedure TFRMOperation.ebFeeChange(Sender: TObject);
begin
  If FDisabled then exit;
  TAccountComp.TxtToMoney(ebFee.text,FFee);
  If not rbTransaction.Checked then begin
    rbTransaction.Checked := true;
    rbTransactionClick(Nil);
  end;
end;

procedure TFRMOperation.ebNewPublicKeyExit(Sender: TObject);
var errors : AnsiString;
begin
  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSenderAccounts);
end;

procedure TFRMOperation.memoPayloadClick(Sender: TObject);
Var errors : AnsiString;
begin
  if SenderAccounts.Count>0 then begin
    UpdatePayload(TNode.Node.Bank.SafeBox.Account(SenderAccounts.Get(0)),errors);
  end;
end;

procedure TFRMOperation.OnSenderAccountsChanged(Sender: TObject);
Var errors : AnsiString;
begin
  if SenderAccounts.Count>1 then begin
    ebAmount.Text := 'ALL BALANCE';
    ebAmount.font.Style := [fsBold];
    ebAmount.ReadOnly := true;
  end else begin
    ebAmount.Text := TAccountComp.FormatMoney(0);
    ebAmount.ReadOnly := false;
    ebAmount.Enabled := true;
  end;
  UpdateAccountsInfo;
  UpdateOperationOptions(errors);
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

procedure TFRMOperation.SetWalletKeys(const Value: TWalletKeys);
Var i : Integer;
  wk : TWalletKey;
  s : String;
begin
  FWalletKeys := Value;
  cbNewPrivateKey.items.BeginUpdate;
  Try
    cbNewPrivateKey.Items.Clear;
    //cbNewPrivateKey.Items.AddObject('Generate a new Private Key',TObject(-1));
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

procedure TFRMOperation.UpdateAccountsInfo;
Var ld : Boolean;
  i : Integer;
  balance : int64;
  acc : TAccount;
  accountstext : String;
begin
  ld := FDisabled;
  FDisabled := true;
  Try
    lblAccountCaption.Caption := 'Account';
    lblAccountsCount.Visible := false;
    lblAccountsCount.caption := inttostr(senderAccounts.Count)+' accounts';
    balance := 0;
    if SenderAccounts.Count<=0 then begin
      ebSenderAccount.Text := '';
      memoAccounts.Visible := false;
      ebSenderAccount.Visible := true;
    end else if SenderAccounts.Count=1 then begin
      ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(SenderAccounts.Get(0));
      memoAccounts.Visible := false;
      ebSenderAccount.Visible := true;
      balance := TNode.Node.Operations.SafeBoxTransaction.Account(SenderAccounts.Get(0)).balance;
    end else begin
      // Multiple sender accounts
      lblAccountCaption.Caption := 'Accounts';
      lblAccountsCount.Visible := true;
      ebSenderAccount.Visible := false;
      accountstext := '';
      for i := 0 to SenderAccounts.Count - 1 do begin
         acc := TNode.Node.Operations.SafeBoxTransaction.Account(SenderAccounts.Get(i));
         balance := balance + acc.balance;
         if (accountstext<>'') then accountstext:=accountstext+'; ';
         accountstext := accountstext+TAccountComp.AccountNumberToAccountTxtNumber(acc.account)+' ('+TAccountComp.FormatMoney(acc.balance)+')';
      end;
      memoAccounts.Lines.Text := accountstext;
      memoAccounts.Visible := true;
    end;
    ebSenderAccount.Enabled := ebSenderAccount.Visible;
    lblAccountBalance.Caption := TAccountComp.FormatMoney(balance);
  Finally
    FDisabled := ld;
  End;
end;

function TFRMOperation.UpdateOperationOptions(var errors : AnsiString) : Boolean;
Var
  iWallet,iAcc : Integer;
  wk : TWalletKey;
  e : AnsiString;
  sender_account : TAccount;
begin
  Result := false;
  sender_account := CT_Account_NUL;
  errors := '';
  rbEncryptedWithOldEC.Enabled := rbChangeKey.Checked;
  lblDestAccount.Enabled := rbTransaction.Checked;
  lblAmount.Enabled := rbTransaction.Checked;
  lblFee.Enabled := rbTransaction.Checked;
  lblNewPrivateKey.Enabled := rbChangeKey.Checked;
  lblNewOwnerPublicKey.Enabled := rbTransferToANewOwner.Checked;
  try
    Try
      bbPassword.Visible := false;
      bbPassword.Enabled := false;
      if Not Assigned(WalletKeys) then begin
        errors := 'No wallet keys';
        lblGlobalErrors.Caption := errors;
        exit;
      end;
      if SenderAccounts.Count=0 then begin
        errors := 'No sender account';
        lblGlobalErrors.Caption := errors;
        exit;
      end else begin
        for iAcc := 0 to SenderAccounts.Count - 1 do begin
          sender_account := TNode.Node.Bank.SafeBox.Account(SenderAccounts.Get(iAcc));
          iWallet := WalletKeys.IndexOfAccountKey(sender_account.accountkey);
          if (iWallet<0) then begin
            errors := 'Private key of account '+TAccountComp.AccountNumberToAccountTxtNumber(sender_account.account)+' not found in wallet';
            lblGlobalErrors.Caption := errors;
            exit;
          end;
          wk := WalletKeys.Key[iWallet];
          if not assigned(wk.PrivateKey) then begin
            if wk.CryptedKey<>'' then begin
              errors := 'Wallet is password protected. Need password';
              bbPassword.Visible := true;
              bbPassword.Enabled := true;
            end else begin
              errors := 'Only public key of account '+TAccountComp.AccountNumberToAccountTxtNumber(sender_account.account)+' found in wallet. You cannot operate with this account';
            end;
            lblGlobalErrors.Caption := errors;
            exit;
          end;
        end;
      end;
      lblGlobalErrors.Caption := '';
    Finally
      if lblGlobalErrors.Caption<>'' then begin
        tsGlobalError.visible := true;
        tsGlobalError.tabvisible := {$IFDEF LINUX}true{$ELSE}false{$ENDIF};
        tsOperation.TabVisible := false;
        PageControl.ActivePage := tsGlobalError;
        ActiveControl := bbPassword;
      end else begin
        tsOperation.visible := true;
        tsOperation.tabvisible := {$IFDEF LINUX}true{$ELSE}false{$ENDIF};
        tsGlobalError.TabVisible := false;
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
      rbEncryptedWithEC.Caption := 'Encrypted with dest. account public key';
      ebDestAccount.ParentFont := true;
      ebFee.ParentFont := true;
      cbNewPrivateKey.Font.Color := clGrayText;
      ebNewPublicKey.Font.Color := clGrayText;
      if not (TAccountComp.AccountTxtNumberToAccountNumber(ebDestAccount.Text,FTxDestAccount)) then begin
        errors := 'Invalid dest. account ('+ebDestAccount.Text+')';
        lblTransactionErrors.Caption := errors;
        exit;
      end;
      if SenderAccounts.Count>1 then begin
        // If multisender then amount is ALL balance
        ebAmount.Font.Color := clNavy;
      end else begin
        ebAmount.ParentFont := true;
        if not TAccountComp.TxtToMoney(ebAmount.Text,FTxAmount) then begin
          errors := 'Invalid amount ('+ebAmount.Text+')';
          lblTransactionErrors.Caption := errors;
          exit;
        end;
      end;
      if not TAccountComp.TxtToMoney(ebFee.Text,FFee) then begin
        errors := 'Invalid fee ('+ebFee.Text+')';
        lblTransactionErrors.Caption := errors;
        exit;
      end;
      if (SenderAccounts.Count=1) then begin
        if (sender_account.balance<(FTxAmount+FFee)) then begin
          errors := 'Insufficient funds';
          lblTransactionErrors.Caption := errors;
          exit;
        end;
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
        errors := 'Must select a new private key';
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
        lblNewOwnerErrors.Caption := errors;
        lblNewOwnerErrors.Font.Color := clRed;
        exit;
      end else begin
        lblNewOwnerErrors.Caption := 'New key type: '+TAccountComp.GetECInfoTxt(FNewAccountPublicKey.EC_OpenSSL_NID);
        lblNewOwnerErrors.Font.Color := clGreen;
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
  UpdatePayload(sender_account, e);
end;

function TFRMOperation.UpdatePayload(Const SenderAccount : TAccount; var errors : AnsiString) : Boolean;
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
      account := FNode.Node.Operations.SafeBoxTransaction.Account(SenderAccount.account);
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
          i := PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex]);
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
          errors := 'New owner of public key not valid';
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
