{$mode delphi}

unit UFRMSaleAccounts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UAccounts, Buttons, ActnList;

type
  TFRMSaleAccounts = class(TApplicationForm)
    lblAccountCaption: TLabel;
    ebSenderAccount: TEdit;
    memoAccounts: TMemo;
    Label1: TLabel;
    ebSalePrice: TEdit;
    Label2: TLabel;
    ebLockedUntilBlock: TEdit;
    gbSellOptions: TGroupBox;
    rbEveryoneCanBuy: TRadioButton;
    rbReservedForAPublickKey: TRadioButton;
    lblNewOwnerPublicKey: TLabel;
    ebNewOwnerPublicKey: TEdit;
    Label3: TLabel;
    ebSellerAccount: TEdit;
    bbExecute: TBitBtn;
    bbCancel: TBitBtn;
    lblAccountBalance: TLabel;
    lblAccountsCount: TLabel;
    lblBalanceCaption: TLabel;
    ActionList: TActionList;
    actExecute: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDisabled : Boolean;
    FSenderAccounts: TOrderedCardinalList;
    FWalletKeys: TWalletKeys;
    FOldOnChanged : TNotifyEvent;
    { Private declarations }
    Procedure UpdateAccountsInfo;
    procedure SetWalletKeys(const Value: TWalletKeys);
  public
    { Public declarations }
    Property SenderAccounts : TOrderedCardinalList read FSenderAccounts;
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
  end;


implementation

uses UNode;

{$R *.dfm}

procedure TFRMSaleAccounts.actExecuteExecute(Sender: TObject);
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
      i := WalletKeys.IndexOfAccountKey(account.accountInfo.accountKey);
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
          op := TOpTransaction.CreateTransaction(account.account,account.n_operation+1,FTxDestAccount,wk.PrivateKey,_amount,_fee,FEncodedPayload);
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
    i := FNode.AddOperations(nil,ops,Nil,errors);
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

procedure TFRMSaleAccounts.FormCreate(Sender: TObject);
begin
  FSenderAccounts := TOrderedCardinalList.Create;
  ebSenderAccount.Text := '';
  ebSalePrice.Text := '';
  ebLockedUntilBlock.Text := '';
  ebSellerAccount.Text := '';
  ebNewOwnerPublicKey.Text := '';
  memoAccounts.lines.Clear;
  FDisabled := false;

end;

procedure TFRMSaleAccounts.FormDestroy(Sender: TObject);
begin
  FSenderAccounts.Free;
end;

procedure TFRMSaleAccounts.SetWalletKeys(const Value: TWalletKeys);
begin
  if FWalletKeys=Value then exit;
  if Assigned(FWalletKeys) then FWalletKeys.OnChanged := FOldOnChanged;
  FWalletKeys := Value;
  if Assigned(FWalletKeys) then begin
    FOldOnChanged := FWalletKeys.OnChanged;
    FWalletKeys.OnChanged := OnWalletKeysChanged;
  end;
  UpdateWalletKeys;
end;

procedure TFRMSaleAccounts.UpdateAccountsInfo;
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

end.
