unit UWIZEnlistAccountForSale;

{$mode delphi}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses
  Classes, SysUtils, Forms, Dialogs, UCrypto, UCommon, UWizard, UAccounts, LCLType;

type

  { TWIZEnlistAccountForSaleModel }
  TWIZAccountSaleMode = (akaPublicSale, akaPrivateSale);
  TWIZPayloadEncryptionMode = (akaEncryptWithOldEC, akaEncryptWithEC,
    akaEncryptWithPassword, akaNotEncrypt);

  TWIZEnlistAccountForSaleModel = class(TComponent)
  public
    DefaultFee: int64;
    NewPublicKey, Payload, EncryptionPassword: string;
    SelectedIndex: integer;
    SalePrice: int64;
    NewOwnerPublicKey: TAccountKey;
    LockedUntilBlock: cardinal;
    EncodedPayload: TRawBytes;
    SignerAccount, SellerAccount: TAccount;
    SelectedAccounts: TArray<TAccount>;
    PayloadEncryptionMode: TWIZPayloadEncryptionMode;
    AccountSaleMode: TWIZAccountSaleMode;
  end;

  { TWIZEnlistAccountForSaleWizard }

  TWIZEnlistAccountForSaleWizard = class(TWizard<TWIZEnlistAccountForSaleModel>)
  private
    function UpdatePayload(const SenderAccount: TAccount;
      var errors: string): boolean;
    function UpdateOperationOptions(var errors: string): boolean;
    function UpdateOpListForSale(const TargetAccount: TAccount;
      var SalePrice: int64; var SellerAccount, SignerAccount: TAccount;
      var NewOwnerPublicKey: TAccountKey; var LockedUntilBlock: cardinal;
      var errors: ansistring): boolean;
    procedure EnlistAccountForSale();
  public
    constructor Create(AOwner: TComponent); override;
    function DetermineHasNext: boolean; override;
    function DetermineHasPrevious: boolean; override;
    function FinishRequested(out message: ansistring): boolean; override;
    function CancelRequested(out message: ansistring): boolean; override;
  end;

implementation

uses
  UBlockChain,
  UOpTransaction,
  UNode,
  UConst,
  UWallet,
  UECIES,
  UAES,
  UWIZEnlistAccountForSale_Start,
  UWIZEnlistAccountForSale_Confirmation;

{ TWIZEnlistAccountForSaleWizard }

function TWIZEnlistAccountForSaleWizard.UpdatePayload(const SenderAccount: TAccount;
  var errors: string): boolean;
var
  valid: boolean;
  payload_encrypted, payload_u: string;
  account: TAccount;
begin
  valid := False;
  payload_encrypted := '';
  Model.EncodedPayload := '';
  errors := 'Unknown error';
  payload_u := Model.Payload;

  try
    if (payload_u = '') then
    begin
      valid := True;
      Exit;
    end;
    case Model.PayloadEncryptionMode of

      akaEncryptWithOldEC:
      begin
        // Use sender
        errors := 'Error encrypting';
        account := SenderAccount;
        payload_encrypted := ECIESEncrypt(account.accountInfo.accountKey, payload_u);
        valid := payload_encrypted <> '';
      end;

      akaEncryptWithEC:
      begin
        errors := 'Public key: ' + 'Error encrypting';

        account := Model.SignerAccount;
        payload_encrypted := ECIESEncrypt(account.accountInfo.accountKey, payload_u);
        valid := payload_encrypted <> '';
      end;

      akaEncryptWithPassword:
      begin
        payload_encrypted := TAESComp.EVP_Encrypt_AES256(
          payload_u, Model.EncryptionPassword);
        valid := payload_encrypted <> '';
      end;

      akaNotEncrypt:
      begin
        payload_encrypted := payload_u;
        valid := True;
      end

      else
      begin
        raise Exception.Create('Invalid Encryption Selection');
      end;
    end;

  finally
    if valid then
    begin
      if length(payload_encrypted) > CT_MaxPayloadSize then
      begin
        valid := False;
        errors := 'Payload size is bigger than ' + IntToStr(CT_MaxPayloadSize) +
          ' (' + IntToStr(length(payload_encrypted)) + ')';
      end;

    end;
    Model.EncodedPayload := payload_encrypted;
    Result := valid;
  end;

end;

function TWIZEnlistAccountForSaleWizard.UpdateOperationOptions(
  var errors: string): boolean;
var
  iAcc, iWallet: integer;
  sender_account, signer_account, seller_account: TAccount;
  publicKey: TAccountKey;
  wk: TWalletKey;
  e: string;
  salePrice: int64;
  auxC: cardinal;
begin
  Result := False;
  errors := '';
  if not Assigned(TWallet.Keys) then
  begin
    errors := 'No wallet keys';
    Exit;
  end;

  if Length(Model.SelectedAccounts) = 0 then
  begin
    errors := 'No sender account';
    Exit;
  end
  else
  begin

    for iAcc := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
    begin
      sender_account := Model.SelectedAccounts[iAcc];
      iWallet := TWallet.Keys.IndexOfAccountKey(sender_account.accountInfo.accountKey);
      if (iWallet < 0) then
      begin
        errors := 'Private key of account ' +
          TAccountComp.AccountNumberToAccountTxtNumber(sender_account.account) +
          ' not found in wallet';
        Exit;
      end;
      wk := TWallet.Keys.Key[iWallet];
      if not assigned(wk.PrivateKey) then
      begin
        if wk.CryptedKey <> '' then
        begin
          // TODO: handle unlocking of encrypted wallet here
          errors := 'Wallet is password protected. Need password';
        end
        else
        begin
          errors := 'Only public key of account ' +
            TAccountComp.AccountNumberToAccountTxtNumber(sender_account.account) +
            ' found in wallet. You cannot operate with this account';
        end;
        Exit;
      end;
    end;
  end;

  Result := UpdateOpListForSale(Model.SelectedAccounts[0], salePrice,
    seller_account, signer_account, publicKey, auxC, errors);
  UpdatePayload(sender_account, e);
end;

function TWIZEnlistAccountForSaleWizard.UpdateOpListForSale(
  const TargetAccount: TAccount; var SalePrice: int64;
  var SellerAccount, SignerAccount: TAccount; var NewOwnerPublicKey: TAccountKey;
  var LockedUntilBlock: cardinal; var errors: ansistring): boolean;
begin
  Result := False;
  SalePrice := 0;
  SellerAccount := CT_Account_NUL;
  NewOwnerPublicKey := CT_TECDSA_Public_Nul;
  LockedUntilBlock := 0;
  errors := '';
  try
    if TAccountComp.IsAccountForSale(TargetAccount.accountInfo) then
    begin
      errors := 'Account ' + TAccountComp.AccountNumberToAccountTxtNumber(
        TargetAccount.account) + ' is already enlisted for sale';
      Exit;
    end;
    salePrice := Model.SalePrice;

    SignerAccount := Model.SignerAccount;

    if (Model.SellerAccount.account = TargetAccount.account) then
    begin
      errors := 'Seller account cannot be same account';
      exit;
    end;

    SellerAccount := Model.SellerAccount;
    if Model.AccountSaleMode = akaPrivateSale then
    begin

      NewOwnerPublicKey := Model.NewOwnerPublicKey;

      if TAccountComp.EqualAccountKeys(NewOwnerPublicKey,
        TargetAccount.accountInfo.accountKey) then
      begin
        errors := 'New public key for private sale is the same public key';
        Exit;
      end;
      LockedUntilBlock := Model.LockedUntilBlock;
      if LockedUntilBlock = 0 then
      begin
        errors := 'Insert locking block';
        exit;
      end;
    end;
    if (TNode.Node.Bank.SafeBox.CurrentProtocol = CT_PROTOCOL_1) then
    begin
      errors := 'This operation needs PROTOCOL 2 active';
      exit;
    end;
  finally
    Result := errors = '';
  end;
end;

procedure TWIZEnlistAccountForSaleWizard.EnlistAccountForSale();
var
  _V2, dooperation: boolean;
  iAcc, i: integer;
  _totalamount, _totalfee, _totalSignerFee, _amount, _fee, _salePrice: int64;
  _signer_n_ops, _lockedUntil: cardinal;
  operationstxt, operation_to_string, errors, auxs: string;
  wk: TWalletKey;
  ops: TOperationsHashTree;
  op: TPCOperation;
  account, signerAccount, destAccount: TAccount;
  _newOwnerPublicKey: TECDSA_Public;
label
  loop_start;
begin
  if not Assigned(TWallet.Keys) then
    raise Exception.Create('No wallet keys');
  if not UpdateOperationOptions(errors) then
    raise Exception.Create(errors);
  ops := TOperationsHashTree.Create;

  try
    _V2 := TNode.Node.Bank.SafeBox.CurrentProtocol >= CT_PROTOCOL_2;
    _totalamount := 0;
    _totalfee := 0;
    _totalSignerFee := 0;
    _signer_n_ops := 0;
    operationstxt := '';
    operation_to_string := '';
    for iAcc := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
    begin
      loop_start:
        op := nil;
      account := Model.SelectedAccounts[iAcc];
      if not UpdatePayload(account, errors) then
      begin
        raise Exception.Create('Error encoding payload of sender account ' +
          TAccountComp.AccountNumberToAccountTxtNumber(account.account) + ': ' + errors);
      end;
      i := TWallet.Keys.IndexOfAccountKey(account.accountInfo.accountKey);
      if i < 0 then
      begin
        raise Exception.Create('Sender account private key not found in Wallet');
      end;

      wk := TWallet.Keys.Key[i];
      dooperation := True;
      // Default fee
      if account.balance > uint64(Model.DefaultFee) then
        _fee := Model.DefaultFee
      else
        _fee := account.balance;

      if not UpdateOpListForSale(account, _salePrice, destAccount,
        signerAccount, _newOwnerPublicKey, _lockedUntil, errors) then
      begin
        raise Exception.Create(errors);
      end;
      // Special fee account:
      if signerAccount.balance > Model.DefaultFee then
        _fee := Model.DefaultFee
      else
        _fee := signerAccount.balance;
      if Model.AccountSaleMode = akaPublicSale then
      begin
        op := TOpListAccountForSale.CreateListAccountForSale(
          signerAccount.account, signerAccount.n_operation + 1 + iAcc,
          account.account, _salePrice, _fee, destAccount.account,
          CT_TECDSA_Public_Nul, 0, wk.PrivateKey, Model.EncodedPayload);
      end
      else if Model.AccountSaleMode = akaPrivateSale then
      begin
        op := TOpListAccountForSale.CreateListAccountForSale(
          signerAccount.account, signerAccount.n_operation + 1 + iAcc,
          account.account, _salePrice, _fee, destAccount.account,
          _newOwnerPublicKey, _lockedUntil, wk.PrivateKey, Model.EncodedPayload);
      end
      else
      begin
        raise Exception.Create('Invalid Sale type');
      end;

      if Assigned(op) and (dooperation) then
      begin
        ops.AddOperationToHashTree(op);
        if operation_to_string <> '' then
          operation_to_string := operation_to_string + #10;
        operation_to_string := operation_to_string + op.ToString;
      end;
      FreeAndNil(op);
    end;

    if (ops.OperationsCount = 0) then
      raise Exception.Create('No valid operation to execute');

    if (Length(Model.SelectedAccounts) > 1) then
    begin
      auxs := '';
      if Application.MessageBox(
        PChar('Execute ' + IntToStr(Length(Model.SelectedAccounts)) +
        ' operations?' + #10 + 'Operation: ' + operationstxt + #10 +
        auxs + 'Total fee: ' + TAccountComp.FormatMoney(_totalfee) +
        #10 + #10 + 'Note: This operation will be transmitted to the network!'),
        PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
        idYes then
      begin
        Exit;
      end;
    end
    else
    begin
      if Application.MessageBox(PChar('Execute this operation:' +
        #10 + #10 + operation_to_string + #10 + #10 +
        'Note: This operation will be transmitted to the network!'),
        PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
        idYes then
      begin
        Exit;
      end;
    end;
    i := TNode.Node.AddOperations(nil, ops, nil, errors);
    if (i = ops.OperationsCount) then
    begin
      operationstxt := 'Successfully executed ' + IntToStr(i) +
        ' operations!' + #10 + #10 + operation_to_string;
      if i > 1 then
      begin

        ShowMessage(operationstxt);
      end
      else
      begin
        Application.MessageBox(
          PChar('Successfully executed ' + IntToStr(i) + ' operations!' +
          #10 + #10 + operation_to_string),
          PChar(Application.Title), MB_OK + MB_ICONINFORMATION);
      end;

    end
    else if (i > 0) then
    begin
      operationstxt := 'One or more of your operations has not been executed:' +
        #10 + 'Errors:' + #10 + errors + #10 + #10 +
        'Total successfully executed operations: ' + IntToStr(i);

      ShowMessage(operationstxt);
    end
    else
    begin
      raise Exception.Create(errors);
    end;


  finally
    ops.Free;
  end;

end;

constructor TWIZEnlistAccountForSaleWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, [TWIZEnlistAccountForSale_Start,
    TWIZEnlistAccountForSale_Confirmation]);
  TitleText := 'Enlist Account';
  FinishText := 'Enlist Account';
end;

function TWIZEnlistAccountForSaleWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZEnlistAccountForSale_Confirmation);
end;

function TWIZEnlistAccountForSaleWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZEnlistAccountForSaleWizard.FinishRequested(
  out message: ansistring): boolean;
begin
  // Execute the Enlist Account For Sale Action here
  try
    Result := True;
    EnlistAccountForSale();
  except
    On E: Exception do
    begin
      Result := False;
      message := E.ToString;
    end;
  end;
end;

function TWIZEnlistAccountForSaleWizard.CancelRequested(
  out message: ansistring): boolean;
begin
  Result := True;
end;

end.
