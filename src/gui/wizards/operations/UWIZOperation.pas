unit UWIZOperation;

{$mode delphi}

{ Copyright (c) 2018 Sphere 10 Software (http://www.sphere10.com/)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Ugochukwu Mmaduekwe - main developer
  Herman Schoenfeld - designer
}

interface

uses
  Classes, SysUtils, Forms, Dialogs, UBaseTypes, UCommon, UWizard, UAccounts, UBlockChain, UNode, UWallet, UCoreObjects, LCLType;

type

  { TWIZOperationsModel }

  TWIZOperationsModel = class(TComponent)
    public
      type

      { TExecuteOperationType }

      TExecuteOperationType = (omtAccount, omtSendPasc, omtChangeKey, omtTransferAccount, omtChangeAccountPrivateKey, omtAddKey, omtEnlistAccountForSale);

      { TPayloadEncryptionMode }

      TPayloadEncryptionMode = (pemEncryptWithSender, pemEncryptWithReceiver, pemEncryptWithPassword, pemNotEncrypt);

      { TOperationSigningMode }

      TOperationSigningMode = (akaPrimary, akaSecondary);

      { TChangeKeyMode }

      TChangeKeyMode = (akaTransferAccountOwnership, akaChangeAccountPrivateKey);

      { TSendPASCMode }

      TSendPASCMode = (akaAllBalance, akaSpecifiedAmount);

      { TAccountSaleMode }
      TAccountSaleMode = (akaPublicSale, akaPrivateSale);

      { TOperationExecuteResultHandler }

      TOperationExecuteResultHandler =
      procedure(const ASourceAccount: TAccount; AOpType: TExecuteOperationType; const AOpText: ansistring; Result: boolean; const Message: ansistring) of object;


      { TAccountModel }

      TAccountModel = class(TComponent)
      public
        SelectedAccounts: TArray<TAccount>;
      end;

      { TSendPASCModel }

      TSendPASCModel = class(TComponent)
      public
        SingleAmountToSend: int64;
        DestinationAccount: TAccount;
        SendPASCMode: TSendPASCMode;
      end;

      { TChangeKeyModel }

      TChangeKeyModel = class(TComponent)
      public
        ChangeKeyMode: TChangeKeyMode;
      end;

      { TTransferAccountModel }

      TTransferAccountModel = class(TComponent)
      public
        AccountKey: TAccountKey;
      end;

      { TChangeAccountPrivateKeyModel }

      TChangeAccountPrivateKeyModel = class(TComponent)
      public
        SelectedIndex: integer;
        NewWalletKey: TWalletKey;
      end;

      { TWIZEnlistAccountForSaleModel }
      TEnlistAccountForSaleModel = class(TComponent)
      public
        SalePrice: int64;
        NewOwnerPublicKey: TAccountKey;
        LockedUntilBlock: cardinal;
        SellerAccount: TAccount;
        AccountSaleMode: TAccountSaleMode;
      end;

      { TFeeModel }

      TFeeModel = class(TComponent)
      public
        DefaultFee, SingleOperationFee: int64;
      end;

      { TSignerModel }

      TSignerModel = class(TComponent)
      public
        OperationSigningMode: TOperationSigningMode;
        SignerAccount: TAccount;
        SignerCandidates: TArray<TAccount>;
        SelectedIndex: integer;
      end;

      { TPayloadModel }

      TPayloadModel = class(TComponent)
      public
        HasPayload: boolean;
        Content, Password: string;
        PayloadEncryptionMode: TPayloadEncryptionMode;
        EncodedBytes: TRawBytes;
      end;

    private
      FExecuteOperationType: TExecuteOperationType;
      FRelockOnFinish: Boolean; static;
      FAccount: TAccountModel;
      FSendPASC: TSendPASCModel;
      FChangeKey: TChangeKeyModel;
      FTransferAccount: TTransferAccountModel;
      FChangeAccountPrivateKey: TChangeAccountPrivateKeyModel;
      FEnlistAccountForSale: TEnlistAccountForSaleModel;
      FFee: TFeeModel;
      FSigner: TSignerModel;
      FPayload: TPayloadModel;
    public
      constructor Create(AOwner: TComponent; AType: TExecuteOperationType); overload;
      property ExecuteOperationType: TExecuteOperationType read FExecuteOperationType;
      property Account: TAccountModel read FAccount;
      property SendPASC: TSendPASCModel read FSendPASC;
      property ChangeKey: TChangeKeyModel read FChangeKey;
      property TransferAccount: TTransferAccountModel read FTransferAccount;
      property ChangeAccountPrivateKey: TChangeAccountPrivateKeyModel read FChangeAccountPrivateKey;
      property EnlistAccountForSale: TEnlistAccountForSaleModel read FEnlistAccountForSale;
      property Fee: TFeeModel read FFee;
      property Signer: TSignerModel read FSigner;
      property Payload: TPayloadModel read FPayload;
      class property RelockOnFinish: Boolean read FRelockOnFinish write FRelockOnFinish;
  end;


  { TWIZOperationsHelper }

  TWIZOperationsHelper = class
  private
    class function UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
    class function SendPASCFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
    class function OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
  public
    class function ExecuteOperations(const ANewOps: TWIZOperationsModel; AHandler: TWIZOperationsModel.TOperationExecuteResultHandler; var errors: ansistring): boolean; static;
    class function ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount, ASignerAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TWIZOperationsModel.TSendPASCMode; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TWIZOperationsModel.TAccountSaleMode; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
  end;

implementation

uses
  UCrypto, UECIES, UAES, UConst, UCoreUtils, UOpTransaction;

{ TWIZOperationsModel }

constructor TWIZOperationsModel.Create(AOwner: TComponent; AType: TWIZOperationsModel.TExecuteOperationType);
begin
  inherited Create(AOwner);
  FExecuteOperationType := AType;
  FRelockOnFinish := False;
  FAccount := TAccountModel.Create(Self);
  FSendPASC := TSendPASCModel.Create(Self);
  FChangeKey := TChangeKeyModel.Create(Self);
  FTransferAccount := TTransferAccountModel.Create(Self);
  FChangeAccountPrivateKey := TChangeAccountPrivateKeyModel.Create(Self);
  FEnlistAccountForSale := TEnlistAccountForSaleModel.Create(Self);
  FFee := TFeeModel.Create(Self);
  FSigner := TSignerModel.Create(Self);
  FPayload := TPayloadModel.Create(Self);
end;

{ TWIZOperationsHelper }

class function TWIZOperationsHelper.SendPASCFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
var
  LAuxs, LOperationsTxt: string;
  i: integer;
begin
  LOperationsTxt := AOperationsTxt;
  if (ANoOfOperations > 1) then
  begin
    LAuxs := 'Total amount that dest will receive: ' + TAccountComp.FormatMoney(
      ATotalAmount) + #10;
    if Application.MessageBox(
      PChar('Execute ' + IntToStr(ANoOfOperations) +
      ' operations?' + #10 + 'Operation: ' + LOperationsTxt + #10 +
      LAuxs + 'Total fee: ' + TAccountComp.FormatMoney(ATotalFee) +
      #10 + #10 + 'Note: This operation will be transmitted to the network!'),
      PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
      idYes then
      Exit;
  end
  else
  if Application.MessageBox(PChar('Execute this operation:' +
    #10 + #10 + AOperationToString + #10 + #10 +
    'Note: This operation will be transmitted to the network!'),
    PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
    idYes then
    Exit;
  Result := True;
  i := TNode.Node.AddOperations(nil, AOperationsHashTree, nil, AErrorMessage);
  if (i = AOperationsHashTree.OperationsCount) then
  begin
    LOperationsTxt := 'Successfully executed ' + IntToStr(i) +
      ' operations!' + #10 + #10 + AOperationToString;
    if i > 1 then
      ShowMessage(LOperationsTxt)
    else
    begin
      Application.MessageBox(
        PChar('Successfully executed ' + IntToStr(i) + ' operations!' +
        #10 + #10 + AOperationToString),
        PChar(Application.Title), MB_OK + MB_ICONINFORMATION);
    end;
  end
  else if (i > 0) then
  begin
    LOperationsTxt := 'One or more of your operations has not been executed:' +
      #10 + 'Errors:' + #10 + AErrorMessage + #10 + #10 +
      'Total successfully executed operations: ' + IntToStr(i);
    ShowMessage(LOperationsTxt);
  end
  else
    Result := False;
end;

class function TWIZOperationsHelper.UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LValid: boolean;
begin

  if (APayloadContent = '') then
    Exit(True);

  LValid := False;
  AErrorMessage := 'An Error Occured During Payload Encryption.';

  try

    case APayloadEncryptionMode of

      pemEncryptWithSender:
      begin
        // Use sender public key
        AEncodedPayloadBytes := ECIESEncrypt(ASenderPublicKey, APayloadContent);
        LValid := AEncodedPayloadBytes <> '';
      end;

      pemEncryptWithReceiver:
      begin
        // With destination public key
        AEncodedPayloadBytes := ECIESEncrypt(ADestinationPublicKey, APayloadContent);
        LValid := AEncodedPayloadBytes <> '';
      end;

      pemEncryptWithPassword:
      begin
        // With defined password
        if APayloadEncryptionPassword = '' then
        begin
          AErrorMessage := 'Payload Encryption Password Cannot Be Empty With The Chosen Option : "Encrypt With Password."';
          Exit(False);
        end;
        AEncodedPayloadBytes := TAESComp.EVP_Encrypt_AES256(
          APayloadContent, APayloadEncryptionPassword);
        LValid := AEncodedPayloadBytes <> '';
      end;

      pemNotEncrypt:
      begin
        // no encryption
        AEncodedPayloadBytes := APayloadContent;
        LValid := True;
      end

      else
      begin
        AErrorMessage := 'Unknown Encryption Selected';
        Exit(False);
      end;
    end;

  finally
    if LValid then
      if Length(AEncodedPayloadBytes) > CT_MaxPayloadSize then
      begin
        LValid := False;
        AErrorMessage := Format('Payload Size Is %d Which Is Bigger Than %d', [Length(AEncodedPayloadBytes), CT_MaxPayloadSize]);
      end;
    Result := LValid;
  end;
end;

class function TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
var
  LAuxs, LOperationsTxt: string;
  i: integer;
begin
  LOperationsTxt := AOperationsTxt;
  if (ANoOfOperations > 1) then
  begin
    LAuxs := '';
    if Application.MessageBox(
      PChar('Execute ' + IntToStr(ANoOfOperations) +
      ' operations?' + #10 + 'Operation: ' + LOperationsTxt + #10 +
      LAuxs + 'Total fee: ' + TAccountComp.FormatMoney(ATotalFee) +
      #10 + #10 + 'Note: This operation will be transmitted to the network!'),
      PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
      idYes then
      Exit;
  end
  else
  if Application.MessageBox(PChar('Execute this operation:' +
    #10 + #10 + AOperationToString + #10 + #10 +
    'Note: This operation will be transmitted to the network!'),
    PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
    idYes then
    Exit;
  Result := True;
  i := TNode.Node.AddOperations(nil, AOperationsHashTree, nil, AErrorMessage);
  if (i = AOperationsHashTree.OperationsCount) then
  begin
    LOperationsTxt := 'Successfully executed ' + IntToStr(i) +
      ' operations!' + #10 + #10 + AOperationToString;
    if i > 1 then
      ShowMessage(LOperationsTxt)
    else
    begin
      Application.MessageBox(
        PChar('Successfully executed ' + IntToStr(i) + ' operations!' +
        #10 + #10 + AOperationToString),
        PChar(Application.Title), MB_OK + MB_ICONINFORMATION);
    end;
  end
  else if (i > 0) then
  begin
    LOperationsTxt := 'One or more of your operations has not been executed:' +
      #10 + 'Errors:' + #10 + AErrorMessage + #10 + #10 +
      'Total successfully executed operations: ' + IntToStr(i);
    ShowMessage(LOperationsTxt);
  end
  else
    Result := False;
end;

class function TWIZOperationsHelper.ExecuteOperations(const ANewOps: TWIZOperationsModel; AHandler: TWIZOperationsModel.TOperationExecuteResultHandler; var errors: ansistring): boolean;
begin

end;

class function TWIZOperationsHelper.ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount, ASignerAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TWIZOperationsModel.TSendPASCMode; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalAmount, LTotalSignerFee, LAmount, LFee: int64;
  LDoOperation: boolean;
  LOperationsTxt, LOperationToString: string;
  LIdx, LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
begin
  if Length(ASelectedAccounts) = 0 then
  begin
    AErrorMessage := 'No Selected Account Found';
    Exit(False);
  end;

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;

  if not Assigned(LWalletKeys) then
  begin
    AErrorMessage := 'No Wallet Keys Found';
    Exit(False);
  end;

  if not Assigned(LNode) then
  begin
    AErrorMessage := 'No Node Found';
    Exit(False);
  end;

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LTotalAmount := 0;
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';
    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if LCurrentAccount.account = ADestinationAccount.account then
      begin
        AErrorMessage := Format('Sender "%s" And Destination "%s" Accounts Are The Same', [LCurrentAccount.AccountString, ADestinationAccount.AccountString]);
        Exit(False);
      end;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, ADestinationAccount.accountInfo.accountKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      LIdx := LWalletKeys.IndexOfAccountKey(LCurrentAccount.accountInfo.accountKey);
      if LIdx < 0 then
      begin
        AErrorMessage := Format('Selected Account "%s" Private Key Not Found In Wallet', [LCurrentAccount.AccountString]);
        Exit(False);
      end;
      LWalletKey := LWalletKeys.Key[LIdx];

      if not Assigned(LWalletKey.PrivateKey) then
      begin
        if LWalletKey.HasPrivateKey then
          AErrorMessage := 'Wallet is Password Protected. Please Unlock Before You Proceed.'
        else
          AErrorMessage := Format('Only Public Key of Account %s Was Found in Wallet. You Cannot Operate This Account', [LCurrentAccount.AccountString]);
        Exit(False);
      end;

      LDoOperation := True;

      if LCurrentAccount.balance > 0 then
        case ASendPASCMode of
          akaAllBalance:
          begin
            LAmount := LCurrentAccount.balance - AFee;
            LFee := AFee;
          end;

          akaSpecifiedAmount:
            if LCurrentAccount.balance >= UInt64(AAmount + AFee) then
            begin
              LAmount := AAmount;
              LFee := AFee;
            end
            else
            begin
              AErrorMessage := Format('Insufficient Funds In "%s". %s < (%s + %s = %s)', [LCurrentAccount.AccountString, TAccountComp.FormatMoney(LCurrentAccount.balance), TAccountComp.FormatMoney(AAmount), TAccountComp.FormatMoney(AFee), TAccountComp.FormatMoney(AAmount + AFee)]);
              Exit(False);
            end;
        end
      else
        LDoOperation := False;

      if LDoOperation then
      begin
        LPCOperation := TOpTransaction.CreateTransaction(
          LCurrentAccount.account, LCurrentAccount.n_operation + 1, ADestinationAccount.account, LWalletKey.PrivateKey, LAmount, LFee, LPayloadEncodedBytes);
        try
          LOperationsTxt := Format('Transaction To "%s"', [ADestinationAccount.AccountString]);

          if Assigned(LPCOperation) then
          begin
            LOperationsHashTree.AddOperationToHashTree(LPCOperation);
            Inc(LTotalAmount, LAmount);
            Inc(LTotalSignerFee, LFee);
            Inc(LNoOfOperations);
            if LOperationToString <> '' then
              LOperationToString := LOperationToString + #10;
            LOperationToString := LOperationToString + LPCOperation.ToString;
          end;
        finally
          FreeAndNil(LPCOperation);
        end;
      end;

    end;
    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation To Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.SendPASCFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalAmount, LTotalSignerFee, LOperationsHashTree, AErrorMessage));
  finally
    LOperationsHashTree.Free;
  end;

end;

class function TWIZOperationsHelper.ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalSignerFee, LFee: int64;
  LIsV2: boolean;
  LOperationsTxt, LOperationToString: string;
  LIdx, LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount, LSignerAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
label
  loop_start;
begin
  if Length(ASelectedAccounts) = 0 then
  begin
    AErrorMessage := 'No Selected Account Found';
    Exit(False);
  end;

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;

  if not Assigned(LWalletKeys) then
  begin
    AErrorMessage := 'No Wallet Keys Found';
    Exit(False);
  end;

  if not Assigned(LNode) then
  begin
    AErrorMessage := 'No Node Found';
    Exit(False);
  end;

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LIsV2 := LNode.Bank.SafeBox.CurrentProtocol >= CT_PROTOCOL_2;
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';
    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      loop_start:
        LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if (TAccountComp.EqualAccountKeys(LCurrentAccount.accountInfo.accountKey,
        APublicKey)) then
      begin
        AErrorMessage := 'New Key Is Same As Current Key';
        Exit(False);
      end;

      if LNode.Bank.SafeBox.CurrentProtocol >= 1 then
      begin
        // Signer:
        LSignerAccount := ASignerAccount;
        if (TAccountComp.IsAccountLocked(LSignerAccount.accountInfo,
          LNode.Bank.BlocksCount)) then
        begin
          AErrorMessage := Format('Signer Account "%s"  Is Locked Until Block %u', [LSignerAccount.AccountString, LSignerAccount.accountInfo.locked_until_block]);
          Exit(False);
        end;
        if (not TAccountComp.EqualAccountKeys(
          LSignerAccount.accountInfo.accountKey, LCurrentAccount.accountInfo.accountKey)) then
        begin
          AErrorMessage := Format('Signer Account %s Is Not The Owner Of Account %s', [LSignerAccount.AccountString, LCurrentAccount.AccountString]);
          Exit(False);
        end;
      end
      else
        LSignerAccount := LCurrentAccount;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, APublicKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      LIdx := LWalletKeys.IndexOfAccountKey(LCurrentAccount.accountInfo.accountKey);
      if LIdx < 0 then
      begin
        AErrorMessage := Format('Selected Account "%s" Private Key Not Found In Wallet', [LCurrentAccount.AccountString]);
        Exit(False);
      end;
      LWalletKey := LWalletKeys.Key[LIdx];

      if not Assigned(LWalletKey.PrivateKey) then
      begin
        if LWalletKey.HasPrivateKey then
          AErrorMessage := 'Wallet Is Password Protected. Please Unlock Before You Proceed.'
        else
          AErrorMessage := Format('Only Public Key of Account %s Was Found In Wallet. You Cannot Operate This Account', [LCurrentAccount.AccountString]);
        Exit(False);
      end;

      if LIsV2 then
      begin
        // must ensure is Signer account last if included in sender accounts (not necessarily ordered enumeration)
        if (LAccountIdx < Length(ASelectedAccounts) - 1) and
          (LCurrentAccount.account = LSignerAccount.account) then
        begin
          TArrayTool<TAccount>.Swap(ASelectedAccounts, LAccountIdx,
            Length(ASelectedAccounts) - 1); // ensure signer account processed last
          goto loop_start; // TODO: remove ugly hack with refactoring!
        end;

        // Maintain correct signer fee distribution
        if Uint64(LTotalSignerFee) >= LSignerAccount.balance then
          LFee := 0
        else if LSignerAccount.balance - uint64(LTotalSignerFee) >
          UInt64(AFee) then
          LFee := AFee
        else
          LFee := LSignerAccount.balance - UInt64(LTotalSignerFee);
        LPCOperation := TOpChangeKeySigned.Create(LSignerAccount.account,
          LSignerAccount.n_operation + LNoOfOperations + 1, LCurrentAccount.account,
          LWalletKey.PrivateKey, APublicKey, LFee, LPayloadEncodedBytes);
      end
      else
        LPCOperation := TOpChangeKey.Create(LCurrentAccount.account, LCurrentAccount.n_operation +
          1, LCurrentAccount.account, LWalletKey.PrivateKey, APublicKey, LFee, LPayloadEncodedBytes);

      try
        LOperationsTxt := Format('Change Key To "%s"', [TAccountComp.GetECInfoTxt(APublicKey.EC_OpenSSL_NID)]);
        if Assigned(LPCOperation) then
        begin
          LOperationsHashTree.AddOperationToHashTree(LPCOperation);
          Inc(LNoOfOperations);
          Inc(LTotalSignerFee, LFee);
          if LOperationToString <> '' then
            LOperationToString := LOperationToString + #10;
          LOperationToString := LOperationToString + LPCOperation.ToString;
        end;
      finally
        FreeAndNil(LPCOperation);
      end;

    end;

    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));
  finally
    LOperationsHashTree.Free;
  end;

end;

class function TWIZOperationsHelper.ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TWIZOperationsModel.TAccountSaleMode; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalSignerFee, LFee: int64;
  LOperationsTxt, LOperationToString: string;
  LIdx, LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount, LSignerAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
begin
  if Length(ASelectedAccounts) = 0 then
  begin
    AErrorMessage := 'No Selected Account Found';
    Exit(False);
  end;

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;

  if not Assigned(LWalletKeys) then
  begin
    AErrorMessage := 'No Wallet Keys Found';
    Exit(False);
  end;

  if not Assigned(LNode) then
  begin
    AErrorMessage := 'No Node Found';
    Exit(False);
  end;

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';

    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if TAccountComp.IsAccountForSale(LCurrentAccount.accountInfo) then
      begin
        AErrorMessage := Format('Account "%s" Is Already Enlisted For Sale', [LCurrentAccount.AccountString]);
        Exit(False);
      end;

      if (ASellerAccount.account = LCurrentAccount.account) then
      begin
        AErrorMessage := 'Seller Account Cannot Be Same As Account To Be Sold.';
        Exit(False);
      end;

      if (LNode.Node.Bank.SafeBox.CurrentProtocol = CT_PROTOCOL_1) then
      begin
        AErrorMessage := 'This Operation Needs PROTOCOL 2 Active';
        Exit(False);
      end;

      if AAccountSaleMode = akaPrivateSale then
      begin

        if TAccountComp.EqualAccountKeys(APublicKey,
          LCurrentAccount.accountInfo.accountKey) then
        begin
          AErrorMessage := 'You Cannot Sell To An Account That You Want To Enlist For Sale.';
          Exit(False);
        end;

        if ALockedUntilBlock = 0 then
        begin
          AErrorMessage := 'You Didn''t Insert a Locking Block.';
          Exit(False);
        end;
      end;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, APublicKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      LIdx := LWalletKeys.IndexOfAccountKey(LCurrentAccount.accountInfo.accountKey);
      if LIdx < 0 then
      begin
        AErrorMessage := Format('Selected Account "%s" Private Key Not Found In Wallet', [LCurrentAccount.AccountString]);
        Exit(False);
      end;
      LWalletKey := LWalletKeys.Key[LIdx];

      if not Assigned(LWalletKey.PrivateKey) then
      begin
        if LWalletKey.HasPrivateKey then
          AErrorMessage := 'Wallet Is Password Protected. Please Unlock Before You Proceed.'
        else
          AErrorMessage := Format('Only Public Key Of Account %s Was Found In Wallet. You Cannot Operate This Account', [LCurrentAccount.AccountString]);
        Exit(False);
      end;

      if ASignerAccount.balance > AFee then
        LFee := AFee
      else
        LFee := ASignerAccount.balance;

      case AAccountSaleMode of
        akaPublicSale:

          LPCOperation := TOpListAccountForSale.CreateListAccountForSale(
            ASignerAccount.account, ASignerAccount.n_operation + 1 + LAccountIdx,
            LCurrentAccount.account, ASalePrice, LFee, ASellerAccount.account,
            APublicKey, 0, LWalletKey.PrivateKey, LPayloadEncodedBytes);

        akaPrivateSale:

          LPCOperation := TOpListAccountForSale.CreateListAccountForSale(
            ASignerAccount.account, ASignerAccount.n_operation + 1 + LAccountIdx,
            LCurrentAccount.account, ASalePrice, LFee, ASellerAccount.account,
            APublicKey, ALockedUntilBlock, LWalletKey.PrivateKey, LPayloadEncodedBytes)

        else
          raise Exception.Create('Invalid Account Sale Type')
      end;

      try
        LOperationsTxt := Format('Enlist Account For Sale At a Price Of "%s" PASC', [TAccountComp.FormatMoney(ASalePrice)]);
        if Assigned(LPCOperation) then
        begin
          LOperationsHashTree.AddOperationToHashTree(LPCOperation);
          Inc(LNoOfOperations);
          Inc(LTotalSignerFee, LFee);
          if LOperationToString <> '' then
            LOperationToString := LOperationToString + #10;
          LOperationToString := LOperationToString + LPCOperation.ToString;
        end;
      finally
        FreeAndNil(LPCOperation);
      end;

    end;

    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));

  finally
    LOperationsHashTree.Free;
  end;
end;

end.
