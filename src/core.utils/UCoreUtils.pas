unit UCoreUtils;

{ Copyright (c) 2018 by PascalCoin Project

  Contains common types for Core module.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com>: unit creator
    Ugochukwu Mmaduekwe - added TOperationsManager class
}

{$mode delphi}

interface

uses
  Classes, SysUtils, UCrypto, UAccounts, UBlockChain, UOpTransaction, UNode, UCommon, UNetProtocol,
  Generics.Collections, Generics.Defaults, UCoreObjects, Forms, Dialogs, LCLType, UCellRenderers;

type

  { TAccountComparer }

  TAccountComparer = class(TComparer<TAccount>)
    function Compare(constref ALeft, ARight: T): integer; override;
    class function DoCompare(constref ALeft, ARight: TAccount): integer; inline;
  end;

  { TAccountEqualityComparer }

  TAccountEqualityComparer = class(TEqualityComparer<TAccount>)
  public
    function Equals(constref ALeft, ARight: TAccount): boolean; override;
    function GetHashCode(constref AValue: TAccount): UInt32; override;
    class function AreEqual(constref ALeft, ARight: TAccount): boolean;
    class function CalcHashCode(constref AValue: TAccount): UInt32;
  end;

  { TAccountKeyComparer }

  TAccountKeyComparer = class(TComparer<TAccountKey>)
    function Compare(constref ALeft, ARight: T): integer; override;
    class function DoCompare(constref ALeft, ARight: TAccountKey): integer; inline;
  end;

  { TAccountKeyEqualityComparer }

  TAccountKeyEqualityComparer = class(TEqualityComparer<TAccountKey>)
  public
    function Equals(constref ALeft, ARight: TAccountKey): boolean; override;
    function GetHashCode(constref AValue: TAccountKey): UInt32; override;
    class function AreEqual(constref ALeft, ARight: TAccountKey): boolean;
    class function CalcHashCode(constref AValue: TAccountKey): UInt32;
  end;

  { TCoreTool }

  TCoreTool = class
  public
    class function GetSignerCandidates(ANumOps: integer; ASingleOperationFee: int64; const ACandidates: array of TAccount): TArray<TAccount>; static;
  end;

  { TOperationsManager }

  TOperationsManager = class
  private
    class function UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;

    class function SendPASCFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
    class function OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
  public
    class function GetOperationShortText(const OpType, OpSubType: DWord): ansistring; static; inline;
    class function ExecuteOperations(const ANewOps: TExecuteOperationsModel; AHandler: TExecuteOperationsModel.TOperationExecuteResultHandler; var errors: ansistring): boolean; static;
    class function ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount, ASignerAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TExecuteOperationsModel.TSendPASCMode; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TExecuteOperationsModel.TAccountSaleMode; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
  end;

  { TOrderedAccountKeysListHelper }

  TOrderedAccountKeysListHelper = class helper for TOrderedAccountKeysList
  public
    function GetBalance(IncludePending: boolean = False): TBalanceSummary;
    function GetAccounts(IncludePending: boolean = False): TArray<TAccount>;
    function GetAccountNumbers: TArray<cardinal>;
  end;

  { TSafeBoxHelper }

  TSafeBoxHelper = class helper for TPCSafeBox
  private
    function GetBalanceInternal(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary;
  public
    function GetModifiedAccounts(const AAccounts: array of TAccount): TArray<TAccount>;
    function GetBalance(const AKey: TAccountKey; IncludePending: boolean = False): TBalanceSummary; overload;
    function GetBalance(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary; overload;
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TNode
    function HasBestKnownBlockchainTip: boolean;
  end;

  { TAccountHelper }

  TAccountHelper = record helper for TAccount
    function GetAccountString : AnsiString;
    function GetDisplayString : AnsiString;
    function GetInfoText(const ABank : TPCBank) : utf8string;
    property AccountString : AnsiString read GetAccountString;
    property DisplayString : AnsiString read GetDisplayString;
  end;

  { TOperationResumeHelper }

  TOperationResumeHelper = record helper for TOperationResume
    function GetPrintableOPHASH : AnsiString;
    function GetInfoText(const ABank : TPCBank) : utf8string;
  end;

  { TTimeSpanHelper }

  TTimeSpanHelper = record helper for TTimeSpan
    function TotalBlockCount : Integer;
  end;

implementation

uses
  UMemory, UConst, UWallet, UECIES, UAES;

{ TOperationsManager }

class function TOperationsManager.SendPASCFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
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

class function TOperationsManager.UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LValid: boolean;
begin

  if (APayloadContent = '') then
    Exit(True);

  LValid := False;
  AErrorMessage := 'An Error Occured During Payload Encryption.';

  try

    case APayloadEncryptionMode of

      akaEncryptWithSender:
      begin
        // Use sender public key
        AEncodedPayloadBytes := ECIESEncrypt(ASenderPublicKey, APayloadContent);
        LValid := AEncodedPayloadBytes <> '';
      end;

      akaEncryptWithReceiver:
      begin
        // With destination public key
        AEncodedPayloadBytes := ECIESEncrypt(ADestinationPublicKey, APayloadContent);
        LValid := AEncodedPayloadBytes <> '';
      end;

      akaEncryptWithPassword:
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

      akaNotEncrypt:
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

class function TOperationsManager.OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
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

class function TOperationsManager.ExecuteOperations(const ANewOps: TExecuteOperationsModel; AHandler: TExecuteOperationsModel.TOperationExecuteResultHandler; var errors: ansistring): boolean;
begin

end;

class function TOperationsManager.GetOperationShortText(const OpType, OpSubType: DWord): ansistring;
begin
  case OpType of
    CT_PseudoOp_Reward: case OpSubType of
      0, CT_PseudoOpSubtype_Miner : result := 'Miner Reward';
      CT_PseudoOpSubtype_Developer : result := 'Developer Reward';
      else result := 'Unknown';
    end;
    CT_Op_Transaction: case OpSubType of
      CT_OpSubtype_TransactionSender: Result := 'Send';
      CT_OpSubtype_TransactionReceiver: Result := 'Receive';
      CT_OpSubtype_BuyTransactionBuyer: result := 'Buy Account Direct';
      CT_OpSubtype_BuyTransactionTarget: result := 'Purchased Account Direct';
      CT_OpSubtype_BuyTransactionSeller: result := 'Sold Account Direct';
      else result := 'Unknown';
    end;
    CT_Op_Changekey: Result := 'Change Key (legacy)';
    CT_Op_Recover: Result := 'Recover';
    CT_Op_ListAccountForSale: case OpSubType of
      CT_OpSubtype_ListAccountForPublicSale: result := 'For Sale';
      CT_OpSubtype_ListAccountForPrivateSale: result := 'Exclusive Sale';
      else result := 'Unknown';
    end;
    CT_Op_DelistAccount: result := 'Remove Sale';
    CT_Op_BuyAccount: case OpSubType of
      CT_OpSubtype_BuyAccountBuyer: result := 'Buy Account';
      CT_OpSubtype_BuyAccountTarget: result := 'Purchased Account';
      CT_OpSubtype_BuyAccountSeller: result := 'Sold Account';
      else result := 'Unknown';
    end;
    CT_Op_ChangeKeySigned: result :=  'Change Key';
    CT_Op_ChangeAccountInfo: result := 'Change Info';
    CT_Op_MultiOperation: case OpSubType of
      CT_OpSubtype_MultiOperation_Global: Result := 'Mixed-Transfer';
      CT_OpSubtype_MultiOperation_AccountInfo: Result := 'Mixed-Change';
    end;
    else result := 'Unknown';
  end;
end;

class function TOperationsManager.ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount, ASignerAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TExecuteOperationsModel.TSendPASCMode; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
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

    Exit(TOperationsManager.SendPASCFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalAmount, LTotalSignerFee, LOperationsHashTree, AErrorMessage));
  finally
    LOperationsHashTree.Free;
  end;

end;

class function TOperationsManager.ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
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

    Exit(TOperationsManager.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));
  finally
    LOperationsHashTree.Free;
  end;

end;

class function TOperationsManager.ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TExecuteOperationsModel.TAccountSaleMode; const APayloadEncryptionMode: TExecuteOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
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

    Exit(TOperationsManager.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));

  finally
    LOperationsHashTree.Free;
  end;
end;

{ TCoreTool }

class function TCoreTool.GetSignerCandidates(ANumOps: integer; ASingleOperationFee: int64; const ACandidates: array of TAccount): TArray<TAccount>;
var
  i, PoorSenderCount: integer;
  Fee, maxSignerFee, minSignerFee: int64;
  acc: TAccount;
begin
  //make deep copy of accounts!!! Very Important
  Result := TArrayTool<TAccount>.Copy(ACandidates);
  Fee := ASingleOperationFee;
  PoorSenderCount := 0;
  for i := Low(Result) to High(Result) do
  begin
    acc := Result[i];
    if (acc.Balance < Fee) then
      Inc(PoorSenderCount);
  end;

  maxSignerFee := ANumOps * Fee;
  minSignerFee := maxSignerFee - (PoorSenderCount * Fee);

  for i := High(Result) downto Low(Result) do
  begin
    acc := Result[i];
    if not (acc.Balance >= maxSignerFee) then
      TArrayTool<TAccount>.RemoveAt(Result, i);
  end;
end;

{ TNodeHelper }

function TNodeHelper.HasBestKnownBlockchainTip: boolean;
var
  LReady: boolean;
  LMsg: ansistring;
begin
  LReady := Self.Bank.IsReady(LMsg);
  if LReady and TNetData.NetData.IsGettingNewBlockChainFromClient then
    Result := Self.Bank.BlocksCount = TNetData.NetData.MaxRemoteOperationBlock.block;
end;

{ TOrderedAccountKeysListHelper }

function TOrderedAccountKeysListHelper.GetBalance(IncludePending: boolean = False): TBalanceSummary;
var
  i: integer;
  LAccs: TArray<TAccount>;
begin
  Result := CT_BalanceSummary_Nil;
  LAccs := Self.GetAccounts(IncludePending);
  for i := Low(LAccs) to High(LAccs) do
  begin
    Inc(Result.TotalPASA);
    Inc(Result.TotalPASC, LAccs[i].Balance);
  end;
end;

function TOrderedAccountKeysListHelper.GetAccounts(IncludePending: boolean = False): TArray<TAccount>;
var
  i, j: integer;
  LAccs: TList<TAccount>;
  LAcc: TAccount;
  LList: TOrderedCardinalList;
  Disposables: TDisposables;
begin
  LAccs := Disposables.AddObject(TList<TAccount>.Create) as TList<TAccount>;
  for i := 0 to Self.Count - 1 do
  begin
    LList := Self.AccountKeyList[i];
    for j := 0 to LList.Count - 1 do
    begin
      if IncludePending then
        LAcc := TNode.Node.Operations.SafeBoxTransaction.Account(j)
      else
        LAcc := TNode.Node.Bank.SafeBox.Account(LList.Get(j));
      LAccs.Add(LAcc);
    end;
  end;
  Result := LAccs.ToArray;
end;

function TOrderedAccountKeysListHelper.GetAccountNumbers: TArray<cardinal>;
var
  i, j: integer;
  LAccs: TList<cardinal>;
  LList: TOrderedCardinalList;
  Disposables: TDisposables;
begin
  LAccs := Disposables.AddObject(TList<cardinal>.Create) as TList<cardinal>;
  for i := 0 to Self.Count - 1 do
  begin
    LList := Self.AccountKeyList[i];
    for j := 0 to LList.Count - 1 do
      LAccs.Add(j);
  end;
  Result := LAccs.ToArray;
end;

{ TSafeBoxHelper }

function TSafeBoxHelper.GetModifiedAccounts(const AAccounts: array of TAccount): TArray<TAccount>;
var
  i: integer;
  LChanged: TList<TAccount>;
  LAcc: TAccount;
  GC: TDisposables;
begin
  LChanged := GC.AddObject(TList<TAccount>.Create) as TList<TAccount>;
  for i := Low(AAccounts) to High(AAccounts) do
  begin
    LAcc := Self.Account(AAccounts[i].account);
    if (LAcc.n_Operation <> AAccounts[i].n_operation) or (LAcc.Balance <> AAccounts[i].balance) then
      LChanged.Add(LAcc);
  end;
  Result := LChanged.ToArray;
end;

function TSafeBoxHelper.GetBalance(const AKey: TAccountKey; IncludePending: boolean = False): TBalanceSummary;
begin
  Result := GetBalance([AKey], IncludePending);
end;

function TSafeBoxHelper.GetBalance(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary;
begin
  Result := GetBalanceInternal(AKeys, IncludePending);
end;

function TSafeBoxHelper.GetBalanceInternal(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary;
var
  i: integer;
  LAcc: TAccount;
  LAccs: THashSet<TAccount>;
  LKeys: THashSet<TAccountKey>;
  GC: TDisposables;
begin
  // Setup local collections
  LAccs := GC.AddObject(THashSet<TAccount>.Create(TAccountEqualityComparer.Create)) as THashSet<TAccount>;
  LKeys := GC.AddObject(THashSet<TAccountKey>.Create(TAccountKeyEqualityComparer.Create)) as THashSet<TAccountKey>;

  // Gather all keys into hashset
  for i := Low(AKeys) to High(AKeys) do
    LKeys.Add(AKeys[i]);

  // Gather all referenced accounts
  for i := 0 to Self.AccountsCount - 1 do
  begin
    LAcc := Self.Account(i);
    if LKeys.Contains(LAcc.accountInfo.accountKey) then
      LAccs.Add(LAcc);
  end;

  // Build the results
  Result := CT_BalanceSummary_Nil;
  for LAcc in LAccs do
  begin
    Inc(Result.TotalPASA);
    Inc(Result.TotalPASC, LAcc.Balance);
  end;
end;

{ TAccountComparer }

function TAccountComparer.Compare(constref ALeft, ARight: TAccount): integer;
begin
  Result := TAccountComparer.DoCompare(ALeft, ARight);
end;

class function TAccountComparer.DoCompare(constref ALeft, ARight: TAccount): integer;
begin
  Result := TCompare.UInt64(ALeft.account, ARight.account);
end;

{ TAccountEqualityComparer }

function TAccountEqualityComparer.Equals(constref ALeft, ARight: TAccount): boolean;
begin
  Result := TAccountEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountEqualityComparer.GetHashCode(constref AValue: TAccount): UInt32;
begin
  Result := TAccountEqualityComparer.CalcHashCode(AValue);
end;

class function TAccountEqualityComparer.AreEqual(constref ALeft, ARight: TAccount): boolean;
begin
  Result :=
    (ALeft.account = ARight.account) and
    (ALeft.balance = ARight.balance) and
    (ALeft.updated_block = ARight.updated_block) and
    (ALeft.n_operation = ARight.n_operation) and
    TAccountKeyEqualityComparer.AreEqual(ALeft.accountInfo.accountKey, ARight.accountInfo.accountKey);
end;

class function TAccountEqualityComparer.CalcHashCode(constref AValue: TAccount): UInt32;
begin
  Result := AValue.account;
end;

{ TAccountKeyComparer }

function TAccountKeyComparer.Compare(constref ALeft, ARight: T): integer;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight);
end;

class function TAccountKeyComparer.DoCompare(constref ALeft, ARight: TAccountKey): integer;
begin
  Result := BinStrComp(ALeft.x, ARight.x);
  if Result = 0 then
    Result := BinStrComp(ALeft.y, ARight.y);
end;

{ TAccountKeyEqualityComparer }

function TAccountKeyEqualityComparer.Equals(constref ALeft, ARight: TAccountKey): boolean;
begin
  Result := TAccountKeyEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountKeyEqualityComparer.GetHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TAccountKeyEqualityComparer.CalcHashCode(AValue);
end;

class function TAccountKeyEqualityComparer.AreEqual(constref ALeft, ARight: TAccountKey): boolean;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight) = 0;
end;

class function TAccountKeyEqualityComparer.CalcHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TEqualityComparer<ansistring>.Default.GetHashCode(IntToStr(AValue.EC_OpenSSL_NID) + AValue.x + AValue.y);
end;

{ TAccountHelper }

function TAccountHelper.GetAccountString: ansistring;
begin
  Result := TAccountComp.AccountNumberToAccountTxtNumber(Self.account);
end;

function TAccountHelper.GetDisplayString: ansistring;
begin
  Result := GetAccountString;
  if Self.Name <> '' then
    Result := Result + ': ' + Self.Name;
end;

function TAccountHelper.GetInfoText(const ABank: TPCBank): utf8string;
var
  builder: TStrings;
  GC: TDisposables;
begin
  builder := GC.AddObject(TStringList.Create) as TStrings;
  builder.Append(Format('Account: %s %s Type:%d', [TAccountComp.AccountNumberToAccountTxtNumber(self.account), IIF(Self.Name <> '', 'Name: ' + Self.Name, ''), Self.account_type]));
  builder.Append('');
  builder.Append(Format('Current balance: %s', [TAccountComp.FormatMoney(Self.balance)]));
  builder.Append('');
  builder.Append(Format('Updated on block: %d  (%d blocks ago)', [Self.updated_block, ABank.BlocksCount - Self.updated_block]));
  builder.Append(Format('Public key type: %s', [TAccountComp.GetECInfoTxt(Self.accountInfo.accountKey.EC_OpenSSL_NID)]));
  builder.Append(Format('Base58 Public key: %s', [TAccountComp.AccountPublicKeyExport(Self.accountInfo.accountKey)]));
  if TAccountComp.IsAccountForSale(Self.accountInfo) then
  begin
    builder.Append('');
    builder.Append('** Account is for sale: **');
    builder.Append(Format('Price: %s', [TAccountComp.FormatMoney(Self.accountInfo.price)]));
    builder.Append(Format('Seller account (where to pay): %s', [TAccountComp.AccountNumberToAccountTxtNumber(Self.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForSaleAcceptingTransactions(Self.accountInfo) then
    begin
      builder.Append('');
      builder.Append('** Private sale **');
      builder.Append(Format('New Base58 Public key: %s', [TAccountComp.AccountPublicKeyExport(Self.accountInfo.new_publicKey)]));
      builder.Append('');
      if TAccountComp.IsAccountLocked(Self.accountInfo, ABank.BlocksCount) then
        builder.Append(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount, Self.accountInfo.locked_until_block - ABank.BlocksCount]))
      else
        builder.Append(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount]));
    end;
  end;
  Result := builder.Text;
end;

{ TOperationResumeHelper }

function TOperationResumeHelper.GetPrintableOPHASH: ansistring;
begin
  Result := TCrypto.ToHexaString(Self.OperationHash);
end;

function TOperationResumeHelper.GetInfoText(const ABank: TPCBank): utf8string;
var
  builder: TStrings;
  GC: TDisposables;
begin
  if (not Self.valid) then
    exit;
  builder := GC.AddObject(TStringList.Create) as TStrings;
  if Self.Block < ABank.BlocksCount then
    if (Self.NOpInsideBlock >= 0) then
      builder.Add(Format('Block: %d/%d', [Self.Block, Self.NOpInsideBlock]))
    else
    begin
      builder.Add(Format('Block: %d', [Self.Block]));
    end
  else
    builder.Add('** Pending operation not included on blockchain **');
  builder.Add(Format('%s', [Self.OperationTxt]));
  builder.Add(Format('OpType:%d Subtype:%d', [Self.OpType, Self.OpSubtype]));
  builder.Add(Format('Operation Hash (ophash): %s', [TCrypto.ToHexaString(Self.OperationHash)]));
  if (Self.OperationHash_OLD <> '') then
    builder.Add(Format('Old Operation Hash (old_ophash): %s', [TCrypto.ToHexaString(Self.OperationHash_OLD)]));
  if (Self.OriginalPayload <> '') then
  begin
    builder.Add(Format('Payload length:%d', [length(Self.OriginalPayload)]));
    if Self.PrintablePayload <> '' then
      builder.Add(Format('Payload (human): %s', [Self.PrintablePayload]));
    builder.Add(Format('Payload (Hexadecimal): %s', [TCrypto.ToHexaString(Self.OriginalPayload)]));
  end;
  if Self.Balance >= 0 then
    builder.Add(Format('Final balance: %s', [TAccountComp.FormatMoney(Self.Balance)]));
  Result := builder.Text;
end;

{ TTimeSpanHelper }

function TTimeSpanHelper.TotalBlockCount: integer;
begin
  Result := Round(Self.TotalSeconds / CT_NewLineSecondsAvg);
end;

end.
