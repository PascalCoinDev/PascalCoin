unit UCoreUtils;

{ Copyright (c) 2018 by PascalCoin Project

  Contains common types for Core module.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com>: unit creator
    Ugochukwu Mmaduekwe - added TOperationsManager Class
}

{$mode delphi}

interface

uses
  Classes, SysUtils, UCrypto, UAccounts, UBlockChain, UOpTransaction, UNode, UCommon, UNetProtocol,
  Generics.Collections, Generics.Defaults, UCoreObjects, UWIZModels, Forms, Dialogs, LCLType;

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
    class function GetSignerCandidates(ANumOps: integer; ASingleOperationFee: Int64; const ACandidates: array of TAccount): TArray<TAccount>; static;
  end;

  { TOperationsManager }

  TOperationsManager = class
  private
    class function SendPASCFinalizeAndDisplayMessage(AOperationsTxt: string; const AOperationToString: string; ANoOfOperations: Integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
    class function UpdateSendPASCPayload(ASenderAccount, ADestinationAccount: TAccount; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
  public
    class function ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; ADestinationAccount, ASignerAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TWIZOperationsModel.TSendPASCMode; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class procedure ExecuteChangeKey(); static;
    class procedure ExecuteEnlistAccountForSale(); static;
  end;

  { TSafeBoxHelper }

  TSafeBoxHelper = class helper for TPCSafeBox
  private
    function GetKeysSummaryInternal(UseFilter: boolean; const AFilterKeys: array of TAccountKey; FetchAccounts: boolean = False): TUserSummary;
  public
    function GetModifiedAccounts(const AAccounts: array of TAccount): TArray<TAccount>;
    function GetKeySummary(const AKey: TAccountKey; FetchAccounts: boolean = False): TKeySummary;
    function GetUserSummary(const AKeys: array of TAccountKey; FetchAccounts: boolean = False): TUserSummary;
    function GetSummaryAllKeys: TUserSummary;
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

class function TOperationsManager.SendPASCFinalizeAndDisplayMessage(
  AOperationsTxt: string; const AOperationToString: string;
  ANoOfOperations: Integer; ATotalAmount, ATotalFee: int64;
  AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
var
  auxs: string;
  i: integer;
begin
  if (ANoOfOperations > 1) then
  begin
    auxs := 'Total amount that dest will receive: ' + TAccountComp.FormatMoney(
      ATotalAmount) + #10;
    if Application.MessageBox(
      PChar('Execute ' + IntToStr(ANoOfOperations) +
      ' operations?' + #10 + 'Operation: ' + AOperationsTxt + #10 +
      auxs + 'Total fee: ' + TAccountComp.FormatMoney(ATotalFee) +
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
    AOperationsTxt := 'Successfully executed ' + IntToStr(i) +
      ' operations!' + #10 + #10 + AOperationToString;
    if i > 1 then
      ShowMessage(AOperationsTxt)
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
    AOperationsTxt := 'One or more of your operations has not been executed:' +
      #10 + 'Errors:' + #10 + AErrorMessage + #10 + #10 +
      'Total successfully executed operations: ' + IntToStr(i);
    ShowMessage(AOperationsTxt);
  end
  else
    Result := False;
end;

class function TOperationsManager.UpdateSendPASCPayload(ASenderAccount, ADestinationAccount: TAccount; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LValid: boolean;
  LWorkingAccount: TAccount;
begin

  if (APayloadContent = '') then
    Exit(True);

  LValid := False;
  AErrorMessage := 'An Error Occured During Payload Encryption.';

  try

    case APayloadEncryptionMode of

      akaEncryptWithSender:
      begin
        // Use sender account public key
        LWorkingAccount := ASenderAccount;
        AEncodedPayloadBytes := ECIESEncrypt(LWorkingAccount.accountInfo.accountKey, APayloadContent);
        LValid := AEncodedPayloadBytes <> '';
      end;

      akaEncryptWithReceiver:
      begin
        // With destination account public key
        LWorkingAccount := ADestinationAccount;
        AEncodedPayloadBytes := ECIESEncrypt(LWorkingAccount.accountInfo.accountKey, APayloadContent);
        LValid := AEncodedPayloadBytes <> '';
      end;

      akaEncryptWithPassword:
      begin
        // With defined password
        if APayloadEncryptionPassword = '' then
        begin
          AErrorMessage := 'Payload Encryption Password Cannot be Empty with the Chosen Option : "Encrypt With Password."';
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
        AErrorMessage := Format('Payload Size is %d Which is Bigger Than %d', [Length(AEncodedPayloadBytes), CT_MaxPayloadSize]);
      end;
    Result := LValid;
  end;
end;

class function TOperationsManager.ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; ADestinationAccount, ASignerAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TWIZOperationsModel.TSendPASCMode; const APayloadEncryptionMode: TWIZOperationsModel.TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalAmount, LTotalFee, LAmount, LFee: int64;
  LDoOperation: boolean;
  LOperationsTxt, LOperationToString: string;
  LIdx, LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
begin
  if Length(ASelectedAccounts) = 0 then
  begin
    AErrorMessage := 'No Sender Account Found';
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
    LTotalFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';
    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if LCurrentAccount.account = ADestinationAccount.account then
      begin
        AErrorMessage := Format('Sender "%s" and Destination "%s" Accounts are the Same', [LCurrentAccount.AccountString, ADestinationAccount.AccountString]);
        Exit(False);
      end;

      if not UpdateSendPASCPayload(LCurrentAccount, ADestinationAccount, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Sender Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      LIdx := LWalletKeys.IndexOfAccountKey(LCurrentAccount.accountInfo.accountKey);
      if LIdx < 0 then
      begin
        AErrorMessage := Format('Sender Account "%s" Private Key Not Found In Wallet', [LCurrentAccount.AccountString]);
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
              AErrorMessage := Format('Insufficient Funds in "%s". %s < (%s + %s = %s)', [LCurrentAccount.AccountString, TAccountComp.FormatMoney(LCurrentAccount.balance), TAccountComp.FormatMoney(AAmount), TAccountComp.FormatMoney(AFee), TAccountComp.FormatMoney(AAmount + AFee)]);
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
          Inc(LTotalAmount, LAmount);
          Inc(LTotalFee, LFee);
          Inc(LNoOfOperations);
          LOperationsTxt := Format('Transaction to "%s"', [ADestinationAccount.AccountString]);

          if Assigned(LPCOperation) then
          begin
            LOperationsHashTree.AddOperationToHashTree(LPCOperation);
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
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TOperationsManager.SendPASCFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalAmount, LTotalFee, LOperationsHashTree, AErrorMessage));
  finally
    LOperationsHashTree.Free;
  end;

end;

class procedure TOperationsManager.ExecuteChangeKey();
begin

end;

class procedure TOperationsManager.ExecuteEnlistAccountForSale();
begin

end;

{ TCoreTool }

class function TCoreTool.GetSignerCandidates(ANumOps: integer;
  ASingleOperationFee: Int64; const ACandidates: array of TAccount): TArray<TAccount>;
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

function TSafeBoxHelper.GetKeySummary(const AKey: TAccountKey; FetchAccounts: boolean = False): TKeySummary;
var
  AKeysResult: TUserSummary;
begin
  AKeysResult := GetUserSummary([AKey], FetchAccounts);
  if Length(AKeysResult.Items) = 0 then
  begin
    Result := CT_KeySummary_Nil;
    exit;
  end;
  Result := AKeysResult.Items[0];
end;

function TSafeBoxHelper.GetUserSummary(const AKeys: array of TAccountKey; FetchAccounts: boolean = False): TUserSummary;
begin
  Result := GetKeysSummaryInternal(True, AKeys, FetchAccounts);
end;

function TSafeBoxHelper.GetSummaryAllKeys: TUserSummary;
begin
  Result := GetKeysSummaryInternal(False, [], False);
end;

function TSafeBoxHelper.GetKeysSummaryInternal(UseFilter: boolean; const AFilterKeys: array of TAccountKey; FetchAccounts: boolean = False): TUserSummary;
type
  __TList_TAccount = TList<TAccount>;
  __TPair_TAccountKey_TList_TAccount = TPair<TAccountKey, __TList_TAccount>;
  __TObjectDictionary_TAccountKey_TList_TAccount = TObjectDictionary<TAccountKey, __TList_TAccount>;
var
  i, j: integer;
  LAcc: TAccount;
  LAccs: TSortedHashSet<TAccount>;
  LKey: TAccountKey;
  LValue: __TList_TAccount;
  safeBox: TPCSafeBox;
  GC: TDisposables;
  LKeys: __TObjectDictionary_TAccountKey_TList_TAccount;
  LPair: __TPair_TAccountKey_TList_TAccount;
begin
  // Setup local dictionary key -> account[]
  LAccs := GC.AddObject(TSortedHashSet<TAccount>.Create(TAccountComparer.Create, TAccountEqualityComparer.Create)) as TSortedHashSet<TAccount>;
  LKeys := GC.AddObject(__TObjectDictionary_TAccountKey_TList_TAccount.Create([doOwnsValues], TAccountKeyEqualityComparer.Create)) as __TObjectDictionary_TAccountKey_TList_TAccount;

  if UseFilter then
  begin
    for i := Low(AFilterKeys) to High(AFilterKeys) do
      LKeys.Add(AFilterKeys[i], __TList_TAccount.Create);

    for i := 0 to Self.AccountsCount - 1 do
    begin
      LAcc := Self.Account(i);
      if LKeys.TryGetValue(LAcc.accountInfo.accountKey, LValue) then
        LValue.Add(LAcc);
    end;
  end
  else
    for i := 0 to Self.AccountsCount - 1 do
    begin
      LAcc := Self.Account(i);
      if not LKeys.TryGetValue(LAcc.accountInfo.accountKey, LValue) then
      begin
        LValue := __TList_TAccount.Create;
        LKeys.Add(LAcc.accountInfo.accountKey, LValue);
      end;
      LValue.Add(LAcc);
    end;

  // Build the results
  SetLength(Result.Items, LKeys.Count);
  i := 0;
  for LPair in LKeys do
  begin
    Result.Items[i].Key := CT_AccountInfo_NUL.accountKey;
    Result.Items[i].TotalPASA := 0;
    Result.Items[i].TotalPASC := 0;
    SetLength(Result.Items[i].Accounts, 0);
    for j := 0 to LPair.Value.Count - 1 do
    begin
      LAcc := LPair.Value[j];
      Inc(Result.Items[i].TotalPASA);
      Inc(Result.Items[i].TotalPASC, LAcc.balance);
    end;
    if FetchAccounts then
    begin
      Result.Items[i].Accounts := LPair.Value.ToArray;
      LAccs.AddRange(Result.Items[i].Accounts);
    end;
    Inc(i);
  end;
  Result := CT_UserSummary_Nil;
  Result.Keys := TArrayTool<TAccountKey>.Copy(AFilterKeys);
  if FetchAccounts then
    Result.Accounts := LAccs.ToArray;
  Result.TotalPASA := LAccs.Count;
  for LAcc in LAccs do
    Inc(Result.TotalPASC, LAcc.Balance);
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
