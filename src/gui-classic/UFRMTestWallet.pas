unit UFRMTestWallet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UFRMWallet, Vcl.Menus, Vcl.ExtCtrls,
  UFrameMessages, UFrameNodeStats, UFrameLogs, UFrameOperationsExplorer,
  UFrameBlockExplorer, UFramePendingOperations, UFrameAccountExplorer,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  {$IFNDEF FPC}
  System.Generics.Collections
  {$ELSE}
  Generics.Collections
  {$ENDIF};

type
  TFRMTestWallet = class(TFRMWallet)
    procedure FormCreate(Sender: TObject);
  private
    procedure Test_ShowDiagnosticTool(Sender: TObject);

    { Private declarations }
    Procedure InitMenuForTesting;
    {$IFDEF TESTNET}
    Procedure Test_RandomOperations(Sender: TObject);
    {$IFDEF TESTING_NO_POW_CHECK}
    Procedure Test_CreateABlock(Sender: TObject);
    {$ENDIF}
    Procedure Test_ConnectDisconnect(Sender: TObject);
    {$ENDIF}
//    Procedure Test_ShowPublicKeys(Sender: TObject);
    Procedure Test_ShowOperationsInMemory(Sender: TObject);
    Procedure Test_FindAccountsForPrivateBuyOrSwapToMe(Sender : TObject);

  public
    { Public declarations }
  end;

var
  FRMTestWallet: TFRMTestWallet;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

Uses
  UFolderHelper,
  {$IFDEF USE_GNUGETTEXT}gnugettext,{$ENDIF}
  {$IFDEF Use_OpenSSL}
  UOpenSSL,
  {$ENDIF}
  ULog, Grids, UAppParams, UBlockChain,
  UNode, UGridUtils, UJSONFunctions, UAccounts, UNetProtocol,
  UCrypto, Buttons, UPoolMining, URPC, UFRMAccountSelect, UConst,
  UAccountKeyStorage, UBaseTypes, UPCDataTypes, UOrderedList,
  UFRMRPCCalls, UTxMultiOperation, USettings, UEPasa,
  UTime,
  UFileStorage,
  UThread,
  UOpTransaction,
  UFRMPascalCoinWalletConfig,
  UFRMOperationsExplorer,
  {$IFDEF TESTNET}
  UFRMRandomOperations,
  UFRMDiagnosticTool,
  {$ENDIF}
  UPCTNetDataExtraMessages,
  UFRMAskForAccount,
  UAbstractBTree, UEPasaDecoder,
  UFRMAbout,
  UFRMOperation,
  UFRMWalletKeys,
  UFRMPayloadDecoder,
  UFRMNodesIp,
  UFRMMemoText,
  UCommon,
  UPCOrderedLists,
  UWallet;

procedure TFRMTestWallet.InitMenuForTesting;
var mi : TMenuItem;
begin
{$IFDEF TESTNET}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='-';
  miAbout.Add(mi);
  {$IFDEF TESTING_NO_POW_CHECK}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Create a block';
  mi.OnClick:=Test_CreateABlock;
  {$IFnDEF FPC}
  mi.ShortCut := TextToShortCut('CTRL+B');
  {$ENDIF}
  miAbout.Add(mi);
  {$ENDIF}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Connect/Disconnect';
  mi.OnClick:=Test_ConnectDisconnect;
  {$IFnDEF FPC}
  mi.ShortCut := TextToShortCut('CTRL+D');
  {$ENDIF}
  miAbout.Add(mi);

  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Create Random operations';
  mi.OnClick:=Test_RandomOperations;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Diagnostic Tool';
  mi.OnClick:=Test_ShowDiagnosticTool;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Show public keys state';
  mi.OnClick:=Test_ShowPublicKeys;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Show operations in memory';
  mi.OnClick:=Test_ShowOperationsInMemory;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Search accounts for private or swap to me';
  mi.OnClick:=Test_FindAccountsForPrivateBuyOrSwapToMe;
  miAbout.Add(mi);
{$ELSE}
{$ENDIF}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='-';
  MiOperations.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Ask for Free Account';
  mi.OnClick:=miAskForAccountClick;
  MiOperations.Add(mi);
end;

{$IFDEF TESTING_NO_POW_CHECK}
procedure TFRMTestWallet.Test_CreateABlock(Sender: TObject);
var ops, mempoolOps : TPCOperationsComp;
  nba : TBlockAccount;
  errors : String;

begin
  {$IFDEF TESTNET}
  ops := TPCOperationsComp.Create(Nil);
  Try
    ops.bank := FNode.Bank;
    mempoolOps := FNode.LockMempoolRead;
    try
      ops.CopyFrom(mempoolOps);
    finally
      FNode.UnlockMempoolRead;
    end;
    ops.BlockPayload.FromString(IntToStr(FNode.Bank.BlocksCount));
    ops.nonce := FNode.Bank.BlocksCount;
    ops.UpdateTimestamp;
    FNode.AddNewBlockChain(Nil,ops,errors);
  finally
    ops.Free;
  end;
  {$ELSE}
  Raise Exception.Create('NOT ALLOWED!');
  {$ENDIF}
end;
{$ENDIF}



{$IFDEF TESTNET}

procedure TFRMTestWallet.Test_ConnectDisconnect(Sender: TObject);
begin
  TNetData.NetData.NetConnectionsActive := Not TNetData.NetData.NetConnectionsActive;
  Exit;
  if FNode.NetServer.Active then begin
    FNode.NetServer.Active := False;
  end else begin
    FNode.NetServer.Active := True;
  end;
end;

procedure TFRMTestWallet.Test_RandomOperations(Sender: TObject);
Var FRM : TFRMRandomOperations;
begin
  FRM := TFRMRandomOperations.Create(Self);
  Try
    FRM.SourceNode := FNode;
    FRM.SourceWalletKeys := FWalletKeys;
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

{$ENDIF}

procedure TFRMTestWallet.Test_ShowOperationsInMemory(Sender: TObject);
var LFRM : TFRMMemoText;
  i, nOps : Integer;
  Lslist : TStrings;
begin
  Lslist := TStringList.Create;
  try
    TPCOperationsStorage.PCOperationsStorage.GetStats(Lslist);
    nOps := TPCOperationsStorage.PCOperationsStorage.Count;
    LFRM := TFRMMemoText.Create(Self);
    try
      LFRM.InitData('Operations in Memory '+IntToStr(nOps),Lslist.Text);
      LFRM.ShowModal;
    finally
      LFRM.Free;
    end;
  finally
    Lslist.Free;
  end;
end;

(*

// Skybuck: Code is broken ?

procedure TFRMTestWallet.Test_ShowPublicKeys(Sender: TObject);
var F : TFRMMemoText;
  i : Integer;
  sl : TStrings;
  ak : TAccountKey;
  nmin,nmax : Integer;
  l : TList<Pointer>;
  Pacsd : PAccountKeyStorageData;
  acc : TAccount;
begin
  sl := TStringList.Create;
  try
    for i:=0 to FNode.Bank.SafeBox.AccountsCount-1 do begin
      acc := FNode.Bank.SafeBox.Account(i);
      if acc.accountInfo.new_publicKey.EC_OpenSSL_NID<>0 then begin
        sl.Add(Format('Account %d new public key %d %s',[acc.account,
          acc.accountInfo.new_publicKey.EC_OpenSSL_NID,
          TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(acc.accountInfo.new_publicKey))]));
      end;
    end;
    {$IFnDEF USE_ABSTRACTMEM}
    l := TAccountKeyStorage.KS.LockList;
    try
      sl.Add(Format('%d public keys in TAccountKeyStorage data',[l.count]));
      for i:=0 to l.count-1 do begin
        Pacsd := l[i];
        if (Pacsd^.counter<=0) then begin
          sl.Add(Format('%d/%d public keys counter %d',[i+1,l.count,Pacsd^.counter]));
        end;
        if FNode.Bank.SafeBox.OrderedAccountKeysList.IndexOfAccountKey(Pacsd^.ptrAccountKey^)<0 then begin
          sl.Add(Format('%d/%d public keys counter %d Type %d NOT FOUND %s',[i+1,l.count,Pacsd^.counter,
          Pacsd^.ptrAccountKey^.EC_OpenSSL_NID,
          TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(Pacsd^.ptrAccountKey^))]));
        end;
      end;
    finally
      TAccountKeyStorage.KS.UnlockList;
    end;
    sl.Add(Format('%d public keys in %d accounts',[FNode.Bank.SafeBox.OrderedAccountKeysList.Count,FNode.Bank.Safebox.AccountsCount]));
    for i:=0 to FNode.Bank.SafeBox.OrderedAccountKeysList.Count-1 do begin
      ak := FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKey[i];
      if ( FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Count > 0) then begin
        nmin := FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Get(0);
        nmax := FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Get( FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Count-1 );
      end else begin
        nmin := -1; nmax := -1;
      end;
      sl.Add(Format('%d/%d %d accounts (%d to %d) for key type %d %s',[
        i+1,FNode.Bank.SafeBox.OrderedAccountKeysList.Count,
        FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Count,
        nmin,nmax,
        ak.EC_OpenSSL_NID,
        TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(ak)) ]));
    end;
    {$ENDIF}
    F := TFRMMemoText.Create(Self);
    try
      F.InitData('Keys in safebox',sl.Text);
      F.ShowModal;
    finally
      F.Free;
    end;
  finally
    sl.Free;
  end;
end;

*)

procedure TFRMTestWallet.Test_FindAccountsForPrivateBuyOrSwapToMe(Sender: TObject);
{ This procedure will search in Safebox all accounts in "for_private_sale" state
  or in "for_account_swap" that can be self-signed using one of my private keys
  }

  function CaptureSender0Coins(var AAccountSender0Coins : TAccount; var ANeededWalletKey : TWalletKey) : Boolean;
  var ii : Integer;
  begin
    //
    Result := False;
    for ii := 0 to WalletKeys.AccountsKeyList.Count-1 do begin
      if WalletKeys.AccountsKeyList.AccountKeyList[ii].Count>0 then begin
        if WalletKeys.TryGetKey(WalletKeys.AccountsKeyList.AccountKey[ii],ANeededWalletKey) then begin
          AAccountSender0Coins := FNode.Bank.SafeBox.Account( WalletKeys.AccountsKeyList.AccountKeyList[ii].Get(0) );
          Result := True;
        end;
      end;
    end;

  end;


var i : Integer;
  LLines : TStrings;
  LAccount, LAccountSender0Coins : TAccount;
  LAccountOpDesc : String;
  LCountAccountsFound_total, LCountAccountsFound_Operation : Integer;
  LNeededWalletKey : TWalletKey;
  s : String;
  LOpTransaction : TOpTransaction;
  LOperationsHashTree, LGlobalOperationsHashTree : TOperationsHashTree;
  LStream : TStream;
  LRaw : TRawBytes;
  LFRM : TFRMMemoText;
  LOpPayload : TOperationPayload;
begin
  if Not WalletKeys.IsValidPassword then raise Exception.Create('Your wallet keys are locked');
  LOpPayload := CT_TOperationPayload_NUL;
  if InputQuery('Search ATOMIC SWAP by SECRET','Insert SECRET value (use 0x... for Hexadecimal, otherwise will be a String)',s) then begin
    if s.StartsWith('0x') then begin
      if not UCommon.TryHex2Bytes(s,LOpPayload.payload_raw) then raise Exception.Create('SECRET value is not an Hexadecimal'+#10+s);
    end else begin
      LOpPayload.payload_raw.FromString(s);
    end;
  end;
  LCountAccountsFound_total := 0;
  LCountAccountsFound_Operation := 0;
  LLines := TStringList.Create;
  LGlobalOperationsHashTree := TOperationsHashTree.Create;
  try
    for i:=0 to FNode.Bank.SafeBox.AccountsCount-1 do begin
      LAccountOpDesc := '';
      LAccount := FNode.Bank.SafeBox.Account(i);

      LOpTransaction := Nil;
      Try
        Case LAccount.accountInfo.state of
          as_ForSale : begin
            if Not TAccountComp.IsNullAccountKey( LAccount.accountInfo.new_publicKey ) then begin
              if Not WalletKeys.TryGetKey(LAccount.accountInfo.new_publicKey,LNeededWalletKey) then Continue;
              if Not Assigned(LNeededWalletKey.PrivateKey) then Continue; // Key not available!
            end else Continue;
            // Private sale to me
            // Is in time?
            if TAccountComp.IsAccountLocked(LAccount.accountInfo,FNode.Bank.BlocksCount) then begin
              //
              if LAccount.balance>=LAccount.accountInfo.price then begin
                LAccountOpDesc := Format('Account %s is for private sale to me and with enough balance to pay %s (balance %s)',[
                  TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),
                  TAccountComp.FormatMoney(LAccount.accountInfo.price),
                  TAccountComp.FormatMoney(LAccount.balance)]);
                // No key needed... just a transaction SELF SIGNED
                LOpTransaction := TOpBuyAccount.CreateBuy(FNode.Bank.SafeBox.CurrentProtocol,
                    LAccount.account, LAccount.n_operation+1,
                    LAccount.account, LAccount.accountInfo.account_to_pay, LAccount.accountInfo.price,
                    0,0,
                    LAccount.accountInfo.new_publicKey,
                    LNeededWalletKey.PrivateKey,
                    LOpPayload);
              end else begin
                LAccountOpDesc := Format('Account %s is for private sale to me but needs a Buy operation paying %s PASC (%s pending)',[
                   TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),
                   TAccountComp.FormatMoney(LAccount.accountInfo.price),
                   TAccountComp.FormatMoney(LAccount.accountInfo.price - LAccount.balance)]);
              end;
            end else begin
              LAccountOpDesc := Format('Account %s is for private sale to me but is out-of-lock period',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account)]);
            end;
          end;
          as_ForAtomicAccountSwap : begin
            if Not WalletKeys.TryGetKey(LAccount.accountInfo.new_publicKey,LNeededWalletKey) then Continue;
            if TAccountComp.IsAccountLocked(LAccount.accountInfo,FNode.Bank.BlocksCount) then begin
              if TAccountComp.IsValidAccountInfoHashLockKey(LAccount.accountInfo,LOpPayload.payload_raw) then begin
                // Atomic Account swap using provided SECRET
                LAccountOpDesc := Format('Account %s is for Atomic Account Swap to me using SECRET %s',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
                //
                // No key needed... just a Buy Operation SELF SIGNED
                LOpTransaction := TOpBuyAccount.CreateBuy(FNode.Bank.SafeBox.CurrentProtocol,
                    LAccount.account, LAccount.n_operation+1,
                    LAccount.account, LAccount.accountInfo.account_to_pay, LAccount.accountInfo.price,
                    0,0,
                    LAccount.accountInfo.new_publicKey,
                    LNeededWalletKey.PrivateKey,
                    LOpPayload);
              end else begin
                LAccountOpDesc := Format('Account %s is for Atomic Account Swap to me but SECRET %s is not valid',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
              end;
            end else begin
              LAccountOpDesc := Format('Account %s is for Atomic Account Swap to me but is out-of-lock period',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account)]);
            end;

          end;
          as_ForAtomicCoinSwap : begin
            if Not TAccountComp.IsValidAccountInfoHashLockKey(LAccount.accountInfo,LOpPayload.payload_raw) then Continue;
            // Atomic Coin swap using provided SECRET
            if TAccountComp.IsAccountLocked(LAccount.accountInfo,FNode.Bank.BlocksCount) then begin
              // Single transaction using amount 0 from ANY sender
              if CaptureSender0Coins(LAccountSender0Coins,LNeededWalletKey) then begin
                // Atomic Account swap using provided SECRET
                LAccountOpDesc := Format('Account %s is for Atomic Coin Swap to me using SECRET %s',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
                //
                // No key needed... just a transaction SELF SIGNED
                LOpTransaction := TOpTransaction.CreateTransaction(FNode.Bank.SafeBox.CurrentProtocol,
                    LAccountSender0Coins.account, LAccountSender0Coins.n_operation+1,
                    LAccount.account,
                    LNeededWalletKey.PrivateKey,
                    0,0, // No Amount no Fee
                    LOpPayload);
              end else begin
                LAccountOpDesc := Format('Account %s is for Atomic Coin Swap using SECRET %s but I have no key to sign',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
              end;
            end else begin
              LAccountOpDesc := Format('Account %s is for Atomic Coin Swap using SECRET %s but is out-of-lock period',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
            end;
          end;
        else Continue;
        End;

        // Do
        Inc(LCountAccountsFound_total);
        LLines.Add(Format('%s',[LAccountOpDesc]));
        if Assigned(LOpTransaction) then begin
          Inc(LCountAccountsFound_Operation);
          LOperationsHashTree := TOperationsHashTree.Create;
          LStream := TMemoryStream.Create;
          try
            LOperationsHashTree.AddOperationToHashTree(LOpTransaction);
            LGlobalOperationsHashTree.AddOperationToHashTree(LOpTransaction);
            LLines.Add(Format('Operation: %s',[LOpTransaction.ToString]));
            LOperationsHashTree.SaveOperationsHashTreeToStream(LStream,False);
            LRaw.FromStream(LStream);
            LLines.Add(Format('rawoperations (for JSON-RPC call): %s',[LRaw.ToHexaString]));
          finally
            LOperationsHashTree.Free;
            LStream.Free;
          end;
        end;

      Finally
        FreeAndNil(LOpTransaction);
      End;
    end; // For
    LLines.Add('');
    LLines.Add(Format('Found %d of %d available account from a Safebox with %d accounts',[
      LCountAccountsFound_Operation,
      LCountAccountsFound_total,
      FNode.Bank.SafeBox.AccountsCount]));
    LStream := TMemoryStream.Create;
    try
      LGlobalOperationsHashTree.SaveOperationsHashTreeToStream(LStream,False);
      LRaw.FromStream(LStream);
      LLines.Add(Format('rawoperations (for JSON-RPC call) of %d operations: %s',[LGlobalOperationsHashTree.OperationsCount, LRaw.ToHexaString]));
    finally
      LStream.Free;
    end;
    //
    LFRM := TFRMMemoText.Create(Self);
    try
      LFRM.InitData('',LLines.Text);
      LFRM.ShowModal;
    finally
      LFRM.Free;
    end;
  finally
    LLines.Free;
    LGlobalOperationsHashTree.Free;
  end;
end;

procedure TFRMTestWallet.Test_ShowDiagnosticTool(Sender: TObject);
{$IFDEF TESTNET}
var
 LDialog : TFRMDiagnosticTool;
{$ENDIF}
begin
{$IFDEF TESTNET}
  LDialog := TFRMDiagnosticTool.Create(Nil);
  try
    LDialog.ShowModal;
  finally
    LDialog.Free;
  end;
{$ENDIF}
end;


procedure TFRMTestWallet.FormCreate(Sender: TObject);
begin
  inherited;

  // Things for testing purposes only
  InitMenuForTesting;
end;

end.
