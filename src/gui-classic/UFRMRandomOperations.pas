unit UFRMRandomOperations;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016-2018 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

{$I ../config.inc}

uses
  {$IFnDEF FPC}
    Windows, AppEvnts,
  {$ELSE}
    LCLIntf, LCLType, LMessages, FileUtil,
  {$ENDIF}
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, ActnList, UAccounts, UBlockChain, UNode, UCrypto, UBaseTypes,
  UWallet, UConst, UTxMultiOperation, UOpTransaction, UThread, ULog;

type

  { TRandomGeneratorThread }

  TRandomGeneratorThread = Class(TPCThread)
  private
    FLastCall_Error: String;
    FLastCall_OperationsExecuted: Integer;
    FLastCall_OperationsFailed: Integer;
    FLastCall_OperationsTotal: Integer;
    FOnUpdated: TNotifyEvent;
    FNeedSanitize : Boolean;
    FAllowExecute: Boolean;
    FMaxOperationsPerSecond: Integer;
    procedure OnBankNewBlock(Sender : TObject);
  protected
    FBankNotify : TPCBankNotify;
    FSourceNode: TNode;
    FSourceWalletKeys: TWalletKeysExt;
    FnOperationsCreated : Int64;
    FnOperationsCreatedFailed : Int64;
    FnOperationsExecutedOk : Int64;
    FnCallsToAddNodeTotal : Int64;
    FnCallsToAddNodeFailed : Int64;
    procedure BCExecute; override;
  public
    Constructor Create(ASourceNode: TNode; ASourceWalletKeys: TWalletKeysExt; AMaxOperationsPerSecond : Integer);
    Destructor Destroy; override;
    property LastCall_OperationsTotal : Integer read FLastCall_OperationsTotal;
    property LastCall_OperationsExecuted : Integer read FLastCall_OperationsExecuted;
    property LastCall_OperationsFailed : Integer read FLastCall_OperationsFailed;
    property LastCall_Error : String read FLastCall_Error;
    property OnUpdated : TNotifyEvent read FOnUpdated write FOnUpdated;
    property AllowExecute : Boolean read FAllowExecute write FAllowExecute;
    property MaxOperationsPerSecond : Integer read FMaxOperationsPerSecond write FMaxOperationsPerSecond;
  end;

  { TFRMRandomOperations }

  TFRMRandomOperations = class(TForm)
    bbRandomOperations: TButton;
    lblTopCaption: TLabel;
    mLogs: TMemo;
    pnlClient: TPanel;
    pnlTop: TPanel;
    pnlTop1: TPanel;
    procedure bbRandomOperationsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbMaxSpeedModeClick(Sender: TObject);
  private
    FSourceNode: TNode;
    FSourceWalletKeys: TWalletKeysExt;
    FBankNotify : TPCBankNotify;
    FCurrOperationsComp : TPCOperationsComp;
    FRandomGeneratorThread : TRandomGeneratorThread;
    FInternalLog : TLog;
    FMaxOperationsPerSecond : Integer;
    procedure SetSourceNode(AValue: TNode);
    procedure SetSourceWalletKeys(AValue: TWalletKeysExt);
    procedure NewLog(logTxt : String);
    procedure OnBankNewBlock(Sender : TObject);
    procedure UpdateRandomGeneratorThread(DestroyOnly : Boolean);
    procedure OnRandomGeneratoThreadUpdated(Sender : TObject);
    function IsProcessingRandomOperations : Boolean;
    procedure OnInternalLog(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : String);
  public
    Property SourceNode : TNode read FSourceNode write SetSourceNode;
    Property SourceWalletKeys : TWalletKeysExt read FSourceWalletKeys write SetSourceWalletKeys;
  end;

  { TRandomGenerateOperation }

  TRandomGenerateOperation = Class
  private
    class function GetRandomPayload(Const AStartsWith : String) : TOperationPayload;
  public
    class function GetRandomOwnDestination(const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt; out nAccount : Cardinal) : Boolean;
    class function GetRandomSigner(const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt; out iKey : Integer; out nAccount : Cardinal) : Boolean;
    class function GenerateOpTransactions(current_protocol : Word; Maxtransaction : Integer; const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Integer;
    class function GenerateOpMultiOperation(current_protocol : Word; const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Boolean;
    class function GenerateOpChangeKey(current_protocol : Word; const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Integer;
    class function GenerateOpListAccountForSale(current_protocol : Word; const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Integer;
    class function GenerateOpBuyAccount(current_protocol : Word; const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Integer;
    class function GetHashLock_Public : T32Bytes;
    class function GetHashLock_Private : TRawBytes;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TRandomGeneratorThread }

procedure TRandomGeneratorThread.OnBankNewBlock(Sender: TObject);
begin
  FNeedSanitize := True;
end;

procedure TRandomGeneratorThread.BCExecute;
Var nCounter, nTotalRound, iLastSend, i : Integer;
  operationsComp : TPCOperationsComp;
  ohtToAdd : TOperationsHashTree;
  errors : String;
  nAddedOperations, nMaxTransactionsValue, nExecutedSinceLastTC : Integer;
  LLastTC : TTickCount;
begin
  operationsComp := TPCOperationsComp.Create(Nil);
  try
    operationsComp.bank := FSourceNode.Bank;
    iLastSend := -1;
    LLastTC := TPlatform.GetTickCount;
    nExecutedSinceLastTC := 0;
    while (Not Terminated) do begin
      if FMaxOperationsPerSecond<=0 then begin
        nTotalRound := Random(100);
      end else begin
        nTotalRound := Random(FMaxOperationsPerSecond)+1;
      end;
      nCounter := 0;
      if FNeedSanitize then begin
        FNeedSanitize := False;
        operationsComp.SanitizeOperations;
        iLastSend := operationsComp.Count-1;
        TLog.NewLog(ltdebug,ClassName,Format('Sanitized. Current pending operations %d',[operationsComp.Count]));
      end;
      while (nCounter<nTotalRound) And (Not Terminated) And (FAllowExecute) do begin
        inc(nCounter);
        //
        Case Random(30) of
          0..10 : begin
            if FMaxOperationsPerSecond>0 then nMaxTransactionsValue := Random(FMaxOperationsPerSecond)+1
            else nMaxTransactionsValue := Random(200)+1;

            inc(FnOperationsCreated,TRandomGenerateOperation.GenerateOpTransactions(FSourceNode.Bank.SafeBox.CurrentProtocol,nMaxTransactionsValue,operationsComp,FSourceWalletKeys));
          end;
          11..15 : begin
            inc(FnOperationsCreated,TRandomGenerateOperation.GenerateOpChangeKey(FSourceNode.Bank.SafeBox.CurrentProtocol,operationsComp,FSourceWalletKeys));
          end;
          18..22 : begin
            inc(FnOperationsCreated,TRandomGenerateOperation.GenerateOpListAccountForSale(FSourceNode.Bank.SafeBox.CurrentProtocol,operationsComp,FSourceWalletKeys));
          end;
          25..29 : begin
            If TRandomGenerateOperation.GenerateOpMultiOperation(FSourceNode.Bank.SafeBox.CurrentProtocol,operationsComp,FSourceWalletKeys) then inc(FnOperationsCreated)
            else inc(FnOperationsCreatedFailed);
          end;
        end;
      end;

      if (Not Terminated) And (Not FNeedSanitize) And (FAllowExecute) then begin
        //
        ohtToAdd := TOperationsHashTree.Create;
        Try
          for i := iLastSend+1 to operationsComp.OperationsHashTree.OperationsCount-1 do begin
            ohtToAdd.AddOperationToHashTree(operationsComp.Operation[i]);
          end;
          errors := '';
          nAddedOperations := FSourceNode.AddOperations(Nil,ohtToAdd,nil,errors);
          iLastSend := operationsComp.OperationsHashTree.OperationsCount-1;
          // Notify info
          inc(FnCallsToAddNodeTotal);
          inc(FnOperationsExecutedOk,nAddedOperations);
          FLastCall_OperationsTotal:=ohtToAdd.OperationsCount;
          FLastCall_OperationsExecuted:=nAddedOperations;
          FLastCall_OperationsFailed:=ohtToAdd.OperationsCount - nAddedOperations;
          FLastCall_Error:=errors;
          if (ohtToAdd.OperationsCount <> nAddedOperations) then begin
            inc(FnOperationsCreatedFailed,(ohtToAdd.OperationsCount - nAddedOperations));
            inc(FnCallsToAddNodeFailed,1);
          end;
        Finally
          ohtToAdd.Free;
        End;
        //
        if FLastCall_OperationsTotal>0 then begin
          if Assigned(FOnUpdated) then FOnUpdated(Self);
        end;

        inc(nExecutedSinceLastTC,nAddedOperations);

      end;
      if (FMaxOperationsPerSecond>0) and (nExecutedSinceLastTC>=FMaxOperationsPerSecond) then begin
        while (Not Terminated) and (FAllowExecute) and (TPlatform.GetElapsedMilliseconds(LLastTC)<1000) do sleep(5);
      end;
      LLastTC := TPlatform.GetTickCount;
      nExecutedSinceLastTC := 0;
      Sleep(1);
    end;
  finally
    operationsComp.Free;
  end;
end;

constructor TRandomGeneratorThread.Create(ASourceNode: TNode; ASourceWalletKeys: TWalletKeysExt; AMaxOperationsPerSecond : Integer);
begin
  FSourceNode := ASourceNode;
  FSourceWalletKeys := ASourceWalletKeys;
  FnOperationsCreated := 0;
  FnOperationsCreatedFailed := 0;
  FnOperationsExecutedOk := 0;
  FnCallsToAddNodeTotal := 0;
  FnCallsToAddNodeFailed := 0;
  FLastCall_Error:='';
  FLastCall_OperationsFailed:=0;
  FLastCall_OperationsExecuted:=0;
  FLastCall_OperationsTotal:=0;
  //FOperationsComp := TPCOperationsComp.Create(Nil);
  //FOperationsComp.bank := FSourceNode.Bank;
  FBankNotify := TPCBankNotify.Create(Nil);
  FBankNotify.Bank := FSourceNode.Bank;
  FBankNotify.OnNewBlock:=OnBankNewBlock;
  FNeedSanitize := True;
  FAllowExecute := False;
  if AMaxOperationsPerSecond>=0 then
    FMaxOperationsPerSecond := AMaxOperationsPerSecond
  else FMaxOperationsPerSecond := 0;
  inherited Create(False);
end;

destructor TRandomGeneratorThread.Destroy;
begin
  FBankNotify.Bank := Nil;
  FBankNotify.OnNewBlock := Nil;
  FBankNotify.Free;
  inherited Destroy;
end;

{ TRandomGenerateOperation }

class function TRandomGenerateOperation.GetHashLock_Private: TRawBytes;
begin
  Result.FromString('PRIVATE');
end;

class function TRandomGenerateOperation.GetHashLock_Public: T32Bytes;
begin
  Result := TBaseType.To32Bytes( TCrypto.DoSha256( GetHashLock_Private ) );
end;

class function TRandomGenerateOperation.GetRandomOwnDestination(const operationsComp: TPCOperationsComp; const aWalletKeys: TWalletKeysExt; out nAccount: Cardinal): Boolean;
var
  nRounds : Integer;
  iKey, iNAcc : Integer;
begin
  Result := False; nAccount:=0; nRounds := 0;
  if (aWalletKeys.AccountsKeyList.Count<=0) then Exit;
  iKey := Random( aWalletKeys.AccountsKeyList.Count );
  Repeat
    if (aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Count>0) then begin
      // Destination;
      iNAcc := Random(aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Count);
      nAccount := aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Get( iNAcc );
      Result := True;
    end;
    if (iKey<aWalletKeys.AccountsKeyList.Count-1) then inc(iKey) else begin
      iKey:=0;
      inc(nRounds);
    end;
  until (Result) Or (nRounds>0);
end;

class function TRandomGenerateOperation.GetRandomPayload(
  const AStartsWith: String): TOperationPayload;
var i,j : Integer;
begin
  Result := CT_TOperationPayload_NUL;
  Result.payload_raw.FromString(AStartsWith);
  j := Random(255);
  if j<Length(Result.payload_raw) then j := Length(Result.payload_raw);

  SetLength(Result.payload_raw,j);
  for i := Length(Result.payload_raw) to j-1 do begin
    Result.payload_raw[j] := Random(127-32)+32;
  end;

end;

class function TRandomGenerateOperation.GetRandomSigner(const operationsComp: TPCOperationsComp; const aWalletKeys: TWalletKeysExt; out iKey: Integer; out nAccount: Cardinal): Boolean;
var
  bRoundsIKey, bRoundsNAccount : Boolean;
  iInt : Integer;
begin
  Result := False; iKey := -1; nAccount:=0;
  if (aWalletKeys.AccountsKeyList.Count<=0) then Exit;
  iKey := Random( aWalletKeys.AccountsKeyList.Count );
  bRoundsIKey := False;
  Repeat
    if (aWalletKeys.Key[iKey].HasPrivateKey) And (Assigned(aWalletKeys.Key[iKey].PrivateKey)) And (aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Count>0) then begin
      // Sender:
      bRoundsNAccount := False;
      iInt := Random(aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Count);
      Repeat
        nAccount := aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Get( iInt );
        If Not TAccountComp.IsAccountBlockedByProtocol(nAccount,operationsComp.SafeBoxTransaction.FreezedSafeBox.BlocksCount) then begin
          if (operationsComp.OperationsHashTree.CountOperationsBySameSignerWithoutFee(nAccount)<=0) then begin
            Result := True;
            Exit;
          end;
        end;
        if (iInt < aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Count-1) then inc(iInt)
        else begin
          iInt := 0;
          if bRoundsNAccount then Break
          else bRoundsNAccount:=True;
        end;
      until (Result);
    end;
    if (iKey<aWalletKeys.AccountsKeyList.Count-1) then inc(iKey) else begin
      iKey:=0;
      if bRoundsIKey then Break
      else bRoundsIKey:=True;
    end;
  until (Result);
end;

class function TRandomGenerateOperation.GenerateOpTransactions(current_protocol : Word; Maxtransaction : Integer; const operationsComp: TPCOperationsComp; const aWalletKeys: TWalletKeysExt): Integer;
var nAccount, nAccountTarget : Cardinal;
  iKey, nRounds : Integer;
  opTx : TOpTransaction;
  senderAcc, LDestAcc : TAccount;
  amount,fees : Int64;
  errors : String;
  LPayload : TOperationPayload;
begin
  Result := 0;
  If Not GetRandomSigner(operationsComp,aWalletKeys,iKey,nAccount) then Exit;
  if Not GetRandomOwnDestination(operationsComp,aWalletKeys,nAccountTarget) then Exit;
  if (nAccount = nAccountTarget) then Exit;
  nRounds := 0;
  while (nRounds<Maxtransaction) do begin
    senderAcc := operationsComp.SafeBoxTransaction.Account(nAccount);
    LDestAcc := operationsComp.SafeBoxTransaction.Account(nAccountTarget);
    LPayload := CT_TOperationPayload_NUL;
    if TAccountComp.IsAccountForSwap( LDestAcc.accountInfo ) then begin
      // Special case, will swap? Will need to provide a HASHLOCK in payload
      LPayload.payload_raw := GetHashLock_Private;
    end;

    amount := 1; // Minimal amount
    if (Random(500)<1) then fees := 0
    else fees := 1; // Minimal fee
    if (senderAcc.balance>2) then begin
      opTx := TOpTransaction.CreateTransaction(current_protocol,senderAcc.account,senderAcc.n_operation+1,nAccountTarget,aWalletKeys.Key[iKey].PrivateKey,amount,fees,LPayload);
      Try
        if operationsComp.AddOperation(True,opTx,errors) then inc(Result);
      finally
        opTx.Free;
      end;
    end;

    inc(nRounds);
  end;
end;

class function TRandomGenerateOperation.GenerateOpBuyAccount(
  current_protocol: Word; const operationsComp: TPCOperationsComp;
  const aWalletKeys: TWalletKeysExt): Integer;
begin
  raise Exception.Create('XXXXXX TODO TRandomGenerateOperation');
end;

class function TRandomGenerateOperation.GenerateOpChangeKey(
  current_protocol: Word;
  const operationsComp: TPCOperationsComp;
  const aWalletKeys: TWalletKeysExt): Integer;
var nAccount, nAccountTarget : Cardinal;
  iKey, iNewPubKey : Integer;
  opCk : TOpChangeKey;
  senderAcc : TAccount;
  fees : Int64;
  errors : String;
  opClass : TPCOperationClass;
begin
  Result := 0;
  If Not GetRandomSigner(operationsComp,aWalletKeys,iKey,nAccount) then Exit;
  if Random(1)=0 then begin
    nAccountTarget := Random( aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Count );
    opClass := TOpChangeKeySigned;
  end else begin
    nAccountTarget := nAccount;
    opClass := TOpChangeKey;
  end;

  if Not GetRandomOwnDestination(operationsComp,aWalletKeys,nAccountTarget) then Exit;
  iNewPubKey := Random(aWalletKeys.Count);

  if iKey=iNewPubKey then Exit;

  senderAcc := operationsComp.SafeBoxTransaction.Account(nAccount);
  if (Random(500)<1) then fees := 0
  else fees := 1; // Minimal fee
  if (senderAcc.balance>2) then begin
    opCk := TOpChangeKey(opClass.NewInstance).Create(current_protocol,senderAcc.account,senderAcc.n_operation+1,nAccountTarget,
      aWalletKeys.Key[iKey].PrivateKey,
      aWalletKeys.Key[iNewPubKey].AccountKey,
      fees,CT_TOperationPayload_NUL);
    Try
      if operationsComp.AddOperation(True,opCk,errors) then inc(Result);
    finally
      opCk.Free;
    end;
  end;

end;

class function TRandomGenerateOperation.GenerateOpListAccountForSale(
  current_protocol: Word; const operationsComp: TPCOperationsComp;
  const aWalletKeys: TWalletKeysExt): Integer;
var nSigner, nTarget : Cardinal;
  iKey, iNewPubKey, i, j : Integer;
  opList : TOpListAccountForSaleOrSwap;
  opDelist : TOpDelistAccountForSale;
  SignerAccount, AuxAccount : TAccount;
  fees : Int64;
  errors : String;
  DoDelist : Boolean;
begin
  Result := 0;
  If Not GetRandomSigner(operationsComp,aWalletKeys,iKey,nSigner) then Exit;
  SignerAccount := operationsComp.SafeBoxTransaction.Account(nSigner);

  if (Random(500)<1) then fees := 0
  else fees := 1; // Minimal fee

  if TAccountComp.IsAccountForSaleOrSwap(SignerAccount.accountInfo) then begin
    // Delist:
    i := Random( aWalletKeys.AccountsKeyList.AccountKeyList[ iKey ].Count );
    j := i;
    DoDelist := False;
    Repeat
      AuxAccount := operationsComp.SafeBoxTransaction.Account(j);
      if TAccountComp.IsAccountForSale(AuxAccount.accountInfo) then begin
        nTarget := j;
        DoDelist := True;
      end;

      //
      if j<aWalletKeys.AccountsKeyList.AccountKeyList[ iKey ].Count then inc(j)
      else j:=0;
    Until (j=i) or (DoDelist);
    if (DoDelist) then begin
      try
        opDelist := TOpDelistAccountForSale.CreateDelistAccountForSale(current_protocol,
          nSigner,SignerAccount.n_operation+1,nTarget,fees,
          aWalletKeys.Key[iKey].PrivateKey,
          GetRandomPayload(''));
        if operationsComp.AddOperation(True,opDelist,errors) then inc(Result);
      finally
        opDelist.Free;
      end;
    end;

  end else begin

    if Random(1)=0 then begin
      nTarget := Random( aWalletKeys.AccountsKeyList.AccountKeyList[iKey].Count );
    end else begin
      nTarget := nSigner;
    end;


    opList := Nil;
    try
      case Random(4) of
        0 : // Private sale:
          begin
            // Private sale:
            opList := TOpListAccountForSaleOrSwap.CreateListAccountForSaleOrSwap(current_protocol,
              as_ForSale,
              nSigner, SignerAccount.n_operation+1, nTarget, Random(50000)+1,fees,
              Random( operationsComp.SafeBoxTransaction.FreezedSafeBox.AccountsCount ),
              aWalletKeys.Key[ Random(aWalletKeys.Count) ].AccountKey,
              operationsComp.OperationBlock.block + Random(1000),
              aWalletKeys.Key[iKey].PrivateKey,
              CT_HashLock_NUL,
              GetRandomPayload(''));
          end;
        1 : // Publis sale:
          begin
            opList := TOpListAccountForSaleOrSwap.CreateListAccountForSaleOrSwap(current_protocol,
              as_ForSale,
              nSigner, SignerAccount.n_operation+1, nTarget, Random(50000)+1,fees,
              Random( operationsComp.SafeBoxTransaction.FreezedSafeBox.AccountsCount ),
              CT_TECDSA_Public_Nul,
              0,
              aWalletKeys.Key[iKey].PrivateKey,
              CT_HashLock_NUL,
              GetRandomPayload(''));
          end;
        2 : // Atomic Account Swap
          begin
            opList := TOpListAccountForSaleOrSwap.CreateListAccountForSaleOrSwap(current_protocol,
              as_ForAtomicAccountSwap,
              nSigner, SignerAccount.n_operation+1, nTarget, Random(50000)+1,fees,
              Random( operationsComp.SafeBoxTransaction.FreezedSafeBox.AccountsCount ),
              aWalletKeys.Key[ Random(aWalletKeys.Count) ].AccountKey,
              operationsComp.OperationBlock.block + Random(1000),
              aWalletKeys.Key[iKey].PrivateKey,
              GetHashLock_Public,
              GetRandomPayload(''));
          end;
        3 : // Atomic Coin Swap
          begin
            opList := TOpListAccountForSaleOrSwap.CreateListAccountForSaleOrSwap(current_protocol,
              as_ForAtomicCoinSwap,
              nSigner, SignerAccount.n_operation+1, nTarget, Random(50000)+1,fees,
              Random( operationsComp.SafeBoxTransaction.FreezedSafeBox.AccountsCount ),
              CT_TECDSA_Public_Nul,
              operationsComp.OperationBlock.block + Random(1000),
              aWalletKeys.Key[iKey].PrivateKey,
              GetHashLock_Public,
              GetRandomPayload(''));
          end;
      end;
      if operationsComp.AddOperation(True,opList,errors) then inc(Result);
    finally
      opList.Free;
    end;

  end;
end;

class function TRandomGenerateOperation.GenerateOpMultiOperation(current_protocol : Word; const operationsComp: TPCOperationsComp; const aWalletKeys: TWalletKeysExt): Boolean;
   procedure DoSign(opMulti : TOpMultiOperation);
   var n : Integer;
     i,j : Integer;
   begin
     n := 0;
     For i:=0 to High(opMulti.Data.txSenders) do begin
       j := aWalletKeys.IndexOfAccountKey(operationsComp.bank.SafeBox.Account(opMulti.Data.txSenders[i].Account).accountInfo.accountKey);
       If (j>=0) then begin
         // Can sign
         If (Assigned(aWalletKeys.Key[j].PrivateKey)) then begin
           inc(n, opMulti.DoSignMultiOperationSigner(current_protocol,opMulti.Data.txSenders[i].Account,aWalletKeys.Key[j].PrivateKey));
         end;
       end;
     end;
     For i:=0 to High(opMulti.Data.changesInfo) do begin
       j := aWalletKeys.IndexOfAccountKey(operationsComp.bank.SafeBox.Account(opMulti.Data.changesInfo[i].Account).accountInfo.accountKey);
       If (j>=0) then begin
         // Can sign
         If (Assigned(aWalletKeys.Key[j].PrivateKey)) then begin
           inc(n, opMulti.DoSignMultiOperationSigner(current_protocol,opMulti.Data.changesInfo[i].Account,aWalletKeys.Key[j].PrivateKey));
         end;
       end;
     end;
   end;

Var opMulti : TOpMultiOperation;
  iKey : Integer;
  nAccount : Cardinal;
  sender : TMultiOpSender;
  receiver : TMultiOpReceiver;
  changer: TMultiOpChangeInfo;
  acc : TAccount;
  errors : String;
begin
  Result := False;
  opMulti := TOpMultiOperation.Create(current_protocol);
  Try
    if (Random(100)<5) then begin
      Repeat
        If GetRandomSigner(operationsComp,aWalletKeys,iKey,nAccount) then begin
          acc := operationsComp.SafeBoxTransaction.Account(nAccount);
          if Not OpMulti.IsSignerAccount(nAccount) then begin
            sender := CT_TMultiOpSender_NUL;
            sender.Account:=nAccount;
            sender.Amount:= Random( Integer(acc.balance) );
            sender.N_Operation:=acc.n_operation+1;
            opMulti.AddTxSender(sender);
          end;
        end;
      until (Random(100)=0) Or ((Length(opMulti.Data.txSenders)>Random(1000)) And (opMulti.OperationFee>Random(1000000)));
      Repeat
        nAccount := Random( operationsComp.SafeBoxTransaction.FreezedSafeBox.AccountsCount );
        If Not TAccountComp.IsAccountBlockedByProtocol(nAccount,operationsComp.SafeBoxTransaction.FreezedSafeBox.BlocksCount) then begin
          receiver := CT_TMultiOpReceiver_NUL;
          receiver.Account:=nAccount;
          receiver.Amount:=Random( Integer(opMulti.OperationFee) );
          opMulti.AddTxReceiver(receiver);
        end;
      until (Random(100)=0) Or (opMulti.OperationFee<=0);
    end;
    if (Random(100)<5) then begin
      Repeat
        changer := CT_TMultiOpChangeInfo_NUL;
        If GetRandomSigner(operationsComp,aWalletKeys,iKey,nAccount) then begin
          acc := operationsComp.SafeBoxTransaction.Account(nAccount);
          if Not OpMulti.IsSignerAccount(nAccount) then begin
            changer.Account:=nAccount;
            changer.N_Operation:=acc.n_operation+1;
            case Random(3) of // public_key,account_name,account_type
              0 : begin
                changer.Changes_type:=[public_key];
                acc := operationsComp.SafeBoxTransaction.Account(Random(operationsComp.SafeBoxTransaction.FreezedSafeBox.AccountsCount));
                changer.New_Accountkey := acc.accountInfo.accountKey;
              end;
              1 : begin
                changer.Changes_type:=[account_name];
                changer.New_Name:=TEncoding.ANSI.GetBytes('random'+IntToStr(Random(100))); // <- This will generate collisions
              end;
            else
              changer.Changes_type:=[account_type];
              changer.New_Type:=Random(65535);
            end;
            opMulti.AddChangeInfo(changer);
          end;
        end;
      until (Random(100)=0) Or (Length(opMulti.Data.changesInfo)>Random(1000));
    end;
    // If nothing added... invalid op.
    if (Length(opMulti.Data.changesInfo)=0) And (Length(opMulti.Data.txSenders)=0) And (Length(opMulti.Data.txReceivers)=0) then Exit;
    //
    DoSign(opMulti);
    if Not opMulti.CheckSignatures(operationsComp.SafeBoxTransaction,errors) then begin
      errors := opMulti.toString + errors;
      Exit;
    end;
    Result := operationsComp.AddOperation(True,opMulti,errors);
  finally
    opMulti.Free;
  end;
end;

{ TFRMRandomOperations }

procedure TFRMRandomOperations.FormCreate(Sender: TObject);
begin
  FSourceNode := Nil;
  FSourceWalletKeys := Nil;
  FBankNotify := TPCBankNotify.Create(Nil);
  FBankNotify.OnNewBlock:=OnBankNewBlock;
  FCurrOperationsComp := TPCOperationsComp.Create(Nil);
  FRandomGeneratorThread := Nil;
  FInternalLog := TLog.Create(Self);
  FInternalLog.ProcessGlobalLogs := False;
  FInternalLog.OnNewLog := OnInternalLog;
  FMaxOperationsPerSecond := 0;
  mLogs.Clear;
end;

procedure TFRMRandomOperations.FormDestroy(Sender: TObject);
begin
  UpdateRandomGeneratorThread(True);
  FreeAndNil(FBankNotify);
  FreeAndNil(FCurrOperationsComp);
end;

procedure TFRMRandomOperations.bbRandomOperationsClick(Sender: TObject);
var sValue : String;
  n : Integer;
begin
  {$IFDEF TESTNET}
  If IsProcessingRandomOperations then begin
    FRandomGeneratorThread.AllowExecute := False;
    bbRandomOperations.Caption:='GENERATE RANDOM';
  end else begin
    if Assigned(FRandomGeneratorThread) then begin
      sValue := IntToStr(FMaxOperationsPerSecond);
      if InputQuery('Max Operations per second','Max operations per second (0=Max)',sValue) then begin
        n := StrToIntDef(sValue,-1);
        if (n>=0) then FMaxOperationsPerSecond := n;
      end;
      FRandomGeneratorThread.MaxOperationsPerSecond := n;
      FRandomGeneratorThread.AllowExecute := True;
      bbRandomOperations.Caption:='STOP RANDOM';
    end else bbRandomOperations.Caption:='???';
  end;
  {$ELSE}
  Raise Exception.Create('Random operations not valid in PRODUCTION MODE');
  {$ENDIF}
end;

procedure TFRMRandomOperations.cbMaxSpeedModeClick(Sender: TObject);
begin
//
end;

procedure TFRMRandomOperations.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFRMRandomOperations.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
end;

procedure TFRMRandomOperations.SetSourceNode(AValue: TNode);
begin
  if FSourceNode=AValue then Exit;
  FBankNotify.Bank := Nil;
  FSourceNode:=AValue;

  If Assigned(AValue) then begin
    FBankNotify.Bank := AValue.Bank;
    FCurrOperationsComp.bank := AValue.Bank;
  end;
  UpdateRandomGeneratorThread(False);
end;

procedure TFRMRandomOperations.SetSourceWalletKeys(AValue: TWalletKeysExt);
begin
  if FSourceWalletKeys=AValue then Exit;
  FSourceWalletKeys:=AValue;
  UpdateRandomGeneratorThread(False);
end;

procedure TFRMRandomOperations.NewLog(logTxt: String);
begin
  if length(logTxt)>300 then logTxt := Copy(logTxt,1,300)+'...';
  FInternalLog.NotifyNewLog(ltdebug,'',logTxt);
end;

procedure TFRMRandomOperations.OnBankNewBlock(Sender: TObject);
begin
  NewLog(Format('Updating to new block %d',[FBankNotify.Bank.BlocksCount]));
  FCurrOperationsComp.SanitizeOperations;
end;

procedure TFRMRandomOperations.OnInternalLog(logtype: TLogType; Time: TDateTime;
  ThreadID: TThreadID; const sender, logtext: String);
begin
  mLogs.Lines.Add(Format('%s %s',[FormatDateTime('hh:nn:ss.zzz',Now),logtext]));
end;

procedure TFRMRandomOperations.UpdateRandomGeneratorThread(DestroyOnly : Boolean);
begin
  if Assigned(FRandomGeneratorThread) then begin
    FRandomGeneratorThread.AllowExecute:=False;
    FRandomGeneratorThread.Terminate;
    FRandomGeneratorThread.WaitFor;
    FreeAndNil(FRandomGeneratorThread);
  end;
  if (Not DestroyOnly) And Assigned(FSourceNode) And Assigned(FSourceWalletKeys) then begin
    FRandomGeneratorThread := TRandomGeneratorThread.Create(FSourceNode,FSourceWalletKeys,FMaxOperationsPerSecond);
    FRandomGeneratorThread.OnUpdated:=OnRandomGeneratoThreadUpdated;
  end;
end;

procedure TFRMRandomOperations.OnRandomGeneratoThreadUpdated(Sender: TObject);
Var RGT : TRandomGeneratorThread;
begin
  RGT := TRandomGeneratorThread(Sender);
  NewLog(Format('Generated %d Operations-> Ok:%d Failed:%d Errors:%s',[RGT.LastCall_OperationsTotal,RGT.LastCall_OperationsExecuted,RGT.LastCall_OperationsFailed,RGT.LastCall_Error]));
end;

function TFRMRandomOperations.IsProcessingRandomOperations: Boolean;
begin
  Result := Assigned(FRandomGeneratorThread) And (FRandomGeneratorThread.AllowExecute);
end;

end.

