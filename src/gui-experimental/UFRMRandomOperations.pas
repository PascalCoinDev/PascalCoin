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
  UWallet, UConst, UTxMultiOperation, UOpTransaction, UThread;

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
    Constructor Create(ASourceNode: TNode; ASourceWalletKeys: TWalletKeysExt);
    Destructor Destroy; override;
    property LastCall_OperationsTotal : Integer read FLastCall_OperationsTotal;
    property LastCall_OperationsExecuted : Integer read FLastCall_OperationsExecuted;
    property LastCall_OperationsFailed : Integer read FLastCall_OperationsFailed;
    property LastCall_Error : String read FLastCall_Error;
    property OnUpdated : TNotifyEvent read FOnUpdated write FOnUpdated;
    property AllowExecute : Boolean read FAllowExecute write FAllowExecute;
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
  private
    FSourceNode: TNode;
    FSourceWalletKeys: TWalletKeysExt;
    FBankNotify : TPCBankNotify;
    FCurrOperationsComp : TPCOperationsComp;
    FRandomGeneratorThread : TRandomGeneratorThread;
    procedure SetSourceNode(AValue: TNode);
    procedure SetSourceWalletKeys(AValue: TWalletKeysExt);
    procedure NewLog(logTxt : String);
    procedure OnBankNewBlock(Sender : TObject);
    procedure UpdateRandomGeneratorThread(DestroyOnly : Boolean);
    procedure OnRandomGeneratoThreadUpdated(Sender : TObject);
    function IsProcessingRandomOperations : Boolean;
  public
    Property SourceNode : TNode read FSourceNode write SetSourceNode;
    Property SourceWalletKeys : TWalletKeysExt read FSourceWalletKeys write SetSourceWalletKeys;
  end;

  { TRandomGenerateOperation }

  TRandomGenerateOperation = Class
  private
  public
    class function GetRandomOwnDestination(const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt; out nAccount : Cardinal) : Boolean;
    class function GetRandomSigner(const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt; out iKey : Integer; out nAccount : Cardinal) : Boolean;
    class function GenerateOpTransactions(current_protocol : Word; Maxtransaction : Integer; const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Integer;
    class function GenerateOpMultiOperation(current_protocol : Word; const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Boolean;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

Uses ULog;

{ TRandomGeneratorThread }

procedure TRandomGeneratorThread.OnBankNewBlock(Sender: TObject);
begin
  FNeedSanitize := True;
end;

procedure TRandomGeneratorThread.BCExecute;
Var nCounter, nTotalRound, iLastSend, i : Integer;
  operationsComp : TPCOperationsComp;
  ohtToAdd : TOperationsHashTree;
  errors : AnsiString;
  nAddedOperations : Integer;
begin
  operationsComp := TPCOperationsComp.Create(Nil);
  try
    operationsComp.bank := FSourceNode.Bank;
    iLastSend := -1;
    while (Not Terminated) do begin
      nTotalRound := Random(100);
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
          0..20 : begin
            inc(FnOperationsCreated,TRandomGenerateOperation.GenerateOpTransactions(FSourceNode.Bank.SafeBox.CurrentProtocol,100,operationsComp,FSourceWalletKeys));
          end;
          21..25 : begin
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
        if Assigned(FOnUpdated) then FOnUpdated(Self);
      end;
      Sleep(1);
    end;
  finally
    operationsComp.Free;
  end;
end;

constructor TRandomGeneratorThread.Create(ASourceNode: TNode; ASourceWalletKeys: TWalletKeysExt);
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
  senderAcc : TAccount;
  amount,fees : Int64;
  errors : AnsiString;
begin
  Result := 0;
  If Not GetRandomSigner(operationsComp,aWalletKeys,iKey,nAccount) then Exit;
  if Not GetRandomOwnDestination(operationsComp,aWalletKeys,nAccountTarget) then Exit;
  if (nAccount = nAccountTarget) then Exit;
  nRounds := 0;
  while (nRounds<Maxtransaction) do begin
    senderAcc := operationsComp.SafeBoxTransaction.Account(nAccount);
    amount := 1; // Minimal amount
    if (Random(500)<1) then fees := 0
    else fees := 1; // Minimal fee
    if (senderAcc.balance>2) then begin
      opTx := TOpTransaction.CreateTransaction(current_protocol,senderAcc.account,senderAcc.n_operation+1,nAccountTarget,aWalletKeys.Key[iKey].PrivateKey,amount,fees,Nil);
      Try
        if operationsComp.AddOperation(True,opTx,errors) then inc(Result);
      finally
        opTx.Free;
      end;
    end;

    inc(nRounds);
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
  errors : AnsiString;
begin
  Result := False;
  opMulti := TOpMultiOperation.Create;
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
  mLogs.Clear;
end;

procedure TFRMRandomOperations.FormDestroy(Sender: TObject);
begin
  UpdateRandomGeneratorThread(True);
  FreeAndNil(FBankNotify);
  FreeAndNil(FCurrOperationsComp);
end;

procedure TFRMRandomOperations.bbRandomOperationsClick(Sender: TObject);
begin
  {$IFDEF TESTNET}
  If IsProcessingRandomOperations then begin
    FRandomGeneratorThread.AllowExecute := False;
    bbRandomOperations.Caption:='GENERATE RANDOM';
  end else begin
    if Assigned(FRandomGeneratorThread) then begin
      FRandomGeneratorThread.AllowExecute := True;
      bbRandomOperations.Caption:='STOP RANDOM';
    end else bbRandomOperations.Caption:='???';
  end;
  {$ELSE}
  Raise Exception.Create('Random operations not valid in PRODUCTION MODE');
  {$ENDIF}
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
  
  mLogs.Lines.Add(Format('%s %s',[FormatDateTime('hh:nn:ss.zzz',Now),logTxt]));
end;

procedure TFRMRandomOperations.OnBankNewBlock(Sender: TObject);
begin
  NewLog(Format('Updating to new block %d',[FBankNotify.Bank.BlocksCount]));
  FCurrOperationsComp.SanitizeOperations;
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
    FRandomGeneratorThread := TRandomGeneratorThread.Create(FSourceNode,FSourceWalletKeys);
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

