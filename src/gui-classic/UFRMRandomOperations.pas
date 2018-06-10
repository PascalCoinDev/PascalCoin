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
  UWallet, UConst, UTxMultiOperation, UOpTransaction;

type

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
    FStopRandomOperations : Boolean;
    FIsProcessingRandomOperations : Boolean;
    FBankNotify : TPCBankNotify;
    FCurrOperationsComp : TPCOperationsComp;
    procedure SetSourceNode(AValue: TNode);
    procedure SetSourceWalletKeys(AValue: TWalletKeysExt);
    procedure DoRandomOperations(max : Integer; operationsComp : TPCOperationsComp);
    procedure DoProcessRandomOperations;
    procedure NewLog(logTxt : String);
    procedure OnBankNewBlock(Sender : TObject);
  public
    Property SourceNode : TNode read FSourceNode write SetSourceNode;
    Property SourceWalletKeys : TWalletKeysExt read FSourceWalletKeys write SetSourceWalletKeys;
  end;

  { TRandomGenerateOperation }

  TRandomGenerateOperation = Class
  private
  public
    class function GetRandomSigner(const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt; out iKey : Integer; out nAccount : Cardinal) : Boolean;
    class function GenerateOpTransaction(const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Boolean;
    class function GenerateOpMultiOperation(const operationsComp : TPCOperationsComp; const aWalletKeys : TWalletKeysExt) : Boolean;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TRandomGenerateOperation }

class function TRandomGenerateOperation.GetRandomSigner(const operationsComp: TPCOperationsComp; const aWalletKeys: TWalletKeysExt; out iKey: Integer; out nAccount: Cardinal): Boolean;
var availAccounts : TOrderedCardinalList;
  acc : TAccount;
  i, nRounds : Integer;
begin
  Result := False; iKey := -1; nAccount:=0; nRounds := 0;
  if (aWalletKeys.AccountsKeyList.Count<=0) then Exit;
  Repeat
    iKey := Random( aWalletKeys.AccountsKeyList.Count );
    i := aWalletKeys.AccountsKeyList.Count;
    if i<0 then Exit;
    availAccounts := aWalletKeys.AccountsKeyList.AccountKeyList[iKey];
    if availAccounts.Count<=0 then Exit; // No valid accounts
    i := availAccounts.Count;
    if (i<0) then Exit;
    // Sender:
    nAccount := availAccounts.Get( Random(availAccounts.Count) );
    acc.balance := 0;
    If Not TAccountComp.IsAccountBlockedByProtocol(nAccount,operationsComp.SafeBoxTransaction.FreezedSafeBox.BlocksCount) then begin
      if (operationsComp.OperationsHashTree.CountOperationsBySameSignerWithoutFee(nAccount)<=0) then acc := operationsComp.SafeBoxTransaction.Account(nAccount);
    end;
    inc(nRounds);
    if (nRounds>1000) then Exit;
  until (acc.balance>0);
  Result := True;
end;

class function TRandomGenerateOperation.GenerateOpTransaction(const operationsComp: TPCOperationsComp; const aWalletKeys: TWalletKeysExt): Boolean;
var nAccount : Cardinal;
  iKey : Integer;
  opTx : TOpTransaction;
  senderAcc,destAcc : TAccount;
  amount,fees : Int64;
  errors : AnsiString;
begin
  Result := False;
  If Not GetRandomSigner(operationsComp,aWalletKeys,iKey,nAccount) then Exit;
  senderAcc := operationsComp.SafeBoxTransaction.Account(nAccount);
  amount := Random(Integer(senderAcc.balance));
  if amount<=0 then Exit;
  If (senderAcc.balance - amount)>0 then begin
    fees := Random( senderAcc.balance - amount )
  end else fees := 0;

  iKey := aWalletKeys.IndexOfAccountKey( aWalletKeys.AccountsKeyList.AccountKey[iKey] );
  if iKey<0 then Exit;
  if Not aWalletKeys.Key[iKey].HasPrivateKey then Exit;
  if Not Assigned(aWalletKeys.Key[iKey].PrivateKey) then Exit;
  // Dest
  Repeat
    destAcc := operationsComp.SafeBoxTransaction.Account( Random(operationsComp.SafeBoxTransaction.FreezedSafeBox.AccountsCount) );
  until (destAcc.account <> senderAcc.account);

  // Search account
  opTx := TOpTransaction.CreateTransaction(senderAcc.account,senderAcc.n_operation+1,destAcc.account,aWalletKeys.Key[iKey].PrivateKey,amount,fees,'');
  Try
    Result := operationsComp.AddOperation(True,opTx,errors);
  finally
    opTx.Free;
  end;
end;

class function TRandomGenerateOperation.GenerateOpMultiOperation(const operationsComp: TPCOperationsComp; const aWalletKeys: TWalletKeysExt): Boolean;
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
           inc(n, opMulti.DoSignMultiOperationSigner(opMulti.Data.txSenders[i].Account,aWalletKeys.Key[j].PrivateKey));
         end;
       end;
     end;
     For i:=0 to High(opMulti.Data.changesInfo) do begin
       j := aWalletKeys.IndexOfAccountKey(operationsComp.bank.SafeBox.Account(opMulti.Data.changesInfo[i].Account).accountInfo.accountKey);
       If (j>=0) then begin
         // Can sign
         If (Assigned(aWalletKeys.Key[j].PrivateKey)) then begin
           inc(n, opMulti.DoSignMultiOperationSigner(opMulti.Data.changesInfo[i].Account,aWalletKeys.Key[j].PrivateKey));
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
                changer.New_Name:='random'+IntToStr(Random(100)); // <- This will generate collisions
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
  FIsProcessingRandomOperations := False;
  FStopRandomOperations := True;
  FBankNotify := TPCBankNotify.Create(Nil);
  FBankNotify.OnNewBlock:=OnBankNewBlock;
  FCurrOperationsComp := TPCOperationsComp.Create(Nil);
  mLogs.Clear;
end;

procedure TFRMRandomOperations.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBankNotify);
  FreeAndNil(FCurrOperationsComp);
end;

procedure TFRMRandomOperations.bbRandomOperationsClick(Sender: TObject);
begin
  {$IFDEF TESTNET}
  If FIsProcessingRandomOperations then begin
    FStopRandomOperations := True;
  end else begin
    DoProcessRandomOperations;
  end;
  {$ELSE}
  Raise Exception.Create('Random operations not valid in PRODUCTION MODE');
  {$ENDIF}
end;

procedure TFRMRandomOperations.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FStopRandomOperations := True;
  CloseAction := caFree;
end;

procedure TFRMRandomOperations.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FStopRandomOperations := True;
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
end;

procedure TFRMRandomOperations.SetSourceWalletKeys(AValue: TWalletKeysExt);
begin
  if FSourceWalletKeys=AValue then Exit;
  FSourceWalletKeys:=AValue;
end;

procedure TFRMRandomOperations.DoRandomOperations(max: Integer; operationsComp : TPCOperationsComp);
Var
  nCounter : Integer;
begin
  nCounter := 0;
  While (nCounter<max) And (Not FStopRandomOperations) do begin
    Case Random(30) of
      0..20 : begin
        If TRandomGenerateOperation.GenerateOpTransaction(operationsComp,FSourceWalletKeys) then inc(nCounter);
      end;
      21..25 : begin
        If TRandomGenerateOperation.GenerateOpMultiOperation(operationsComp,FSourceWalletKeys) then inc(nCounter);
      end;
    else Sleep(10);
    end;
    Application.ProcessMessages;
  end;
end;

procedure TFRMRandomOperations.DoProcessRandomOperations;
Var errors : AnsiString;
  i : Integer;
begin
  newLog('Start Random');
  Try
    FCurrOperationsComp.Clear(True);
    FStopRandomOperations:=False;
    FIsProcessingRandomOperations:=True;
    Try
      bbRandomOperations.Caption:='STOP';
      Application.ProcessMessages;
      While (FIsProcessingRandomOperations) And (Not FStopRandomOperations) do begin
        //
        FCurrOperationsComp.Clear(True);
        FCurrOperationsComp.SafeBoxTransaction.CopyFrom(FSourceNode.Operations.SafeBoxTransaction);
        //
        DoRandomOperations(Random(50),FCurrOperationsComp);
        i := FSourceNode.AddOperations(Nil,FCurrOperationsComp.OperationsHashTree,nil,errors);
        //
        newLog(Format('Added %d/%d operations - Errors: %s',[i,FCurrOperationsComp.Count, errors]));
        Application.ProcessMessages;
      end;
    finally
      FIsProcessingRandomOperations := False;
      bbRandomOperations.Caption:='Random Operations';
    end;
  finally
    newLog('End Random');
  end;
end;

procedure TFRMRandomOperations.NewLog(logTxt: String);
begin
  mLogs.Lines.Add(Format('%s %s',[FormatDateTime('hh:nn:ss.zzz',Now),logTxt]));
end;

procedure TFRMRandomOperations.OnBankNewBlock(Sender: TObject);
begin
  NewLog(Format('Updating to new block %d',[FBankNotify.Bank.BlocksCount]));
  FCurrOperationsComp.SanitizeOperations;
end;

end.

