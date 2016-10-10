unit UMiner;

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

Uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  UBlockChain, Classes, SyncObjs, UAccounts, UThread;

Type
  TMinerPrivateKey = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  TMinerThread = Class;

  TMinerNewBlockFound = procedure(sender : TMinerThread; Operations : TPCOperationsComp; Var Correct : Boolean) of object;
  TMinerNewBlockFoundNotify = procedure(sender : TMinerThread; Operations : TPCOperationsComp) of object;

  TMinerThread = Class(TPCThread)
  private
    FOperations : TPCOperationsComp;
    FLock: TCriticalSection;
    FPlayCount : Int64;
    FTotalActiveTime : Int64;
    FLastStartTickCount : Cardinal;
    //
    errors : AnsiString;
    FAccountKey: TAccountKey;
    FPaused: Boolean;
    FOnNewBlockFound: TMinerNewBlockFound;
    FOnThreadSafeNewBlockFound: TMinerNewBlockFoundNotify;
    procedure SetAccountKey(const Value: TAccountKey);
    Procedure CheckIfCanRecoverBlocks;
    Procedure NotifyNewBlockFoundThreadSafe;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(Bank : TPCBank; minerAccountKey : TAccountKey; AOnNewBlockFound : TMinerNewBlockFound; AOnThreadSafeNewBlockFound : TMinerNewBlockFoundNotify);
    destructor Destroy; override;
    Function MinerLockOperations : TPCOperationsComp;
    Procedure MinerUnLockOperations(IsNewBlock : Boolean);
    Property OnNewBlockFound : TMinerNewBlockFound read FOnNewBlockFound write FOnNewBlockFound;
    Property OnThreadSafeNewBlockFound : TMinerNewBlockFoundNotify read FOnThreadSafeNewBlockFound write FOnThreadSafeNewBlockFound;
    Property PlayCount : Int64 read FPlayCount;
    Property AccountKey : TAccountKey read FAccountKey write SetAccountKey;
    Property Paused : Boolean read FPaused Write FPaused;
    Function HashRate : Int64;
    Class function AllMinersPlayCount : Int64;
  End;

implementation

uses ULog, SysUtils, UConst, UOpTransaction, UCrypto;

{ TMinerThread }

var _all_miners_play_count : Int64;

procedure TMinerThread.CheckIfCanRecoverBlocks;
Var n_account : Cardinal;
  recover_block : Cardinal;
  OpR : TOpRecoverFounds;
  recover_q : UInt64;
  errors : AnsiString;
Begin
  if FOperations.bank.SafeBox.BlocksCount<CT_RecoverFoundsWaitInactiveCount then exit;
  recover_block := FOperations.bank.SafeBox.BlocksCount-CT_RecoverFoundsWaitInactiveCount;
  n_account := 0;
  while (n_account < FOperations.bank.SafeBox.AccountsCount) do begin
   If FOperations.SafeBoxTransaction.Account(n_account).updated_block<recover_block then begin
     recover_q := FOperations.SafeBoxTransaction.Account(n_account).balance;
     if (recover_q>0) then begin
       // Recover it!
       if (recover_q > CT_MaxTransactionFee) then recover_q := CT_MaxTransactionFee;
       OpR := TOpRecoverFounds.Create(n_account,FOperations.SafeBoxTransaction.Account(n_account).n_operation+1,recover_q);
       If FOperations.AddOperation(true,OpR,errors) then begin
         TLog.NewLog(ltinfo,Classname,Format('Recover founds executed at account: %d amount: %s Operation: %s',[n_account,TAccountComp.FormatMoney(recover_q),OpR.ToString]));
       end else begin
         TLog.NewLog(lterror,Classname,Format('Error recovering founds executed at account: %d amount: %s Error: %s',[n_account,TAccountComp.FormatMoney(recover_q),errors]));
       end;
     end;
   end;
   inc(n_account);
  end;
End;

constructor TMinerThread.Create(Bank : TPCBank; minerAccountKey : TAccountKey; AOnNewBlockFound : TMinerNewBlockFound; AOnThreadSafeNewBlockFound : TMinerNewBlockFoundNotify);
begin
  inherited Create(true);
  FTotalActiveTime := 0;
  FLastStartTickCount := 0;
  FPaused := true;
  FPlayCount := 0;
  FAccountKey := minerAccountKey;
  FLock := TCriticalSection.Create;
  FOperations := TPCOperationsComp.Create(nil);
  FOperations.Bank := Bank;
  FOperations.AccountKey := AccountKey;
  FOnNewBlockFound := AOnNewBlockFound;
  FOnThreadSafeNewBlockFound := AOnThreadSafeNewBlockFound;
  Priority := tpLower;
  Suspended := false;
end;

class function TMinerThread.AllMinersPlayCount: Int64;
begin
  Result := _all_miners_play_count;
end;

procedure TMinerThread.BCExecute;
Var i : Integer;
  winner : Boolean;
  newBlockAccount : TBlockAccount;
  c : Boolean;
begin
  TLog.NewLog(ltinfo,ClassName,'New miner');
  while (not Terminated) do begin
    while (FPaused) And (Not Terminated) do begin
      sleep(1);
    end;
    FLastStartTickCount := GetTickCount;
    Try
      if Terminated then exit;
      winner := false;
      TPCThread.ProtectEnterCriticalSection(Self,FLock);
      try
        FOperations.UpdateTimestamp;
        FOperations.AccountKey := FAccountKey;
        for i := 0 to 100000 do begin
          inc(FPlayCount);
          inc(_all_miners_play_count);
          if Terminated then exit;

          if FOperations.IncrementNOnce then begin
            winner := true;
            break;
          end;

        end;
      finally
        FLock.Release;
      end;
      if (winner) then begin
        Try
          c := true;
          if Assigned(FOnNewBlockFound) then begin
            FOnNewBlockFound(Self,FOperations,c);
          end;
          if (c) And (Assigned(FOnThreadSafeNewBlockFound)) then Synchronize(NotifyNewBlockFoundThreadSafe);
        Except
          On E:Exception do begin
            TLog.NewLog(lterror,Classname,'Exception on adding new block by miner: '+E.Message);
            Raise;
          end;
        End;
      end;
    Finally
      if FLastStartTickCount>0 then begin
        FTotalActiveTime := FTotalActiveTime + (GetTickCount - FLastStartTickCount);
      end;
    End;
    if Not Terminated then Sleep(1);
  end;
  TLog.NewLog(ltinfo,ClassName,'Destroying Miner');
end;

destructor TMinerThread.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FOperations);
  inherited;
end;

function TMinerThread.HashRate: Int64;
begin
  if FTotalActiveTime>0 then
    Result := (FPlayCount*1000) DIV FTotalActiveTime
  else Result := 0;
end;

function TMinerThread.MinerLockOperations: TPCOperationsComp;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FLock);
  Result := FOperations;
end;

procedure TMinerThread.MinerUnLockOperations(IsNewBlock : Boolean);
begin
  FLock.Release;
  if IsNewBlock then CheckIfCanRecoverBlocks;
end;

procedure TMinerThread.NotifyNewBlockFoundThreadSafe;
begin
  if Assigned(FOnThreadSafeNewBlockFound) then FOnThreadSafeNewBlockFound(Self,FOperations);
end;

procedure TMinerThread.SetAccountKey(const Value: TAccountKey);
begin
  FAccountKey := Value;
end;

initialization
  _all_miners_play_count := 0;
finalization
end.
