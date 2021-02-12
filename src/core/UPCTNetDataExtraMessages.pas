unit UPCTNetDataExtraMessages;

{ Copyright (c) 2019 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{
  This unit adds a Extra available messages to PascalCoin net protocol

  Will be used, for example, to give/receive free accounts on TESTNET

  }


interface

{$I ./../config.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

Uses Classes, UThread, UAccounts, UBlockChain, UNetProtocol, SysUtils, UNode,
  UWallet, UNetProtection, UPCDataTypes, UPCAccountsOrdenations, UOrderedList,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TPCTNetDataExtraMessages = Class
  private
    FNode : TNode;
    FNetData : TNetData;
    FWalletKeys : TWalletKeysExt;
    function DoAskForFreeAccount(const ANewPubliKey : TAccountKey; const AMessage : String) : Integer;
    procedure DoGiveMeAnAccount(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
    {$IFDEF TESTNET}
    procedure DoGiveMeMoney(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
    {$ENDIF}
    procedure OnTNetDataProcessReservedAreaMessage(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
    function RandomGetWalletKeysAccount(const ASafebox : TPCSafeBox; AWalletKeys: TWalletKeysExt; AMinBalance, AMaxBalance : Int64; out AKey: Integer; out AAccount: TAccount): Boolean;
  protected
  public
    class function InitNetDataExtraMessages(ANode : TNode; ANetData : TNetData; AWalletKeys : TWalletKeysExt) : TPCTNetDataExtraMessages;
    constructor Create(ANode : TNode; ANetData : TNetData; AWalletKeys : TWalletKeysExt);
    destructor Destroy; override;
    class function AskForFreeAccount(const ANewPubliKey : TAccountKey; const AMessage : String): Integer;
  End;

const
  CT_NetProtocol_Extra_NetOp_GIVE_ME_AN_ACCOUNT   = $1101;
  CT_NetProtocol_Extra_NetOp_GIVE_ME_MONEY        = $1102;

implementation

Uses UOpTransaction, UBaseTypes, ULog, UPCAbstractMemAccountKeys;

var _PCTNetDataExtraMessages : TPCTNetDataExtraMessages = Nil;

{ TPCTNetDataExtraMessages }

class function TPCTNetDataExtraMessages.AskForFreeAccount(const ANewPubliKey : TAccountKey; const AMessage : String): Integer;
begin
  if Assigned(_PCTNetDataExtraMessages) then begin
    Result := _PCTNetDataExtraMessages.DoAskForFreeAccount(ANewPubliKey,AMessage);
  end else Result := 0;
end;

constructor TPCTNetDataExtraMessages.Create(ANode : TNode; ANetData : TNetData; AWalletKeys : TWalletKeysExt);
begin
  FNode := ANode;
  FWalletKeys := AWalletKeys;
  FNetData := ANetData;
  if Assigned(ANetData) then begin
    if (Not Assigned(ANetData.OnProcessReservedAreaMessage)) then begin
      ANetData.OnProcessReservedAreaMessage := OnTNetDataProcessReservedAreaMessage;
      _PCTNetDataExtraMessages := Self;
    end;
  end;
end;

destructor TPCTNetDataExtraMessages.Destroy;
begin
  if (_PCTNetDataExtraMessages = Self) And (TNetData.NetDataExists) then begin
    FNetData.OnProcessReservedAreaMessage := Nil;
    _PCTNetDataExtraMessages := Nil;
  end;
  inherited;
end;

function TPCTNetDataExtraMessages.DoAskForFreeAccount(const ANewPubliKey : TAccountKey; const AMessage : String): Integer;
var i : Integer;
  LNetConnection : TNetConnection;
  LRequestStream : TMemoryStream;
  Lraw : TRawBytes;
begin
  Result := 0;
  LRequestStream := TMemoryStream.Create;
  try
    TStreamOp.WriteAccountKey(LRequestStream,ANewPubliKey);
    Lraw.FromString(AMessage);
    TStreamOp.WriteAnsiString(LRequestStream,Lraw);
    LRequestStream.position := 0;
    for i := 0 to FNetData.ConnectionsCountAll-1 do begin
      LNetConnection := FNetData.Connection(i);
      if Assigned(LNetConnection) And (FNetData.ConnectionLock(Self,LNetConnection,100)) then begin
        try
          if LNetConnection.Connected then begin
            inc(Result);
            LNetConnection.DoSend(ntp_request,CT_NetProtocol_Extra_NetOp_GIVE_ME_AN_ACCOUNT,0,FNetData.NewRequestId,LRequestStream);
          end;
        finally
          FNetData.ConnectionUnlock(LNetConnection);
        end;
      end;
    end;
  finally
    LRequestStream.Free;
  end;
end;

procedure TPCTNetDataExtraMessages.DoGiveMeAnAccount(ANetData: TNetData;
  ASenderConnection: TNetConnection; const AHeaderData: TNetHeaderData;
  AReceivedData, AResponseData: TStream);
var LSenderPublicKey : TAccountKey;
  LIndexKey,LOnSafebox,LOnMempool : Integer;
  LAccount : TAccount;
  LOpRecoverFounds : TOpRecoverFounds;
  LPayload : TOperationPayload;
  LErrors, LSenderMessage : String;
  LWord : Word;
  LAccOrd : TAccountsOrderedByUpdatedBlock;
  LRaw : TRawBytes;
begin
  if Not (AHeaderData.header_type in [ntp_request,ntp_autosend]) then Exit; // Nothing to do
  // Protection to allow spam
  if ANetData.IpInfos.Update_And_ReachesLimits(ASenderConnection.Client.RemoteHost,'EXTRA','GIVE_ME_AN_ACCOUNT',
    AHeaderData.buffer_data_length,True,
    TArray<TLimitLifetime>.Create(TLimitLifetime.Create(300,2,20000))) then Exit;
  // Read info
  if TStreamOp.ReadAccountKey(AReceivedData,LSenderPublicKey)<=0 then Exit;
  if TStreamOp.ReadString(AReceivedData,LSenderMessage)<0 then Exit;

  if FNode.GetAccountsAvailableByPublicKey(LSenderPublicKey,LOnSafebox,LOnMempool)>0 then begin
    // Exit;
    TLog.NewLog(ltdebug,ClassName,Format('Not Sending to %s because PublicKey %s is used %d and mempool %d',[ASenderConnection.Client.RemoteHost,
      TAccountComp.AccountPublicKeyExport(LSenderPublicKey),LOnSafebox,LOnMempool]));
    Lword := 0;
    AResponseData.Write(Lword,2);
    Exit;
  end;

  LAccOrd := FNode.Bank.SafeBox.AccountsOrderedByUpdatedBlock;
  if Assigned(LAccOrd) then begin
    LAccount := CT_Account_NUL;
    if LAccOrd.First(LIndexKey) then begin
      LAccount := FNode.GetMempoolAccount(LIndexKey);
      while (Random(100)>0) or (LAccount.balance>0) or (Length(LAccount.name)>0) do begin
        if Not LAccOrd.Next(LIndexKey) then Exit;
        LAccount := FNode.GetMempoolAccount(LIndexKey);
      end;
    end;
    //
  end;

  TLog.NewLog(ltdebug,ClassName,Format('Sending to %s Account %s PublicKey %s',
    [ASenderConnection.Client.RemoteHost,
     TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),
     TAccountComp.AccountPublicKeyExport(LSenderPublicKey)]));

  LOpRecoverFounds := TOpRecoverFounds.Create(FNode.Bank.SafeBox.CurrentProtocol,LAccount.account,LAccount.n_operation+1,0,LSenderPublicKey);
  try
    if FNode.AddOperation(Nil,LOpRecoverFounds,LErrors) then begin
      Lword := 1;
      AResponseData.Write(Lword,2);
      LRaw := LOpRecoverFounds.OperationHashValid(LOpRecoverFounds,0);
      TStreamOp.WriteAnsiString(AResponseData,LRaw);
    end else begin
      Lword := 0;
      AResponseData.Write(Lword,2);
      TLog.NewLog(ltdebug,ClassName,Format('Error %s sending to %s Account %s PublicKey %s',
        [LErrors, ASenderConnection.Client.RemoteHost,
         TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),
         TAccountComp.AccountPublicKeyExport(LSenderPublicKey)]));
    end;
  finally
    LOpRecoverFounds.Free;
  end;
end;

{$IFDEF TESTNET}
procedure TPCTNetDataExtraMessages.DoGiveMeMoney(ANetData: TNetData;
  ASenderConnection: TNetConnection; const AHeaderData: TNetHeaderData;
  AReceivedData, AResponseData: TStream);
var LSenderAccount : Cardinal;
  LIndexKey : Integer;
  LAccount : TAccount;
  LOpTransaction : TOpTransaction;
  LPayload : TOperationPayload;
  LErrors : String;
  LSendAmount : Int64;
  LInt : Integer;
begin
  if Not (AHeaderData.header_type in [ntp_request,ntp_autosend]) then Exit; // Nothing to do
  // Protection to allow spam
  if ANetData.IpInfos.Update_And_ReachesLimits(ASenderConnection.Client.RemoteHost,'EXTRA','GIVE_ME_MONEY',AHeaderData.buffer_data_length,True,
    TArray<TLimitLifetime>.Create(TLimitLifetime.Create(300,2,20000))) then Exit;
  // Read info
  if AReceivedData.Read(LSenderAccount,SizeOf(LSenderAccount))<>SizeOf(LSenderAccount) then Exit;
  if LSenderAccount >= FNode.Bank.AccountsCount then Exit; // Not exists
  if Not RandomGetWalletKeysAccount(FNode.Bank.SafeBox,FWalletKeys,1000,-1,LIndexKey,LAccount) then Exit;
  LInt := Integer(LAccount.balance);
  if LInt>0 then LSendAmount := Random(LInt)
  else LSendAmount := 0;
  if LSendAmount<=0 then Exit;

  // Send
  LPayload := CT_TOperationPayload_NUL;
  LPayload.payload_raw.FromString('Free Money to '+ASenderConnection.Client.RemoteHost);
  LOpTransaction := TOpTransaction.CreateTransaction(FNode.Bank.SafeBox.CurrentProtocol,LAccount.account,LAccount.n_operation+1,
    LSenderAccount,FWalletKeys.Key[LIndexKey].PrivateKey,LSendAmount,0,LPayload);
  try
    FNode.AddOperation(Nil,LOpTransaction,LErrors);
  finally
    LOpTransaction.Free;
  end;
  // Response
  AResponseData.Write(LAccount.account,SizeOf(LAccount.account));
  AResponseData.Write(LSendAmount,SizeOf(LSendAmount));
end;
{$ENDIF}

class function TPCTNetDataExtraMessages.InitNetDataExtraMessages(ANode: TNode;
  ANetData: TNetData; AWalletKeys: TWalletKeysExt): TPCTNetDataExtraMessages;
begin
  if not Assigned(_PCTNetDataExtraMessages) then begin
    TLog.NewLog(ltinfo,ClassName,'InitNetDataExtraMessages');
    _PCTNetDataExtraMessages := TPCTNetDataExtraMessages.Create(ANode,ANetData,AWalletKeys);
  end;
  Result := _PCTNetDataExtraMessages;
end;

procedure TPCTNetDataExtraMessages.OnTNetDataProcessReservedAreaMessage(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
begin
  TLog.NewLog(ltdebug,ClassName,Format('Received extra message from %s Operation:%d',[ASenderConnection.ClientRemoteAddr,AHeaderData.operation]));
  case AHeaderData.operation of
    CT_NetProtocol_Extra_NetOp_GIVE_ME_AN_ACCOUNT : DoGiveMeAnAccount(ANetData,ASenderConnection,AHeaderData,AReceivedData,AResponseData);
    {$IFDEF TESTNET}
    CT_NetProtocol_Extra_NetOp_GIVE_ME_MONEY : DoGiveMeMoney(ANetData,ASenderConnection,AHeaderData,AReceivedData,AResponseData);
    {$ENDIF}
  end;
end;

function TPCTNetDataExtraMessages.RandomGetWalletKeysAccount(
  const ASafebox: TPCSafeBox; AWalletKeys: TWalletKeysExt; AMinBalance,
  AMaxBalance: Int64; out AKey: Integer; out AAccount: TAccount): Boolean;
var LRoundsIKey, LRoundsNAccount : Boolean;
  iInt : Integer;
  L0Account : TAccount;
begin
  Result := False; AKey := -1; AAccount := CT_Account_NUL;
  if (AWalletKeys.AccountsKeyList.Count<=0) then Exit;
  if ASafebox.AccountsCount>0 then L0Account := ASafebox.Account(0)
  else L0Account := CT_Account_NUL;
  AKey := Random( AWalletKeys.AccountsKeyList.Count );
  LRoundsIKey := False;
  Repeat
    if (AWalletKeys.Key[AKey].HasPrivateKey) And (Assigned(AWalletKeys.Key[AKey].PrivateKey)) And (AWalletKeys.AccountsKeyList.AccountKeyList[AKey].Count>0) then begin
      // Sender:
      LRoundsNAccount := False;
      iInt := Random(aWalletKeys.AccountsKeyList.AccountKeyList[AKey].Count);
      Repeat
        AAccount := ASafebox.Account( AWalletKeys.AccountsKeyList.AccountKeyList[AKey].Get( iInt ) );
        //
        if (L0Account.account<>AAccount.account) And (L0Account.account_type<>AAccount.account) And
           (AAccount.balance>=AMinBalance) and
           ((AMaxBalance<0) or (AAccount.balance<=AMaxBalance)) then Exit(True);
        //
        if (iInt < AWalletKeys.AccountsKeyList.AccountKeyList[AKey].Count-1) then inc(iInt)
        else begin
          iInt := 0;
          if LRoundsNAccount then Break
          else LRoundsNAccount:=True;
        end;
      until (Result);
    end;
    if (AKey<aWalletKeys.AccountsKeyList.Count-1) then inc(AKey) else begin
      AKey:=0;
      if LRoundsIKey then Break
      else LRoundsIKey:=True;
    end;
  until (Result);
end;

initialization
  _PCTNetDataExtraMessages := Nil;
finalization
  FreeAndNil(_PCTNetDataExtraMessages);
end.
