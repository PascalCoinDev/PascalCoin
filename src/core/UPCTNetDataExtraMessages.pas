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

{$I config.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

Uses Classes, UThread, UAccounts, UBlockChain, UNetProtocol, SysUtils, UNode,
  UWallet, UNetProtection,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TPCTNetDataExtraMessages = Class
  private
    FNode : TNode;
    FNetData : TNetData;
    FWalletKeys : TWalletKeysExt;
    function DoAskForFreeAccount(ANewPubliKey : TAccountKey) : Integer;
    {$IFDEF TESTNET}
    procedure DoGiveMeAnAccount(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
    procedure DoGiveMeMoney(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
    {$ENDIF}
    procedure OnTNetDataProcessReservedAreaMessage(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
    function RandomGetWalletKeysAccount(const ASafebox : TPCSafeBox; AWalletKeys: TWalletKeysExt; AMinBalance, AMaxBalance : Int64; out AKey: Integer; out AAccount: TAccount): Boolean;
  protected
  public
    class function InitNetDataExtraMessages(ANode : TNode; ANetData : TNetData; AWalletKeys : TWalletKeysExt) : TPCTNetDataExtraMessages;
    constructor Create(ANode : TNode; ANetData : TNetData; AWalletKeys : TWalletKeysExt);
    destructor Destroy; override;
    class function AskForFreeAccount(ANewPubliKey : TAccountKey) : Integer;
  End;

const
  CT_NetProtocol_Extra_NetOp_GIVE_ME_AN_ACCOUNT   = $1101;
  CT_NetProtocol_Extra_NetOp_GIVE_ME_MONEY        = $1102;

implementation

Uses UOpTransaction, UBaseTypes, ULog;

var _PCTNetDataExtraMessages : TPCTNetDataExtraMessages = Nil;

{ TPCTNetDataExtraMessages }

class function TPCTNetDataExtraMessages.AskForFreeAccount(ANewPubliKey : TAccountKey): Integer;
begin
  if Assigned(_PCTNetDataExtraMessages) then begin
    Result := _PCTNetDataExtraMessages.DoAskForFreeAccount(ANewPubliKey);
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

function TPCTNetDataExtraMessages.DoAskForFreeAccount(ANewPubliKey : TAccountKey): Integer;
var i : Integer;
  LNetConnection : TNetConnection;
  LRequestStream : TMemoryStream;
begin
  Result := 0;
  LRequestStream := TMemoryStream.Create;
  try
    TStreamOp.WriteAccountKey(LRequestStream,ANewPubliKey);
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

{$IFDEF TESTNET}
procedure TPCTNetDataExtraMessages.DoGiveMeAnAccount(ANetData: TNetData;
  ASenderConnection: TNetConnection; const AHeaderData: TNetHeaderData;
  AReceivedData, AResponseData: TStream);
var LSenderPublicKey : TAccountKey;
  LIndexKey : Integer;
  LAccount : TAccount;
  LOpChangeKey : TOpChangeKey;
  LPayload : TRawBytes;
  LErrors : String;
  LWord : Word;
begin
  if Not (AHeaderData.header_type in [ntp_request,ntp_autosend]) then Exit; // Nothing to do
  // Protection to allow spam
  if ANetData.IpInfos.Update_And_ReachesLimits(ASenderConnection.Client.RemoteHost,'EXTRA','GIVE_ME_AN_ACCOUNT',AHeaderData.buffer_data_length,True,
    TArray<TLimitLifetime>.Create(TLimitLifetime.Create(300,2,20000))) then Exit;
  // Read info
  if TStreamOp.ReadAccountKey(AReceivedData,LSenderPublicKey)<=0 then Exit;
  if Not RandomGetWalletKeysAccount(FNode.Bank.SafeBox,FWalletKeys,0,10000,LIndexKey,LAccount) then Exit;
  // Send
  LPayload.FromString('Free Account to '+ASenderConnection.Client.RemoteHost);
  LOpChangeKey := TOpChangeKey.Create(FNode.Bank.SafeBox.CurrentProtocol,LAccount.account,LAccount.n_operation+1,
    LAccount.account,FWalletKeys.Key[LIndexKey].PrivateKey,LSenderPublicKey,0,LPayload);
  try
    FNode.AddOperation(Nil,LOpChangeKey,LErrors);
  finally
    LOpChangeKey.Free;
  end;
  // Response
  TStreamOp.WriteAccountKey(AResponseData,LSenderPublicKey);
  LWord := 1;
  AResponseData.Write(LWord,SizeOf(LWord));
  AResponseData.Write(LAccount.account,SizeOf(LAccount.account));
end;

procedure TPCTNetDataExtraMessages.DoGiveMeMoney(ANetData: TNetData;
  ASenderConnection: TNetConnection; const AHeaderData: TNetHeaderData;
  AReceivedData, AResponseData: TStream);
var LSenderAccount : Cardinal;
  LIndexKey : Integer;
  LAccount : TAccount;
  LOpTransaction : TOpTransaction;
  LPayload : TRawBytes;
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
  LPayload.FromString('Free Money to '+ASenderConnection.Client.RemoteHost);
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
    _PCTNetDataExtraMessages := TPCTNetDataExtraMessages.Create(ANode,ANetData,AWalletKeys);
  end;
  Result := _PCTNetDataExtraMessages;
end;

procedure TPCTNetDataExtraMessages.OnTNetDataProcessReservedAreaMessage(ANetData : TNetData; ASenderConnection : TNetConnection; const AHeaderData : TNetHeaderData; AReceivedData : TStream; AResponseData : TStream);
begin
  TLog.NewLog(ltdebug,ClassName,Format('Received extra message from %s Operation:%d',[ASenderConnection.ClientRemoteAddr,AHeaderData.operation]));
  {$IFDEF TESTNET}
  case AHeaderData.operation of
    CT_NetProtocol_Extra_NetOp_GIVE_ME_AN_ACCOUNT : DoGiveMeAnAccount(ANetData,ASenderConnection,AHeaderData,AReceivedData,AResponseData);
    CT_NetProtocol_Extra_NetOp_GIVE_ME_MONEY : DoGiveMeMoney(ANetData,ASenderConnection,AHeaderData,AReceivedData,AResponseData);
  end;
  {$ENDIF}
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
