unit upcdaemon;

{ Copyright (c) 2016-2020 by Albert Molina

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

{$mode objfpc}{$H+}

{$I ./../config.inc}

interface

uses
  Classes, SysUtils, daemonapp,
  SyncObjs, UOpenSSL, UCrypto, UNode, UFileStorage, UFolderHelper, UWallet, UConst, ULog, UNetProtocol,
  IniFiles, UBaseTypes,
  {$IF Defined(FPC) and Defined(WINDOWS)}windows,jwawinsvc,crt,{$ENDIF}
  UThread, URPC, UPoolMining, UAccounts, UPCDataTypes;

Const
  CT_INI_SECTION_GLOBAL = 'GLOBAL';
  CT_INI_IDENT_SAVELOGS = 'SAVELOGS';
  CT_INI_IDENT_NODE_PORT = 'NODE_PORT';
  CT_INI_IDENT_NODE_MAX_CONNECTIONS = 'NODE_MAX_CONNECTIONS';
  CT_INI_IDENT_RPC_PORT = 'RPC_PORT';
  CT_INI_IDENT_RPC_WHITELIST = 'RPC_WHITELIST';
  CT_INI_IDENT_RPC_ALLOWUSEPRIVATEKEYS = 'RPC_ALLOWUSEPRIVATEKEYS';
  CT_INI_IDENT_RPC_SAVELOGS = 'RPC_SAVELOGS';
  CT_INI_IDENT_RPC_SERVERMINER_PORT = 'RPC_SERVERMINER_PORT';
  CT_INI_IDENT_MINER_B58_PUBLICKEY = 'RPC_SERVERMINER_B58_PUBKEY';
  CT_INI_IDENT_MINER_NAME = 'RPC_SERVERMINER_NAME';
  CT_INI_IDENT_MINER_MAX_CONNECTIONS = 'RPC_SERVERMINER_MAX_CONNECTIONS';
  CT_INI_IDENT_MINER_MAX_OPERATIONS_PER_BLOCK = 'RPC_SERVERMINER_MAX_OPERATIONS_PER_BLOCK';
  CT_INI_IDENT_MINER_MAX_ZERO_FEE_OPERATIONS  = 'RPC_SERVERMINER_MAX_ZERO_FEE_OPERATIONS';
  CT_INI_IDENT_LOWMEMORY = 'LOWMEMORY';
  CT_INI_IDENT_MINPENDINGBLOCKSTODOWNLOADCHECKPOINT = 'MINPENDINGBLOCKSTODOWNLOADCHECKPOINT';
  CT_INI_IDENT_PEERCACHE = 'PEERCACHE';
  CT_INI_IDENT_DATA_FOLDER = 'DATAFOLDER';
  {$IFDEF USE_ABSTRACTMEM}
  CT_INI_IDENT_ABSTRACTMEM_MAX_CACHE_MB = 'ABSTRACTMEM_MAX_CACHE_MB';
  CT_INI_IDENT_ABSTRACTMEM_USE_CACHE_ON_LISTS = 'ABSTRACTMEM_USE_CACHE_ON_LISTS';
  CT_INI_IDENT_ABSTRACTMEM_CACHE_MAX_ACCOUNTS = 'ABSTRACTMEM_CACHE_MAX_ACCOUNTS';
  CT_INI_IDENT_ABSTRACTMEM_CACHE_MAX_PUBKEYS = 'ABSTRACTMEM_CACHE_MAX_PUBKEYS';
  {$ENDIF}

Type
  { TPCDaemonThread }

  TPCDaemonThread = Class(TPCThread)
  private
    FIniFile : TIniFile;
    FMaxBlockToRead: Int64;
    FLastNodesCacheUpdatedTS : TTickCount;
    function GetDataFolder : String;
    procedure OnNetDataReceivedHelloMessage(Sender : TObject);
    procedure OnInitSafeboxProgressNotify(sender : TObject; const message : String; curPos, totalCount : Int64);
  protected
    Procedure BCExecute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property MaxBlockToRead : Int64 read FMaxBlockToRead write FMaxBlockToRead;
  end;

  { TPCDaemon }

  TPCDaemon = Class(TCustomDaemon)
  Private
    FThread : TPCDaemonThread;
  public
    Function Start : Boolean; override;
    Function Stop : Boolean; override;
    Function Pause : Boolean; override;
    Function Continue : Boolean; override;
    Function Execute : Boolean; override;
    Function ShutDown : Boolean; override;
    Function Install : Boolean; override;
    Function UnInstall: boolean; override;
  end;

  { TPCDaemonMapper }

  TPCDaemonMapper = Class(TCustomDaemonMapper)
  private
    FLog : TLog;
  protected
    Procedure DoOnCreate; override;
    Procedure DoOnDestroy; override;
    Procedure DoOnRun; override;
  public
  end;


implementation

{$IFDEF TESTNET}
uses UPCTNetDataExtraMessages;
{$ENDIF}

Var _FLog : TLog;

{ TPCDaemonThread }

function TPCDaemonThread.GetDataFolder: String;
Var LIniDataFolder : String;
begin
  LIniDataFolder := FIniFile.ReadString(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_DATA_FOLDER,'').Trim;
  if (LIniDataFolder.Length=0) then begin
    LIniDataFolder:=TNode.GetPascalCoinDataFolder;
  end else begin
    TNode.SetPascalCoinDataFolder(LIniDataFolder);
  end;
  ForceDirectories(LIniDataFolder);
  Result := LIniDataFolder;
end;

procedure TPCDaemonThread.OnNetDataReceivedHelloMessage(Sender: TObject);
Var LNsarr : TNodeServerAddressArray;
  i : Integer;
  s : AnsiString;
begin
  If (TPlatform.GetElapsedMilliseconds(FLastNodesCacheUpdatedTS)<60000) then Exit; // Prevent continuous saving
  FLastNodesCacheUpdatedTS := TPlatform.GetTickCount;
  // Update node servers Peer Cache
  LNsarr := TNetData.NetData.NodeServersAddresses.GetValidNodeServers(true,0);
  s := '';
  for i := low(LNsarr) to High(LNsarr) do begin
    if (s<>'') then s := s+';';
    s := s + LNsarr[i].ip+':'+IntToStr( LNsarr[i].port );
  end;
  FIniFile.WriteString(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_PEERCACHE,s);
  TNode.Node.PeerCache := s;
end;

procedure TPCDaemonThread.OnInitSafeboxProgressNotify(sender: TObject;
  const message: String; curPos, totalCount: Int64);
begin
  TLog.NewLog(ltdebug,ClassName,Format('Progress (%d/%d): %s',[curPos,totalCount,message]));
end;

procedure TPCDaemonThread.BCExecute;
var
  FNode : TNode;
  FWalletKeys : TWalletKeysExt;
  FRPC : TRPCServer;
  FMinerServer : TPoolMiningServer;

  Procedure InitRPCServer;
  Var port : Integer;
  Begin
    port := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_PORT,-1);
    if (port<=0) then begin
      FIniFile.WriteInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_PORT,CT_JSONRPC_Port);
      port:=CT_JSONRPC_Port;
      TLog.NewLog(ltInfo,ClassName,'Saving RPC server port to IniFile: '+IntToStr(port));
    end;
    FRPC := TRPCServer.Create;
    FRPC.WalletKeys := FWalletKeys;
    FRPC.Port:=port;
    FRPC.Active:=true;
    FRPC.AllowUsePrivateKeys:=FIniFile.ReadBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_ALLOWUSEPRIVATEKEYS,True);
    FRPC.ValidIPs:=FIniFile.ReadString(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_WHITELIST,'127.0.0.1;');
    TLog.NewLog(ltInfo,ClassName,'RPC server is active on port '+IntToStr(port));
    If FIniFile.ReadBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_SAVELOGS,true) then begin
      FIniFile.WriteBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_SAVELOGS,true);
      FRPC.LogFileName:= GetDataFolder+PathDelim+'pascalcoin_rpc.log';
      TLog.NewLog(ltInfo,ClassName,'Activating RPC logs on file '+FRPC.LogFileName);
    end else begin
      FIniFile.WriteBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_SAVELOGS,false);
      TLog.NewLog(ltInfo,ClassName,'RPC logs not enabled on IniFile value '+CT_INI_IDENT_RPC_SAVELOGS+'=0');
    end;
  end;

  Procedure InitRPCMinerServer;
  Var i, port, maxconnections : Integer;
    s : String;
    pubkey : TAccountKey;
    errors : AnsiString;
    ECPK : TECPrivateKey;
  Begin
    i := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_SERVERMINER_PORT,-1);
    if (i<0) then i:=CT_JSONRPCMinerServer_Port;
    if (i>0) then begin
      port := i;
      FIniFile.WriteInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_RPC_SERVERMINER_PORT,port);
      pubkey := CT_TECDSA_Public_Nul;
      s := Trim(FIniFile.ReadString(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_MINER_B58_PUBLICKEY,''));
      If (s='') Or (Not TAccountComp.AccountKeyFromImport(s,pubkey,errors)) then begin
        If s<>'' then TLog.NewLog(lterror,Classname,'Invalid INI file public key: '+errors);
        i := 0;
        While (i<FWalletKeys.Count) And (pubkey.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) do begin
          if (Length(FWalletKeys.Key[i].CryptedKey)>0) then pubkey := FWalletKeys[i].AccountKey
          else inc(i);
        end;
        if (pubkey.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
          // New key
          ECPK := TECPrivateKey.Create;
          try
            ECPK.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
            FWalletKeys.AddPrivateKey('RANDOM NEW BY DAEMON '+FormatDateTime('yyyy-mm-dd hh:nn:dd',now),ECPK);
            pubkey := ECPK.PublicKey;
            FIniFile.WriteString(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_MINER_B58_PUBLICKEY,
              TAccountComp.AccountPublicKeyExport(pubkey));
            TLog.NewLog(ltInfo,ClassName, 'Generated new pubkey for miner: '+TAccountComp.AccountPublicKeyExport(pubkey));
          finally
            ECPK.Free;
          end;
        end;
      end else begin
        // pubkey is mine?
        if (FWalletKeys.IndexOfAccountKey(pubkey)<0) then begin
          TLog.NewLog(lterror,classname,'WARNING: Using a public key without private key in wallet! '+TAccountComp.AccountPublicKeyExport(pubkey));
        end;
      end;
      i := FWalletKeys.IndexOfAccountKey(pubkey);
      s := Trim(FIniFile.ReadString(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_MINER_NAME,''));
      if (SameText(s,'TIME')) then begin
        s := FormatDateTime('yyyy-mm-dd hh:nn',Now);
        TLog.NewLog(ltInfo,ClassName,'Generated new miner name: '+s);
      end;
      maxconnections:=FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_MINER_MAX_CONNECTIONS,1000);
      TLog.NewLog(ltinfo,ClassName,Format('Activating RPC Miner Server on port %d, name "%s", max conections %d and public key %s',
        [port,s,maxconnections,TAccountComp.AccountPublicKeyExport(pubkey)]));
      FMinerServer := TPoolMiningServer.Create;
      FMinerServer.UpdateAccountAndPayload(pubkey,TEncoding.ANSI.GetBytes(s));
      FMinerServer.Port:=port;
      FMinerServer.Active:=True;
      FMinerServer.MaxConnections:=maxconnections;
      FMinerServer.MaxOperationsPerBlock := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_MINER_MAX_OPERATIONS_PER_BLOCK,CT_MAX_Operations_per_block_by_miner);
      FMinerServer.Max0FeeOperationsPerBlock := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_MINER_MAX_ZERO_FEE_OPERATIONS,CT_MAX_0_fee_operations_per_block_by_miner);
    end else begin
      TLog.NewLog(ltinfo,ClassName,'RPC Miner Server NOT ACTIVE (Ini file is '+CT_INI_IDENT_RPC_SERVERMINER_PORT+'=0)');
    end;
  end;
  {$IFDEF USE_ABSTRACTMEM}
  var LMaxMemMb : Integer;
    LUseCacheOnMemLists : Boolean;
    LCacheMaxAccounts, LCacheMaxPubKeys : Integer;
  {$ENDIF}
begin
  FMInerServer := Nil;
  TLog.NewLog(ltinfo,Classname,'START PascalCoin Server');
  try
    try
      FWalletKeys := TWalletKeysExt.Create(Nil);
      // Load Node
      // Check OpenSSL dll
      if Not LoadSSLCrypt then begin
        raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
      end;
      TCrypto.InitCrypto;
      FWalletKeys.WalletFileName := GetDataFolder+PathDelim+'WalletKeys.dat';
      // Creating Node:
      FNode := TNode.Node;
      {$IFDEF USE_ABSTRACTMEM}
      LMaxMemMb := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_ABSTRACTMEM_MAX_CACHE_MB,100);
      LUseCacheOnMemLists:= FIniFile.ReadBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_ABSTRACTMEM_USE_CACHE_ON_LISTS,False);
      LCacheMaxAccounts := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_ABSTRACTMEM_CACHE_MAX_ACCOUNTS,10000);
      LCacheMaxPubKeys := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_ABSTRACTMEM_CACHE_MAX_PUBKEYS,5000);

      TLog.NewLog(ltinfo,ClassName,Format('Init abstract mem library to %d mb %d accounts and %d Pubkeys and use cache lists: %s',[LMaxMemMb,LCacheMaxAccounts,LCacheMaxPubKeys,LUseCacheOnMemLists.ToString]));
      FNode.Bank.SafeBox.PCAbstractMem.MaxMemUsage := LMaxMemMb * 1024 * 1024;
      FNode.Bank.SafeBox.PCAbstractMem.UseCacheOnAbstractMemLists := LUseCacheOnMemLists;
      FNode.Bank.SafeBox.PCAbstractMem.MaxAccountsCache := LCacheMaxAccounts;
      FNode.Bank.SafeBox.PCAbstractMem.MaxAccountKeysCache := LCacheMaxPubKeys;
      {$ENDIF}
      {$IFDEF TESTNET}
      TPCTNetDataExtraMessages.InitNetDataExtraMessages(FNode,TNetData.NetData,FWalletKeys);
      {$ENDIF}
      // RPC Server
      InitRPCServer;
      Try
        // Check Database
        FNode.Bank.StorageClass := TFileStorage;
        TFileStorage(FNode.Bank.Storage).DatabaseFolder := GetDataFolder+PathDelim+'Data';
        TFileStorage(FNode.Bank.Storage).LowMemoryUsage := FIniFile.ReadBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_LOWMEMORY,False);
        // By default daemon will not download checkpoint except if specified on INI file
        TNetData.NetData.MinFutureBlocksToDownloadNewSafebox := FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_MINPENDINGBLOCKSTODOWNLOADCHECKPOINT,0);
        TNetData.NetData.OnReceivedHelloMessage:=@OnNetDataReceivedHelloMessage;
        FNode.PeerCache:=  FIniFile.ReadString(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_PEERCACHE,'');
        // Reading database
        FNode.InitSafeboxAndOperations(MaxBlockToRead,@OnInitSafeboxProgressNotify);
        FWalletKeys.SafeBox := FNode.Node.Bank.SafeBox;
        FNode.Node.NetServer.Port:=FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_NODE_PORT,CT_NetServer_Port);
        FNode.Node.NetServer.MaxConnections:=FIniFile.ReadInteger(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_NODE_MAX_CONNECTIONS,CT_MaxClientsConnected);
        FNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
        FNode.Node.NetServer.Active := true;

        // RPC Miner Server
        InitRPCMinerServer;
        Try
          Repeat
            Sleep(100);
          Until (Terminated) or (Application.Terminated);
        finally
          FreeAndNil(FMinerServer);
        end;
      Finally
        FreeAndNil(FRPC);
      end;
      FNode.NetServer.Active := false;
      TNetData.NetData.Free;
      FreeAndNil(FNode);
    except
      on e:Exception do begin
        TLog.NewLog(lterror,Classname,'Exception '+E.Classname+': '+E.Message);
      end;
    end;
  finally
    TLog.NewLog(ltinfo,Classname,'EXIT PascalCoin Server');
  end;
end;

constructor TPCDaemonThread.Create;
begin
  inherited Create(True);
  FLastNodesCacheUpdatedTS := TPlatform.GetTickCount;
  FIniFile := TIniFile.Create(ExtractFileDir(Application.ExeName)+PathDelim+'pascalcoin_daemon.ini');
  If FIniFile.ReadBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_SAVELOGS,true) then begin
    _FLog.SaveTypes:=CT_TLogTypes_ALL;
    _FLog.FileName:=TNode.GetPascalCoinDataFolder+PathDelim+'pascalcoin_'+FormatDateTime('yyyymmddhhnn',Now)+'.log';
    FIniFile.WriteBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_SAVELOGS,true);
  end else begin
    FIniFile.WriteBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_SAVELOGS,false);
  end;
  FMaxBlockToRead:=$FFFFFFFF;
  TLog.NewLog(ltinfo,ClassName,'Create');
end;

destructor TPCDaemonThread.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited Destroy;
end;


{ TPCDaemon }

function TPCDaemon.Start: Boolean;
begin
  Result:=inherited Start;
  TLog.NewLog(ltinfo,ClassName,'Daemon Start '+BoolToStr(Result));
  FThread:=TPCDaemonThread.Create;
  FThread.FreeOnTerminate:=True;
  if (Application.HasOption('b','block')) then begin
    FThread.MaxBlockToRead:=StrToInt64Def(Application.GetOptionValue('b','block'),$FFFFFFFF);
    TLog.NewLog(ltinfo,ClassName,'Max block to read: '+IntToStr(FThread.MaxBlockToRead));
  end;
  FThread.Start;
end;

function TPCDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  TLog.NewLog(ltinfo,ClassName,'Daemon Stop Start');
  FThread.Terminate;
  FThread.WaitFor;
  TLog.NewLog(ltinfo,ClassName,'Daemon Stop Finished');
end;

function TPCDaemon.Pause: Boolean;
begin
  Result:=inherited Pause;
  TLog.NewLog(ltinfo,ClassName,'Daemon pause: '+BoolToStr(Result));
  FThread.Suspend;
end;

function TPCDaemon.Continue: Boolean;
begin
  Result:=inherited Continue;
  TLog.NewLog(ltinfo,ClassName,'Daemon continue: '+BoolToStr(Result));
  FThread.Resume;
end;

function TPCDaemon.Execute: Boolean;
begin
  Result:=inherited Execute;
  TLog.NewLog(ltinfo,ClassName,'Daemon execute: '+BoolToStr(Result));
end;

function TPCDaemon.ShutDown: Boolean;
begin
  Result:=inherited ShutDown;
  TLog.NewLog(ltinfo,ClassName,'Daemon Shutdown Start');
  FThread.Terminate;
  FThread.WaitFor;
  TLog.NewLog(ltinfo,ClassName,'Daemon Shutdown Finished');
end;

function TPCDaemon.Install: Boolean;
begin
  Result:=inherited Install;
  TLog.NewLog(ltinfo,ClassName,'Daemon Install: '+BoolToStr(Result));
end;

function TPCDaemon.UnInstall: boolean;
begin
  Result:=inherited UnInstall;
  TLog.NewLog(ltinfo,ClassName,'Daemon UnInstall: '+BoolToStr(Result));
end;

{ TPCDaemonMapper }

procedure TPCDaemonMapper.DoOnCreate;
Var D : TDaemonDef;
begin
  inherited DoOnCreate;
  FLog := TLog.Create(Nil);
  _FLog := FLog;
  D:=DaemonDefs.Add as TDaemonDef;
  D.DisplayName:='PascalCoin Daemon';
  D.Name:='PascalCoinDaemon';
  D.DaemonClassName:='TPCDaemon';
  D.Options:=[doAllowStop];
  D.WinBindings.ServiceType:=stWin32;
end;

procedure TPCDaemonMapper.DoOnDestroy;
begin
  inherited DoOnDestroy;
  If Assigned(FLog) then begin
    FLog.OnInThreadNewLog:=Nil;
    FreeAndNil(FLog);
  end;
end;

procedure TPCDaemonMapper.DoOnRun;
{$IF Defined(FPC) and Defined(WINDOWS)}
var LDT : TPCDaemonThread;
{$ENDIF}
begin
  inherited DoOnRun;
  {$IF Defined(FPC) and Defined(WINDOWS)}
  // We are running -r command on windows
  if Application.HasOption('d','debug') then begin
    LDT:=TPCDaemonThread.Create;
    LDT.FreeOnTerminate:=True;
    if (Application.HasOption('b','block')) then begin
      LDT.MaxBlockToRead:=StrToInt64Def(Application.GetOptionValue('b','block'),$FFFFFFFF);
      TLog.NewLog(ltinfo,ClassName,'Max block to read: '+IntToStr(LDT.MaxBlockToRead));
    end;
    LDT.Start;
    repeat
      CheckSynchronize(10);
      Sleep(1);

      if Keypressed then begin
        if (ReadKey in ['q','Q']) then begin
          LDT.Terminate;
        end;
      end;

    until LDT.Terminated;
    LDT.Terminate;
    LDT.WaitFor;
    Application.Terminate;
  end;
  {$ENDIF}
end;

end.

