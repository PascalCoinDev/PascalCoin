program pascalcoin_daemon;

{$mode objfpc}{$H+}
{$define usecthreads}
{$apptype gui}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  Classes, daemonapp, 
  SyncObjs,
  UOpenSSL, UCrypto, UNode, UFileStorage, UFolderHelper, UWalletKeys, UConst, ULog, UNetProtocol,
  URPC;

Type

  { TPCDaemonThread }

  TPCDaemonThread = Class(TThread)
    procedure OnPascalCoinLog(logtype : TLogType; Time : TDateTime; AThreadID : Cardinal; Const sender, logtext : AnsiString);
    Procedure Execute; override;
  end;

  { TPCDaemon }

  TPCDaemon = Class(TCustomDaemon)
  Private
    FThread : TPCDaemonThread;
    Procedure ThreadStopped (Sender : TObject);
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

{ TPCDaemonThread }

procedure TPCDaemonThread.OnPascalCoinLog(logtype: TLogType; Time: TDateTime;
  AThreadID: Cardinal; const sender, logtext: AnsiString);
Var s : AnsiString;
begin
  if (logtype=ltdebug)  then exit;
  if AThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
  WriteLn(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(AThreadID,8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
end;

Procedure AWriteln(MSg : String; B : Boolean);
begin
  Application.Log(etcustom,Msg+BoolToStr(B));
end;

procedure TPCDaemonThread.Execute;
var
  FNode : TNode;
  FWalletKeys : TWalletKeysExt;
  FRPC : TRPCServer;
  FLog : TLog;
begin
  FLog := TLog.Create(Nil);
  FLog.OnInThreadNewLog:=@OnPascalCoinLog;
  try
  FWalletKeys := TWalletKeysExt.Create(Nil);

  TLog.NewLog(ltinfo,Classname,'PascalCoin Server');
  // Load Node
  // Check OpenSSL dll
  if Not LoadSSLCrypt then raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
  TCrypto.InitCrypto;
  FWalletKeys.WalletFileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'WalletKeys.dat';
  // Creating Node:
  FNode := TNode.Node;
  // RPC Server
  FRPC := TRPCServer.Create;
  FRPC.WalletKeys := FWalletKeys;
  FRPC.Active:=true;
  // Check Database
  FNode.Bank.StorageClass := TFileStorage;
  TFileStorage(FNode.Bank.Storage).DatabaseFolder := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'Data';
  // Reading database
  FNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
  FWalletKeys.SafeBox := FNode.Node.Bank.SafeBox;
  FNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  FNode.Node.NetServer.Active := true;

  Repeat
    Sleep(100);
  Until Terminated;



  FreeAndNil(FRPC);
  FNode.NetServer.Active := false;
  TNetData.NetData.Free;
  FreeAndNil(FNode);
  except
    on e:Exception do begin
      TLog.NewLog(lterror,Classname,'Exception '+E.Classname+': '+E.Message);
      AWriteln('Exception '+E.Classname+': '+E.Message,false);
    end;
  end;
end;

{ TPCDaemon }

procedure TPCDaemon.ThreadStopped(Sender: TObject);
begin
  FreeAndNil(FThread);
end;

function TPCDaemon.Start: Boolean;
begin
  Result:=inherited Start;
  AWriteln('Daemon Start',Result);
  FThread:=TPCDaemonThread.Create(True);
  FThread.OnTerminate:=@ThreadStopped;
  FThread.FreeOnTerminate:=False;
  FThread.Resume;
end;

function TPCDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  AWriteln('Daemon Stop: ',Result);
  FThread.Terminate;
end;

function TPCDaemon.Pause: Boolean;
begin
  Result:=inherited Pause;
  AWriteln('Daemon pause: ',Result);
  FThread.Suspend;
end;

function TPCDaemon.Continue: Boolean;
begin
  Result:=inherited Continue;
  AWriteln('Daemon continue: ',Result);
  FThread.Resume;
end;

function TPCDaemon.Execute: Boolean;
begin
  Result:=inherited Execute;
  AWriteln('Daemon execute: ',Result);
end;

function TPCDaemon.ShutDown: Boolean;
begin
  Result:=inherited ShutDown;
  AWriteln('Daemon Shutdown: ',Result);
  FThread.Terminate;
end;

function TPCDaemon.Install: Boolean;
begin
  Result:=inherited Install;
  AWriteln('Daemon Install: ',Result);
end;

function TPCDaemon.UnInstall: boolean;
begin
  Result:=inherited UnInstall;
  AWriteln('Daemon UnInstall: ',Result);
end;

Type

  { TPCDaemonMapper }

  TPCDaemonMapper = Class(TCustomDaemonMapper)
    Constructor Create(AOwner : TComponent); override;
  end;

{ TPCDaemonMapper }

constructor TPCDaemonMapper.Create(AOwner: TComponent);

Var
  D : TDaemonDef;

begin
  inherited Create(AOwner);
  D:=DaemonDefs.Add as TDaemonDef;
  D.DisplayName:='Pascal Coin Daemon';
  D.Name:='PascalCoinDaemon';
  D.DaemonClassName:='TPCDaemon';
  D.WinBindings.ServiceType:=stWin32;
end;

begin
  TCrypto.InitCrypto;
  RegisterDaemonClass(TPCDaemon);
  RegisterDaemonMapper(TPCDaemonMapper);
  Application.Title:='Daemon application';
  Application.Run;
end.

