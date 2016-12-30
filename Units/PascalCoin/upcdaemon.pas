unit upcdaemon;

{$mode objfpc}{$H+}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, SysUtils, daemonapp,
  SyncObjs, UOpenSSL, UCrypto, UNode, UFileStorage, UFolderHelper, UWalletKeys, UConst, ULog, UNetProtocol,
  UThread, URPC;

Type
  { TPCDaemonThread }

  TPCDaemonThread = Class(TPCThread)
  private
  protected
    Procedure BCExecute; override;
  public
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

  { TPCDaemonMapper }

  TPCDaemonMapper = Class(TCustomDaemonMapper)
  private
    FLog : TLog;
    FLogRPCCalls : TLog;
    procedure OnPascalCoinInThreadLog(logtype : TLogType; Time : TDateTime; AThreadID : Cardinal; Const sender, logtext : AnsiString);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  end;


implementation

{ TPCDaemonThread }

procedure TPCDaemonThread.BCExecute;
var
  FNode : TNode;
  FWalletKeys : TWalletKeysExt;
  FRPC : TRPCServer;
begin
  TLog.NewLog(ltinfo,Classname,'START PascalCoin Server');
  try
    try
      FWalletKeys := TWalletKeysExt.Create(Nil);
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

      if (Application.HasOption('r','logrpc')) then begin
        FRPC.LogFileName:= TFolderHelper.GetPascalCoinDataFolder+PathDelim+'pascalcoin_rpc.log';
      end;

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
      end;
    end;
  finally
    TLog.NewLog(ltinfo,Classname,'EXIT PascalCoin Server');
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
  TLog.NewLog(ltinfo,ClassName,'Daemon Start '+BoolToStr(Result));
  FThread:=TPCDaemonThread.Create(True);
  FThread.OnTerminate:=@ThreadStopped;
  FThread.FreeOnTerminate:=False;
  FThread.Resume;
end;

function TPCDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  TLog.NewLog(ltinfo,ClassName,'Daemon Stop: '+BoolToStr(Result));
  FThread.Terminate;
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
  TLog.NewLog(ltinfo,ClassName,'Daemon Shutdown: '+BoolToStr(Result));
  FThread.Terminate;
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

procedure TPCDaemonMapper.OnPascalCoinInThreadLog(logtype: TLogType;
  Time: TDateTime; AThreadID: Cardinal; const sender, logtext: AnsiString);
Var s : AnsiString;
begin
  if AThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
  WriteLn(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(AThreadID,8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
end;

constructor TPCDaemonMapper.Create(AOwner: TComponent);
Var D : TDaemonDef;
begin
  inherited Create(AOwner);
  if (Application.HasOption('l','log')) then begin
    FLog := TLog.Create(Nil);
    FLog.SaveTypes:=CT_TLogTypes_ALL;
    FLog.FileName:=TFolderHelper.GetPascalCoinDataFolder+PathDelim+'pascalcoin_'+FormatDateTime('yyyymmddhhnn',Now)+'.log';
  end;
  D:=DaemonDefs.Add as TDaemonDef;
  D.DisplayName:='Pascal Coin Daemon';
  D.Name:='PascalCoinDaemon';
  D.DaemonClassName:='TPCDaemon';
  D.WinBindings.ServiceType:=stWin32;
end;

destructor TPCDaemonMapper.Destroy;
begin
  If Assigned(FLog) then begin
    FLog.OnInThreadNewLog:=Nil;
    FreeAndNil(FLog);
  end;
  inherited Destroy;
end;

end.

