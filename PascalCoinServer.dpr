program PascalCoinServer;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SyncObjs,
  UAES in 'Units\PascalCoin\UAES.pas',
  UCrypto in 'Units\PascalCoin\UCrypto.pas',
  UConst in 'Units\PascalCoin\UConst.pas',
  ULog in 'Units\PascalCoin\ULog.pas',
  UServerApp in 'Units\PascalCoin\UServerApp.pas',
  UAccounts in 'Units\PascalCoin\UAccounts.pas',
  UBlockChain in 'Units\PascalCoin\UBlockChain.pas',
  UECIES in 'Units\PascalCoin\UECIES.pas',
  UFileStorage in 'Units\PascalCoin\UFileStorage.pas',
  UNetProtocol in 'Units\PascalCoin\UNetProtocol.pas',
  UNode in 'Units\PascalCoin\UNode.pas',
  UOpenSSL in 'Units\PascalCoin\UOpenSSL.pas',
  UOpenSSLdef in 'Units\PascalCoin\UOpenSSLdef.pas',
  UOpTransaction in 'Units\PascalCoin\UOpTransaction.pas',
  URPC in 'Units\PascalCoin\URPC.pas',
  UTCPIP in 'Units\PascalCoin\UTCPIP.pas',
  UThread in 'Units\PascalCoin\UThread.pas',
  UTime in 'Units\PascalCoin\UTime.pas',
  UWalletKeys in 'Units\PascalCoin\UWalletKeys.pas',
  UMiner in 'Units\PascalCoin\UMiner.pas',
  UFolderHelper in 'Units\Utils\UFolderHelper.pas',
  UJSONFunctions in 'Units\Utils\UJSONFunctions.pas';

type
  TOutputLogger = class
  protected
    FLock : TCriticalSection;
    procedure ServerAppLog(LogType: TPascalCoinServerLogType;
      Msg: String; Level: Integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TOutputLogger.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TOutputLogger.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TOutputLogger.ServerAppLog(LogType: TPascalCoinServerLogType;
          Msg: String; Level: Integer);
var
  M : String;
begin
  FLock.Acquire;
  try
    M := FormatDateTime('hhnnss.zzz', Now) + ' ' + Msg;
    Writeln(M);
  finally
    FLock.Release;
  end;
end;

var
  OutputLogger : TOutputLogger = nil;

begin
  try
    OutputLogger := TOutputLogger.Create;
    try
      ServerApp := TPascalCoinServerApp.Create;
      try
        ServerApp.OnLog := OutputLogger.ServerAppLog;
        ServerApp.Init;
        ServerApp.Run;
        ServerApp.Stop;
      finally
        FreeAndNil(ServerApp);
      end;
    finally
      FreeAndNil(OutputLogger);
    end;
  except
    on E: Exception do
      Writeln('Fatal error:', E.ClassName, ': ', E.Message);
  end;
end.
