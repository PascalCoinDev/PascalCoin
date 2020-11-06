program pascalcoin_daemon;

{$mode objfpc}{$H+}
{$define usecthreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  Classes, daemonapp,
  UCrypto, upcdaemon;

Type

  { TDaemonMainLoop }

  TDaemonMainLoop = Class
  public
    Class procedure DaemonMainLoop;
  end;

{ TDaemonMainLoop }

class procedure TDaemonMainLoop.DaemonMainLoop;
begin
  while not Application.Terminated do begin
    CheckSynchronize(10);
  end;
end;

begin
  Application.Title:='PascalCoin Daemon application';
  IsConsole:=False;
  RegisterDaemonClass(TPCDaemon);
  RegisterDaemonMapper(TPCDaemonMapper);
  Application.GUIMainLoop:=@TDaemonMainLoop.DaemonMainLoop;
  TCrypto.InitCrypto;
  Application.Run;
end.

