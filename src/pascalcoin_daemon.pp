program pascalcoin_daemon;

{$mode objfpc}{$H+}
{$define usecthreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  mormot.core.fpcx64mm,
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
  {$IF Defined(FPC) and Defined(WINDOWS)}
  IsConsole := Not Application.HasOption('r','run');
  {$ELSE}
  IsConsole:=False;
  {$ENDIF}
  RegisterDaemonClass(TPCDaemon);
  RegisterDaemonMapper(TPCDaemonMapper);
  Application.GUIMainLoop:=@TDaemonMainLoop.DaemonMainLoop;
  TCrypto.InitCrypto;
  Application.Run;
end.

