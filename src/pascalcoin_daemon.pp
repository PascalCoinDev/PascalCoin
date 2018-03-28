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


begin
  Application.Title:='PascalCoin Daemon application';
  RegisterDaemonClass(TPCDaemon);
  RegisterDaemonMapper(TPCDaemonMapper);
  TCrypto.InitCrypto;
  Application.Run;
end.

