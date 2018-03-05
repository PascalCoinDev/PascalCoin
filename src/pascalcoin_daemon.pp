program pascalcoin_daemon;

{$mode objfpc}{$H+}
{$define usecthreads}
{$apptype gui}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
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

