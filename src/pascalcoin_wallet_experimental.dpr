program PascalCoinWalletExperimental;

{$mode delphi}
{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  sysutils,
  {$ifdef Use_OpenSSL}
  UOpenSSL,
  {$endif}
  UCrypto,
  Forms,
  UUserInterface,
  UFRMMainForm;

//{$R *.res}

var
   mainForm : TFRMMainForm;
begin
  // Start OpenSSL dll
  {$ifdef Use_OpenSSL}
  if Not LoadSSLCrypt then
    raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
  TCrypto.InitCrypto;
  {$endif}
  // Load application
  Application.Initialize;
  {$IFDEF WINDOWS}{$Warnings OFF}
  Application.MainFormOnTaskBar := True;
  {$Warnings ON}{$ENDIF}
  Application.Title := 'Pascal Coin Wallet, Miner & Explorer';
  Application.CreateForm(TFRMMainForm, mainForm);
  TUserInterface.StartApplication(mainForm);
  Application.Run;
end.

