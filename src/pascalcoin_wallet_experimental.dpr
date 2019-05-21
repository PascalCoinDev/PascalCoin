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

