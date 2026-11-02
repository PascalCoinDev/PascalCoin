program PascalCoinWalletExperimental;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$DEFINE UseCThreads}

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF FPC}Interfaces,{$ENDIF}
  sysutils,
  {$ifdef Use_OpenSSL}
  UOpenSSL,
  {$endif}
  UCrypto,
  Forms,
  UWalletStartup,
  UUserInterface,
  UFRMMainForm;

{$IFDEF FPC}
{$R *.res}
{$ELSE}
{$R pascalcoin_wallet_experimental.res}
{$ENDIF}

var
   mainForm : TFRMMainForm;
   LArguments : array of String;
   LOptions : TWalletStartupOptions;
   LError : String;
   i : Integer;
begin
  // Reject invalid arguments and serve help before loading any wallet state.
  SetLength(LArguments, ParamCount);
  for i := 1 to ParamCount do
    LArguments[i - 1] := ParamStr(i);
  if not TryParseWalletStartupOptions(LArguments, LOptions, LError) then begin
    {$IFDEF FPC}
    WriteLn(StdErr, LError);
    WriteLn(StdErr, WalletStartupHelp);
    {$ELSE}
    // A GUI launch has no standard stream; redirected CLI output remains usable.
    if GetFileType(GetStdHandle(STD_ERROR_HANDLE)) <> FILE_TYPE_UNKNOWN then begin
      WriteLn(ErrOutput, LError);
      WriteLn(ErrOutput, WalletStartupHelp);
    end;
    {$ENDIF}
    Halt(1);
  end;
  if LOptions.ShowHelp then begin
    {$IFNDEF FPC}
    if GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)) <> FILE_TYPE_UNKNOWN then
    {$ENDIF}
    WriteLn(WalletStartupHelp);
    Halt(0);
  end;
  TUserInterface.ConfigureStartup(LOptions);
  // Load application
  Application.Initialize;
  {$IF Defined(WINDOWS) or Defined(MSWINDOWS)}{$Warnings OFF}
  Application.MainFormOnTaskBar := True;
  {$Warnings ON}{$ENDIF}
  Application.Title := 'Pascal Coin Wallet, Miner & Explorer';
  // Use the same embedded identity for the executable, window and taskbar.
  Application.Icon.LoadFromResourceName(HInstance, 'MAINICON');
  Application.CreateForm(TFRMMainForm, mainForm);
  mainForm.Icon.Assign(Application.Icon);
  TUserInterface.StartApplication(mainForm);
  if TUserInterface.Started then
    Application.Run;
end.

