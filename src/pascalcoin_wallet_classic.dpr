program PascalCoin_wallet_classic;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  {$IFDEF LINUX}cthreads,{$ENDIF}
  Interfaces,
{$ENDIF}
  Forms,
  UBlockChain in 'core\UBlockChain.pas',
  UCrypto in 'core\UCrypto.pas',
  UTime in 'core\UTime.pas',
  UWallet in 'core\UWallet.pas',
  UOpTransaction in 'core\UOpTransaction.pas',
  UNetProtocol in 'core\UNetProtocol.pas',
  UAccounts in 'core\UAccounts.pas',
  UConst in 'core\UConst.pas',
  UThread in 'core\UThread.pas',
  ULog in 'core\ULog.pas',
  UNode in 'core\UNode.pas',
  UECIES in 'core\UECIES.pas',
  UAES in 'core\UAES.pas',
  UFRMWallet in 'gui-classic\UFRMWallet.pas' {FRMWallet},
  UFileStorage in 'core\UFileStorage.pas',
  UFRMPascalCoinWalletConfig in 'gui-classic\UFRMPascalCoinWalletConfig.pas' {FRMPascalCoinWalletConfig},
  UFRMAbout in 'gui-classic\UFRMAbout.pas' {FRMAbout},
  UFRMOperation in 'gui-classic\UFRMOperation.pas' {FRMOperation},
  UFRMWalletKeys in 'gui-classic\UFRMWalletKeys.pas' {FRMWalletKeys},
  UFRMNewPrivateKeyType in 'gui-classic\UFRMNewPrivateKeyType.pas' {FRMNewPrivateKeyType},
  UFRMPayloadDecoder in 'gui-classic\UFRMPayloadDecoder.pas' {FRMPayloadDecoder},
  UFRMNodesIp in 'gui-classic\UFRMNodesIp.pas' {FRMNodesIp},
  UTCPIP in 'core\UTCPIP.pas',
  URPC in 'core\URPC.pas',
  UPoolMining in 'core\UPoolMining.pas',
  UOpenSSL in 'core\UOpenSSL.pas',
  UOpenSSLdef in 'core\UOpenSSLdef.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF WINDOWS}Application.MainFormOnTaskbar := True;{$ENDIF}
  Application.Title := 'Pascal Coin Wallet, Miner & Explorer';
  Application.CreateForm(TFRMWallet, FRMWallet);
  Application.Run;
end.
