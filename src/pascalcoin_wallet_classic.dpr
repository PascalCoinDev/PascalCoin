program PascalCoinWallet;

uses
  Forms, Interfaces,
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
  UFRMWallet in 'gui-classic\UFRMWallet.pas' {FRMWallet},
  UFolderHelper in 'common\UFolderHelper.pas',
  UAppParams in 'common\UAppParams.pas',
  UGridUtils in 'common\UGridUtils.pas',
  UFRMPascalCoinWalletConfig in 'gui-classic\UFRMPascalCoinWalletConfig.pas' {FRMPascalCoinWalletConfig},
  UFRMAbout in 'gui-classic\UFRMAbout.pas' {FRMAbout},
  UFRMOperation in 'gui-classic\UFRMOperation.pas' {FRMOperation},
  UFRMWalletKeys in 'gui-classic\UFRMWalletKeys.pas' {FRMWalletKeys},
  UFRMNewPrivateKeyType in 'gui-classic\UFRMNewPrivateKeyType.pas' {FRMNewPrivateKeyType},
  UFRMPayloadDecoder in 'gui-classic\UFRMPayloadDecoder.pas' {FRMPayloadDecoder},
  UFRMNodesIp in 'gui-classic\UFRMNodesIp.pas' {FRMNodesIp},
  UTCPIP in 'core\UTCPIP.pas',
  UJSONFunctions in 'common\UJSONFunctions.pas',
  URPC in 'core\URPC.pas',
  UPoolMining in 'core\UPoolMining.pas',
  UFileStorage in 'core\UFileStorage.pas',
  UOpenSSL in 'core\UOpenSSL.pas',
  UOpenSSLdef in 'core\UOpenSSLdef.pas',
  UAES in 'core\UAES.pas',
  UFRMAccountSelect in 'gui-classic\UFRMAccountSelect.pas' {FRMAccountSelect},
  UFRMAccountInfo in 'gui-classic\UFRMAccountInfo.pas' {FRMAccountInfo},
  UFRMMemoText in 'gui-classic\UFRMMemoText.pas' {FRMMemoText},
  UChunk in 'core\UChunk.pas',
  UBaseTypes in 'core\UBaseTypes.pas',
  UCommon in 'common\UCommon.pas',
  UAccountKeyStorage in 'core\UAccountKeyStorage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Pascal Coin Wallet, Miner & Explorer';
  Application.CreateForm(TFRMWallet, FRMWallet);
  Application.Run;
end.
