program PascalCoin_Wallet_Classic;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFnDEF FPC}
  {$ELSE}
  {$IFDEF LINUX}
  cthreads,
  {$ENDIF }
  Interfaces,
  {$ENDIF }
  Forms,
  UAccountKeyStorage in 'core\UAccountKeyStorage.pas',
  UAccounts in 'core\UAccounts.pas',
  UAES in 'core\UAES.pas',
  UBaseTypes in 'core\UBaseTypes.pas',
  UBlockChain in 'core\UBlockChain.pas',
  UChunk in 'core\UChunk.pas',
  UConst in 'core\UConst.pas',
  UCrypto in 'core\UCrypto.pas',
  UECIES in 'core\UECIES.pas',
  UFileStorage in 'core\UFileStorage.pas',
  ULog in 'core\ULog.pas',
  UNetProtection in 'core\UNetProtection.pas',
  UNetProtocol in 'core\UNetProtocol.pas',
  UNode in 'core\UNode.pas',
  UOpenSSL in 'core\UOpenSSL.pas',
  UOpTransaction in 'core\UOpTransaction.pas',
  UPoolMinerThreads in 'core\UPoolMinerThreads.pas',
  UPoolMining in 'core\UPoolMining.pas',
  URandomHash in 'core\URandomHash.pas',
  URPC in 'core\URPC.pas',
  USettings in 'core\USettings.pas',
  USha256 in 'core\USha256.pas',
  UTCPIP in 'core\UTCPIP.pas',
  UThread in 'core\UThread.pas',
  UTime in 'core\UTime.pas',
  UTxMultiOperation in 'core\UTxMultiOperation.pas',
  UWallet in 'core\UWallet.pas',
  UFRMAbout in 'gui-classic\UFRMAbout.pas' {FRMAbout},
  UFRMAccountSelect in 'gui-classic\UFRMAccountSelect.pas' {FRMAccountSelect},
  UFRMMemoText in 'gui-classic\UFRMMemoText.pas' {FRMMemoText},
  UFRMNewPrivateKeyType in 'gui-classic\UFRMNewPrivateKeyType.pas' {FRMNewPrivateKeyType},
  UFRMNodesIp in 'gui-classic\UFRMNodesIp.pas' {FRMNodesIp},
  UFRMOperation in 'gui-classic\UFRMOperation.pas' {FRMOperation},
  UFRMOperationsExplorer in 'gui-classic\UFRMOperationsExplorer.pas' {FRMOperationsExplorer},
  UFRMPascalCoinWalletConfig in 'gui-classic\UFRMPascalCoinWalletConfig.pas' {FRMPascalCoinWalletConfig},
  UFRMPayloadDecoder in 'gui-classic\UFRMPayloadDecoder.pas' {FRMPayloadDecoder},
  UFRMRandomOperations in 'gui-classic\UFRMRandomOperations.pas' {FRMRandomOperations},
  UFRMRPCCalls in 'gui-classic\UFRMRPCCalls.pas' {FRMRPCCalls},
  UFRMWallet in 'gui-classic\UFRMWallet.pas' {FRMWallet},
  UFRMWalletKeys in 'gui-classic\UFRMWalletKeys.pas' {FRMWalletKeys},
  UGridUtils in 'gui-classic\UGridUtils.pas',
  UGUIUtils in 'gui-classic\UGUIUtils.pas',
  UPCDataTypes in 'core\UPCDataTypes.pas',
  UPCOrderedLists in 'core\UPCOrderedLists.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF WINDOWS}Application.MainFormOnTaskbar := True;{$ENDIF}
  Application.Title := 'Pascal Coin Wallet, Miner & Explorer';
  Application.CreateForm(TFRMWallet, FRMWallet);
  Application.Run;
end.
