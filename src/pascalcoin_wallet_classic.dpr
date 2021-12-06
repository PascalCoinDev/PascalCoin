program PascalCoin_Wallet_Classic;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ./config.inc}

uses
  {$IFnDEF FPC}
  {$ELSE}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF }
  Interfaces,
  {$ENDIF }
  Forms,
  UFRMAbout in 'gui-classic\UFRMAbout.pas' {FRMAbout},
  UFRMAccountSelect in 'gui-classic\UFRMAccountSelect.pas' {FRMAccountSelect},
  UFRMMemoText in 'gui-classic\UFRMMemoText.pas' {FRMMemoText},
  UFRMNewPrivateKeyType in 'gui-classic\UFRMNewPrivateKeyType.pas' {FRMNewPrivateKeyType},
  UFRMNodesIp in 'gui-classic\UFRMNodesIp.pas' {FRMNodesIp},
  UFRMOperation in 'gui-classic\UFRMOperation.pas' {FRMOperation},
  UFRMOperationsExplorer in 'gui-classic\UFRMOperationsExplorer.pas' {FRMOperationsExplorer},
  {$IFDEF USE_GNUGETTEXT}
  UFRMSelectLanguage in 'gui-classic\UFRMSelectLanguage.pas' {FRMChangeLanguage},
  {$ENDIF }
  UFRMWalletConfig in 'gui-classic\UFRMWalletConfig.pas' {FRMPascalCoinWalletConfig},
  UFRMPayloadDecoder in 'gui-classic\UFRMPayloadDecoder.pas' {FRMPayloadDecoder},
  UFRMRandomOperations in 'gui-classic\UFRMRandomOperations.pas' {FRMRandomOperations},
  UFRMRPCCalls in 'gui-classic\UFRMRPCCalls.pas' {FRMRPCCalls},
  UFRMWallet in 'gui-classic\UFRMWallet.pas' {FRMWallet},
  UFRMWalletKeys in 'gui-classic\UFRMWalletKeys.pas' {FRMWalletKeys},
  UGridUtils in 'gui-classic\UGridUtils.pas',
  UGUIUtils in 'gui-classic\UGUIUtils.pas',
  UFRMHashLock in 'gui-classic\UFRMHashLock.pas' {FRMHashLock},
  UFRMDiagnosticTool in 'gui-classic\UFRMDiagnosticTool.pas' {FRMDiagnosticTool},
  UCommon in 'libraries\sphere10\UCommon.pas',
  UMemory in 'libraries\sphere10\UMemory.pas',
  UAccountKeyStorage in 'core\UAccountKeyStorage.pas',
  UAccounts in 'core\UAccounts.pas',
  UAES in 'core\UAES.pas',
  UBaseTypes in 'core\UBaseTypes.pas',
  UBlockChain in 'core\UBlockChain.pas',
  UChunk in 'core\UChunk.pas',
  UConst in 'core\UConst.pas',
  UCrypto in 'core\UCrypto.pas',
  UECIES in 'core\UECIES.pas',
  UEncoding in 'core\UEncoding.pas',
  UEPasa in 'core\UEPasa.pas',
  UFileStorage in 'core\UFileStorage.pas',
  ULog in 'core\ULog.pas',
  UMurMur3Fast in 'core\UMurMur3Fast.pas',
  UNetProtection in 'core\UNetProtection.pas',
  UNetProtocol in 'core\UNetProtocol.pas',
  UNode in 'core\UNode.pas',
  UOpenSSL in 'core\UOpenSSL.pas',
  UOpTransaction in 'core\UOpTransaction.pas',
  {$IFDEF USE_ABSTRACTMEM}
  UPCAbstractMem in 'core\UPCAbstractMem.pas',
  {$ENDIF }
  UPCAbstractMemAccounts in 'core\UPCAbstractMemAccounts.pas',
  UPCAccountsOrdenations in 'core\UPCAccountsOrdenations.pas',
  UPCCryptoLib4Pascal in 'core\UPCCryptoLib4Pascal.pas',
  UPCDataTypes in 'core\UPCDataTypes.pas',
  UPCEncryption in 'core\UPCEncryption.pas',
  UPCHardcodedRandomHashTable in 'core\UPCHardcodedRandomHashTable.pas',
  UPCOperationsBlockValidator in 'core\UPCOperationsBlockValidator.pas',
  UPCOperationsSignatureValidator in 'core\UPCOperationsSignatureValidator.pas',
  UPCOrderedLists in 'core\UPCOrderedLists.pas',
  UPCRPCFileUtils in 'core\UPCRPCFileUtils.pas',
  UPCRPCFindAccounts in 'core\UPCRPCFindAccounts.pas',
  UPCRPCFindBlocks in 'core\UPCRPCFindBlocks.pas',
  UPCRPCOpData in 'core\UPCRPCOpData.pas',
  UPCRPCSend in 'core\UPCRPCSend.pas',
  UPCSafeBoxRootHash in 'core\UPCSafeBoxRootHash.pas',
  UPCTemporalAbstractMem in 'core\UPCTemporalAbstractMem.pas',
  UPCTemporalFileStream in 'core\UPCTemporalFileStream.pas',
  UPCTNetDataExtraMessages in 'core\UPCTNetDataExtraMessages.pas',
  UPoolMinerThreads in 'core\UPoolMinerThreads.pas',
  UPoolMining in 'core\UPoolMining.pas',
  URandomHash in 'core\URandomHash.pas',
  URandomHash2 in 'core\URandomHash2.pas',
  URPC in 'core\URPC.pas',
  USettings in 'core\USettings.pas',
  USha256 in 'core\USha256.pas',
  UTCPIP in 'core\UTCPIP.pas',
  UThread in 'core\UThread.pas',
  UTime in 'core\UTime.pas',
  UTxMultiOperation in 'core\UTxMultiOperation.pas',
  UWallet in 'core\UWallet.pas',
  UFrameAccountExplorer in 'gui-classic\Frames\UFrameAccountExplorer.pas' {FrameAccountExplorer: TFrame},
  UFramePendingOperations in 'gui-classic\Frames\UFramePendingOperations.pas' {FramePendingOperations: TFrame},
  UFrameBlockExplorer in 'gui-classic\Frames\UFrameBlockExplorer.pas' {FrameBlockChainExplorer: TFrame},
  UFrameOperationsExplorer in 'gui-classic\Frames\UFrameOperationsExplorer.pas' {FrameOperationsExplorer: TFrame},
  UFrameLogs in 'gui-classic\Frames\UFrameLogs.pas' {FrameLogs: TFrame},
  UFrameNodeStats in 'gui-classic\Frames\UFrameNodeStats.pas' {FrameNodeStats: TFrame},
  UFrameMessages in 'gui-classic\Frames\UFrameMessages.pas' {FrameMessages: TFrame},
  UnitReIntegrate in 'gui-classic\UnitReIntegrate.pas',
  UFRMTestWallet in 'gui-classic\UFRMTestWallet.pas' {FRMTestWallet},
  UFrameInfo in 'gui-classic\Frames\UFrameInfo.pas' {FrameInfo: TFrame},
  UFRMWalletUserMessages in 'gui-classic\UFRMWalletUserMessages.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF WINDOWS}Application.MainFormOnTaskbar := True;{$ENDIF}
  Application.Title := 'Pascal Coin Wallet, Miner & Explorer';

  {$IFDEF TESTNET}
  Application.CreateForm(TFRMTestWallet, FRMTestWallet);
  {$ELSE}
  Application.CreateForm(TFRMWallet, FRMWallet);
  {$ENDIF}
  Application.Run;
end.
