program ExperimentalWalletTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php. }

{$mode delphi}

uses
  heaptrc, Interfaces, Forms, SysUtils, fpcunit, testregistry, plaintestreport,
  UWalletStartupTests, UPendingRestoreTests, UPendingCleanupTests, UOperationPreparationTests, UChangeInfoBatchTests, UOperationWizardTests,
  UWalletRestoreTests, UWalletViewTests, UWalletAccountEnumerationTests, UWalletHistoryTests,
  UWalletSnapshotTests, UWalletMempoolReadSafetyTests, UWalletProgressDisplayTests, UThreadConcurrencyTests,
  UNodeSynchronizationTests,
  UChangeAccountInfoReplayTests,
  UBlockSanitizeTests,
  UPendingCatchupTests,
  UPendingSaveLifecycleTests,
  UAccountCachePolicyTests,
  UDownloadWorkerLifetimeTests,
  ULegacyConnectionLifetimeTests,
  UOrderedCardinalTests,
  UWalletExplorerTests, UWalletExplorerSchedulingTests,
  UWalletPagingTests,
  UWalletRenderTests, UWalletGridRefreshTests,
  UWalletMiningTests,
  UWalletLanguageTests, UWalletParityTests, UWalletInfoTests,
  UCache.Tests, UWizard.Tests, UCommon.Data.Tests, UCommon.UI.Tests, UVisualGrid.Tests,
  UVisualGrid.Refresh.Tests, UFramework.UI.Integration.Tests, UMemory.Tests, UConcurrency.Tests,
  UScope.Tests, UReadWriteScope.Tests,
  UMemory;

procedure RunTests;
var
  results: TTestResult;
  writer: TPlainResultsWriter;
  disposed: TDisposables;
begin
  // Reverse-order disposal follows FPCUnit's result-before-listener lifetime.
  writer := disposed.AddObject(TPlainResultsWriter.Create(nil)) as TPlainResultsWriter;
  results := disposed.AddObject(TTestResult.Create) as TTestResult;
  writer.FileName := ParamStr(1);
  results.AddListener(writer);
  GetTestRegistry.Run(results);
  writer.WriteResult(results);
  if (results.NumberOfErrors = 0) and (results.NumberOfFailures = 0) then
    ExitCode := 0
  else
    ExitCode := 1;
end;

begin
  if ParamCount <> 1 then Halt(64);
  SetHeapTraceOutput(ParamStr(1) + '.heap.log');
  Application.Initialize;
  RunTests;
end.
