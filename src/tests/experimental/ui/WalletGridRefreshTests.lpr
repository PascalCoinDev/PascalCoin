program WalletGridRefreshTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Dedicated process for the real controller's integration fixture. }

{$mode delphi}

uses
  heaptrc, Interfaces, Forms, SysUtils, fpcunit, testregistry, plaintestreport,
  UWalletGridIntegrationTests, UMemory;

procedure RunTests;
var
  LResults: TTestResult;
  LWriter: TPlainResultsWriter;
  LDisposed: TDisposables;
begin
  LWriter := LDisposed.AddObject(TPlainResultsWriter.Create(nil)) as TPlainResultsWriter;
  LResults := LDisposed.AddObject(TTestResult.Create) as TTestResult;
  LWriter.FileName := ParamStr(1);
  LResults.AddListener(LWriter);
  GetTestRegistry.Run(LResults);
  LWriter.WriteResult(LResults);
  if (LResults.NumberOfErrors = 0) and (LResults.NumberOfFailures = 0) then
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
