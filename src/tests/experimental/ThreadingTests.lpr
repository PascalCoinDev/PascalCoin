program ThreadingTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  heaptrc, SysUtils, fpcunit, testregistry, plaintestreport,
  UThreadConcurrencyTests, UConcurrency.Tests, UMemory;

procedure RunTests;
var
  LResults: TTestResult;
  LWriter: TPlainResultsWriter;
  LDisposed: TDisposables;
begin
  // The result releases its listener before the disposal bag frees the writer.
  LWriter := LDisposed.AddObject(TPlainResultsWriter.Create(nil)) as TPlainResultsWriter;
  LResults := LDisposed.AddObject(TTestResult.Create) as TTestResult;
  LWriter.FileName := ParamStr(1);
  LResults.AddListener(LWriter);
  GetTestRegistry.Run(LResults);
  LWriter.WriteResult(LResults);
  WriteLn('Errors: ', LResults.NumberOfErrors, '; failures: ', LResults.NumberOfFailures);
  if (LResults.NumberOfErrors = 0) and (LResults.NumberOfFailures = 0) then
    ExitCode := 0
  else
    ExitCode := 1;
end;

begin
  if ParamCount <> 1 then
    Halt(64);
  SetHeapTraceOutput(ParamStr(1) + '.heap.log');
  RunTests;
end.
