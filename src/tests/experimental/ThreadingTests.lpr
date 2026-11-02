program ThreadingTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  heaptrc, SysUtils, fpcunit, testregistry, plaintestreport,
  UThreadConcurrencyTests, UConcurrency.Tests, UMemory;

procedure Trace(const AMessage: String);
begin
  WriteLn(AMessage);
  Flush(Output);
end;

type
  TProgressResultsWriter = class(TPlainResultsWriter)
  public
    procedure StartTest(ATest: TTest); override;
    procedure EndTest(ATest: TTest); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
  end;

procedure TProgressResultsWriter.StartTest(ATest: TTest);
begin
  Trace('START ' + ATest.TestSuiteName + '.' + ATest.TestName);
  inherited StartTest(ATest);
end;

procedure TProgressResultsWriter.EndTest(ATest: TTest);
begin
  Trace('END ' + ATest.TestSuiteName + '.' + ATest.TestName);
  inherited EndTest(ATest);
end;

procedure TProgressResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  Trace('ERROR ' + ATest.TestSuiteName + '.' + ATest.TestName + ': ' + AError.AsString);
  inherited AddError(ATest, AError);
end;

procedure TProgressResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  Trace('FAIL ' + ATest.TestSuiteName + '.' + ATest.TestName + ': ' + AFailure.AsString);
  inherited AddFailure(ATest, AFailure);
end;

procedure RunTests;
var
  LResults: TTestResult;
  LWriter: TPlainResultsWriter;
  LDisposed: TDisposables;
begin
  Trace('Creating result writer');
  // The result releases its listener before the disposal bag frees the writer.
  LWriter := LDisposed.AddObject(TProgressResultsWriter.Create(nil)) as TPlainResultsWriter;
  Trace('Creating test result');
  LResults := LDisposed.AddObject(TTestResult.Create) as TTestResult;
  LWriter.FileName := ParamStr(1);
  LResults.AddListener(LWriter);
  Trace('Running registered fixtures');
  GetTestRegistry.Run(LResults);
  Trace('Writing final test report');
  LWriter.WriteResult(LResults);
  WriteLn('Errors: ', LResults.NumberOfErrors, '; failures: ', LResults.NumberOfFailures);
  if (LResults.NumberOfErrors = 0) and (LResults.NumberOfFailures = 0) then
    ExitCode := 0
  else
    ExitCode := 1;
  Trace('Disposing runner objects');
end;

begin
  Trace('Program started; unit initialization completed');
  if ParamCount <> 1 then
    Halt(64);
  Trace('Configuring heap report');
  SetHeapTraceOutput(ParamStr(1) + '.heap.log');
  Trace('Starting runner');
  RunTests;
  Trace('Runner disposal completed');
end.
