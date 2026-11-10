program ScopeProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$APPTYPE CONSOLE}

uses SysUtils, UScope.Checks, UDisposables.Scope.Checks, UReadWriteScope.Checks;

var I: Integer;
begin
  {$IFNDEF FPC}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  try
    for I := 1 to ScopeCheckCount do begin
      RunScopeCheck(I);
      WriteLn('PASS scope: ', ScopeCheckName(I));
    end;
    RunDisposablesScopeChecks;
    WriteLn('PASS disposal bag: ', DisposablesScopeCheckCount, ' checks');
    for I := 1 to ReadWriteScopeCheckCount do begin
      RunReadWriteScopeCheck(I);
      WriteLn('PASS lock scope: ', ReadWriteScopeCheckName(I));
    end;
    WriteLn('PASS all ', ScopeCheckCount + DisposablesScopeCheckCount +
      ReadWriteScopeCheckCount, ' shared scope checks');
    ExitCode := 0;
  except
    on E: Exception do begin
      WriteLn('FAIL ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
