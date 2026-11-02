program GridLoadingProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE. }

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms,
  UVisualGrid, UGridLoadingProbe;

type
  TProbeForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TProbeForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
end;

var
  LForm: TProbeForm;
  LLogPath: String;
begin
  if ParamCount > 1 then Halt(64);
  LLogPath := ParamStr(1);
  if LLogPath = '' then
    LLogPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
      'GridLoadingProbe-' + IntToStr(GetCurrentProcessId) + '.log';
  try
    ReportMemoryLeaksOnShutdown := True;
    Application.Initialize;
    Application.MainFormOnTaskBar := True;
    Application.CreateForm(TProbeForm, LForm);
    try
      LForm.SetBounds(80, 90, 640, 440);
      LForm.Caption := 'Grid loading regression';
      InitializeGridLoadingProbe(LForm, LLogPath);
      LForm.Show;
      Application.Run;
      if GridLoadingProbePassed then ExitCode := 0 else ExitCode := 1;
    finally
      LForm.Free;
    end;
  except
    on E: Exception do begin
      with TStringList.Create do
      try
        Text := 'FAIL initialization ' + E.ClassName + ': ' + E.Message;
        SaveToFile(LLogPath);
      finally
        Free;
      end;
      ExitCode := 2;
    end;
  end;
end.