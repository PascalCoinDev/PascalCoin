program AccountSelectorNativeProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Use the existing owned private-desktop broker and one absolute fresh log path. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

uses {$IFDEF FPC}Interfaces,{$ENDIF} Forms, Classes, SysUtils, UCrypto,
  UAccountSelector.Checks;

var LLog: TStringList;
  {$IFDEF FPC}I: Integer;{$ENDIF}
begin
  ExitCode := 1;
  Application.Initialize;
  LLog := TStringList.Create;
  try
    try
      if ParamCount <> 1 then raise Exception.Create('Pass one absolute log filename');
      TCrypto.InitCrypto;
      CheckAccountSelector(LLog);
      LLog.Add('PASS complete experimental account selector checks');
      ExitCode := 0;
    except
      on E: Exception do begin
        LLog.Add('FAIL '+E.ClassName+': '+E.Message);
        {$IFDEF FPC}
        // Keep checked failures attributable to the actual storage/UI operation.
        LLog.Add(BackTraceStrFunc(ExceptAddr));
        for I := 0 to ExceptFrameCount-1 do
          LLog.Add(BackTraceStrFunc(ExceptFrames[I]));
        {$ENDIF}
      end;
    end;
    if ParamCount = 1 then LLog.SaveToFile(ParamStr(1));
  finally
    LLog.Free;
  end;
end.
