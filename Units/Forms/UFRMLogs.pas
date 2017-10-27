unit UFRMLogs;

{$mode delphi}

interface

uses
  {$IFnDEF FPC}
    pngimage, Windows, AppEvnts, ShlObj,
  {$ELSE}
    LCLIntf,
  {$ENDIF}
    SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, ExtCtrls,  StdCtrls,
    ULog, UBlockChain;

type

  { TFRMLogs }

  TFRMLogs = class(TForm)
    cbShowDebugLogs: TCheckBox;
    memoLogs: TMemo;
    pnlTopLogs: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : Cardinal; Const sender, logtext : AnsiString);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FRMLogs: TFRMLogs = nil;

implementation
uses  UUserInterface;
{$R *.lfm}

{ TFRMLogs }

procedure TFRMLogs.OnNewLog(logtype: TLogType; Time : TDateTime; ThreadID : Cardinal; const sender,logtext: AnsiString);
Var s : AnsiString;
begin
  if (logtype=ltdebug) And (Not cbShowDebugLogs.Checked) then exit;
  if ThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
  if MemoLogs.Lines.Count>300 then begin
    // Limit max lines in logs...
    memoLogs.Lines.BeginUpdate;
    try
      while memoLogs.Lines.Count>250 do memoLogs.Lines.Delete(0);
    finally
      memoLogs.Lines.EndUpdate;
    end;
  end;
  memoLogs.Lines.Add(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(ThreadID,8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
  //
end;

procedure TFRMLogs.FormCreate(Sender: TObject);
begin
  TUserInterface.Log.OnNewLog := OnNewLog;
end;

end.

