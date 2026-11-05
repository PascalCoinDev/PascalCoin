unit UFRMLogs;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/Releases/2.1.6/Units/Forms/UFRMWallet.pas

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Displays runtime log messages and lets the user include debug output. }

{ Public types in this unit

  Type     | Purpose
  ---------+-------------------------------------------------------------------------
  TFRMLogs | Displays application logs with an optional debug-message filter.
}

interface

{$I ..\config.inc}

uses
  {$IFNDEF FPC}Winapi.Windows, Winapi.Messages,{$ENDIF}
    {$IFDEF FPC}LCLIntf,{$ENDIF}
    SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, ExtCtrls,  StdCtrls, UCommon.UI,
    ULog, UBlockChain, UConst;

type

  { TFRMLogs }

  TFRMLogs = class(TApplicationForm)
    cbShowDebugLogs: TCheckBox;
    memoLogs: TMemo;
    pnlTopLogs: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : String);
  private
    FLogBatchLines, FLogEntryLines: TStringList;
    FLogBatchActive, FLogBatchChanged, FLogBatchTerminated: Boolean;
    procedure OnLogBatchBegin(Sender: TObject);
    procedure OnLogBatchEnd(Sender: TObject);
    procedure SplitLogLines(const AText: String; ALines: TStrings);
  public
    { public declarations }
  end;

var
  FRMLogs: TFRMLogs = nil;

implementation
uses {$IFDEF FPC}WSControls,{$ENDIF} UUserInterface;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TFRMLogs }

procedure TFRMLogs.FormCreate(Sender: TObject);
begin
  FLogBatchLines := TStringList(AutoDispose(TStringList.Create));
  FLogEntryLines := TStringList(AutoDispose(TStringList.Create));
  TUserInterface.Log.OnNewLogBatchBegin := OnLogBatchBegin;
  TUserInterface.Log.OnNewLogBatchEnd := OnLogBatchEnd;
  TUserInterface.Log.OnNewLog := OnNewLog;
end;

procedure TFRMLogs.FormDestroy(Sender: TObject);
begin
  TUserInterface.Log.OnNewLog := nil;
  TUserInterface.Log.OnNewLogBatchBegin := nil;
  TUserInterface.Log.OnNewLogBatchEnd := nil;
end;

procedure TFRMLogs.OnLogBatchBegin(Sender: TObject);
var LText: String;
begin
  // BeginUpdate suppresses painting, but native memo Add/Delete still shape
  // every edit and can stall the UI. Build the batch away from the control.
  FLogBatchChanged := False;
  LText := memoLogs.Lines.Text;
  FLogBatchTerminated := (LText='') or (Copy(LText,Length(LText)-Length(sLineBreak)+1,Length(sLineBreak))=sLineBreak);
  SplitLogLines(LText,FLogBatchLines);
  FLogBatchActive := True;
end;

procedure TFRMLogs.OnLogBatchEnd(Sender: TObject);
var LText: String;
begin
  FLogBatchActive := False;
  try
    if FLogBatchChanged then begin
      // One native text replacement avoids per-entry text shaping and keeps
      // the caret at the end, as with the existing immediate append path.
      // Lines.Text normalizes bare CR/LF; native Add preserves them. Publish
      // the raw text through the widget without changing those log bytes.
      LText := FLogBatchLines.Text;
      if not FLogBatchTerminated then SetLength(LText,Length(LText)-Length(sLineBreak));
      {$IFDEF FPC}
      memoLogs.HandleNeeded;
      TWSWinControlClass(memoLogs.WidgetSetClass).SetText(memoLogs,LText);
      memoLogs.Perform(CM_TEXTCHANGED,0,0);
      {$ELSE}
      memoLogs.Text := LText;
      {$ENDIF}
      memoLogs.SelStart := memoLogs.GetTextLen;
      {$IFNDEF FPC}
      // VCL EM_SETSEL moves the caret without scrolling; the replaced native
      // Add did scroll, so keep following the newest line explicitly.
      memoLogs.Perform(EM_SCROLLCARET,0,0);
      {$ENDIF}
    end;
  finally
    FLogBatchChanged := False;
    FLogBatchLines.Clear;
    FLogEntryLines.Clear;
  end;
end;

procedure TFRMLogs.OnNewLog(logtype: TLogType; Time : TDateTime; ThreadID : TThreadID; const sender,logtext: String);
Var s, LText : String;
  LLines : TStrings;
begin
  if (logtype=ltdebug) And (Not cbShowDebugLogs.Checked) then exit;
  if ThreadID=MainThreadID then s := 'MAIN:' else s:='TID:';
  if FLogBatchActive then begin
    LLines := FLogBatchLines;
    FLogBatchChanged := True;
  end else LLines := memoLogs.Lines;
  if LLines.Count>300 then begin
    // Limit max lines in logs...
    LLines.BeginUpdate;
    try
      while LLines.Count>250 do LLines.Delete(0);
    finally
      LLines.EndUpdate;
    end;
  end;
  //memoLogs.Lines.Add('%s %s %p [%s] <%s> %s',                                                             XXXXXXXXXXXXXXXX Windows %p is invalid (not a pointer)
  //  [FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time), s, ThreadID, CT_LogType[Logtype], sender, logtext]); XXXXXXXXXXXXXXXX Windows %p is invalid (not a pointer)
  LText := Format('%s %s %s [%s] <%s> %s',
    [FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time), s, IntToHex(PtrInt(ThreadID),8), CT_LogType[Logtype], sender, logtext]);
  if FLogBatchActive then begin
    // Native memo appends split embedded newlines. Keep those physical lines
    // when applying the same per-entry retention rule to the in-memory batch.
    {$IFDEF FPC}
    LText := LText+sLineBreak;
    {$ELSE}
    // VCL leaves an unterminated user-edited tail unterminated when appending;
    // LCL always appends a line separator. Retain each original native rule.
    if FLogBatchTerminated then LText := LText+sLineBreak;
    {$ENDIF}
    FLogBatchTerminated := Copy(LText,Length(LText)-Length(sLineBreak)+1,Length(sLineBreak))=sLineBreak;
    SplitLogLines(LText,FLogEntryLines);
    FLogBatchLines.AddStrings(FLogEntryLines);
  end else LLines.Add(LText);
end;

procedure TFRMLogs.SplitLogLines(const AText: String; ALines: TStrings);
var I, LStart, LLength, LBreakLength: Integer;
  LBreak: String;
  LMatches: Boolean;
begin
  // Split only the native line separator: on Windows a bare CR or LF is
  // part of a memo line, not an additional line for the retention count.
  ALines.Clear;
  LBreak := sLineBreak;
  LBreakLength := Length(LBreak);
  LLength := Length(AText);
  LStart := 1;
  I := 1;
  while I<=LLength do begin
    LMatches := AText[I]=LBreak[1];
    if LMatches and (LBreakLength>1) then begin
      if I<LLength then LMatches := AText[I+1]=LBreak[2]
      else LMatches := False;
    end;
    if LMatches then begin
      ALines.Add(Copy(AText,LStart,I-LStart));
      Inc(I,LBreakLength);
      LStart := I;
    end else Inc(I);
  end;
  if LStart<=LLength then ALines.Add(Copy(AText,LStart,LLength-LStart+1));
end;

end.

