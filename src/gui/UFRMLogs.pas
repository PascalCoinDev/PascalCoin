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

{$mode delphi}

interface

{$I ..\config.inc}

uses
    LCLIntf,
    SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, ExtCtrls,  StdCtrls, UCommon.UI,
    ULog, UBlockChain;

type

  { TFRMLogs }

  TFRMLogs = class(TApplicationForm)
    cbShowDebugLogs: TCheckBox;
    memoLogs: TMemo;
    pnlTopLogs: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : AnsiString);
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

procedure TFRMLogs.FormCreate(Sender: TObject);
begin
  TUserInterface.Log.OnNewLog := OnNewLog;
end;

procedure TFRMLogs.FormDestroy(Sender: TObject);
begin
  TUserInterface.Log.OnNewLog := nil;
end;

procedure TFRMLogs.OnNewLog(logtype: TLogType; Time : TDateTime; ThreadID : TThreadID; const sender,logtext: AnsiString);
Var s : AnsiString;
begin
  if (logtype=ltdebug) And (Not cbShowDebugLogs.Checked) then exit;
  if ThreadID=MainThreadID then s := 'MAIN:' else s:='TID:';
  if MemoLogs.Lines.Count>300 then begin
    // Limit max lines in logs...
    memoLogs.Lines.BeginUpdate;
    try
      while memoLogs.Lines.Count>250 do memoLogs.Lines.Delete(0);
    finally
      memoLogs.Lines.EndUpdate;
    end;
  end;
  //memoLogs.Lines.Add('%s %s %p [%s] <%s> %s',                                                             XXXXXXXXXXXXXXXX Windows %p is invalid (not a pointer)
  //  [FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time), s, ThreadID, CT_LogType[Logtype], sender, logtext]); XXXXXXXXXXXXXXXX Windows %p is invalid (not a pointer)
  memoLogs.Lines.Add('%s %s %s [%s] <%s> %s',
    [FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time), s, IntToHex(PtrInt(ThreadID),8), CT_LogType[Logtype], sender, logtext]);
end;

end.

