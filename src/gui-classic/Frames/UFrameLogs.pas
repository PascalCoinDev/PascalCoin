unit UFrameLogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ULog, UConst;

type
  TFrameLogs = class(TFrame)
    memoLogs: TMemo;
    pnlTopLogs: TPanel;
    cbShowDebugLogs: TCheckBox;
    ButtonCopyLogToClipboard: TButton;
    procedure ButtonCopyLogToClipboardClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnNewLog
    (
      logtype : TLogType;
      Time : TDateTime;
      ThreadID : TThreadID; // carefull, two different types exist one in UConst.Pas and one in System.pas (in Delphi).
      const sender, logtext : string
    );

  end;

implementation

{$R *.dfm}

constructor TFrameLogs.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

end;

destructor TFrameLogs.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameLogs.ButtonCopyLogToClipboardClick(Sender: TObject);
begin
  memoLogs.SelectAll;
  memoLogs.CopyToClipboard;
end;

procedure TFrameLogs.OnNewLog
(
  logtype: TLogType;
  Time : TDateTime;
  ThreadID : TThreadID;
  const sender,logtext: String
);
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
  memoLogs.Lines.Add(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(PtrInt(ThreadID),8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
  //
end;


end.
