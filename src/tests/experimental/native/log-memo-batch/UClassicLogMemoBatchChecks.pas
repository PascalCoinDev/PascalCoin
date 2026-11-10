unit UClassicLogMemoBatchChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Verifies real log-form publication and retained text through native memo
  messages. The reference memo applies the original per-record retention rules. }

{$IFDEF FPC}{$MODE Delphi}{$H+}{$ENDIF}

interface

function RunClassicLogMemoBatchChecks(const ALogPath: String): Integer;

implementation

uses
  {$IFDEF FPC}Windows,{$ELSE}Winapi.Windows, Winapi.Messages,{$ENDIF}
  SysUtils, Classes, Forms, Controls, StdCtrls, UAppParams, UMemory, UCommon,
  UFRMWallet, USettings, UNetProtocol, UCrypto, ULog, UNode,
  UConst;

type
  TClassicLogAccess = class(TFRMWallet);

  TLogMemoChecks = class
  private
    FForm: TFRMWallet;
    FReferenceHost: TForm;
    FReference: TMemo;
    FLog: TLog;
    FDispatch: TThreadSafeLogEvent;
    FPreviousRecord: TNewLogEvent;
    FPreviousBegin, FPreviousEnd: TNotifyEvent;
    FPreviousGlobal: Boolean;
    FPreviousWindowProc: Pointer;
    {$IFNDEF FPC}
    FPreviousMemoProc: TWndMethod;
    {$ENDIF}
    FMemoHandle: HWND;
    FSetText, FReplaceSelection, FDeliveredBatches: Integer;
    FLines: TStringList;
    {$IFNDEF FPC}
    procedure MemoDispatch(var AMessage: TMessage);
    {$ENDIF}
    procedure Receive(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
    procedure BeginBatch(Sender: TObject);
    procedure EndBatch(Sender: TObject);
    procedure ReferenceAppend(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
    procedure CheckBatch(const AName: String);
    procedure CheckUnchangedBatch(const AName: String);
    procedure Install;
    procedure Uninstall;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

var ActiveChecks: TLogMemoChecks;

procedure Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then
    raise Exception.Create(AMessage);
end;

function MemoMessage(AWindow: HWND; AMessage: UINT; AWParam: WPARAM; ALParam: LPARAM): LRESULT; stdcall;
begin
  if AMessage=WM_SETTEXT then
    Inc(ActiveChecks.FSetText)
  else if AMessage=EM_REPLACESEL then
    Inc(ActiveChecks.FReplaceSelection);
  Result := CallWindowProcW(ActiveChecks.FPreviousWindowProc,AWindow,AMessage,AWParam,ALParam);
end;

{$IFNDEF FPC}
procedure TLogMemoChecks.MemoDispatch(var AMessage: TMessage);
begin
  // VCL Perform calls the saved native edit procedure directly; a window
  // subclass misses those messages. Observe its real control dispatch instead.
  if AMessage.Msg=WM_SETTEXT then
    Inc(FSetText)
  else if AMessage.Msg=EM_REPLACESEL then
    Inc(FReplaceSelection);
  FPreviousMemoProc(AMessage);
end;
{$ENDIF}

constructor TLogMemoChecks.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TLogMemoChecks.Destroy;
begin
  Uninstall;
  FDispatch.Free;
  FReferenceHost.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TLogMemoChecks.Install;
begin
  FForm := FRMWallet;
  Require(Assigned(FForm),'actual Classic form was not created');
  FLog := TClassicLogAccess(FForm).FLog;
  Require(Assigned(FLog) and Assigned(FLog.OnNewLog),'actual log-form subscription is missing');
  FPreviousRecord := FLog.OnNewLog;
  FPreviousBegin := FLog.OnNewLogBatchBegin;
  FPreviousEnd := FLog.OnNewLogBatchEnd;
  FPreviousGlobal := FLog.ProcessGlobalLogs;
  FLog.ProcessGlobalLogs := False;
  FDispatch := TThreadSafeLogEvent.Create(True);
  FDispatch.FLog := FLog;
  // Drain ordinary startup records before taking the independent text baseline.
  FDispatch.SynchronizedProcess;
  FReferenceHost := TForm.CreateNew(nil);
  FReference := TMemo.Create(FReferenceHost);
  FReference.Parent := FReferenceHost;
  FReference.WordWrap := FForm.memoLogs.WordWrap;
  FReference.Font.Assign(FForm.memoLogs.Font);
  FReference.BoundsRect := FForm.memoLogs.BoundsRect;
  FReference.HandleNeeded;
  FReference.Text := FForm.memoLogs.Text;
  FLog.OnNewLog := Receive;
  FLog.OnNewLogBatchBegin := BeginBatch;
  FLog.OnNewLogBatchEnd := EndBatch;
  FForm.memoLogs.HandleNeeded;
  FMemoHandle := FForm.memoLogs.Handle;
  {$IFDEF FPC}
  ActiveChecks := Self;
  SetLastError(0);
  FPreviousWindowProc := Pointer(SetWindowLongPtrW(FMemoHandle,GWLP_WNDPROC,NativeInt(@MemoMessage)));
  Require(Assigned(FPreviousWindowProc),'cannot subclass the actual native memo');
  {$ELSE}
  FPreviousMemoProc := FForm.memoLogs.WindowProc;
  FForm.memoLogs.WindowProc := MemoDispatch;
  {$ENDIF}
end;

procedure TLogMemoChecks.Uninstall;
begin
  {$IFNDEF FPC}
  if Assigned(FPreviousMemoProc) then begin
    FForm.memoLogs.WindowProc := FPreviousMemoProc;
    FPreviousMemoProc := nil;
  end;
  {$ENDIF}
  if Assigned(FPreviousWindowProc) then begin
    if IsWindow(FMemoHandle) then
      SetWindowLongPtrW(FMemoHandle,GWLP_WNDPROC,NativeInt(FPreviousWindowProc));
    FPreviousWindowProc := nil;
    ActiveChecks := nil;
  end;
  if Assigned(FLog) then begin
    FLog.OnNewLog := FPreviousRecord;
    FLog.OnNewLogBatchBegin := FPreviousBegin;
    FLog.OnNewLogBatchEnd := FPreviousEnd;
    FLog.ProcessGlobalLogs := FPreviousGlobal;
    FLog := nil;
  end;
end;

procedure TLogMemoChecks.ReferenceAppend(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
var LPrefix: String;
begin
  if (AType=ltdebug) and not FForm.cbShowDebugLogs.Checked then
    Exit;
  if AThreadID=MainThreadID then
    LPrefix := ' MAIN:'
  else
    LPrefix := ' TID:';
  // This bounded native reference preserves the old platform newline and
  // trim semantics, including continuation/empty lines within one log record.
  if FReference.Lines.Count>300 then
    while FReference.Lines.Count>250 do
      FReference.Lines.Delete(0);
  FReference.Lines.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz',ATime)+LPrefix+IntToHex(PtrInt(AThreadID),8)+' ['+CT_LogType[AType]+'] <'+ASender+'> '+AText);
end;

procedure TLogMemoChecks.Receive(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
var LBefore: Integer;
begin
  LBefore := FReference.Lines.Count;
  ReferenceAppend(AType,ATime,AThreadID,ASender,AText);
  if (Pos(#13,AText)>0) or (Pos(#10,AText)>0) then
    FLines.Add(Format('REFERENCE newline lines before=%d after=%d text=%s',[LBefore,FReference.Lines.Count,StringReplace(StringReplace(AText,#13,'\r',[rfReplaceAll]),#10,'\n',[rfReplaceAll])]));
  FPreviousRecord(AType,ATime,AThreadID,ASender,AText);
end;

procedure TLogMemoChecks.BeginBatch(Sender: TObject);
begin
  Inc(FDeliveredBatches);
  if Assigned(FPreviousBegin) then
    FPreviousBegin(Sender);
end;

procedure TLogMemoChecks.EndBatch(Sender: TObject);
begin
  if Assigned(FPreviousEnd) then
    FPreviousEnd(Sender);
end;

procedure TLogMemoChecks.CheckBatch(const AName: String);
var LBefore: Integer;
begin
  Require(FForm.memoLogs.Handle=FMemoHandle,'native memo handle changed unexpectedly');
  FSetText := 0;
  FReplaceSelection := 0;
  LBefore := FDeliveredBatches;
  FDispatch.SynchronizedProcess;
  Require(FDeliveredBatches=LBefore+1,AName+': expected one actual logger delivery');
  Require((FSetText=1) and (FReplaceSelection=0),Format('%s: expected one WM_SETTEXT and no native per-entry edits, got WM_SETTEXT=%d EM_REPLACESEL=%d',[AName,FSetText,FReplaceSelection]));
  if FForm.memoLogs.Text<>FReference.Text then begin
    FLines.Add('ACTUAL TAIL '+StringReplace(StringReplace(Copy(FForm.memoLogs.Text,Length(FForm.memoLogs.Text)-900,901),#13,'\r',[rfReplaceAll]),#10,'\n',[rfReplaceAll]));
    FLines.Add('EXPECTED TAIL '+StringReplace(StringReplace(Copy(FReference.Text,Length(FReference.Text)-900,901),#13,'\r',[rfReplaceAll]),#10,'\n',[rfReplaceAll]));
  end;
  Require(FForm.memoLogs.Text=FReference.Text,AName+': native retained text/order differs from the original per-record rules');
  Require(FForm.memoLogs.Lines.Count=FReference.Lines.Count,AName+': native retained line count differs');
  FLines.Add(Format('PASS %s: one publication, retained lines=%d',[AName,FForm.memoLogs.Lines.Count]));
end;

procedure TLogMemoChecks.CheckUnchangedBatch(const AName: String);
begin
  FSetText := 0;
  FReplaceSelection := 0;
  FDispatch.SynchronizedProcess;
  Require((FSetText=0) and (FReplaceSelection=0),AName+': unexpected native text publication');
  Require(FForm.memoLogs.Text=FReference.Text,AName+': retained text changed');
  FLines.Add('PASS '+AName+': no native text publication');
end;

procedure TLogMemoChecks.Run;
var I: Integer; LTime: TDateTime;
begin
  Install;
  FForm.cbShowDebugLogs.Checked := False;
  for I := 1 to 350 do
    FLog.NotifyNewLog(ltinfo,'publication-probe',Format('record %.3d',[I]));
  CheckBatch('retention across 300/250 boundaries');
  for I := 1 to 8 do begin
    FLog.NotifyNewLog(ltdebug,'publication-probe','filtered debug');
    FLog.NotifyNewLog(ltinfo,'publication-probe','visible info '+IntToStr(I));
  end;
  CheckBatch('debug excluded');
  FLog.NotifyNewLog(ltdebug,'publication-probe','only filtered debug');
  CheckUnchangedBatch('all-filtered batch');
  CheckUnchangedBatch('empty batch');
  FForm.cbShowDebugLogs.Checked := True;
  FLog.NotifyNewLog(ltdebug,'publication-probe','included debug');
  FLog.NotifyNewLog(ltinfo,'publication-probe','line one'#13#10'line two');
  FLog.NotifyNewLog(ltinfo,'publication-probe','line three'#10'line four');
  FLog.NotifyNewLog(ltinfo,'publication-probe','trailing continuation'#13#10);
  FLog.NotifyNewLog(ltinfo,'publication-probe','bare carriage'#13'return');
  FLog.NotifyNewLog(ltinfo,'publication-probe','double carriage'#13#13#10'line');
  FLog.NotifyNewLog(ltinfo,'publication-probe','blank line'#13#10#13#10'end');
  CheckBatch('debug included and multiline');
  // Call the real public handler outside the logger's brackets. Publication
  // must remain immediate and the following batch must preserve that text.
  LTime := EncodeDate(2026,11,3)+EncodeTime(12,34,56,789);
  ReferenceAppend(ltinfo,LTime,MainThreadID,'publication-probe','direct outside batch');
  TClassicLogAccess(FForm).OnNewLog(ltinfo,LTime,MainThreadID,'publication-probe','direct outside batch');
  Require(FForm.memoLogs.Text=FReference.Text,'direct OnNewLog did not publish the original text immediately');
  FLines.Add('PASS direct OnNewLog outside batch');
  FLog.NotifyNewLog(ltinfo,'publication-probe','after direct callback');
  CheckBatch('batch after direct callback');
  FForm.memoLogs.Text := 'manual tail';
  FReference.Text := 'manual tail';
  FLog.NotifyNewLog(ltinfo,'publication-probe','after unterminated text');
  FLog.NotifyNewLog(ltinfo,'publication-probe','continued'#13#10'line');
  FLog.NotifyNewLog(ltinfo,'publication-probe','ends in separator'#13#10);
  FLog.NotifyNewLog(ltinfo,'publication-probe','last record');
  CheckBatch('unterminated manual tail and subsequent multiline');
  FLines.Add('PASS 8');
end;

procedure SeedSettings(const ADirectory: String);
var LBag: TDisposables; LParams: TAppParams;
begin
  LParams := LBag.AddObject(TAppParams.Create(nil)) as TAppParams;
  LParams.FileName := IncludeTrailingPathDelimiter(ADirectory)+'AppParams.prm';
  LParams.ParamByName[CT_PARAM_FirstTime].SetAsBoolean(False);
  LParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(0);
  LParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(False);
  LParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(False);
  LParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].SetAsInteger(0);
  LParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString('127.0.0.1:1');
  LParams.ParamByName[CT_PARAM_PeerCache].SetAsString('127.0.0.1:1');
  LParams.ParamByName[CT_PARAM_ShowModalMessages].SetAsBoolean(False);
  LParams.ParamByName[CT_PARAM_ShowLogs].SetAsBoolean(True);
  LParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(False);
  LParams.ParamByName[CT_PARAM_SaveDebugLogs].SetAsBoolean(False);
end;

function RunClassicLogMemoBatchChecks(const ALogPath: String): Integer;
var LChecks: TLogMemoChecks; LDirectory, LOldDirectory: String;
begin
  Result := 1;
  Require(not FileExists(ALogPath),'refusing an existing probe log');
  LDirectory := ExpandFileName(ALogPath+'.owned-profile');
  Require(not DirectoryExists(LDirectory),'refusing an existing data profile');
  Require(ForceDirectories(LDirectory),'cannot create owned empty profile');
  Require(not TNode.NodeExists and not TNetData.NetDataExists,'Classic probe requires fresh core singletons');
  LOldDirectory := TNode.GetPascalCoinDataFolder;
  LChecks := TLogMemoChecks.Create;
  try
    try
      // FormCreate loads settings itself. Redirect its owner before any form,
      // node or network object can inspect a wallet or persistence directory.
      TNode.SetPascalCoinDataFolder(LDirectory);
      SeedSettings(LDirectory);
      TCrypto.InitCrypto;
      TNetData.NetData.NetConnectionsActive := False;
      Application.Initialize;
      Application.MainFormOnTaskBar := True;
      Application.CreateForm(TFRMWallet,FRMWallet);
      // No Show/activation: the real logger and memo are sufficient here;
      // starting the Classic chain loader is outside this focused regression.
      Require(not TClassicLogAccess(FRMWallet).FIsActivated and not Assigned(TClassicLogAccess(FRMWallet).FThreadActivate),'Classic activation unexpectedly started');
      Require(not TNetData.NetData.NetConnectionsActive,'Classic networking was unexpectedly enabled');
      LChecks.Run;
      Result := 0;
    except
      on E: Exception do
        LChecks.FLines.Add('FAIL '+E.ClassName+': '+E.Message);
    end;
  finally
    try
      LChecks.Uninstall;
      FreeAndNil(FRMWallet);
      Require(not TNode.NodeExists and not TNetData.NetDataExists,'Classic form did not release its owned core singletons');
    except
      on E: Exception do begin
        Result := 1;
        LChecks.FLines.Add('FAIL cleanup '+E.ClassName+': '+E.Message);
      end;
    end;
    TNode.SetPascalCoinDataFolder(LOldDirectory);
    LChecks.FLines.SaveToFile(ALogPath);
    LChecks.Free;
  end;
end;

end.
