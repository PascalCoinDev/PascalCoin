unit USyncFlickerChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Observes actual synchronization captions, Info logs and native paint messages.
  The separate erase challenge exposes an intermediate blank frame; it is not
  presented as proof that a live event caused that exact screen frame. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UWalletGridMocks;

procedure CheckSynchronizationPainting(AMain: TMockMainForm; const ALogPath: String);
procedure CheckSafeboxProgressLogging(AMain: TMockMainForm; const ALogPath: String);

implementation

uses Windows, Messages, Classes, SysUtils, Types, Forms, Controls, Graphics,
  SyncObjs, UNode, UAccounts, UUserInterface, UCTRLSyncronization,
  UProgressDisplay, ULog, UThread, UConst, UNetProtocol, UCommon;

type
  TPaintObserver = class
  private
    FControl: TWinControl;
    FOriginal: TWndMethod;
    procedure Observe(var AMessage: TMessage);
  public
    Paints, FullPaints, Erases, ExternalErases, ShowMessages, PositionMessages: Integer;
    constructor Create(AControl: TWinControl);
    destructor Destroy; override;
    function Summary: String;
  end;

  TSafeboxLogEntry = record
    Kind: TLogType;
    ThreadID: {$IFDEF FPC}TThreadID{$ELSE}UConst.TThreadID{$ENDIF};
    Source, Text: String;
  end;

  TSafeboxProgressObserver = class(TComponent)
  private
    FSource: TLog;
    FPreviousLog: TNewLogEvent;
    FLock: TCriticalSection;
    FEntries: array of TSafeboxLogEntry;
    FUIEvents: Integer;
    FAttached: Boolean;
    procedure OnLog(AKind: TLogType; ATime: TDateTime;
      AThread: {$IFDEF FPC}TThreadID{$ELSE}UConst.TThreadID{$ENDIF}; const ASource, AText: String);
    procedure OnUIChanged(Sender: TObject);
    function GetCount: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach;
    procedure Detach;
    procedure Check(AExpectedCount: Integer; const AExpectedText: String);
    property Count: Integer read GetCount;
    property UIEvents: Integer read FUIEvents;
  end;

  TSafeboxProgressProducer = class(TPCThread)
  private
    FNode: TNode;
    FProgress: TProgressInfo;
  protected
    procedure BCExecute; override;
  public
    Completed: TEvent;
    Error: String;
    constructor Create(ANode: TNode; const AProgress: TProgressInfo);
    destructor Destroy; override;
  end;

var
  SafeboxProgressObserver: TSafeboxProgressObserver;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure Log(const APath, AText: String);
var LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    if FileExists(APath) then LLines.LoadFromFile(APath);
    LLines.Add(FormatDateTime('hh:nn:ss.zzz',Now)+' '+AText);
    LLines.SaveToFile(APath);
  finally LLines.Free; end;
end;

procedure Pump(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64+AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64>=LUntil;
end;

constructor TSafeboxProgressObserver.Create;
begin
  inherited Create(nil);
  FLock := TCriticalSection.Create;
end;

destructor TSafeboxProgressObserver.Destroy;
begin
  Detach;
  FLock.Free;
  inherited;
end;

procedure TSafeboxProgressObserver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) and (AComponent=FSource) then FSource := nil;
end;

procedure TSafeboxProgressObserver.Attach;
begin
  Require(not FAttached,'Safebox log observer is already attached');
  FSource := TUserInterface.Log;
  Require(Assigned(FSource),'Actual application log is missing');
  FSource.FreeNotification(Self);
  FPreviousLog := FSource.OnInThreadNewLog;
  FLock.Acquire;
  try
    SetLength(FEntries,0);
    FUIEvents := 0;
  finally
    FLock.Release;
  end;
  FSource.OnInThreadNewLog := OnLog;
  TUserInterface.SynchronizationChanged.Add(OnUIChanged);
  FAttached := True;
end;

procedure TSafeboxProgressObserver.Detach;
begin
  if not FAttached then Exit;
  TUserInterface.SynchronizationChanged.Remove(OnUIChanged);
  if Assigned(FSource) then FSource.OnInThreadNewLog := FPreviousLog;
  FAttached := False;
end;

procedure TSafeboxProgressObserver.OnLog(AKind: TLogType; ATime: TDateTime;
  AThread: {$IFDEF FPC}TThreadID{$ELSE}UConst.TThreadID{$ENDIF}; const ASource, AText: String);
var I: Integer;
begin
  if Assigned(FPreviousLog) then FPreviousLog(AKind,ATime,AThread,ASource,AText);
  if Pos('Downloading Safebox chunks ',AText)<>1 then Exit;
  FLock.Acquire;
  try
    I := Length(FEntries);
    SetLength(FEntries,I+1);
    FEntries[I].Kind := AKind;
    FEntries[I].ThreadID := AThread;
    FEntries[I].Source := ASource;
    FEntries[I].Text := AText;
  finally
    FLock.Release;
  end;
end;

procedure TSafeboxProgressObserver.OnUIChanged(Sender: TObject);
begin
  Inc(FUIEvents);
end;

function TSafeboxProgressObserver.GetCount: Integer;
begin
  FLock.Acquire;
  try
    Result := Length(FEntries);
  finally
    FLock.Release;
  end;
end;

procedure TSafeboxProgressObserver.Check(AExpectedCount: Integer; const AExpectedText: String);
var I: Integer;
begin
  FLock.Acquire;
  try
    Require(Length(FEntries)=AExpectedCount,Format(
      'Expected %d safebox Info messages, received %d',[AExpectedCount,Length(FEntries)]));
    for I := 0 to High(FEntries) do begin
      Require(FEntries[I].Kind=ltinfo,'Chunk-counter progress was not logged at Info');
      Require(FEntries[I].Source='TNetData','Chunk-counter log lost its network progress source');
      Require(FEntries[I].ThreadID=MainThreadID,'Progress formatting/logging ran on its producer thread');
    end;
    if AExpectedCount>0 then
      Require(FEntries[High(FEntries)].Text=AExpectedText,'Safebox Info text differs from the visible counter');
  finally
    FLock.Release;
  end;
end;

constructor TSafeboxProgressProducer.Create(ANode: TNode; const AProgress: TProgressInfo);
begin
  FNode := ANode;
  FProgress := AProgress;
  Completed := TEvent.Create(nil,True,False,'');
  inherited Create(True);
  FreeOnTerminate := False;
end;

destructor TSafeboxProgressProducer.Destroy;
begin
  WaitFor;
  Completed.Free;
  inherited;
end;

procedure TSafeboxProgressProducer.BCExecute;
begin
  try
    try
      FNode.SetSynchronizationProgress(FProgress);
    except
      on E: Exception do Error := E.ClassName+': '+E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

constructor TPaintObserver.Create(AControl: TWinControl);
begin
  inherited Create;
  FControl := AControl;
  FOriginal := FControl.WindowProc;
  FControl.WindowProc := Observe;
end;

destructor TPaintObserver.Destroy;
begin
  FControl.WindowProc := FOriginal;
  inherited;
end;

procedure TPaintObserver.Observe(var AMessage: TMessage);
var LUpdate, LClient: TRect;
begin
  case AMessage.Msg of
    WM_PAINT: begin
      Inc(Paints);
      LClient := FControl.ClientRect;
      if GetUpdateRect(FControl.Handle,LUpdate,False) and
        (LUpdate.Left<=LClient.Left) and (LUpdate.Top<=LClient.Top) and
        (LUpdate.Right>=LClient.Right) and (LUpdate.Bottom>=LClient.Bottom) then
        Inc(FullPaints);
    end;
    WM_ERASEBKGND: begin
      Inc(Erases);
      if AMessage.LParam=0 then Inc(ExternalErases);
    end;
    WM_SHOWWINDOW: Inc(ShowMessages);
    WM_WINDOWPOSCHANGED: Inc(PositionMessages);
  end;
  FOriginal(AMessage);
end;

function TPaintObserver.Summary: String;
begin
  Result := Format('paints=%d full_client_paints=%d erases=%d external_erases=%d show_messages=%d position_messages=%d',
    [Paints,FullPaints,Erases,ExternalErases,ShowMessages,PositionMessages]);
end;

{$IFNDEF FPC}
function PaintSurface(AControl: TWinControl): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(AControl.ClientWidth,AControl.ClientHeight);
  Result.Canvas.Brush.Color := clFuchsia;
  Result.Canvas.FillRect(AControl.ClientRect);
  // PaintTo includes native graphic children; plain WM_PRINT omits VCL labels.
  AControl.PaintTo(Result.Canvas.Handle,0,0);
end;

function ChangedPixels(AFirst, ASecond: TBitmap): Integer;
type TPixelRow = array[0..$FFFFFF] of Cardinal; PPixelRow = ^TPixelRow;
var X,Y: Integer; LFirst,LSecond: PPixelRow;
begin
  Require((AFirst.Width=ASecond.Width) and (AFirst.Height=ASecond.Height),
    'Paint comparison dimensions differ');
  Result := 0;
  for Y := 0 to AFirst.Height-1 do begin
    LFirst := AFirst.ScanLine[Y]; LSecond := ASecond.ScanLine[Y];
    for X := 0 to AFirst.Width-1 do
      if (LFirst[X] and $FFFFFF)<>(LSecond[X] and $FFFFFF) then Inc(Result);
  end;
end;

function CheckEraseBoundary(AControl: TWinControl; const AName, APath: String): Integer;
var LBefore,LExternal,LInternal: TBitmap; LInternalChanges: Integer;
begin
  LBefore := PaintSurface(AControl);
  LExternal := TBitmap.Create;
  LInternal := TBitmap.Create;
  try
    LExternal.Assign(LBefore); LInternal.Assign(LBefore);
    // The native external erase has lParam=0. A buffered internal erase uses
    // the same DC in both parameters and may clear only its offscreen buffer.
    AControl.Perform(WM_ERASEBKGND,WPARAM(LExternal.Canvas.Handle),0);
    AControl.Perform(WM_ERASEBKGND,WPARAM(LInternal.Canvas.Handle),LPARAM(LInternal.Canvas.Handle));
    Result := ChangedPixels(LBefore,LExternal);
    LInternalChanges := ChangedPixels(LBefore,LInternal);
    LBefore.SaveToFile(ChangeFileExt(APath,'.'+AName+'-painted.bmp'));
    LExternal.SaveToFile(ChangeFileExt(APath,'.'+AName+'-external-erase.bmp'));
    LInternal.SaveToFile(ChangeFileExt(APath,'.'+AName+'-buffer-erase.bmp'));
    Log(APath,Format('ERASE_CHALLENGE %s external_changed_pixels=%d internal_changed_pixels=%d',
      [AName,Result,LInternalChanges]));
  finally LInternal.Free; LExternal.Free; LBefore.Free; end;
end;
{$ENDIF}

procedure CheckSynchronizationPainting(AMain: TMockMainForm; const ALogPath: String);
var LSync: TCTRLSyncronization; LControls: array[0..3] of TWinControl;
  LParents: array[0..3] of TWinControl; LObservers: array[0..3] of TPaintObserver;
  I,J,LEraseFailures: Integer; LProgress: TProgressInfo; LUntil: UInt64;
  LExpected: String; LWasUpdating: Boolean;
begin
  LSync := AMain.SyncControl;
  Require(Assigned(LSync),'Actual main synchronization control is missing');
  TUserInterface.ShowSyncDialog;
  Pump(200);
  Require(LSync.Visible and AMain.paSyncPanel.Visible and LSync.paSync.Visible,
    'Actual synchronization details did not become visible');
  Require(not LSync.paSplash.Visible,'Synchronization details are hidden behind the splash');
  LControls[0] := AMain.paSyncPanel; LControls[1] := LSync;
  LControls[2] := LSync.paSync; LControls[3] := LSync.GroupBox1;
  for I := Low(LControls) to High(LControls) do begin
    LParents[I] := LControls[I].Parent;
    LObservers[I] := TPaintObserver.Create(LControls[I]);
  end;
  LWasUpdating := False;
  try
    TUserInterface.Node.BeginBlockChainUpdate;
    LWasUpdating := True;
    for I := 1 to 16 do begin
      LProgress := TProgressInfo.Create(psDownloadingBlockchain,I,16);
      LProgress.BlockNumber := I;
      LProgress.PeerAddress := '127.0.0.1:4004';
      TUserInterface.Node.SetSynchronizationProgress(LProgress);
      TUserInterface.Node.NotifyBlocksChanged;
      LExpected := FormatProgressInfo(LProgress);
      LUntil := GetTickCount64+2000;
      repeat Pump(5) until (LSync.lblNodeStatus.Caption=LExpected) or (GetTickCount64>=LUntil);
      Require(LSync.lblNodeStatus.Caption=LExpected,'A real progress event did not publish the latest details');
      Require(LSync.DisplayedSynchronizationStatus.Progress.CurrentPosition=I,
        'The details retained an obsolete node progress record');
      for J := Low(LControls) to High(LControls) do begin
        Require(LControls[J].Visible,'A synchronization update hid '+LControls[J].Name);
        Require(LControls[J].Parent=LParents[J],'A synchronization update reparented '+LControls[J].Name);
        // Flush only pending native invalidations; this does not invalidate
        // the view or replace its normal rendering and event handlers.
        UpdateWindow(LControls[J].Handle);
      end;
      Pump(15);
    end;
    for I := Low(LControls) to High(LControls) do
      Log(ALogPath,'LIVE_EVENTS '+LControls[I].Name+' '+LObservers[I].Summary);
    Log(ALogPath,'PASS sixteen actual node progress/block events retained current visible details and stable parents');
    LEraseFailures := 0;
    {$IFNDEF FPC}
    // Exercise the actual details surfaces. The outer host is observed above;
    // it owns a child window covering its client and is not an erase target.
    // Report every layer before failing so diagnosis does not stop at the form.
    for I := 1 to High(LControls) do
      if CheckEraseBoundary(LControls[I],LControls[I].Name,ALogPath)>0 then Inc(LEraseFailures);
    Require(LEraseFailures=0,'Standalone native background erase exposes a blank intermediate details frame on '+IntToStr(LEraseFailures)+' surfaces');
    Log(ALogPath,'PASS external native erases preserve the complete details frame; internal erase diagnostics retained');
    {$ELSE}
    Log(ALogPath,'PASS Lazarus actual-event visibility/caption checks; VCL erase-boundary challenge is Delphi-only');
    {$ENDIF}
  finally
    if LWasUpdating then TUserInterface.Node.EndBlockChainUpdate;
    Pump(100);
    for I := High(LObservers) downto Low(LObservers) do LObservers[I].Free;
  end;
end;

procedure CheckSafeboxProgressLogging(AMain: TMockMainForm; const ALogPath: String);
var LProgress, LSaving: TProgressInfo; LNode: TNode; LSync: TCTRLSyncronization;
  LProducer: TSafeboxProgressProducer; LPeer: TObject;
  LScope: Boolean; LBefore, I: Integer; LUntil: UInt64; LLastText: String;

  procedure AwaitUI(ABefore: Integer; const AExpectedText: String);
  var LDeadline: UInt64;
  begin
    LDeadline := GetTickCount64+3000;
    repeat
      Pump(5);
    until ((SafeboxProgressObserver.UIEvents>ABefore) and
      (LSync.lblNodeStatus.Caption=AExpectedText)) or (GetTickCount64>=LDeadline);
    Require(SafeboxProgressObserver.UIEvents>ABefore,'Actual typed node event did not reach the UI');
    Require(LSync.lblNodeStatus.Caption=AExpectedText,'Actual synchronization label did not show current progress');
  end;

  procedure Publish(const AProgress: TProgressInfo; AExpectedCount: Integer;
    const AExpectedText: String; const ALastLogText: String);
  var LBefore: Integer;
  begin
    LBefore := SafeboxProgressObserver.UIEvents;
    LNode.SetSynchronizationProgress(AProgress);
    AwaitUI(LBefore,AExpectedText);
    SafeboxProgressObserver.Check(AExpectedCount,ALastLogText);
  end;

  function ChunkText(ACurrent, ATotal, APercentage: Integer): String;
  begin
    Result := Format('Downloading Safebox chunks %d/%d ',[ACurrent,ATotal])+
      FormatFloat('0.00',APercentage)+'%';
  end;

begin
  LNode := TUserInterface.Node;
  LSync := AMain.SyncControl;
  Require(Assigned(LNode) and Assigned(LSync),'Actual node/synchronization control is missing');
  TUserInterface.ShowSyncDialog;
  Pump(100);
  Require(LSync.Visible and LSync.paSync.Visible,'Actual synchronization details are not visible');
  if not Assigned(SafeboxProgressObserver) then
    SafeboxProgressObserver := TSafeboxProgressObserver.Create;
  SafeboxProgressObserver.Attach;
  LScope := False;
  LProducer := nil;
  LPeer := TObject.Create;
  try
    LNode.BeginBlockChainUpdate;
    LScope := True;
    LProgress := TProgressInfo.Create(psDownloadingSafeboxChunks,0,4);
    LProgress.ChunksCount := 4;
    LProgress.BlockCount := 120000;
    LLastText := ChunkText(0,4,0);
    Publish(LProgress,1,LLastText,LLastText);
    for I := 1 to 3 do Publish(LProgress,1,LLastText,LLastText);
    Log(ALogPath,'PASS initial counter logged once at Info; repeated actual UI deliveries add no logs');

    LProgress.CurrentPosition := 1;
    LLastText := ChunkText(1,4,25);
    Publish(LProgress,2,LLastText,LLastText);
    // Only cached synchronization facts change. The actual fresh wallet stays
    // offline; its networking owner must never acquire a real peer here.
    Require(not TNetData.NetData.NetConnectionsActive,'Fixture transport must remain offline');
    LBefore := SafeboxProgressObserver.UIEvents;
    LNode.SetSynchronizationNetworkActive(True);
    AwaitUI(LBefore,LLastText);
    SafeboxProgressObserver.Check(2,LLastText);
    LBefore := SafeboxProgressObserver.UIEvents;
    LNode.SetSynchronizationPeerConnected(LPeer,True);
    AwaitUI(LBefore,LLastText);
    Require(LNode.GetSynchronizationStatus.PeerCount=1,'Unrelated peer event did not change cached peer count');
    SafeboxProgressObserver.Check(2,LLastText);
    LBefore := SafeboxProgressObserver.UIEvents;
    LNode.SetSynchronizationPeerConnected(LPeer,False);
    AwaitUI(LBefore,LLastText);
    Require(LNode.GetSynchronizationStatus.PeerCount=0,'Unrelated peer removal did not change cached peer count');
    SafeboxProgressObserver.Check(2,LLastText);
    LBefore := SafeboxProgressObserver.UIEvents;
    LNode.SetSynchronizationNetworkActive(False);
    AwaitUI(LBefore,LLastText);
    SafeboxProgressObserver.Check(2,LLastText);
    Require(not TNetData.NetData.NetConnectionsActive,'Cached status changes activated real networking');
    Log(ALogPath,'PASS increment logs the displayed counter; unrelated peer changes do not repeat it');

    LProgress.TotalCount := 8;
    LProgress.ChunksCount := 8;
    LLastText := 'Downloading Safebox chunks 1/8 '+FormatFloat('0.00',12.5)+'%';
    Publish(LProgress,3,LLastText,LLastText);
    LSaving := TProgressInfo.Create(psSavingSafebox);
    LSaving.ChunksCount := 8;
    LSaving.FileName := 'checkpoint.safebox';
    Publish(LSaving,3,'Saving Safebox 8 chunks to checkpoint.safebox ',LLastText);
    Publish(LProgress,4,LLastText,LLastText);
    Log(ALogPath,'PASS changed total logs current percentage; another phase emits no chunk log and resets deduplication');

    LProgress.CurrentPosition := 2;
    LLastText := ChunkText(2,8,25);
    LBefore := SafeboxProgressObserver.UIEvents;
    LProducer := TSafeboxProgressProducer.Create(LNode,LProgress);
    LProducer.Start;
    // Do not pump UI work until the real producer returns. Logging must happen
    // on the existing UI dispatch, never hold the producer behind a log sink.
    Require(LProducer.Completed.WaitFor(2000)=wrSignaled,
      'Progress producer waited for the non-pumping UI thread');
    Require(LProducer.Error='',LProducer.Error);
    Require(SafeboxProgressObserver.UIEvents=LBefore,'UI delivery occurred before its dispatcher was pumped');
    SafeboxProgressObserver.Check(4,'Downloading Safebox chunks 1/8 '+FormatFloat('0.00',12.5)+'%');
    AwaitUI(LBefore,LLastText);
    SafeboxProgressObserver.Check(5,LLastText);
    FreeAndNil(LProducer);
    Log(ALogPath,'PASS producer returns before UI delivery; Info formatting/logging executes on main thread');

    LBefore := SafeboxProgressObserver.UIEvents;
    LNode.EndBlockChainUpdate;
    LScope := False;
    LUntil := GetTickCount64+3000;
    repeat Pump(5) until ((LNode.GetSynchronizationStatus.State<>nssSynchronizing) and
      (SafeboxProgressObserver.UIEvents>LBefore)) or (GetTickCount64>=LUntil);
    Require(LNode.GetSynchronizationStatus.State<>nssSynchronizing,'Completed physical session did not leave synchronization');
    Require(SafeboxProgressObserver.UIEvents>LBefore,'Completed session did not notify the actual UI');
    SafeboxProgressObserver.Check(5,LLastText);
    LNode.BeginBlockChainUpdate;
    LScope := True;
    Publish(LProgress,6,LLastText,LLastText);
    Publish(LProgress,6,LLastText,LLastText);
    Log(ALogPath,'PASS a new physical session logs the same counter again, then suppresses its duplicates');
    Log(ALogPath,'PASS SAFEBOX INFO: six Info records match real labels and network source; duplicate, phase, session and nonblocking checks passed');
  finally
    // The observer stays alive until unit finalization: restoring a borrowed
    // callback alone does not prove no background logger already captured it.
    if Assigned(LProducer) then begin
      LUntil := GetTickCount64+3000;
      repeat Pump(5) until (LProducer.Completed.WaitFor(0)=wrSignaled) or (GetTickCount64>=LUntil);
      LProducer.Free;
    end;
    LNode.SetSynchronizationPeerConnected(LPeer,False);
    LNode.SetSynchronizationNetworkActive(False);
    if LScope then LNode.EndBlockChainUpdate;
    Pump(100);
    SafeboxProgressObserver.Detach;
    LPeer.Free;
  end;
end;

initialization
  SafeboxProgressObserver := nil;

finalization
  SafeboxProgressObserver.Free;

end.
