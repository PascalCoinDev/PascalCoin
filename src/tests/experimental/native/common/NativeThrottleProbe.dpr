program NativeThrottleProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT license; see the repository LICENSE. }

{$APPTYPE CONSOLE}
uses System.SysUtils, System.Classes, System.JSON, System.DateUtils, System.IOUtils,
  Winapi.Windows, Vcl.Forms, Vcl.ExtCtrls, UCommon, UCommon.UI;
type
  EControlledTrailingError = class(Exception);
  TProbe = class;
  TThrottleCase = class(TComponent)
  private
    FProbe: TProbe;
    FName: String;
    FThrottle: TThrottledEvent;
    FTimer: TTimer;
    FOriginalTimer: TNotifyEvent;
    FTimes: TArray<Cardinal>;
    FNext, FNotifications, FCalls, FLeading, FTrailing, FTimers, FEarlyTimers, FEarlyCalls: Integer;
    FWaitingStarts, FWaitingStops, FFinished, FWaitingErrors, FExceptions, FUnexpectedErrors: Integer;
    FLastInput: UInt64;
    FInNotify, FPeriodic, FLastWaiting, FActionDone, FModeRestored: Boolean;
    procedure Delivered(Sender: TObject);
    procedure WaitingChanged(Sender: TObject);
    procedure BurstFinished(Sender: TObject);
    procedure TimerFired(Sender: TObject);
    procedure Trace(const Kind: String);
  public
    constructor Create(AProbe: TProbe; const AName: String; AMode: TThrottledEventMode; const ATimes: TArray<Cardinal>);
    procedure Tick(AElapsed: UInt64);
    function Validate: TJSONObject;
  end;
  TProbe = class(TComponent)
  private
    FLog: TFileStream;
    FOutput: String;
    FStarted: UInt64;
    FCases: TArray<TThrottleCase>;
  public
    constructor Create(const AOutput: String); reintroduce;
    destructor Destroy; override;
    procedure Log(const S: String);
    function Run: Boolean;
    property Started: UInt64 read FStarted;
  end;
function Sequence(Step, Finish: Cardinal): TArray<Cardinal>;
var I: Integer;
begin
  SetLength(Result, Finish div Step + 1);
  for I:=0 to High(Result) do Result[I]:=Cardinal(I)*Step;
end;
constructor TThrottleCase.Create(AProbe: TProbe; const AName: String; AMode: TThrottledEventMode; const ATimes: TArray<Cardinal>);
var I: Integer;
begin
  inherited Create(AProbe); FProbe:=AProbe; FName:=AName; FTimes:=Copy(ATimes);
  FPeriodic:=AMode=temNotifyEveryInterval;
  FThrottle:=TThrottledEvent.Create(Self);
  FThrottle.Interval:=TTimeSpan.FromSeconds(5);
  FThrottle.Mode:=AMode; FThrottle.Add(Delivered);
  FThrottle.OnWaitingChanged.Add(WaitingChanged);
  FThrottle.OnBurstFinished.Add(BurstFinished);
  for I:=0 to FThrottle.ComponentCount-1 do
    if FThrottle.Components[I] is TTimer then FTimer:=TTimer(FThrottle.Components[I]);
  if not Assigned(FTimer) then raise Exception.Create('Owned native timer absent');
  FOriginalTimer:=FTimer.OnTimer; FTimer.OnTimer:=TimerFired;
  FProbe.Log(FName+' CONFIG interval_ms='+FloatToStr(FThrottle.Interval.TotalMilliseconds)+
    ';interval_ticks='+IntToStr(FThrottle.Interval.Ticks)+';mode='+IntToStr(Ord(AMode)));
end;
procedure TThrottleCase.Trace(const Kind: String);
begin
  FProbe.Log(Format('%s %s notify=%d call=%d leading=%d trailing=%d timers=%d '+
    'timer_enabled=%s timer_ms=%d quiet_tick_ms=%d quiet_span_ms=%.3f '+
    'client_datetime=%.12f actual_datetime=%.12f suppressed=%d waiting=%s burst_active=%s '+
    'waiting_on=%d waiting_off=%d finished=%d exceptions=%d',
    [FName,Kind,FNotifications,FCalls,FLeading,FTrailing,FTimers,
    BoolToStr(FTimer.Enabled,True),FTimer.Interval,GetTickCount64-FLastInput,
    TTimeSpan.Subtract(Now,FThrottle.LastClientNotify).TotalMilliseconds,
    Double(FThrottle.LastClientNotify),Double(FThrottle.LastActualNotify),FThrottle.SuppressedInvocations,
    BoolToStr(FThrottle.Waiting,True),BoolToStr(FThrottle.BurstActive,True),
    FWaitingStarts,FWaitingStops,FFinished,FExceptions]));
end;
procedure TThrottleCase.Tick(AElapsed: UInt64);
begin
  if not FActionDone and(AElapsed>=1000)and
    ((FName='cancel-recovery')or(FName='mode-change-recovery'))then begin
    FActionDone:=True;
    if FName='cancel-recovery'then FThrottle.Cancel
    else FThrottle.Mode:=temNotifyEveryInterval;
    if FThrottle.Waiting or FTimer.Enabled or(FFinished<>0)then Inc(FWaitingErrors);
    Trace('CANCEL_OR_MODE_CHANGE');
  end;
  if(FName='mode-change-recovery')and not FModeRestored and(AElapsed>=1500)then begin
    FModeRestored:=True;
    FThrottle.Mode:=temNotifyOnEventBurstStartAndFinished;
    if FThrottle.Waiting or FTimer.Enabled then Inc(FWaitingErrors);
    Trace('MODE_RESTORED');
  end;
  while (FNext<Length(FTimes)) and(AElapsed>=FTimes[FNext]) do begin
    Inc(FNotifications); Inc(FNext); FLastInput:=GetTickCount64;
    FInNotify:=True;
    try FThrottle.Notify; finally FInNotify:=False; end;
    if FThrottle.Waiting<>FThrottle.BurstActive then Inc(FWaitingErrors);
    if FThrottle.Waiting=FPeriodic then Inc(FWaitingErrors);
    Trace('NOTIFY');
  end;
end;
procedure TThrottleCase.WaitingChanged(Sender: TObject);
begin
  if(FThrottle.Waiting<>FThrottle.BurstActive)or(FLastWaiting=FThrottle.Waiting)then
    Inc(FWaitingErrors);
  FLastWaiting:=FThrottle.Waiting;
  if FLastWaiting then Inc(FWaitingStarts)else Inc(FWaitingStops);
  Trace('WAITING_CHANGED');
end;
procedure TThrottleCase.BurstFinished(Sender: TObject);
begin
  Inc(FFinished);
  if FThrottle.Waiting or FThrottle.BurstActive then Inc(FWaitingErrors);
  Trace('BURST_FINISHED');
end;
procedure TThrottleCase.Delivered(Sender: TObject);
begin
  Inc(FCalls);
  if(FThrottle.Waiting<>FThrottle.BurstActive)or(FThrottle.Waiting=FPeriodic)then
    Inc(FWaitingErrors);
  if FInNotify then Inc(FLeading) else begin
    Inc(FTrailing);
    if GetTickCount64-FLastInput<4980 then Inc(FEarlyCalls);
  end;
  Trace('INVOKE');
  if(FName='throwing-trailing-recovery')and not FInNotify and(FExceptions=0)then
    raise EControlledTrailingError.Create('Controlled ordinary trailing callback failure');
end;
procedure TThrottleCase.TimerFired(Sender: TObject);
begin
  Inc(FTimers);
  if(not FPeriodic)and(GetTickCount64-FLastInput<4980)then Inc(FEarlyTimers);
  Trace('TIMER_BEFORE');
  try
    FOriginalTimer(Sender);
  except
    on E:EControlledTrailingError do begin
      Inc(FExceptions);
      if FThrottle.Waiting or FTimer.Enabled then Inc(FWaitingErrors);
      Trace('CONTROLLED_TRAILING_EXCEPTION');
    end;
    on E:Exception do begin
      Inc(FUnexpectedErrors);
      FProbe.Log(FName+' UNEXPECTED_EXCEPTION '+E.ClassName+': '+E.Message);
    end;
  end;
  if FThrottle.Waiting<>FThrottle.BurstActive then Inc(FWaitingErrors);
  Trace('TIMER_AFTER');
end;
function TThrottleCase.Validate: TJSONObject;
var DeliveryPass, PhysicalPass, WaitingPass: Boolean;
  ExpectedCalls, ExpectedBursts, ExpectedFinished, ExpectedExceptions: Integer;
begin
  ExpectedCalls:=2;
  ExpectedBursts:=1;ExpectedFinished:=1;ExpectedExceptions:=0;
  if FName='two-bursts' then ExpectedCalls:=4;
  if FName='finished-only-100ms' then ExpectedCalls:=1;
  if FName='throwing-trailing-recovery'then begin
    ExpectedCalls:=3;ExpectedExceptions:=1;
  end;
  if(FName='two-bursts')or(FName='single-input-lifecycle')or
    (FName='throwing-trailing-recovery')then begin
    ExpectedBursts:=2;ExpectedFinished:=2;
  end;
  if(FName='cancel-recovery')or(FName='mode-change-recovery')then ExpectedBursts:=2;
  if FPeriodic then begin
    ExpectedCalls:=5;
    ExpectedBursts:=0;
    ExpectedFinished:=0;
  end;
  DeliveryPass:=(FCalls=ExpectedCalls)and(FNotifications=Length(FTimes));
  if FPeriodic then DeliveryPass:=DeliveryPass and(FLeading=1)
  else DeliveryPass:=DeliveryPass and(FEarlyCalls=0);
  if FName='two-bursts' then DeliveryPass:=DeliveryPass and(FLeading=2)and(FTrailing=2);
  if FName='finished-only-100ms' then DeliveryPass:=DeliveryPass and(FLeading=0)and(FTrailing=1);
  if(FName='single-input-lifecycle')or(FName='cancel-recovery')or
    (FName='mode-change-recovery')then
    DeliveryPass:=DeliveryPass and(FLeading=2)and(FTrailing=0);
  if FName='throwing-trailing-recovery'then
    DeliveryPass:=DeliveryPass and(FLeading=2)and(FTrailing=1);
  PhysicalPass:=FEarlyTimers=0;
  WaitingPass:=(FWaitingErrors=0)and(FUnexpectedErrors=0)and
    (FWaitingStarts=ExpectedBursts)and(FWaitingStops=ExpectedBursts)and
    (FFinished=ExpectedFinished)and(FExceptions=ExpectedExceptions)and
    not FThrottle.Waiting and not FThrottle.BurstActive;
  Result:=TJSONObject.Create;
  Result.AddPair('case',FName);
  Result.AddPair('notifications',TJSONNumber.Create(FNotifications));
  Result.AddPair('invocations',TJSONNumber.Create(FCalls));
  Result.AddPair('leading',TJSONNumber.Create(FLeading));
  Result.AddPair('trailing',TJSONNumber.Create(FTrailing));
  Result.AddPair('physicalTimerFires',TJSONNumber.Create(FTimers));
  Result.AddPair('physicalFiresBeforeQuiet',TJSONNumber.Create(FEarlyTimers));
  Result.AddPair('deliveredBeforeQuiet',TJSONNumber.Create(FEarlyCalls));
  Result.AddPair('waitingStarts',TJSONNumber.Create(FWaitingStarts));
  Result.AddPair('waitingStops',TJSONNumber.Create(FWaitingStops));
  Result.AddPair('finished',TJSONNumber.Create(FFinished));
  Result.AddPair('controlledTrailingExceptions',TJSONNumber.Create(FExceptions));
  Result.AddPair('waitingErrors',TJSONNumber.Create(FWaitingErrors));
  Result.AddPair('unexpectedErrors',TJSONNumber.Create(FUnexpectedErrors));
  Result.AddPair('deliveryPass',TJSONBool.Create(DeliveryPass));
  Result.AddPair('physicalTimerResetPass',TJSONBool.Create(PhysicalPass));
  Result.AddPair('waitingPass',TJSONBool.Create(WaitingPass));
  Result.AddPair('pass',TJSONBool.Create(DeliveryPass and PhysicalPass and WaitingPass));
end;
constructor TProbe.Create(const AOutput: String);
begin
  inherited Create(nil); FOutput:=IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(AOutput)));
  FLog:=TFileStream.Create(AOutput,fmCreate or fmShareDenyNone);
  FStarted:=GetTickCount64;
  Log('NATIVE Delphi13 VCL real TTimer and production TThrottledEvent; interval5seconds');
  Log(Format('NATIVE_TIME_SPAN days=%.12f milliseconds=%.3f; known_4900_ms=%.3f',
    [TTimeSpan.FromSeconds(5).TotalDays,TTimeSpan.FromSeconds(5).TotalMilliseconds,
    TTimeSpan.Subtract(Now,Now-TTimeSpan.FromMilliseconds(4900).TotalDays).TotalMilliseconds]));
  SetLength(FCases,10);
  FCases[0]:=TThrottleCase.Create(Self,'continuous-100ms',temNotifyOnEventBurstStartAndFinished,Sequence(100,16000));
  FCases[1]:=TThrottleCase.Create(Self,'continuous-1s',temNotifyOnEventBurstStartAndFinished,Sequence(1000,16000));
  FCases[2]:=TThrottleCase.Create(Self,'late-4.9s',temNotifyOnEventBurstStartAndFinished,TArray<Cardinal>.Create(0,100,4900));
  FCases[3]:=TThrottleCase.Create(Self,'two-bursts',temNotifyOnEventBurstStartAndFinished,TArray<Cardinal>.Create(0,1000,9000,10000));
  FCases[4]:=TThrottleCase.Create(Self,'finished-only-100ms',temNotifyOnEventBurstFinished,Sequence(100,16000));
  FCases[5]:=TThrottleCase.Create(Self,'periodic-control-100ms',temNotifyEveryInterval,Sequence(100,16000));
  FCases[6]:=TThrottleCase.Create(Self,'single-input-lifecycle',temNotifyOnEventBurstStartAndFinished,TArray<Cardinal>.Create(0,6000));
  FCases[7]:=TThrottleCase.Create(Self,'throwing-trailing-recovery',temNotifyOnEventBurstStartAndFinished,TArray<Cardinal>.Create(0,100,6000));
  FCases[8]:=TThrottleCase.Create(Self,'cancel-recovery',temNotifyOnEventBurstStartAndFinished,TArray<Cardinal>.Create(0,100,2000));
  FCases[9]:=TThrottleCase.Create(Self,'mode-change-recovery',temNotifyOnEventBurstStartAndFinished,TArray<Cardinal>.Create(0,100,2000));
end;
destructor TProbe.Destroy;
begin
  inherited;
  FLog.Free;
end;
procedure TProbe.Log(const S: String);
var B:TBytes;
begin
  B:=TEncoding.UTF8.GetBytes(UIntToStr(GetTickCount64-FStarted)+' '+S+#13#10);
  FLog.WriteBuffer(B[0],Length(B));
end;
function TProbe.Run: Boolean;
var I: Integer; Root, Item: TJSONObject; Items:TJSONArray;
begin
  while GetTickCount64-FStarted<23500 do begin
    for I:=0 to High(FCases)do FCases[I].Tick(GetTickCount64-FStarted);
    Application.ProcessMessages; Sleep(1);
  end;
  Result:=True;Root:=TJSONObject.Create;Items:=TJSONArray.Create;
  try
    for I:=0 to High(FCases)do begin
      Item:=FCases[I].Validate;Items.AddElement(Item);
      Result:=Result and Item.GetValue<Boolean>('pass');
    end;
    Root.AddPair('pass',TJSONBool.Create(Result));Root.AddPair('cases',Items);
    Root.AddPair('elapsedMs',TJSONNumber.Create(GetTickCount64-FStarted));
    TFile.WriteAllText(FOutput+'result.json',Root.ToJSON,TEncoding.UTF8);
    Log('RESULT '+Root.ToJSON);
  finally Root.Free;end;
end;
var Probe:TProbe;Code:Integer;
begin
  Code:=2;
  try
    if ParamCount<>1 then raise Exception.Create('Expected trace logfile in an existing output directory');
    Application.Initialize;
    Probe:=TProbe.Create(ParamStr(1));
    try if Probe.Run then Code:=0 else Code:=1;finally Probe.Free;end;
  except on E:Exception do begin
    WriteLn(E.ClassName+': '+E.Message);
    Code:=2;
  end;end;
  ExitCode:=Code;
end.
