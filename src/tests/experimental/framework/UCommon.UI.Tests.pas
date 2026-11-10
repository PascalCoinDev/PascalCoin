unit UCommon.UI.Tests;

{ Copyright © 2026 Herman Schoenfeld
  Distributed under the MIT software license, see the accompanying LICENSE. }

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, Forms, ExtCtrls, SyncObjs, Generics.Collections, fpcunit, testregistry,
  UCommon, UCommon.UI;

type
  TWaitingTestAction = (wtaNone, wtaNotify, wtaCancel, wtaChangeMode, wtaDispose);

  TThrottledEventTests = class(TTestCase)
  private
    FEvent : TThrottledEvent;
    FDeliveryTicks : TList<QWord>;
    FLastInputTick : QWord;
    FCallbacksOnMainThread : Boolean;
    FRearmNextDelivery : Boolean;
    FLastSuppressedCount : Integer;
    FNativeTimer : TTimer;
    FOriginalTimerHandler : TNotifyEvent;
    FTimerWakeTicks : TList<QWord>;
    FTimingTrace : TStringList;
    FNotifyDuringDelivery : Boolean;
    FThrowNextDelivery: Boolean;
    FAsyncExceptionCount: Integer;
    FUnexpectedException: String;
    FPreviousException: TExceptionEvent;
    FStartedCount, FFinishingCount, FFinishedCount: Integer;
    FFinishedTick: QWord;
    FLifecycleStateValid, FPostponeFinish, FNotifyOnStart, FNotifyOnFinish: Boolean;
    FWaitingStates: TList<Boolean>;
    FWaitingStateValid: Boolean;
    FWaitingAction: TWaitingTestAction;
    FWaitingActionState: Boolean;
    FThrowStarted, FThrowFinishing, FThrowFinished: Boolean;
    FThrowWaitingTrue, FThrowWaitingFalse: Boolean;
    FNotifyOnTrailing, FCancelOnDelivery, FDisposeOnDelivery, FDisposeOnFinishing: Boolean;
    FSlowLeadingMilliseconds: Cardinal;
    FSlowLeadingEntered: TEvent;
    procedure OnExpectedException(Sender: TObject; E: Exception);
    procedure OnWaitingChanged(Sender: TObject);
    procedure ThrowOnce(var ARequested: Boolean);
    procedure DisposeEvent;
    procedure OnBurstStarted(Sender: TObject);
    procedure OnBurstFinishing(Sender: TObject);
    procedure OnBurstFinished(Sender: TObject);
    procedure WaitForFinishes(ACount: Integer);
    procedure OnNativeTimer(Sender : TObject);
    procedure CheckPhysicalTimerReset(AMode : TThrottledEventMode);
    procedure OnNotified(Sender : TObject);
    procedure Notify;
    procedure PumpFor(AMilliseconds : Cardinal);
    procedure WaitForDeliveries(ACount : Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FirstNotificationIsImmediate;
    procedure IntervalRetainsItsNumericMilliseconds;
    procedure FirstNotificationAfterTrailingStartsImmediately;
    procedure ContinuousBurstHasOnlyLeadingAndFinalDeliveries;
    procedure LaterNotificationsExtendTheQuietDeadline;
    procedure SingleNotificationDoesNotDeliverTwice;
    procedure PostponeDoesNotStartAnotherLeadingDelivery;
    procedure PostponeRetainsElapsedProducerQuietTime;
    procedure ListenerCanPostponeATrailingDeliveryAgain;
    procedure LeadingBurstInputRestartsItsNativeTimer;
    procedure TrailingBurstInputRestartsItsNativeTimer;
    procedure PeriodicInputsRetainTheArmedTimerDeadline;
    procedure ReentrantBusyListenerRetainsTheResetDeadline;
    procedure SingleInputFinishesItsLifecycleWithoutAnotherDelivery;
    procedure ContinuousInputExtendsTheLifecycleDeadline;
    procedure FinishCandidateCanPostponeWithoutPublishingFinished;
    procedure StartedListenerCanNotifyWithoutDuplicatingTheStart;
    procedure FinishedListenerCanStartTheNextBurst;
    procedure DisposingActiveBurstCancelsItsLifecycleTimer;
    procedure NonBurstModesDoNotPublishBurstLifecycle;
    procedure TrailingOnlyModePublishesOneStartAndFinish;
    procedure TrailingListenerExceptionDoesNotLeaveAnUnarmedBurst;
    procedure WaitingIsCommittedBeforeStartAndLeadingDelivery;
    procedure WaitingChangesOnceForAQuietSingleInput;
    procedure PendingInputsRetainWaitingUntilTheirOwnDeadline;
    procedure CancelClearsWaitingWithoutDeliveringPendingWork;
    procedure CancelWhileIdleDoesNotPublishAStateChange;
    procedure ModeChangeCancelsThePreviousWaitingRequest;
    procedure ChangingBetweenBurstModesCancelsTheOldBurst;
    procedure WaitingStartListenerCanCancelItsRequest;
    procedure WaitingStartListenerCanChangeMode;
    procedure WaitingStartListenerCanNotifyReentrantly;
    procedure WaitingEndListenerCanStartANewRequest;
    procedure CancelStateListenerCanStartANewRequest;
    procedure WaitingStartListenerCanDisposeTheComponent;
    procedure WaitingEndListenerCanDisposeTheComponent;
    procedure OrdinaryListenerCanDisposeTheComponent;
    procedure FinishingListenerCanDisposeTheComponent;
    procedure LeadingListenerFailureRetainsTheOwnedTimer;
    procedure StartedListenerFailureRetainsTheOwnedTimer;
    procedure FinishingListenerFailureCommitsIdleState;
    procedure FinishedListenerFailureLeavesIdleState;
    procedure WaitingStartFailureRetainsTheOwnedTimer;
    procedure WaitingEndFailureLeavesIdleState;
    procedure CancelStateFailureStillLeavesIdleState;
    procedure PostponedFinishingFailureRetainsTheRearmedRequest;
    procedure ReentrantTrailingRequestDeliversImmediatelyOnce;
    procedure FiveSecondInputsAt4900MsRetainOneOwnedBurst;
    procedure CapturedLateTimerAfterFreshInputCannotFinishWaiting;
    procedure SlowLeadingListenerAcceptsSynchronizedUiInputsWithoutEarlyFinish;
  end;

  TThreadNotifyTests = class(TTestCase)
  private
    FNotifyCount : Integer;
    FNotifierToDispose : TThreadNotify;
    procedure OnNotified(Sender : TObject);
    procedure DisposeOtherNotifier(Sender : TObject);
  published
    procedure DisposingNotifierCancelsQueuedMethod;
    procedure DisposingNotifierPreservesOtherQueuedNotifications;
    procedure CallbackCanDisposeAnotherPendingNotifier;
  end;

implementation

type
  EThrottleTestCallback = class(Exception);

  TReusedThreadNotify = class(TThreadNotify)
  public
    procedure FreeInstance; override;
    procedure ReleaseMemory;
  end;

  TNotifyProducerThread = class(TThread)
  private
    FNotifier : TThreadNotify;
    FOtherNotifier : TThreadNotify;
    FHandler : TNotifyEvent;
    FOtherHandler : TNotifyEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(ANotifier : TThreadNotify; AHandler : TNotifyEvent; AOtherNotifier : TThreadNotify = nil; AOtherHandler : TNotifyEvent = nil);
  end;

  TSynchronizedThrottleInputs = class(TThread)
  private
    FStarted, FCompleted: TEvent;
    FInput: TThreadMethod;
    procedure InvokeInput;
  protected
    procedure Execute; override;
  public
    InputCount: Integer;
    WrongThread, StartTimedOut: Boolean;
    constructor Create(AStarted: TEvent; AInput: TThreadMethod);
    destructor Destroy; override;
    procedure Stop;
  end;

const
  CT_TEST_INTERVAL = 120;

constructor TSynchronizedThrottleInputs.Create(AStarted: TEvent; AInput: TThreadMethod);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FStarted := AStarted;
  FInput := AInput;
  FCompleted := TEvent.Create(nil, True, False, '');
end;

destructor TSynchronizedThrottleInputs.Destroy;
begin
  Stop;
  FCompleted.Free;
  inherited;
end;

procedure TSynchronizedThrottleInputs.InvokeInput;
begin
  WrongThread := WrongThread or (GetCurrentThreadID <> MainThreadID);
  FInput;
end;

procedure TSynchronizedThrottleInputs.Execute;
begin
  try
    StartTimedOut := FStarted.WaitFor(5000) <> wrSignaled;
    if StartTimedOut then Exit;
    while not Terminated do begin
      Synchronize(InvokeInput);
      Inc(InputCount);
      Sleep(50);
    end;
  finally
    FCompleted.SetEvent;
  end;
end;

procedure TSynchronizedThrottleInputs.Stop;
var LDeadline: QWord;
begin
  Terminate;
  FStarted.SetEvent;
  LDeadline := GetTickCount64 + 2500;
  while (FCompleted.WaitFor(0) <> wrSignaled) and (GetTickCount64 < LDeadline) do begin
    CheckSynchronize(0);
    Sleep(1);
  end;
  if FCompleted.WaitFor(0) <> wrSignaled then
    raise Exception.Create('Synchronized throttle-input fixture did not stop');
  WaitFor;
end;

procedure TReusedThreadNotify.FreeInstance;
begin
  CleanupInstance;
  // Retain the allocation and simulate allocator reuse. A stale queued method
  // now fails deterministically, without reading memory returned to the heap.
  FillChar((PByte(Self)+SizeOf(Pointer))^,InstanceSize-SizeOf(Pointer),$A5);
end;

procedure TReusedThreadNotify.ReleaseMemory;
begin
  System.FreeMem(Pointer(Self));
end;

constructor TNotifyProducerThread.Create(ANotifier : TThreadNotify; AHandler : TNotifyEvent; AOtherNotifier : TThreadNotify; AOtherHandler : TNotifyEvent);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNotifier := ANotifier;
  FOtherNotifier := AOtherNotifier;
  FHandler := AHandler;
  FOtherHandler := AOtherHandler;
end;

procedure TNotifyProducerThread.Execute;
begin
  FNotifier.Invoke(Self,FHandler);
  if Assigned(FOtherNotifier) then begin
    if Assigned(FOtherHandler) then FOtherNotifier.Invoke(Self,FOtherHandler)
    else FOtherNotifier.Invoke(Self,FHandler);
  end;
end;

procedure TThreadNotifyTests.OnNotified(Sender : TObject);
begin
  Inc(FNotifyCount);
end;

procedure TThreadNotifyTests.DisposeOtherNotifier(Sender : TObject);
begin
  Inc(FNotifyCount);
  FreeAndNil(FNotifierToDispose);
end;

procedure TThreadNotifyTests.DisposingNotifierCancelsQueuedMethod;
var
  LNotifier : TReusedThreadNotify;
  LProducer : TNotifyProducerThread;
  LDisposed, LInvokedAfterDisposal : Boolean;
  {$IFNDEF WINDOWS}LDeadline : QWord;{$ENDIF}
begin
  CheckSynchronize(0);
  FNotifyCount := 0;
  LNotifier := TReusedThreadNotify.Create(TThread.CurrentThread);
  LProducer := TNotifyProducerThread.Create(LNotifier,OnNotified);
  LDisposed := False;
  try
    LProducer.Start;
    // Join the producer without pumping its queued notification. Main-thread
    // TThread.WaitFor can dispatch that notification before disposal is tested.
    {$IFDEF WINDOWS}
    AssertEquals('Notification producer did not finish',LongInt(WAIT_OBJECT_0),
      LongInt(WaitForSingleObject(LProducer.Handle,5000)));
    {$ELSE}
    LDeadline := GetTickCount64+5000;
    while (not LProducer.Finished) and (GetTickCount64<LDeadline) do Sleep(1);
    AssertTrue('Notification producer did not finish',LProducer.Finished);
    {$ENDIF}
    LNotifier.Free;
    LDisposed := True;
    LInvokedAfterDisposal := False;
    try
      CheckSynchronize(0);
    except
      on E : EAccessViolation do LInvokedAfterDisposal := True;
    end;
    AssertFalse('A disposed notifier left its queued method behind',LInvokedAfterDisposal);
    AssertEquals('A disposed notifier must not deliver its callback',0,FNotifyCount);
  finally
    LProducer.WaitFor;
    LProducer.Free;
    if not LDisposed then LNotifier.Free;
    CheckSynchronize(0);
    LNotifier.ReleaseMemory;
  end;
end;

procedure TThreadNotifyTests.DisposingNotifierPreservesOtherQueuedNotifications;
var
  LNotifier, LOtherNotifier : TThreadNotify;
  LProducer : TNotifyProducerThread;
  {$IFNDEF WINDOWS}LDeadline : QWord;{$ENDIF}
begin
  CheckSynchronize(0);
  FNotifyCount := 0;
  LNotifier := TThreadNotify.Create(TThread.CurrentThread);
  LOtherNotifier := TThreadNotify.Create(TThread.CurrentThread);
  LProducer := TNotifyProducerThread.Create(LNotifier,OnNotified,LOtherNotifier);
  try
    LProducer.Start;
    {$IFDEF WINDOWS}
    AssertEquals('Notification producer did not finish',LongInt(WAIT_OBJECT_0),
      LongInt(WaitForSingleObject(LProducer.Handle,5000)));
    {$ELSE}
    LDeadline := GetTickCount64+5000;
    while (not LProducer.Finished) and (GetTickCount64<LDeadline) do Sleep(1);
    AssertTrue('Notification producer did not finish',LProducer.Finished);
    {$ENDIF}
    FreeAndNil(LNotifier);
    CheckSynchronize(0);
    AssertEquals('Another notifier on the same thread must retain its callback',1,FNotifyCount);
  finally
    LProducer.WaitFor;
    LProducer.Free;
    LNotifier.Free;
    LOtherNotifier.Free;
  end;
end;

procedure TThreadNotifyTests.CallbackCanDisposeAnotherPendingNotifier;
var
  LFirst : TThreadNotify;
  LSecond : TReusedThreadNotify;
  LProducer : TNotifyProducerThread;
  LInvokedAfterDisposal : Boolean;
  {$IFNDEF WINDOWS}LDeadline : QWord;{$ENDIF}
begin
  CheckSynchronize(0);
  FNotifyCount := 0;
  LFirst := TThreadNotify.Create(TThread.CurrentThread);
  LSecond := TReusedThreadNotify.Create(TThread.CurrentThread);
  FNotifierToDispose := LSecond;
  LProducer := TNotifyProducerThread.Create(LFirst,DisposeOtherNotifier,LSecond,OnNotified);
  try
    LProducer.Start;
    {$IFDEF WINDOWS}
    AssertEquals('Notification producer did not finish',LongInt(WAIT_OBJECT_0),
      LongInt(WaitForSingleObject(LProducer.Handle,5000)));
    {$ELSE}
    LDeadline := GetTickCount64+5000;
    while (not LProducer.Finished) and (GetTickCount64<LDeadline) do Sleep(1);
    AssertTrue('Notification producer did not finish',LProducer.Finished);
    {$ENDIF}
    LInvokedAfterDisposal := False;
    try
      CheckSynchronize(0);
    except
      on E : EAccessViolation do LInvokedAfterDisposal := True;
    end;
    AssertFalse('An earlier callback disposed a receiver still present in the dispatch batch',
      LInvokedAfterDisposal);
    AssertEquals('Only the first callback may run',1,FNotifyCount);
    AssertTrue('The first callback must have disposed the second receiver',
      FNotifierToDispose=nil);
  finally
    LProducer.WaitFor;
    LProducer.Free;
    FreeAndNil(FNotifierToDispose);
    CheckSynchronize(0);
    LSecond.ReleaseMemory;
    LFirst.Free;
  end;
end;
procedure TThrottledEventTests.SetUp;
begin
  inherited SetUp;
  FCallbacksOnMainThread := True;
  FRearmNextDelivery := False;
  FLastSuppressedCount := 0;
  FNotifyDuringDelivery := False;
  FThrowNextDelivery := False;
  FAsyncExceptionCount := 0;
  FUnexpectedException := '';
  FStartedCount := 0;
  FFinishingCount := 0;
  FFinishedCount := 0;
  FFinishedTick := 0;
  FLifecycleStateValid := True;
  FPostponeFinish := False;
  FNotifyOnStart := False;
  FNotifyOnFinish := False;
  FWaitingStateValid := True;
  FWaitingAction := wtaNone;
  FWaitingActionState := True;
  FThrowStarted := False;
  FThrowFinishing := False;
  FThrowFinished := False;
  FThrowWaitingTrue := False;
  FThrowWaitingFalse := False;
  FNotifyOnTrailing := False;
  FCancelOnDelivery := False;
  FDisposeOnDelivery := False;
  FDisposeOnFinishing := False;
  FSlowLeadingMilliseconds := 0;
  FSlowLeadingEntered := nil;
  FWaitingStates := TList<Boolean>.Create;
  FDeliveryTicks := TList<QWord>.Create;
  FEvent := TThrottledEvent.Create(nil);
  FEvent.Mode := temNotifyOnEventBurstStartAndFinished;
  FEvent.Interval := TTimeSpan.FromMilliseconds(CT_TEST_INTERVAL);
  FEvent.Add(OnNotified);
  FEvent.OnBurstStarted.Add(OnBurstStarted);
  FEvent.OnBurstFinishing.Add(OnBurstFinishing);
  FEvent.OnBurstFinished.Add(OnBurstFinished);
  FEvent.OnWaitingChanged.Add(OnWaitingChanged);
  FTimerWakeTicks := TList<QWord>.Create;
  FTimingTrace := TStringList.Create;
  FNativeTimer := FEvent.Components[0] as TTimer;
  FOriginalTimerHandler := FNativeTimer.OnTimer;
  FNativeTimer.OnTimer := OnNativeTimer;
  FPreviousException := Application.OnException;
  Application.OnException := OnExpectedException;
end;

procedure TThrottledEventTests.TearDown;
begin
  Application.OnException := FPreviousException;
  if Assigned(FEvent) then begin
    FNativeTimer.OnTimer := FOriginalTimerHandler;
    FEvent.Remove(OnNotified);
    FEvent.OnBurstStarted.Remove(OnBurstStarted);
    FEvent.OnBurstFinishing.Remove(OnBurstFinishing);
    FEvent.OnBurstFinished.Remove(OnBurstFinished);
    FEvent.OnWaitingChanged.Remove(OnWaitingChanged);
  end;
  FEvent.Free;
  FDeliveryTicks.Free;
  FTimerWakeTicks.Free;
  FTimingTrace.Free;
  FWaitingStates.Free;
  FSlowLeadingEntered.Free;
  inherited TearDown;
end;

procedure TThrottledEventTests.OnNotified(Sender : TObject);
var LSlowUntil: QWord;
begin
  FCallbacksOnMainThread := FCallbacksOnMainThread and
    (GetCurrentThreadID = MainThreadID);
  FDeliveryTicks.Add(GetTickCount64);
  FTimingTrace.Add(Format('%d invoke suppressed=%d',[GetTickCount64,FEvent.SuppressedInvocations]));
  FLastSuppressedCount := FEvent.SuppressedInvocations;
  if FEvent.Mode in [temNotifyOnEventBurstFinished, temNotifyOnEventBurstStartAndFinished] then
    FWaitingStateValid := FWaitingStateValid and FEvent.Waiting;
  if FSlowLeadingMilliseconds > 0 then begin
    LSlowUntil := GetTickCount64 + FSlowLeadingMilliseconds;
    FSlowLeadingMilliseconds := 0;
    FSlowLeadingEntered.SetEvent;
    // Keep the actual leading listener active while accepting real worker
    // Synchronize inputs on this UI thread. Do not manually fire its timer.
    repeat
      CheckSynchronize(0);
      Sleep(1);
    until GetTickCount64 >= LSlowUntil;
  end;
  if FRearmNextDelivery then begin
    FRearmNextDelivery := False;
    FEvent.Postpone;
  end;
  if FNotifyDuringDelivery then begin
    FNotifyDuringDelivery := False;
    Sleep(40);
    Notify;
  end;
  if FNotifyOnTrailing and (FEvent.SuppressedInvocations > 0) then begin
    FNotifyOnTrailing := False;
    Notify;
  end;
  if FCancelOnDelivery then begin
    FCancelOnDelivery := False;
    FEvent.Cancel;
  end;
  if FDisposeOnDelivery then begin
    DisposeEvent;
    Exit;
  end;
  if FThrowNextDelivery then begin
    FThrowNextDelivery := False;
    raise EThrottleTestCallback.Create('Expected throttle callback failure');
  end;
end;

procedure TThrottledEventTests.ThrowOnce(var ARequested: Boolean);
begin
  if not ARequested then Exit;
  ARequested := False;
  raise EThrottleTestCallback.Create('Expected throttle callback failure');
end;

procedure TThrottledEventTests.DisposeEvent;
var LEvent: TThrottledEvent;
begin
  LEvent := FEvent;
  FEvent := nil;
  FNativeTimer := nil;
  LEvent.Free;
end;

procedure TThrottledEventTests.OnWaitingChanged(Sender: TObject);
var LWaiting: Boolean; LAction: TWaitingTestAction;
begin
  LWaiting := FEvent.Waiting;
  FWaitingStates.Add(LWaiting);
  FWaitingStateValid := FWaitingStateValid and (Sender = FEvent) and
    (FEvent.Waiting = FEvent.BurstActive) and (FNativeTimer.Enabled = LWaiting) and
    (GetCurrentThreadID = MainThreadID);
  if LWaiting = FWaitingActionState then begin
    LAction := FWaitingAction;
    FWaitingAction := wtaNone;
    case LAction of
      wtaNotify: Notify;
      wtaCancel: FEvent.Cancel;
      wtaChangeMode: FEvent.Mode := temNone;
      wtaDispose: begin
        DisposeEvent;
        Exit;
      end;
    end;
  end;
  if LWaiting then ThrowOnce(FThrowWaitingTrue) else ThrowOnce(FThrowWaitingFalse);
end;

procedure TThrottledEventTests.OnExpectedException(Sender: TObject; E: Exception);
begin
  if E is EThrottleTestCallback then Inc(FAsyncExceptionCount)
  else FUnexpectedException := E.ClassName + ': ' + E.Message;
end;

procedure TThrottledEventTests.OnNativeTimer(Sender : TObject);
begin
  FTimerWakeTicks.Add(GetTickCount64);
  FTimingTrace.Add(Format('%d native timer quiet=%d interval=%d',
    [GetTickCount64,GetTickCount64-FLastInputTick,FNativeTimer.Interval]));
  try
    FOriginalTimerHandler(Sender);
  except
    on E: EThrottleTestCallback do Inc(FAsyncExceptionCount);
  end;
end;

procedure TThrottledEventTests.OnBurstStarted(Sender: TObject);
begin
  Inc(FStartedCount);
  FLifecycleStateValid := FLifecycleStateValid and (Sender = FEvent) and FEvent.BurstActive and
    (GetCurrentThreadID = MainThreadID);
  FWaitingStateValid := FWaitingStateValid and (FWaitingStates.Count > 0) and FEvent.Waiting;
  ThrowOnce(FThrowStarted);
  if FNotifyOnStart then begin
    FNotifyOnStart := False;
    Notify;
  end;
end;

procedure TThrottledEventTests.OnBurstFinishing(Sender: TObject);
begin
  Inc(FFinishingCount);
  FLifecycleStateValid := FLifecycleStateValid and (Sender = FEvent) and FEvent.BurstActive and
    (GetCurrentThreadID = MainThreadID);
  if FPostponeFinish then begin
    FPostponeFinish := False;
    FEvent.Postpone;
  end;
  if FDisposeOnFinishing then begin
    DisposeEvent;
    Exit;
  end;
  ThrowOnce(FThrowFinishing);
end;

procedure TThrottledEventTests.OnBurstFinished(Sender: TObject);
begin
  Inc(FFinishedCount);
  FFinishedTick := GetTickCount64;
  FLifecycleStateValid := FLifecycleStateValid and (Sender = FEvent) and not FEvent.BurstActive and
    not FNativeTimer.Enabled and (GetCurrentThreadID = MainThreadID);
  FWaitingStateValid := FWaitingStateValid and (FWaitingStates.Count > 0) and not FEvent.Waiting;
  ThrowOnce(FThrowFinished);
  if FNotifyOnFinish then begin
    FNotifyOnFinish := False;
    Notify;
  end;
end;

procedure TThrottledEventTests.WaitForFinishes(ACount: Integer);
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 1500;
  while (FFinishedCount < ACount) and (GetTickCount64 < LDeadline) do PumpFor(2);
  AssertEquals('Expected owned-timer lifecycle finish did not arrive', ACount, FFinishedCount);
  AssertTrue('Lifecycle events must report the committed state on the UI thread', FLifecycleStateValid);
  AssertTrue('Waiting and its owned timer are committed before outward callbacks', FWaitingStateValid);
end;

procedure TThrottledEventTests.Notify;
begin
  FLastInputTick := GetTickCount64;
  FTimingTrace.Add(Format('%d notify',[FLastInputTick]));
  FEvent.Notify;
end;

procedure TThrottledEventTests.PumpFor(AMilliseconds : Cardinal);
var
  LDeadline : QWord;
begin
  LDeadline := GetTickCount64+AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    if FUnexpectedException <> '' then Fail(FUnexpectedException);
    Sleep(1);
  until GetTickCount64>=LDeadline;
end;

procedure TThrottledEventTests.WaitForDeliveries(ACount : Integer);
var
  LDeadline : QWord;
begin
  // This is a failure bound, not an expected delivery time. Allow scheduler
  // jitter while requiring delivery counts and the quiet-period lower bound.
  LDeadline := GetTickCount64+1500;
  while (FDeliveryTicks.Count<ACount) and (GetTickCount64<LDeadline) do
    PumpFor(2);
  AssertEquals('Expected timer delivery did not arrive',ACount,FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.TrailingListenerExceptionDoesNotLeaveAnUnarmedBurst;
begin
  Notify;
  FThrowNextDelivery := True;
  Notify;
  PumpFor(2 * CT_TEST_INTERVAL + 80);
  AssertEquals('The actual timer propagates the callback failure', 1, FAsyncExceptionCount);
  AssertEquals('A failed trailing listener still closes the quiet lifecycle', 1, FFinishedCount);
  AssertFalse('No active burst remains with its owned timer disabled', FEvent.BurstActive);
  AssertFalse(FNativeTimer.Enabled);
end;

procedure TThrottledEventTests.WaitingIsCommittedBeforeStartAndLeadingDelivery;
begin
  AssertFalse(FEvent.Waiting);
  Notify;
  AssertTrue(FEvent.Waiting and FEvent.BurstActive and FNativeTimer.Enabled);
  AssertEquals('One owned timer remains the only child component', 1, FEvent.ComponentCount);
  AssertEquals(1, FWaitingStates.Count);
  AssertTrue(FWaitingStates[0]);
  AssertTrue(FWaitingStateValid);
  AssertEquals(1, FStartedCount);
  AssertEquals(1, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.WaitingChangesOnceForAQuietSingleInput;
begin
  Notify;
  WaitForFinishes(1);
  AssertEquals(2, FWaitingStates.Count);
  AssertTrue(FWaitingStates[0]);
  AssertFalse(FWaitingStates[1]);
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  AssertEquals('A lifecycle timer adds no duplicate ordinary notification', 1, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.PendingInputsRetainWaitingUntilTheirOwnDeadline;
begin
  Notify;
  PumpFor(CT_TEST_INTERVAL div 2);
  Notify;
  PumpFor(CT_TEST_INTERVAL div 2);
  AssertTrue(FEvent.Waiting);
  AssertEquals(1, FWaitingStates.Count);
  AssertEquals(0, FFinishedCount);
  WaitForFinishes(1);
  AssertEquals(2, FDeliveryTicks.Count);
  AssertEquals(2, FWaitingStates.Count);
end;

procedure TThrottledEventTests.CancelClearsWaitingWithoutDeliveringPendingWork;
begin
  Notify;
  Notify;
  FEvent.Cancel;
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  AssertEquals(2, FWaitingStates.Count);
  AssertFalse(FWaitingStates[1]);
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(1, FDeliveryTicks.Count);
  AssertEquals('Cancellation is not a quiet completion', 0, FFinishedCount);
  AssertTrue(FWaitingStateValid);
end;

procedure TThrottledEventTests.CancelWhileIdleDoesNotPublishAStateChange;
begin
  FEvent.Cancel;
  AssertEquals(0, FWaitingStates.Count);
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
end;

procedure TThrottledEventTests.ModeChangeCancelsThePreviousWaitingRequest;
begin
  Notify;
  Notify;
  FEvent.Mode := temNotifyEveryInterval;
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  AssertEquals(2, FWaitingStates.Count);
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(1, FDeliveryTicks.Count);
  AssertEquals(0, FFinishedCount);
  Notify;
  AssertEquals('The new mode has no stale delivery cooldown', 2, FDeliveryTicks.Count);
  AssertFalse(FEvent.Waiting);
end;

procedure TThrottledEventTests.ChangingBetweenBurstModesCancelsTheOldBurst;
begin
  Notify;
  FEvent.Mode := temNotifyOnEventBurstFinished;
  AssertFalse(FEvent.Waiting);
  Notify;
  AssertTrue(FEvent.Waiting);
  AssertEquals(1, FDeliveryTicks.Count);
  WaitForFinishes(1);
  AssertEquals(4, FWaitingStates.Count);
  AssertEquals(2, FStartedCount);
  AssertEquals(2, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.WaitingStartListenerCanCancelItsRequest;
begin
  FWaitingAction := wtaCancel;
  Notify;
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  AssertEquals(2, FWaitingStates.Count);
  AssertEquals('The canceled start does not continue into data delivery', 0, FDeliveryTicks.Count);
  AssertEquals(0, FStartedCount);
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(0, FFinishedCount);
end;

procedure TThrottledEventTests.WaitingStartListenerCanChangeMode;
begin
  FWaitingAction := wtaChangeMode;
  Notify;
  AssertEquals(Ord(temNone), Ord(FEvent.Mode));
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  AssertEquals(0, FDeliveryTicks.Count);
  Notify;
  AssertEquals(1, FDeliveryTicks.Count);
  AssertEquals(2, FWaitingStates.Count);
end;

procedure TThrottledEventTests.WaitingStartListenerCanNotifyReentrantly;
begin
  FWaitingAction := wtaNotify;
  Notify;
  AssertTrue(FEvent.Waiting);
  AssertEquals(1, FWaitingStates.Count);
  AssertEquals(1, FStartedCount);
  AssertEquals(1, FDeliveryTicks.Count);
  WaitForFinishes(1);
  AssertEquals(2, FDeliveryTicks.Count);
  AssertEquals(2, FWaitingStates.Count);
end;

procedure TThrottledEventTests.WaitingEndListenerCanStartANewRequest;
begin
  FWaitingAction := wtaNotify;
  FWaitingActionState := False;
  Notify;
  WaitForFinishes(1);
  AssertEquals('A newly committed waiting request survives the old state callback', 4, FWaitingStates.Count);
  AssertEquals(2, FStartedCount);
  AssertEquals(2, FDeliveryTicks.Count);
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
end;

procedure TThrottledEventTests.CancelStateListenerCanStartANewRequest;
begin
  Notify;
  FWaitingAction := wtaNotify;
  FWaitingActionState := False;
  FEvent.Cancel;
  AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
  AssertEquals(3, FWaitingStates.Count);
  AssertEquals(2, FStartedCount);
  AssertEquals(2, FDeliveryTicks.Count);
  WaitForFinishes(1);
  AssertEquals(4, FWaitingStates.Count);
end;

procedure TThrottledEventTests.WaitingStartListenerCanDisposeTheComponent;
begin
  FWaitingAction := wtaDispose;
  Notify;
  AssertFalse(Assigned(FEvent));
  AssertEquals(1, FWaitingStates.Count);
  AssertEquals(0, FStartedCount);
  AssertEquals(0, FDeliveryTicks.Count);
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(0, FFinishedCount);
end;

procedure TThrottledEventTests.WaitingEndListenerCanDisposeTheComponent;
begin
  FWaitingAction := wtaDispose;
  FWaitingActionState := False;
  Notify;
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertFalse(Assigned(FEvent));
  AssertEquals(2, FWaitingStates.Count);
  AssertEquals(0, FFinishedCount);
  AssertEquals(1, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.OrdinaryListenerCanDisposeTheComponent;
begin
  FDisposeOnDelivery := True;
  Notify;
  AssertFalse(Assigned(FEvent));
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(1, FDeliveryTicks.Count);
  AssertEquals(0, FFinishedCount);
end;

procedure TThrottledEventTests.FinishingListenerCanDisposeTheComponent;
begin
  FDisposeOnFinishing := True;
  Notify;
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertFalse(Assigned(FEvent));
  AssertEquals(1, FFinishingCount);
  AssertEquals(0, FFinishedCount);
end;

procedure TThrottledEventTests.LeadingListenerFailureRetainsTheOwnedTimer;
begin
  FThrowNextDelivery := True;
  try
    Notify;
    Fail('A direct ordinary callback exception must propagate');
  except on E: EThrottleTestCallback do ; end;
  AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
  WaitForFinishes(1);
  AssertEquals(1, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.StartedListenerFailureRetainsTheOwnedTimer;
begin
  FThrowStarted := True;
  try
    Notify;
    Fail('A start callback exception must propagate');
  except on E: EThrottleTestCallback do ; end;
  AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
  WaitForFinishes(1);
  AssertEquals(0, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.FinishingListenerFailureCommitsIdleState;
begin
  FThrowFinishing := True;
  Notify;
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(1, FAsyncExceptionCount);
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  AssertEquals(1, FFinishedCount);
  AssertTrue(FWaitingStateValid);
end;

procedure TThrottledEventTests.FinishedListenerFailureLeavesIdleState;
begin
  FThrowFinished := True;
  Notify;
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(1, FAsyncExceptionCount);
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  AssertEquals(1, FFinishedCount);
  Notify;
  AssertEquals(2, FDeliveryTicks.Count);
  WaitForFinishes(2);
end;

procedure TThrottledEventTests.WaitingStartFailureRetainsTheOwnedTimer;
begin
  FThrowWaitingTrue := True;
  try
    Notify;
    Fail('A waiting-start callback exception must propagate');
  except on E: EThrottleTestCallback do ; end;
  AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
  WaitForFinishes(1);
  AssertEquals(0, FStartedCount);
  AssertEquals(0, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.WaitingEndFailureLeavesIdleState;
begin
  FThrowWaitingFalse := True;
  Notify;
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(1, FAsyncExceptionCount);
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  Notify;
  AssertEquals(2, FDeliveryTicks.Count);
  WaitForFinishes(1);
end;

procedure TThrottledEventTests.CancelStateFailureStillLeavesIdleState;
begin
  Notify;
  FThrowWaitingFalse := True;
  try
    FEvent.Cancel;
    Fail('A cancellation state callback exception must propagate');
  except on E: EThrottleTestCallback do ; end;
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  PumpFor(2 * CT_TEST_INTERVAL + 30);
  AssertEquals(0, FFinishedCount);
  AssertEquals(1, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.PostponedFinishingFailureRetainsTheRearmedRequest;
var LDeadline: QWord;
begin
  FPostponeFinish := True;
  FThrowFinishing := True;
  Notify;
  LDeadline := GetTickCount64 + 1000;
  while (FFinishingCount = 0) and (GetTickCount64 < LDeadline) do PumpFor(2);
  AssertEquals(1, FAsyncExceptionCount);
  AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
  AssertEquals(0, FFinishedCount);
  WaitForFinishes(1);
  AssertEquals(2, FFinishingCount);
  AssertEquals(1, FStartedCount);
end;

procedure TThrottledEventTests.ReentrantTrailingRequestDeliversImmediatelyOnce;
begin
  Notify;
  FNotifyOnTrailing := True;
  Notify;
  WaitForDeliveries(3);
  AssertEquals('A new request during the trailing callback is immediate', 0, FLastSuppressedCount);
  AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
  WaitForFinishes(1);
  AssertEquals('The reentrant request needs no duplicate trailing delivery', 3, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.FirstNotificationIsImmediate;
var
  LBefore, LAfter : QWord;
begin
  LBefore := GetTickCount64;
  Notify;
  LAfter := GetTickCount64;
  // No message-loop pumping occurred: the leading callback is synchronous.
  AssertEquals('The first event must deliver before Notify returns',1,FDeliveryTicks.Count);
  AssertTrue(FDeliveryTicks[0]>=LBefore);
  AssertTrue(FDeliveryTicks[0]<=LAfter);
  AssertTrue('Callbacks must run on the LCL thread',FCallbacksOnMainThread);
end;

procedure TThrottledEventTests.IntervalRetainsItsNumericMilliseconds;
begin
  AssertEquals('Timer intervals require numeric duration conversion',
    120.0,FEvent.Interval.TotalMilliseconds,0.0);
end;

procedure TThrottledEventTests.FirstNotificationAfterTrailingStartsImmediately;
begin
  Notify;
  PumpFor(25);
  Notify;
  WaitForDeliveries(2);
  // The previous input has been quiet for a full interval, even though its
  // trailing callback has just run. This input begins a new burst.
  Notify;
  AssertEquals('A new burst must lead synchronously after the trailing callback',
    3,FDeliveryTicks.Count);
  AssertTrue('Callbacks must run on the LCL thread',FCallbacksOnMainThread);
end;

procedure TThrottledEventTests.ContinuousBurstHasOnlyLeadingAndFinalDeliveries;
var
  i : Integer;
begin
  Notify;
  AssertEquals(1,FDeliveryTicks.Count);
  // Keep notifying for longer than two intervals. A periodic implementation
  // would deliver in the middle of this burst instead of waiting for silence.
  for i := 1 to 12 do begin
    PumpFor(25);
    Notify;
    AssertEquals('The burst must not produce middle deliveries',1,FDeliveryTicks.Count);
  end;
  PumpFor(50);
  AssertEquals('A short pause is not the end of the burst',1,FDeliveryTicks.Count);
  WaitForDeliveries(2);
  AssertTrue('The final callback must follow a full quiet period',
    FDeliveryTicks[1]-FLastInputTick>=CT_TEST_INTERVAL-20);
  PumpFor(2*CT_TEST_INTERVAL+30);
  AssertEquals('Exactly one trailing callback is allowed',2,FDeliveryTicks.Count);
  AssertTrue('Callbacks must run on the LCL thread',FCallbacksOnMainThread);
end;

procedure TThrottledEventTests.LaterNotificationsExtendTheQuietDeadline;
const
  CT_INTERVAL = 150;
begin
  FEvent.Interval := TTimeSpan.FromMilliseconds(CT_INTERVAL);
  Notify;
  PumpFor(25);
  Notify; // Starts a delayed delivery with its original deadline near 175ms.
  PumpFor(90);
  Notify; // The final delivery must now follow this later input instead.
  PumpFor(95);
  AssertEquals('A later input must extend the original deadline',1,FDeliveryTicks.Count);
  WaitForDeliveries(2);
  AssertTrue('Quiet time is measured from the latest notification',
    FDeliveryTicks[1]-FLastInputTick>=CT_INTERVAL-20);
  AssertTrue('Callbacks must run on the LCL thread',FCallbacksOnMainThread);
end;

procedure TThrottledEventTests.SingleNotificationDoesNotDeliverTwice;
begin
  Notify;
  AssertEquals(1,FDeliveryTicks.Count);
  PumpFor(3*CT_TEST_INTERVAL+40);
  AssertEquals('An isolated leading event needs no duplicate trailing callback',
    1,FDeliveryTicks.Count);
  AssertTrue('Callbacks must run on the LCL thread',FCallbacksOnMainThread);
end;

procedure TThrottledEventTests.PostponeDoesNotStartAnotherLeadingDelivery;
begin
  Notify;
  FEvent.Postpone;
  AssertEquals('Postponing a requested delivery must not lead immediately',1,FDeliveryTicks.Count);
  PumpFor(40);
  AssertEquals('Postpone must retain a full quiet interval',1,FDeliveryTicks.Count);
  WaitForDeliveries(2);
  PumpFor(CT_TEST_INTERVAL+30);
  AssertEquals('Postpone delivers only one final notification',2,FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.PostponeRetainsElapsedProducerQuietTime;
var LElapsed: Double;
begin
  Notify;
  FEvent.Postpone(CT_TEST_INTERVAL-40);
  LElapsed := TTimeSpan.Subtract(Now,FEvent.LastClientNotify).TotalMilliseconds;
  AssertTrue('The deadline must include time the producer was already quiet',
    LElapsed>=CT_TEST_INTERVAL-60);
  AssertEquals('Remaining quiet time must not produce an immediate leading callback',1,FDeliveryTicks.Count);
  WaitForDeliveries(2);
end;

procedure TThrottledEventTests.ListenerCanPostponeATrailingDeliveryAgain;
begin
  Notify;
  PumpFor(20);
  Notify;
  FRearmNextDelivery := True;
  WaitForDeliveries(2);
  AssertTrue('A trailing callback exposes its suppressed count',FLastSuppressedCount>0);
  AssertTrue('A rearmed timer must retain delayed delivery state',FEvent.SuppressedInvocations>0);
  AssertEquals('An ordinary listener rearm must defer lifecycle completion',0,FFinishedCount);
  WaitForDeliveries(3);
  AssertTrue('The retry must still be recognizable as delayed',FLastSuppressedCount>0);
  AssertEquals('A completed retry clears its pending count',0,FEvent.SuppressedInvocations);
  AssertEquals(1,FFinishedCount);
end;

procedure TThrottledEventTests.CheckPhysicalTimerReset( AMode : TThrottledEventMode);
var LExpected : Integer;
begin
  FEvent.Mode := AMode;
  FEvent.Interval := TTimeSpan.FromMilliseconds(200);
  Notify;
  PumpFor(30);
  Notify;
  PumpFor(130);
  Notify;
  PumpFor(100);
  AssertEquals('Every new input must rearm the physical quiet timer: '+FTimingTrace.Text,
    0,FTimerWakeTicks.Count);
  if AMode=temNotifyOnEventBurstStartAndFinished then LExpected := 2
  else LExpected := 1;
  WaitForDeliveries(LExpected);
  AssertTrue('The native timer starts from the final input: '+FTimingTrace.Text,
    FTimerWakeTicks[0]-FLastInputTick>=180);
end;

procedure TThrottledEventTests.LeadingBurstInputRestartsItsNativeTimer;
begin
  CheckPhysicalTimerReset(temNotifyOnEventBurstStartAndFinished);
end;

procedure TThrottledEventTests.TrailingBurstInputRestartsItsNativeTimer;
begin
  CheckPhysicalTimerReset(temNotifyOnEventBurstFinished);
end;

procedure TThrottledEventTests.PeriodicInputsRetainTheArmedTimerDeadline;
begin
  FEvent.Mode := temNotifyEveryInterval;
  FEvent.Interval := TTimeSpan.FromMilliseconds(500);
  Notify;
  PumpFor(20);
  Notify;
  PumpFor(430);
  Notify;
  PumpFor(200);
  AssertTrue('Periodic mode keeps its previously armed timer: '+FTimingTrace.Text,
    FTimerWakeTicks.Count>0);
  AssertEquals(2,FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.ReentrantBusyListenerRetainsTheResetDeadline;
begin
  FEvent.Interval := TTimeSpan.FromMilliseconds(200);
  FNotifyDuringDelivery := True;
  Notify;
  AssertEquals('Reentrant input must be coalesced while the leading listener runs',1,FDeliveryTicks.Count);
  PumpFor(120);
  Notify;
  PumpFor(100);
  AssertEquals('A later input resets a timer armed by the busy listener: '+FTimingTrace.Text,
    0,FTimerWakeTicks.Count);
  WaitForDeliveries(2);
end;

procedure TThrottledEventTests.SingleInputFinishesItsLifecycleWithoutAnotherDelivery;
begin
  Notify;
  AssertEquals('Start is synchronous',1,FStartedCount);
  AssertEquals(1,FDeliveryTicks.Count);
  AssertTrue(FEvent.BurstActive and FNativeTimer.Enabled);
  AssertEquals(0,FFinishedCount);
  WaitForFinishes(1);
  AssertTrue('Finish is driven by the actual owned timer',FTimerWakeTicks.Count > 0);
  AssertTrue(FFinishedTick-FLastInputTick >= CT_TEST_INTERVAL-20);
  AssertEquals('A single event has no duplicate ordinary callback',1,FDeliveryTicks.Count);
  AssertEquals(1,FFinishingCount);
  AssertFalse(FEvent.BurstActive);
  PumpFor(2*CT_TEST_INTERVAL);
  AssertEquals(1,FFinishedCount);
end;

procedure TThrottledEventTests.ContinuousInputExtendsTheLifecycleDeadline;
var I: Integer;
begin
  Notify;
  for I := 1 to 12 do begin
    PumpFor(25);
    Notify;
    AssertEquals('Continuous input must keep the lifecycle active',0,FFinishedCount);
  end;
  PumpFor(50);
  AssertEquals(0,FFinishedCount);
  WaitForFinishes(1);
  AssertEquals(1,FStartedCount);
  AssertEquals(2,FDeliveryTicks.Count);
  AssertTrue(FFinishedTick-FLastInputTick >= CT_TEST_INTERVAL-20);
end;

procedure TThrottledEventTests.FinishCandidateCanPostponeWithoutPublishingFinished;
var LDeadline: QWord;
begin
  FPostponeFinish := True;
  Notify;
  LDeadline := GetTickCount64+1500;
  while (FFinishingCount = 0) and (GetTickCount64 < LDeadline) do PumpFor(2);
  AssertEquals(1,FFinishingCount);
  AssertEquals('A rearmed candidate is not a finished burst',0,FFinishedCount);
  AssertTrue(FEvent.BurstActive and FNativeTimer.Enabled);
  PumpFor(40);
  AssertEquals(0,FFinishedCount);
  WaitForFinishes(1);
  AssertEquals(2,FFinishingCount);
  AssertEquals('Postpone does not publish another start',1,FStartedCount);
end;

procedure TThrottledEventTests.StartedListenerCanNotifyWithoutDuplicatingTheStart;
begin
  FNotifyOnStart := True;
  Notify;
  AssertEquals(1,FStartedCount);
  AssertEquals(1,FDeliveryTicks.Count);
  WaitForFinishes(1);
  AssertEquals(2,FDeliveryTicks.Count);
  AssertEquals(1,FStartedCount);
end;

procedure TThrottledEventTests.FinishedListenerCanStartTheNextBurst;
begin
  FNotifyOnFinish := True;
  Notify;
  WaitForFinishes(1);
  AssertEquals(2,FStartedCount);
  AssertEquals(2,FDeliveryTicks.Count);
  AssertTrue('The finished listener starts a separately timed burst',FEvent.BurstActive and FNativeTimer.Enabled);
  WaitForFinishes(2);
  AssertFalse(FEvent.BurstActive);
  AssertEquals('Both isolated bursts deliver only their leading callback',2,FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.DisposingActiveBurstCancelsItsLifecycleTimer;
begin
  Notify;
  FreeAndNil(FEvent);
  FNativeTimer := nil;
  PumpFor(2*CT_TEST_INTERVAL+30);
  AssertEquals(1,FStartedCount);
  AssertEquals('Disposal must not deliver a stale lifecycle callback',0,FFinishedCount);
  AssertEquals(1,FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.NonBurstModesDoNotPublishBurstLifecycle;
begin
  FEvent.Mode := temNone;
  Notify;
  Notify;
  AssertEquals(2,FDeliveryTicks.Count);
  FEvent.Mode := temNotifyEveryInterval;
  Notify;
  WaitForDeliveries(3);
  AssertEquals(0,FStartedCount);
  AssertEquals(0,FFinishingCount);
  AssertEquals(0,FFinishedCount);
  AssertFalse(FEvent.BurstActive);
end;

procedure TThrottledEventTests.TrailingOnlyModePublishesOneStartAndFinish;
begin
  FEvent.Mode := temNotifyOnEventBurstFinished;
  Notify;
  AssertEquals(1,FStartedCount);
  AssertEquals(0,FDeliveryTicks.Count);
  PumpFor(25);
  Notify;
  WaitForFinishes(1);
  AssertEquals(1,FDeliveryTicks.Count);
  AssertEquals(1,FStartedCount);
end;

procedure TThrottledEventTests.FiveSecondInputsAt4900MsRetainOneOwnedBurst;
var I: Integer;
begin
  FEvent.Interval := TTimeSpan.FromSeconds(5);
  Notify;
  for I := 1 to 2 do begin
    PumpFor(4900);
    AssertEquals('The next near-deadline input still belongs to this burst: ' +
      FTimingTrace.Text, 1, FDeliveryTicks.Count);
    AssertTrue(FEvent.Waiting);
    AssertEquals('The physical timer must not wake before the latest quiet deadline: ' +
      FTimingTrace.Text, 0, FTimerWakeTicks.Count);
    Notify;
    AssertEquals(1, FStartedCount);
    AssertEquals(0, FFinishedCount);
    AssertEquals('Waiting must not flicker between these inputs', 1, FWaitingStates.Count);
  end;
  PumpFor(4900);
  AssertEquals(1, FDeliveryTicks.Count);
  AssertTrue(FEvent.Waiting);
  WaitForDeliveries(2);
  WaitForFinishes(1);
  AssertTrue('The final delivery follows a real five-second quiet interval: ' +
    FTimingTrace.Text, FDeliveryTicks[1] - FLastInputTick >= 4980);
  AssertEquals(2, FWaitingStates.Count);
  AssertFalse(FEvent.Waiting);
  PumpFor(150);
  AssertEquals('There is exactly one leading and one final delivery', 2, FDeliveryTicks.Count);
end;

procedure TThrottledEventTests.CapturedLateTimerAfterFreshInputCannotFinishWaiting;
var LCapturedHandler: TNotifyEvent;
begin
  FEvent.Interval := TTimeSpan.FromSeconds(5);
  LCapturedHandler := FOriginalTimerHandler;
  Notify;
  PumpFor(4900);
  Notify;
  // Model an already posted WM_TIMER from the old deadline. Invoke the actual
  // saved component handler after that deadline, not a copy of its condition.
  PumpFor(200);
  AssertEquals('Resetting the owned native timer prevents an ordinary old wake: ' +
    FTimingTrace.Text, 0, FTimerWakeTicks.Count);
  FTimingTrace.Add('Deliver captured real timer handler after a fresh input');
  LCapturedHandler(FNativeTimer);
  AssertEquals('A stale timer cannot publish pending work early', 1, FDeliveryTicks.Count);
  AssertEquals(0, FFinishedCount);
  AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
  AssertEquals('The stale callback cannot publish Waiting=False', 1, FWaitingStates.Count);

  FEvent.Cancel;
  LCapturedHandler(FNativeTimer);
  AssertEquals('An already queued callback after Cancel cannot deliver', 1, FDeliveryTicks.Count);
  AssertEquals('Cancellation is not a quiet finish', 0, FFinishedCount);
  AssertFalse(FEvent.Waiting or FNativeTimer.Enabled);
  Notify;
  AssertEquals('The next independent input still leads immediately', 2, FDeliveryTicks.Count);
  FEvent.Cancel;
end;

procedure TThrottledEventTests.SlowLeadingListenerAcceptsSynchronizedUiInputsWithoutEarlyFinish;
var LProducer: TSynchronizedThrottleInputs;
begin
  FEvent.Interval := TTimeSpan.FromSeconds(5);
  FSlowLeadingEntered := TEvent.Create(nil, True, False, '');
  LProducer := TSynchronizedThrottleInputs.Create(FSlowLeadingEntered, Notify);
  try
    LProducer.Start;
    FSlowLeadingMilliseconds := 5500;
    Notify;
    LProducer.Stop;
    AssertFalse('The real worker must reach the leading-listener gate', LProducer.StartTimedOut);
    AssertTrue('A slow leading callback accepted repeated real worker inputs', LProducer.InputCount >= 20);
    AssertFalse('Every Synchronize input executes on the UI thread', LProducer.WrongThread);
    AssertEquals('Inputs during the slow listener stay coalesced', 1, FDeliveryTicks.Count);
    AssertEquals('No intermediate Waiting=False may be published', 1, FWaitingStates.Count);
    AssertTrue(FEvent.Waiting and FNativeTimer.Enabled);
    PumpFor(4800);
    AssertEquals('A stale first deadline cannot end the refreshed burst: ' +
      FTimingTrace.Text, 1, FDeliveryTicks.Count);
    AssertTrue(FEvent.Waiting);
    WaitForDeliveries(2);
    WaitForFinishes(1);
    AssertTrue('Quiet is measured from the last actual synchronized input: ' +
      FTimingTrace.Text, FDeliveryTicks[1] - FLastInputTick >= 4980);
    AssertEquals(2, FWaitingStates.Count);
    AssertTrue(FCallbacksOnMainThread);
    AssertFalse(FEvent.Waiting);
  finally
    LProducer.Free;
  end;
end;

initialization
  RegisterTest(TThrottledEventTests);
  RegisterTest(TThreadNotifyTests);

end.
