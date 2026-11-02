unit UCommon.UI;

{ Copyright (c) 2017 - 2018 Herman Schoenfeld <https://www.sphere10.com>

  Common UI tools.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{ Public types in this unit

  Type                                | Purpose
  ------------------------------------+-------------------------------------------------------------------------
  TApplicationForm                    | Adds first-activation events and owned-object cleanup to forms.
  TFormHelper                         | Provides the form-helper extension point.
  TImageHelper                        | Copies an image-list entry into an image control.
  TLoadingCircle                      | Draws and animates a component-owned loading indicator.
  TThrottledEvent                     | Coalesces notifications using interval or quiet-burst timers.
  TThrottledEventMode                 | Selects immediate, periodic or quiet-burst event delivery.
  TWinControlHelper                   | Removes child controls and docks a control at the center.
}


{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math, Generics.Collections, Generics.Defaults, SyncObjs,
  {$IFDEF FPC}
  Forms, Controls, ExtCtrls, FGL, Graphics, LMessages,
  {$ELSE}
  Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.ImgList, Winapi.Windows,
  {$ENDIF}
  UMemory, UCommon;

type

  { TApplicationForm }

  TApplicationForm = class(TForm)
    private
      FActivatedCount : UInt32;
      FActivateFirstTime : TNotifyEvent;
      FDestroyed : TNotifyEvent;
      FCloseAction : TCloseAction;
      FDisposables : TDisposables;
      procedure NotifyActivateFirstTime;
      procedure NotifyDestroyed;
    protected
      FUILock : TCriticalSection;
      procedure DoCreate; override;
      procedure Activate; override;
      procedure ActivateFirstTime; virtual;
      procedure DoClose(var CloseAction: TCloseAction); override;
      procedure DoDestroy; override;
      procedure DoDestroyed; virtual;
    public
      property ActivationCount : UInt32 read FActivatedCount;
      function AutoDispose(AObject : TObject) : TObject;
      {$IFNDEF FPC}function IsVisible:Boolean;{$ENDIF}
    published
      property CloseAction : TCloseAction read FCloseAction write FCloseAction;
      property OnActivateFirstTime : TNotifyEvent read FActivateFirstTime write FActivateFirstTime;
      property OnDestroyed : TNotifyEvent read FDestroyed write FDestroyed;
  end;

  // Shared delivery policy for throttled UI notifications.
  TThrottledEventMode = (temNone, temNotifyEveryInterval, temNotifyOnEventBurstFinished, temNotifyOnEventBurstStartAndFinished);

  { TThrottledEvent }

  TThrottledEvent = class(TComponent)
    public const
      CT_DEFAULT_DELAYEDREFRESH_MS = 1000;
    private type
      // A callback may dispose its sender. Reuse component free notification
      // to keep the current invocation from touching that released component.
      TInvocation = class(TComponent)
      private
        FSubject: TComponent;
        function GetAlive: Boolean;
      protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public
        constructor Create(ASubject: TComponent); override;
        destructor Destroy; override;
        property Alive: Boolean read GetAlive;
      end;
    private
      FHandler : TNotifyManyEvent;
      FBurstStarted, FBurstFinishing, FBurstFinished : TNotifyManyEvent;
      FWaitingChanged: TNotifyManyEvent;
      FBurstActive : Boolean;
      // Stop obsolete callback snapshots after cancellation or a replacement burst.
      FStateVersion: UInt64;
      FTimer: TTimer;
      FMode : TThrottledEventMode;
      FInterval : TTimeSpan;
      FLastClientNotify : TDateTime;
      FLastActualNotify : TDateTime;
      FSuppressedInvocations : Integer;
      procedure SetInterval(const ATimeSpan : TTimeSpan);
      procedure SetMode(AMode: TThrottledEventMode);
      procedure OnTimer(Sender: TObject);
      procedure NotifyNow(AInvocation: TInvocation; ALeading: Boolean = False);
      procedure NotifyLater(AInvocation: TInvocation);
      function BeginBurst(AInvocation: TInvocation): Boolean;
      procedure FinishBurst(AInvocation: TInvocation);
      procedure ArmBurstTimer;
      procedure Cancel(AInvocation: TInvocation); overload;
      procedure InvokeCallbacks(const AEvent: TNotifyManyEvent; ASender: TObject;
        AInvocation: TInvocation; AVersion: UInt64);
      procedure EndWaiting(AInvocation: TInvocation; AFinished: Boolean);
    public
      property Interval : TTimeSpan read FInterval write SetInterval;
      property Mode : TThrottledEventMode read FMode write SetMode;
      property LastClientNotify : TDateTime read FLastClientNotify;
      property LastActualNotify : TDateTime read FLastActualNotify;
      property SuppressedInvocations : Integer read FSuppressedInvocations;
      // Quiet-burst lifecycle, driven by this component's owned timer. A single
      // leading input finishes its lifecycle without another Add notification.
      property BurstActive : Boolean read FBurstActive;
      // Canonical quiet-burst state, independent of consumers or their work.
      // The state and owned timer are committed before change callbacks run.
      property Waiting: Boolean read FBurstActive;
      property OnWaitingChanged: TNotifyManyEvent read FWaitingChanged;
      property OnBurstStarted : TNotifyManyEvent read FBurstStarted;
      // Optional synchronous listeners can explicitly rearm the current burst.
      // OnBurstFinished is raised only when this attempt was not rearmed.
      property OnBurstFinishing : TNotifyManyEvent read FBurstFinishing;
      property OnBurstFinished : TNotifyManyEvent read FBurstFinished;
      constructor Create(Owner:TComponent); override;
      procedure Add(AListener : TNotifyEvent; ExecuteMainThread : Boolean = False);
      procedure Remove(AListener : TNotifyEvent);
      procedure Notify;
      // Rearm a requested delivery without starting a new leading burst. The
      // optional elapsed quiet time is supplied explicitly by that caller.
      procedure Postpone(AQuietMilliseconds: Cardinal = 0);
      // Cancel pending delivery and waiting without publishing a quiet finish.
      // Changing Mode also cancels its previous pending burst/interval.
      procedure Cancel; overload;
  end;

  { TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
    procedure RemoveAllControls(destroy : boolean);
    procedure AddControlDockCenter(AControl: TWinControl);
  end;

  { TFormHelper }

  TFormHelper = class helper for TForm
  end;

  { TImageHelper }

  TImageHelper = class helper for TImage
    procedure SetImageListPicture(AImageList: TImageList; AIndex : {$IFDEF FPC}SizeInt{$ELSE}NativeInt{$ENDIF});
  end;

{ Native LCL/VCL port copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Rounded-spoke loading indicator adapted from Sphere10 Framework's
  Windows.Forms.LoadingCircle, using its MacOSX appearance and fading palette.

  Original LoadingCircle copyright © 2006, 2007 Martin R. Gagné
  (martingagne@gmail.com). All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
}

type
  { TLoadingCircle

    Component-owned animation: no worker or queued callback outlives the
    control. Active must be changed on the UI thread. Colors are composited
    against Color to reproduce the original alpha-faded spokes on an opaque
    native control. }
  TLoadingCircle = class(TCustomControl)
  private
    FTimer : TTimer;
    FActive : Boolean;
    FPosition : Integer;
    FSpokeColor : TColor;
    procedure SetActive(AValue : Boolean);
    procedure SetSpokeColor(AValue : TColor);
    procedure OnTimer(Sender : TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Active : Boolean read FActive write SetActive default False;
    property SpokeColor : TColor read FSpokeColor write SetSpokeColor default $00A9A9A9;
    property Align;
    property Anchors;
    property Color;
    property ParentColor;
    property Visible;
  end;

implementation

{$IFDEF FPC}
uses LCLIntf;
{$ENDIF}

{%region TApplicationForm}

procedure TApplicationForm.DoCreate;
begin
  {$IFDEF FPC}inherited;{$ENDIF}
  FUILock := TCriticalSection.Create;
  FActivatedCount := 0;
  FCloseAction:=caHide;
  {$IFNDEF FPC}inherited;{$ENDIF}
end;

procedure TApplicationForm.Activate;
begin
  inherited;
  inc(FActivatedCount);
  if (FActivatedCount = 1) then
    NotifyActivateFirstTime;
end;

procedure TApplicationForm.ActivateFirstTime;
begin;
end;

procedure TApplicationForm.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := FCloseAction;
end;

procedure TApplicationForm.DoDestroy;
begin
  inherited;
  FActivatedCount:=0;
  FUILock.Destroy;
  FUILock := nil;
  NotifyDestroyed;
end;

procedure TApplicationForm.DoDestroyed;
begin;
end;

function TApplicationform.AutoDispose(AObject : TObject) : TObject;
begin
  Result := FDisposables.AddObject(AObject);
end;

{$IFNDEF FPC}
function TApplicationForm.IsVisible:Boolean;
var LParent:TControl;
begin
  Result:=Visible;
  LParent:=Parent;
  while Result and Assigned(LParent) do begin
    Result:=LParent.Visible;
    LParent:=LParent.Parent;
  end;
end;
{$ENDIF}

procedure TApplicationForm.NotifyActivateFirstTime;
begin
  ActivateFirstTime;
  if Assigned(FActivateFirstTime) then
    FActivateFirstTime(Self);
end;

procedure TApplicationForm.NotifyDestroyed;
begin
  DoDestroyed;
  if Assigned(FDestroyed) then
    FDestroyed(Self);
end;

{%endregion}

{%region TThrottledEvent }

constructor TThrottledEvent.TInvocation.Create(ASubject: TComponent);
begin
  inherited Create(nil);
  FSubject := ASubject;
  FSubject.FreeNotification(Self);
end;

destructor TThrottledEvent.TInvocation.Destroy;
begin
  if Assigned(FSubject) then FSubject.RemoveFreeNotification(Self);
  inherited;
end;

function TThrottledEvent.TInvocation.GetAlive: Boolean;
begin
  Result := Assigned(FSubject);
end;

procedure TThrottledEvent.TInvocation.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSubject) then FSubject := nil;
end;

constructor TThrottledEvent.Create(Owner:TComponent);
begin
  Inherited Create(Owner);
  FTimer := TTimer.Create(Self);
  FInterval := TTimeSpan.FromMilliseconds( CT_DEFAULT_DELAYEDREFRESH_MS );
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := false;
  FSuppressedInvocations:=0;
  FLastClientNotify := MinDateTime;
  FLastActualNotify := MinDateTime;
  FMode:=temNone;
end;

procedure TThrottledEvent.Add(AListener : TNotifyEvent; ExecuteMainThread : Boolean = False);
begin
  FHandler.Add(AListener, ExecuteMainThread);
end;

procedure TThrottledEvent.Remove(AListener : TNotifyEvent);
begin
  FHandler.Remove(AListener);
end;

procedure TThrottledEvent.Notify;
var LIdleDuration : TTimeSpan;
  LImmediate: Boolean;
  LDisposed: TDisposables;
  LInvocation: TInvocation;
begin
  LInvocation := LDisposed.AddObject(TInvocation.Create(Self)) as TInvocation;
  // A completed trailing delivery does not extend the previous input burst.
  // Measure its quiet time before recording the next client notification.
  if FMode = temNotifyOnEventBurstStartAndFinished then
    LIdleDuration := TTimeSpan.Subtract(Now, FLastClientNotify)
  else
    LIdleDuration := TTimeSpan.Subtract(Now, FLastActualNotify);
  FLastClientNotify:=Now;
  // A single leading input also has a lifecycle timer now, but that timer is
  // not a pending ordinary delivery. Preserve immediate independent inputs.
  LImmediate := (FMode = temNone) or
    (((not FTimer.Enabled) or ((FMode = temNotifyOnEventBurstStartAndFinished) and
      (FSuppressedInvocations = 0))) and (LIdleDuration > Interval) and
      (FMode <> temNotifyOnEventBurstFinished));
  try
    if LImmediate then NotifyNow(LInvocation, True) else NotifyLater(LInvocation);
  finally
    if LInvocation.Alive then
      if FBurstActive and not FTimer.Enabled then ArmBurstTimer;
  end;
end;

procedure TThrottledEvent.InvokeCallbacks(const AEvent: TNotifyManyEvent; ASender: TObject;
  AInvocation: TInvocation; AVersion: UInt64);
var LHandlers, LMainHandlers: TArray<TNotifyEvent>; LHandler: TNotifyEvent;
begin
  // Snapshot the existing event record, not a new event mechanism. Subscription
  // changes cannot invalidate enumeration, and disposal ends the invocation.
  LHandlers := TArrayTool<TNotifyEvent>.Copy(AEvent.Handlers);
  LMainHandlers := TArrayTool<TNotifyEvent>.Copy(AEvent.MainThreadHandlers);
  for LHandler in LHandlers do begin
    if not AInvocation.Alive then Exit;
    if FStateVersion <> AVersion then Exit;
    if TArrayTool<TNotifyEvent>.Contains(AEvent.Handlers, LHandler) then LHandler(ASender);
  end;
  for LHandler in LMainHandlers do begin
    if not AInvocation.Alive then Exit;
    if FStateVersion <> AVersion then Exit;
    if TArrayTool<TNotifyEvent>.Contains(AEvent.MainThreadHandlers, LHandler) then
      TThreadNotify.InvokeMainThread(ASender, LHandler);
  end;
end;

function TThrottledEvent.BeginBurst(AInvocation: TInvocation): Boolean;
var LVersion: UInt64;
begin
  Result := True;
  if FBurstActive or not (FMode in [temNotifyOnEventBurstFinished,
    temNotifyOnEventBurstStartAndFinished]) then Exit;
  FBurstActive := True;
  Inc(FStateVersion);
  LVersion := FStateVersion;
  // Waiting always owns an armed timer before its outward start callbacks.
  ArmBurstTimer;
  InvokeCallbacks(FWaitingChanged, Self, AInvocation, LVersion);
  if not AInvocation.Alive then Exit(False);
  if FStateVersion <> LVersion then Exit(False);
  InvokeCallbacks(FBurstStarted, Self, AInvocation, LVersion);
  if not AInvocation.Alive then Exit(False);
  Result := FBurstActive and (FStateVersion = LVersion);
end;

procedure TThrottledEvent.ArmBurstTimer;
var LQuiet: Double;
begin
  LQuiet := TTimeSpan.Subtract(Now,FLastClientNotify).TotalMilliseconds;
  FTimer.Enabled := False;
  FTimer.Interval := ClipValue(Round(Abs(FInterval.TotalMilliseconds)-LQuiet),10,High(Integer));
  FTimer.Enabled := True;
end;

procedure TThrottledEvent.EndWaiting(AInvocation: TInvocation; AFinished: Boolean);
var LVersion: UInt64;
begin
  if not FBurstActive then Exit;
  FBurstActive := False;
  Inc(FStateVersion);
  LVersion := FStateVersion;
  FTimer.Enabled := False;
  InvokeCallbacks(FWaitingChanged, Self, AInvocation, LVersion);
  if not AInvocation.Alive then Exit;
  if FStateVersion <> LVersion then Exit;
  if AFinished then InvokeCallbacks(FBurstFinished, Self, AInvocation, LVersion);
end;

procedure TThrottledEvent.FinishBurst(AInvocation: TInvocation);
var LException: Pointer; LVersion: UInt64;
begin
  if not FBurstActive then Exit;
  LException := nil;
  LVersion := FStateVersion;
  try
    InvokeCallbacks(FBurstFinishing, Self, AInvocation, LVersion);
  except
    LException := AcquireExceptionObject;
  end;
  if AInvocation.Alive then
    if (FStateVersion = LVersion) and not FTimer.Enabled then begin
      try
        EndWaiting(AInvocation, True);
      except
        if not Assigned(LException) then LException := AcquireExceptionObject;
      end;
    end;
  if Assigned(LException) then raise TObject(LException);
end;

procedure TThrottledEvent.Postpone(AQuietMilliseconds: Cardinal);
var LDisposed: TDisposables; LInvocation: TInvocation;
begin
  LInvocation := LDisposed.AddObject(TInvocation.Create(Self)) as TInvocation;
  if FSuppressedInvocations=0 then FSuppressedInvocations := 1;
  FLastClientNotify := Now-TTimeSpan.FromMilliseconds(AQuietMilliseconds).TotalDays;
  try
    if BeginBurst(LInvocation) then ArmBurstTimer;
  finally
    if LInvocation.Alive then
      if FBurstActive and not FTimer.Enabled then ArmBurstTimer;
  end;
end;

procedure TThrottledEvent.NotifyNow(AInvocation: TInvocation; ALeading: Boolean);
var LVersion: UInt64;
begin
  FTimer.Enabled := false;
  if ALeading then FSuppressedInvocations := 0;
  FLastActualNotify:=Now;
  if not BeginBurst(AInvocation) then Exit;
  LVersion := FStateVersion;
  try
    InvokeCallbacks(FHandler, nil, AInvocation, LVersion);
  finally
    // A listener may postpone this delivery again, or notify during its callback.
    // Retain that pending delayed invocation and its observable suppressed count.
    if AInvocation.Alive then
      if FStateVersion = LVersion then
        if not FTimer.Enabled then FSuppressedInvocations:=0;
  end;
end;

procedure TThrottledEvent.NotifyLater(AInvocation: TInvocation);
begin
  inc(FSuppressedInvocations);
  // Burst modes restart the actual native timer for each input. Periodic mode
  // keeps its existing deadline while notifications continue to arrive.
  if FMode in [temNotifyOnEventBurstFinished,temNotifyOnEventBurstStartAndFinished] then begin
    if BeginBurst(AInvocation) then ArmBurstTimer;
  end else if NOT FTimer.Enabled then begin
    FTimer.Interval := ClipValue( Round( Abs( FInterval.TotalMilliseconds ) ), 10, High(integer)) ;
    FTimer.Enabled:=true;
  end;
end;

procedure TThrottledEvent.OnTimer(Sender: TObject);
var LDuration : TTimeSpan;
  LDisposed: TDisposables;
  LInvocation: TInvocation;
  LException: Pointer;
begin
  if not FTimer.Enabled then Exit;
  LInvocation := LDisposed.AddObject(TInvocation.Create(Self)) as TInvocation;
  LException := nil;
  try
  case FMode of
    temNone: NotifyNow(LInvocation);
    temNotifyEveryInterval: begin
      LDuration := TTimeSpan.Subtract(Now, FLastActualNotify);
      if LDuration > FInterval then
        NotifyNow(LInvocation)
      else
        FTimer.Interval := ClipValue( Round( Abs ( (FInterval - LDuration).TotalMilliseconds)), 10, High(integer));
    end;
    temNotifyOnEventBurstStartAndFinished, temNotifyOnEventBurstFinished: begin
      LDuration := TTimeSpan.Subtract(Now, FLastClientNotify);
      if LDuration > FInterval then begin
        FTimer.Enabled := False;
        if FSuppressedInvocations > 0 then NotifyNow(LInvocation);
      end
      else
        FTimer.Interval := ClipValue( Round( Abs ( (FInterval - LDuration).TotalMilliseconds)), 10, High(integer));
    end;
  end;
  except
    LException := AcquireExceptionObject;
  end;
  // The ordinary callback may fail. Complete its quiet lifecycle anyway, or
  // retain an explicit rearm, before propagating the original exception.
  if LInvocation.Alive then
    if FBurstActive and not FTimer.Enabled then begin
      try
        FinishBurst(LInvocation);
      except
        if not Assigned(LException) then LException := AcquireExceptionObject;
      end;
    end;
  if Assigned(LException) then raise TObject(LException);
end;

procedure TThrottledEvent.Cancel(AInvocation: TInvocation);
begin
  FTimer.Enabled := False;
  FSuppressedInvocations := 0;
  FLastClientNotify := MinDateTime;
  FLastActualNotify := MinDateTime;
  EndWaiting(AInvocation, False);
end;

procedure TThrottledEvent.Cancel;
var LDisposed: TDisposables; LInvocation: TInvocation;
begin
  LInvocation := LDisposed.AddObject(TInvocation.Create(Self)) as TInvocation;
  Cancel(LInvocation);
end;

procedure TThrottledEvent.SetMode(AMode: TThrottledEventMode);
var LDisposed: TDisposables; LInvocation: TInvocation;
begin
  if FMode = AMode then Exit;
  LInvocation := LDisposed.AddObject(TInvocation.Create(Self)) as TInvocation;
  FMode := AMode;
  Cancel(LInvocation);
end;

procedure TThrottledEvent.SetInterval(const ATimeSpan : TTimeSpan);
begin
  if ATimeSpan.TotalMilliseconds = 0 then
    raise EArgumentOutOfRangeException.Create('ATimeSpan was 0');
  FInterval := ATimeSpan;
end;

{%endregion}

{%region TWinControlHelper}

procedure TWinControlHelper.RemoveAllControls(destroy : boolean);
var
  control : TControl;
begin
  while self.ControlCount > 0 do begin
    control := self.Controls[0];
    // A visible form becomes a top-level window when its parent is removed.
    control.Hide;
    self.RemoveControl(control);
    if destroy then control.Destroy;
  end;
end;

procedure TWinControlHelper.AddControlDockCenter(AControl: TWinControl);
begin
  if AControl.ClassType.InheritsFrom(TCustomForm) then begin
    with TForm(AControl) do begin
      // Needed to avoid infinite WMSize loop
      Constraints.MinHeight := 0;
      Constraints.MaxHeight := 0;
      Constraints.MinWidth := 0;
      Constraints.MaxWidth := 0;
      Anchors := [akTop, akLeft, akRight, akBottom];
    end;
  end;

  with AControl do begin
    Hide;
    Align := alNone;
    if AControl is TCustomForm then begin
      TForm(AControl).BorderStyle := bsNone;
      TForm(AControl).Position := poDesigned;
    end;
    Parent := Self;
    BoundsRect := Self.ClientRect;
    HandleNeeded;
    Align := alClient;
    Show;
    if AControl.ClassType.InheritsFrom(TApplicationForm) then
      TApplicationForm(AControl).Activate;
  end;
end;

{%endregion}

{%region TFormHelper}


{%endregion}

{%region TImageHelper}

procedure TImageHelper.SetImageListPicture(AImageList: TImageList; AIndex : {$IFDEF FPC}SizeInt{$ELSE}NativeInt{$ENDIF});
begin
  {$IFDEF FPC}
  AImageList.GetBitmap(AIndex, Self.Picture.Bitmap);
  {$ELSE}
  // VCL GetBitmap flattens the image onto an opaque canvas. Its native icon
  // extraction preserves the existing image-list mask and alpha channel.
  AImageList.GetIcon(AIndex, Self.Picture.Icon);
  {$ENDIF}
end;

{%endregion}

{ TLoadingCircle }

constructor TLoadingCircle.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Width := 36;
  Height := 36;
  Color := clWindow;
  FSpokeColor := $00A9A9A9; // System.Drawing.Color.DarkGray in the original control.
  DoubleBuffered := True;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := OnTimer;
end;

destructor TLoadingCircle.Destroy;
begin
  if Assigned(FTimer) then FTimer.Enabled := False;
  inherited Destroy;
end;

procedure TLoadingCircle.SetActive(AValue : Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  FTimer.Enabled := AValue;
  if not AValue then FPosition := 0;
  Invalidate;
end;

procedure TLoadingCircle.SetSpokeColor(AValue : TColor);
begin
  if FSpokeColor = AValue then Exit;
  FSpokeColor := AValue;
  Invalidate;
end;

procedure TLoadingCircle.OnTimer(Sender : TObject);
begin
  {$IFDEF FPC}if not IsVisible then Exit;{$ELSE}if not IsWindowVisible(Handle) then Exit;{$ENDIF}
  FPosition := (FPosition + 1) mod 12;
  Invalidate;
end;

procedure TLoadingCircle.Paint;
var
  i, LAlpha, LThickness : Integer;
  LRed, LGreen, LBlue, LBackRed, LBackGreen, LBackBlue : Byte;
  LAngle, LRadius, LCenterX, LCenterY : Double;
  {$IFNDEF FPC}
  LBrush: TLogBrush;
  LPen, LPreviousPen: HGDIOBJ;
  {$ENDIF}
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  {$IFDEF FPC}
  RedGreenBlue(ColorToRGB(FSpokeColor),LRed,LGreen,LBlue);
  RedGreenBlue(ColorToRGB(Color),LBackRed,LBackGreen,LBackBlue);
  {$ELSE}
  LRed:=GetRValue(ColorToRGB(FSpokeColor)); LGreen:=GetGValue(ColorToRGB(FSpokeColor)); LBlue:=GetBValue(ColorToRGB(FSpokeColor));
  LBackRed:=GetRValue(ColorToRGB(Color)); LBackGreen:=GetGValue(ColorToRGB(Color)); LBackBlue:=GetBValue(ColorToRGB(Color));
  {$ENDIF}
  LRadius := Min(Width,Height) * 11 / 32;
  LCenterX := Width / 2;
  LCenterY := Height / 2;
  LThickness := Max(2,Round(Min(Width,Height) / 16));
  Canvas.Pen.Width := LThickness;
  {$IFDEF FPC}Canvas.Pen.EndCap := pecRound;{$ENDIF}
  for i := 0 to 11 do begin
    if (not FActive) or (i = 0) then LAlpha := 255
    else LAlpha := i * (255 div 12);
    Canvas.Pen.Color := {$IFDEF FPC}RGBToColor{$ELSE}RGB{$ENDIF}(
      (LRed * LAlpha + LBackRed * (255-LAlpha)) div 255,
      (LGreen * LAlpha + LBackGreen * (255-LAlpha)) div 255,
      (LBlue * LAlpha + LBackBlue * (255-LAlpha)) div 255);
    LAngle := (FPosition + i + 1) * (2 * Pi / 12);
    {$IFDEF FPC}
    Canvas.Line(
      Round(LCenterX + LRadius * 5 / 11 * Cos(LAngle)),
      Round(LCenterY + LRadius * 5 / 11 * Sin(LAngle)),
      Round(LCenterX + LRadius * Cos(LAngle)),
      Round(LCenterY + LRadius * Sin(LAngle)));
    {$ELSE}
    // VCL's TPen has no EndCap property. Retain rounded spokes with a native
    // geometric pen, restoring the canvas HDC before releasing its handle.
    LBrush.lbStyle:=BS_SOLID;
    LBrush.lbColor:=ColorToRGB(Canvas.Pen.Color);
    LBrush.lbHatch:=0;
    LPen:=ExtCreatePen(PS_GEOMETRIC or PS_SOLID or PS_ENDCAP_ROUND or PS_JOIN_ROUND,
      LThickness,LBrush,0,nil);
    if LPen=0 then RaiseLastOSError;
    LPreviousPen:=SelectObject(Canvas.Handle,LPen);
    try
      MoveToEx(Canvas.Handle,Round(LCenterX+LRadius*5/11*Cos(LAngle)),Round(LCenterY+LRadius*5/11*Sin(LAngle)),nil);
      Winapi.Windows.LineTo(Canvas.Handle,Round(LCenterX+LRadius*Cos(LAngle)),Round(LCenterY+LRadius*Sin(LAngle)));
    finally
      SelectObject(Canvas.Handle,LPreviousPen);
      DeleteObject(LPen);
    end;
    {$ENDIF}
  end;
end;

end.
