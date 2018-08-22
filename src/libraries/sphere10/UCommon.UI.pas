unit UCommon.UI;

{ Copyright (c) 2017 - 2018 Sphere 10 Software <https://www.sphere10.com>

  Common UI tools.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}


{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls,ExtCtrls,
  FGL, Graphics, Generics.Collections, Generics.Defaults, syncobjs,
  UMemory, LMessages, UCommon;

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
    published
      property CloseAction : TCloseAction read FCloseAction write FCloseAction;
      property OnActivateFirstTime : TNotifyEvent read FActivateFirstTime write FActivateFirstTime;
      property OnDestroyed : TNotifyEvent read FDestroyed write FDestroyed;
  end;

  { TThrottledEvent }

  TThrottledEvent = class(TComponent)
    public const
      CT_DEFAULT_DELAYEDREFRESH_MS = 1000;
    public type
      TThrottledEventMode = (temNone, temNotifyEveryInterval, temNotifyOnEventBurstFinished, temNotifyOnEventBurstStartAndFinished);
    private
      FHandler : TNotifyManyEvent;
      FTimer: TTimer;
      FMode : TThrottledEventMode;
      FInterval : TTimeSpan;
      FLastClientNotify : TDateTime;
      FLastActualNotify : TDateTime;
      FSuppressedInvocations : Integer;
      procedure SetInterval(const ATimeSpan : TTimeSpan);
      procedure OnTimer(Sender: TObject);
      procedure NotifyNow;
      procedure NotifyLater;
    public
      property Interval : TTimeSpan read FInterval write SetInterval;
      property Mode : TThrottledEventMode read FMode write FMode;
      property LastClientNotify : TDateTime read FLastClientNotify;
      property LastActualNotify : TDateTime read FLastActualNotify;
      property SuppressedInvocations : Integer read FSuppressedInvocations;
      constructor Create(Owner:TComponent); override;
      procedure Add(AListener : TNotifyEvent);
      procedure Remove(AListener : TNotifyEvent);
      procedure Notify;
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
    procedure SetImageListPicture(AImageList: TImageList; AIndex : SizeInt);
  end;

implementation

uses LCLIntf;

{%region TApplicationForm}

procedure TApplicationForm.DoCreate;
begin
  inherited;
  FUILock := TCriticalSection.Create;
  FActivatedCount := 0;
  FCloseAction:=caHide;
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

procedure TThrottledEvent.Add(AListener : TNotifyEvent);
begin
  FHandler.Add(AListener);
end;

procedure TThrottledEvent.Remove(AListener : TNotifyEvent);
begin
  FHandler.Remove(AListener);
end;

procedure TThrottledEvent.Notify;
var LIdleDuration : TTimeSpan;
begin
  FLastClientNotify:=Now;
  LIdleDuration := TTimeSpan.Subtract(Now, FLastActualNotify);
  if (FMode = temNone) OR ((NOT FTimer.Enabled) AND (LIdleDuration > Interval) AND (FMode <> temNotifyOnEventBurstFinished)) then
    NotifyNow
  else
    NotifyLater;
end;

procedure TThrottledEvent.NotifyNow;
begin
  FTimer.Enabled := false;
  FLastActualNotify:=Now;
  FHandler.Invoke(nil);
  FSuppressedInvocations:=0;
end;

procedure TThrottledEvent.NotifyLater;
begin
  inc(FSuppressedInvocations);
  if NOT FTimer.Enabled then begin
    FTimer.Interval := ClipValue( Round( Abs( FInterval.TotalMilliseconds ) ), 10, High(integer)) ;
    FTimer.Enabled:=true;
  end;
end;

procedure TThrottledEvent.OnTimer(Sender: TObject);
var LDuration : TTimeSpan;
begin
  case FMode of
    temNone: NotifyNow;
    temNotifyEveryInterval: begin
      LDuration := TTimeSpan.Subtract(Now, FLastActualNotify);
      if LDuration > FInterval then
        NotifyNow
      else
        FTimer.Interval := ClipValue( Round( Abs ( (FInterval - LDuration).TotalMilliseconds)), 10, High(integer));
    end;
    temNotifyOnEventBurstStartAndFinished, temNotifyOnEventBurstFinished: begin
      LDuration := TTimeSpan.Subtract(Now, FLastClientNotify);
      if LDuration > FInterval then
        NotifyNow
      else
        FTimer.Interval := ClipValue( Round( Abs ( (FInterval - LDuration).TotalMilliseconds)), 10, High(integer));
    end;
  end;
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
    self.RemoveControl(control);
    if destroy then control.Destroy;
  end;
end;

procedure TWinControlHelper.AddControlDockCenter(AControl: TWinControl);
begin
  if AControl.ClassType.InheritsFrom(TCustomForm) then begin
    with TCustomForm(AControl) do begin
      // Needed to avoid infinite WMSize loop
      Constraints.MinHeight := 0;
      Constraints.MaxHeight := 0;
      Constraints.MinWidth := 0;
      Constraints.MaxWidth := 0;
      Anchors := [akTop, akLeft, akRight, akBottom];
    end;
  end;

  with AControl do begin
    Align := alClient;
    Parent := Self;
    Show;
    if AControl.ClassType.InheritsFrom(TApplicationForm) then
      TApplicationForm(AControl).Activate;
  end;
end;

{%endregion}

{%region TFormHelper}


{%endregion}

{%region TImageHelper}

procedure TImageHelper.SetImageListPicture(AImageList: TImageList; AIndex : SizeInt);
begin
  AImageList.GetBitmap(AIndex, Self.Picture.Bitmap);
end;

{%endregion}

end.
