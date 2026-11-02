unit UWizard;

{ Copyright (c) 2017 - 2018 Herman Schoenfeld <https://www.sphere10.com>

  Typed-model wizard screens with validation and dynamic routes for Delphi/VCL
  and Lazarus/LCL.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{ Public types in this unit

  Type                                              | Purpose
  --------------------------------------------------+-------------------------------------------------------------------------
  TActionWizard<T>                                  | Runs a typed wizard with supplied finish and cancel callbacks.
  TActionWizard<T>.TActionWizardCancelFunc          | Validates cancellation using the current screen and model.
  TActionWizard<T>.TActionWizardFinishFunc          | Executes and validates wizard completion.
  TGenericWizardBag                                 | Stores named Variant values as a shared wizard model.
  TPathUpdateType                                   | Selects insertion, replacement or removal of wizard steps.
  TWizard<T>                                        | Coordinates navigation, dynamic routes, finish and cancellation.
  TWizardForm<T>                                    | Provides typed model access and screen validation lifecycle hooks.
  TWizardFormBase                                   | Identifies a hosted wizard screen's model type.
  TWizardFormClass                                  | Identifies a wizard screen class for dynamic creation.
  TWizardHostForm                                   | Hosts one wizard screen and its navigation buttons in one window.
  TWizardHostForm.TWizardHostFormCloseQueryDelegate | Approves or rejects closing the wizard host.
  TWizardScreenPath                                 | Holds an ordered array of wizard screen classes.
  TWizardState                                      | Reports active, finishing, cancelling or completed wizard state.
  TWizardStepTrait                                  | Declares a screen navigation capability.
  TWizardStepTraits                                 | Combines a screen's navigation capabilities.
}


{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo, Types,
  {$IFDEF FPC}
  FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  {$ELSE}
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  {$ENDIF}
  UCommon, Generics.Collections;

const
  // UI captions and messages use the compiler's native String, matching callers.
  CT_WIZARD_DEFAULT_NEXT : String = '&Next';
  CT_WIZARD_DEFAULT_PREVIOUS : String = '&Previous';
  CT_WIZARD_DEFAULT_FINISH : String = '&Finish';
  CT_WIZARD_DEFAULT_TITLE : String = 'Wizard';

type
  { Enums }

  { Path updates preserve screen instances. ptRemoveNext only removes future occurrences. }
  TPathUpdateType = (ptInject, ptReplaceAllNext, ptReplaceAll, ptRemoveNext);
  { A rejected or failed terminal callback returns to wsActive and can be retried. }
  TWizardState = (wsActive, wsFinishing, wsCancelling, wsFinished, wsCancelled);
  { A dynamic screen promises to register a successor from OnNext. }
  TWizardStepTrait = (wstAssumeHasNext);
  TWizardStepTraits = set of TWizardStepTrait;
  TWizardScreenPath = array of TComponentClass;

  { TWizardHostForm - the host form that contains the wizard screens. }
  TWizardHostForm = class(TForm)
    FHorizonalLine: TPanel;
    FNextButton: TButton;
    FPreviousButton: TButton;
    FWizardScreenPanel: TPanel;
    procedure FPreviousButtonClick(Sender: TObject);
    procedure FNextButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    type
      TWizardHostFormCloseQueryDelegate = function(out message : String) : boolean of object;
    protected
      FNextEvent : TNotifyManyEvent;
      FPreviousEvent : TNotifyManyEvent;
      FCloseQueryEvent : TWizardHostFormCloseQueryDelegate;
      procedure SetTitleText(title : String); virtual;
      function GetTitleText : String; virtual;
      procedure SetHideNext(hideNext : boolean); virtual;
      function GetHideNext : boolean; virtual;
      procedure SetHidePrevious(hidePrevious : boolean); virtual;
      function GetHidePrevious : boolean; virtual;
      procedure SetNextText(nextText : String); virtual;
      function GetNextText : String; virtual;
      procedure SetPreviousText(previousText : String); virtual;
      function GetPreviousText : String; virtual;
    public
      property NextEvent : TNotifyManyEvent read FNextEvent write FNextEvent;
      property PreviousEvent : TNotifyManyEvent read FPreviousEvent write FPreviousEvent;
      property CloseQueryEvent : TWizardHostFormCloseQueryDelegate read FCloseQueryEvent write FCloseQueryEvent;
      property HideNext : boolean read GetHideNext write SetHideNext;
      property HidePrevious : boolean read GetHidePrevious write SetHidePrevious;
      property NextText : String read GetNextText write SetNextText;
      property PreviousText : String read GetPreviousText write SetPreviousText;
      property TitleText : String read GetTitleText write SetTitleText;
      procedure SetContentSize(size : TPoint); virtual;
      procedure SetContent(screen : TForm);
  end;

  { FPC can emit distinct generic class identities in different units. Validate
    the nongeneric base and model RTTI instead of generic InheritsFrom checks. }
  TWizardFormBase = class(TForm)
    protected
      class function GetModelTypeInfo : PTypeInfo; virtual; abstract;
  end;

  TWizardFormClass = class of TWizardFormBase;

  { TWizardForm is the base class for wizard screens. The wizard owns the form;
    class/interface models retain their caller's ownership. }
  TWizardForm<T> = class(TWizardFormBase)
    private
      FModel : T;
      FTraits : TWizardStepTraits;
      //FWizard : TWizard<T>;  // FPG Bug: cyclic generic dependencies, via forward decls, don't work
    protected
      UpdatePath : procedure (APathUpdateType: TPathUpdateType; const screens : array of TComponentClass) of object;    // FPC Bug workaround: calling this is equivalent to FWizard.InjectScreen
      class function GetModelTypeInfo : PTypeInfo; override;
      property Model : T read FModel write FModel;

    public
      property Traits : TWizardStepTraits read FTraits write FTraits;
      { Called once after Start assigns Model, including for injected screens. }
      procedure Initialize; virtual;
      procedure OnPresent; virtual;
      procedure OnPrevious; virtual;
      { Runs after successful Validate. May update the continuation through UpdatePath. }
      procedure OnNext; virtual;
      { Rejection leaves the current screen active and prevents OnNext. }
      function Validate(out message : String) : boolean; virtual;
   end;

  { TWizard - Base class for wizards. Encapsulates the entire wizard flow.
    Call navigation and path operations on the UI thread. One accepted Finish or
    Cancel ends the flow; recursive navigation/completion requests are rejected. }
  TWizard<T> = class(TComponent)
    type
      __TScreenType = TWizardForm<T>; // FPC Bug: doesn't support nested generics
      __TList_TComponentClass = TList<TComponentClass>;
    private
        FHost : TWizardHostForm;
        FStarted : Boolean;
        FFinished : Boolean;
        FState : TWizardState;
        FBusy : Boolean;
        FNavigating : Boolean;
        FIsCancellable : Boolean;
        FChangedEvent : TNotifyManyEvent;
        FFinishedEvent : TNotifyManyEvent;
        FCurrentScreen : TWizardForm<T>;
        FModel : T;
        FScreenPath : TList<TComponentClass>;
        FScreenPathBackup : TDictionary<SizeInt, __TList_TComponentClass>;
        FScreenInstances : TDictionary<TComponentClass, __TScreenType>;
        FInitializedScreens : TList<TComponentClass>;
        FCurrentScreenIndex : Integer;
        FNextText : String;
        FPreviousText : String;
        FFinishText : String;
        FTitleText : String;
        procedure CheckStarted;
        procedure CheckNotStarted;
        function CalculateFitSize : TPoint;
        procedure NextHandler(sender : TObject);
        procedure PreviousHandler(sender : TObject);
        procedure ValidateScreenType(AType : TComponentClass);
        function CloseQueryHandler(out message : String) : boolean;
        function GetScreens : TWizardScreenPath;
        procedure SavePath;
        procedure ResetPathBackups;
        procedure InitializeScreen(AType : TComponentClass);
        procedure RefreshNavigation;
        procedure CloseHost(AModalResult : Integer);
        function CompleteFinish(out message : String) : boolean;
    protected
        function CreateScreen(AType: TComponentClass) : TWizardForm<T>;
        property Host : TWizardHostForm read FHost;
        { Modal presentation hook. Start releases the host even if this raises. }
        procedure ShowHost; virtual;
        procedure ShowError(const message : String); virtual;
        function DetermineHasNext : boolean; virtual;
        function DetermineHasPrevious : boolean; virtual;
        procedure PresentScreen(screen : TWizardForm<T>); virtual;
        { Return false with a message to retain the active flow. Exceptions also
          release busy admission so the caller can retry. Do not navigate here. }
        function FinishRequested(out message : String) : boolean; virtual; abstract;
        function CancelRequested(out message : String) : boolean; virtual; abstract;
    public
        constructor Create(AOwner:TComponent; const screens: array of TComponentClass); overload;
        destructor Destroy; override;
        property CurrentScreen : TWizardForm<T> read FCurrentScreen;
        property Model : T read FModel;
        { Returns a membership snapshot. Cached screen forms remain wizard-owned
          after they leave this path and are freed with the wizard. }
        property Screens : TWizardScreenPath read GetScreens;
        property CurrentScreenIndex : Integer read FCurrentScreenIndex;
        property State : TWizardState read FState;
        property IsBusy : boolean read FBusy;
        property IsCancellable : boolean read FIsCancellable write FIsCancellable;
        property ChangedEvent : TNotifyManyEvent read FChangedEvent;
        property FinishedEvent : TNotifyManyEvent read FFinishedEvent;
        property HasNext : boolean read DetermineHasNext;
        property HasPrevious : boolean read DetermineHasPrevious;
        property NextText : String read FNextText write FNextText;
        property PreviousText : String read FPreviousText write FPreviousText;
        property FinishText : String read FFinishText write FFinishText;
        property TitleText : String read FTitleText write FTitleText;
        { Starts once, initializes screens with model, and presents a modal host.
          The wizard does not explicitly dispose model instances. }
        procedure Start({$IFDEF FPC}constref{$ELSE}const{$ENDIF} model : T); virtual;
        { Convenience commands display nonempty rejection messages through ShowError. }
        procedure Next; virtual;
        procedure Previous; virtual;
        { Noninteractive outcomes: TryNext validates before OnNext; TryPrevious
          runs OnPrevious. A terminal Next attempts Finish. Missing dynamic
          successors remain active. }
        function TryNext(out message : String) : boolean; virtual;
        function TryPrevious(out message : String) : boolean; virtual;
        { Require Start to assign the model. Finish validates/runs OnNext before
          checking the final path. Accepted outcomes remain idempotent after closure. }
        function Finish(out message : String) : boolean; virtual;
        { Checks IsCancellable both before and after CancelRequested. }
        function Cancel(out message : String) : boolean; virtual;
        { Allowed from OnNext/OnPrevious for dynamic branching. Repeated identical
          injection is idempotent; Previous restores its screen's saved branch. }
        procedure UpdatePath(APathUpdateType: TPathUpdateType; const screens : array of TComponentClass); virtual;
        { Removes one occurrence while idle; the final screen cannot be removed. }
        procedure RemoveScreen(AType : TComponentClass); virtual;
  end;


  { TActionWizard - a generic Wizard that can be used without subclassing }
  TActionWizard<T> = class(TWizard<T>)
    public type
      TActionWizardCancelFunc = function(screenIndex : Integer; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} model : T; out message : String) : boolean of object;
      TActionWizardFinishFunc = function({$IFDEF FPC}constref{$ELSE}const{$ENDIF}  model : T; out message : String) : boolean of object;
    private
      FCancelHandler : TActionWizardCancelFunc;
      FFinishHandler : TActionWizardFinishFunc;
    protected
      function CancelRequested(out message : String) : boolean; override;
      function FinishRequested(out message : String) : boolean; override;
    public
      constructor Create(AOwner:TComponent; title, finish: String; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
      class procedure Show(AParent: TForm; title, finish: String; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} bag : T; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
  end;

  { TGenericWizardBag - a generic bag for use within wizard }
  TGenericWizardBag = TDictionary<utf8string, Variant>;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{%region TWizardForm }

class function TWizardForm<T>.GetModelTypeInfo : PTypeInfo;
begin
  Result := TypeInfo(T);
end;

procedure TWizardForm<T>.Initialize;
begin
  // Default implementation
end;

procedure TWizardForm<T>.OnPresent;
begin
  // Default implementation
end;

procedure TWizardForm<T>.OnPrevious;
begin
  // Default implementation
end;

procedure TWizardForm<T>.OnNext;
begin
  // Default implementation
end;

function TWizardForm<T>.Validate(out message : String) : boolean;
begin
  // Default implementation
  message := '';
  Validate := true;
end;

{%endregion}

{%region TWizardHostForm }

procedure TWizardHostForm.FPreviousButtonClick(Sender: TObject);
begin
  FPreviousEvent.Invoke(self);
end;

procedure TWizardHostForm.FNextButtonClick(Sender: TObject);
begin
  FNextEvent.Invoke(self);
end;

procedure TWizardHostForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
   message : String;
begin
  message := '';
  if Assigned(FCloseQueryEvent) then begin
    CanClose := FCloseQueryEvent(message);
    if message <> '' then
      ShowMessage(message)
  end else
    CanClose := true
end;

procedure TWizardHostForm.SetTitleText(title : String);
begin
 self.Caption:= title;
end;

function TWizardHostForm.GetTitleText : String;
begin
  Result := self.Caption;
end;

procedure TWizardHostForm.SetHidePrevious(hidePrevious : boolean);
begin
  FPreviousButton.Enabled := NOT hidePrevious;
  FPreviousButton.Visible := NOT hidePrevious;
end;

function TWizardHostForm.GetHidePrevious : boolean;
begin
  Result := NOT FPreviousButton.Enabled;
end;

procedure TWizardHostForm.SetHideNext(hideNext : boolean);
begin
  FNextButton.Enabled := not hideNext;
  FNextButton.Visible := not hideNext;
end;

function TWizardHostForm.GetHideNext : boolean;
begin
  Result := NOT FNextButton.Enabled;
end;

procedure TWizardHostForm.SetNextText(nextText : String);
begin
  FNextButton.Caption := nextText;
end;

function TWizardHostForm.GetNextText : String;
begin
  Result := FNextButton.Caption;
end;

procedure TWizardHostForm.SetPreviousText(previousText : String);
begin
  FPreviousButton.Caption := previousText;
end;

function TWizardHostForm.GetPreviousText : String;
begin
  Result := FPreviousButton.Caption;
end;

procedure TWizardHostForm.SetContent(screen : TForm);
begin
  while FWizardScreenPanel.ControlCount > 0 do begin
    // A visible LCL form becomes a standalone window when its parent clears.
    FWizardScreenPanel.Controls[0].Hide;
    FWizardScreenPanel.Controls[0].Parent := nil;
  end;

  with screen do begin
       Hide;
       Align := alNone;
       BorderStyle := bsNone;
       Position := poDesigned;
       Parent := self.FWizardScreenPanel;
       // Apply alignment to the real child handle, not just cached LCL bounds.
       BoundsRect := self.FWizardScreenPanel.ClientRect;
       HandleNeeded;
       Align := alClient;
       Show;
  end;
end;

procedure TWizardHostForm.SetContentSize(size : TPoint);
var bounds : TRect;
begin
  bounds :=   self.FWizardScreenPanel.BoundsRect;
  self.Width := bounds.TopLeft.x + size.x + (self.Width - bounds.Width);
  self.Height := bounds.TopLeft.y + size.y + (self.Height - bounds.Height);
end;

{%endregion}

{%region TWizard }

constructor TWizard<T>.Create(AOwner:TComponent; const screens: array of TComponentClass);
begin
  inherited Create(AOwner);
  if Length(screens) = 0 then
    raise Exception.Create('Wizard needs at least 1 screen');

  self.FNextText := CT_WIZARD_DEFAULT_NEXT;
  self.FPreviousText := CT_WIZARD_DEFAULT_PREVIOUS;
  self.FFinishText := CT_WIZARD_DEFAULT_FINISH;
  self.FTitleText := CT_WIZARD_DEFAULT_TITLE;
  self.FStarted := false;
  self.FFinished := false;
  self.FState := wsActive;
  self.FIsCancellable := true;
  self.FCurrentScreenIndex := 0;
  self.FScreenPath := TList<TComponentClass>.Create;
  self.FScreenInstances := TDictionary<TComponentClass, __TScreenType>.Create;
  self.FInitializedScreens := TList<TComponentClass>.Create;
  self.FScreenPathBackup := TDictionary<SizeInt, __TList_TComponentClass>.Create;

  // Create the screen path
  UpdatePath(ptReplaceAll, screens);
end;

destructor TWizard<T>.Destroy;
var
  screen : TWizardForm<T>;
  backup : TList<TComponentClass>;
begin
  if Assigned(FHost) then begin
    FHost.CloseQueryEvent := nil;
    while FHost.FWizardScreenPanel.ControlCount > 0 do begin
      FHost.FWizardScreenPanel.Controls[0].Hide;
      FHost.FWizardScreenPanel.Controls[0].Parent := nil;
    end;
    FreeAndNil(FHost);
  end;
  // Screens removed by branching still belong to this wizard.
  if Assigned(FScreenInstances) then
    for screen in FScreenInstances.Values do
      screen.Free;
  FScreenInstances.Free;
  FInitializedScreens.Free;
  FScreenPath.Free;
  if Assigned(FScreenPathBackup) then
    for backup in FScreenPathBackup.Values do
      backup.Free;
  FScreenPathBackup.Free;
  inherited Destroy;
  // note: Property bag not destroyed, left for user to destroy
end;

function TWizard<T>.CreateScreen(AType: TComponentClass) : TWizardForm<T>;
begin
  if NOT FScreenInstances.ContainsKey(AType) then begin
    ValidateScreenType(AType);
    Result := TWizardForm<T>(AType.Create(self));
    Result.UpdatePath := UpdatePath;
    FScreenInstances.Add(AType, Result);
  end else Result := FScreenInstances[AType];
end;

procedure TWizard<T>.ValidateScreenType(AType : TComponentClass);
begin
  if (AType = nil) or NOT AType.InheritsFrom(TWizardFormBase) then
    raise Exception.Create('Supplied type was not a wizard screen');
  if TWizardFormClass(AType).GetModelTypeInfo <> TypeInfo(T) then
    raise Exception.Create('Supplied wizard screen has a different model type');
end;

procedure TWizard<T>.InitializeScreen(AType : TComponentClass);
var
  screen : TWizardForm<T>;
begin
  if FInitializedScreens.Contains(AType) then exit;
  screen := CreateScreen(AType);
  screen.Model := FModel;
  screen.Initialize;
  FModel := screen.Model; // Preserve changes when the model is a record.
  if FNavigating and Assigned(FCurrentScreen) then
    FCurrentScreen.Model := FModel;
  FInitializedScreens.Add(AType);
end;

function TWizard<T>.CalculateFitSize : TPoint;
var
  maxWidth, maxHeight : Integer;
  screen : TWizardForm<T>;
begin
  maxWidth := 0;
  maxHeight := 0;
  for screen in FScreenInstances.Values do begin
    if screen.Width > maxWidth then maxWidth := screen.Width;
    if screen.Height > maxHeight then maxHeight := screen.Height;
  end;
  Result := TPoint.Create(maxWidth, maxHeight);
end;

function TWizard<T>.DetermineHasNext : boolean;
begin
  Result := (FCurrentScreenIndex < FScreenPath.Count - 1) or
    (wstAssumeHasNext in FScreenInstances[FScreenPath[FCurrentScreenIndex]].Traits);
end;

function TWizard<T>.DetermineHasPrevious : boolean;
begin
  DetermineHasPrevious := FCurrentScreenIndex > 0;
end;

procedure TWizard<T>.Start({$IFDEF FPC}constref{$ELSE}const{$ENDIF} model : T);
var
  i : integer;
begin
  CheckNotStarted;
  if FState <> wsActive then
    raise Exception.Create('Only an active wizard can be started');
  self.FModel := model;
  self.FStarted := true;
  self.FHost := TWizardHostForm.Create(self);
  try
    self.FHost.NextEvent.Add(NextHandler);
    self.FHost.PreviousEvent.Add(PreviousHandler);
    self.FHost.CloseQueryEvent := CloseQueryHandler;
    self.FHost.PreviousText := self.PreviousText;
    self.FHost.TitleText := self.TitleText;
    // Initialization sees the model, including for screens injected later.
    i := 0;
    while i < FScreenPath.Count do begin
      InitializeScreen(FScreenPath[i]);
      inc(i);
    end;
    self.FHost.SetContentSize(CalculateFitSize);
    ResetPathBackups;
    self.PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]]);
    ShowHost;
  finally
    self.FHost.CloseQueryEvent := nil;
    // Detach borrowed controls before destroying their native host.
    while FHost.FWizardScreenPanel.ControlCount > 0 do begin
      FHost.FWizardScreenPanel.Controls[0].Hide;
      FHost.FWizardScreenPanel.Controls[0].Parent := nil;
    end;
    FreeAndNil(FHost);
  end;
end;

procedure TWizard<T>.Next;
var
  message : String;
begin
  if NOT TryNext(message) and (message <> '') then
    ShowError(message);
end;

procedure TWizard<T>.Previous;
var
  message : String;
begin
  if NOT TryPrevious(message) and (message <> '') then
    ShowError(message);
end;

function TWizard<T>.TryNext(out message : String) : boolean;
begin
  message := '';
  Result := false;
  if FBusy or (FState <> wsActive) then exit;
  CheckStarted;
  FBusy := true;
  FNavigating := true;
  try
    if NOT FCurrentScreen.Validate(message) then exit;
    FCurrentScreen.OnNext;
    FModel := FCurrentScreen.Model;
    if FCurrentScreenIndex < FScreenPath.Count - 1 then begin
      inc(FCurrentScreenIndex);
      SavePath;
      PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]]);
      Result := true;
    end else if HasNext then
      message := 'The current wizard screen expects a next screen'
    else begin
      // Release navigation admission before entering the terminal operation.
      FNavigating := false;
      FBusy := false;
      Result := CompleteFinish(message);
    end;
  finally
    FNavigating := false;
    FBusy := false;
  end;
end;

function TWizard<T>.TryPrevious(out message : String) : boolean;
begin
  message := '';
  Result := false;
  if FBusy or (FState <> wsActive) then exit;
  CheckStarted;
  if NOT HasPrevious then exit;
  FBusy := true;
  FNavigating := true;
  try
    FCurrentScreen.OnPrevious;
    FModel := FCurrentScreen.Model;
    dec(FCurrentScreenIndex);
    // Restore the branch as it was when the previous screen was presented.
    if FScreenPathBackup.ContainsKey(FCurrentScreenIndex) then begin
      FScreenPath.Free;
      FScreenPath := TList<TComponentClass>.Create(FScreenPathBackup[FCurrentScreenIndex]);
    end;
    PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]]);
    Result := true;
  finally
    FNavigating := false;
    FBusy := false;
  end;
end;

function TWizard<T>.Finish(out message : String) : boolean;
begin
  message := '';
  Result := false;
  if FBusy then exit;
  if (FState <> wsActive) or NOT Assigned(FHost) then begin
    Result := CompleteFinish(message);
    exit;
  end;
  FBusy := true;
  FNavigating := true;
  try
    if NOT FCurrentScreen.Validate(message) then exit;
    FCurrentScreen.OnNext;
    FModel := FCurrentScreen.Model;
    FNavigating := false;
    FBusy := false;
    Result := CompleteFinish(message);
  finally
    FNavigating := false;
    FBusy := false;
  end;
end;

function TWizard<T>.CompleteFinish(out message : String) : boolean;
begin
  message := '';
  Result := false;
  if FBusy then exit;
  if FState <> wsActive then begin
    Result := FState = wsFinished;
    exit;
  end;
  CheckStarted;
  if HasNext then begin
    message := 'The wizard still has remaining screens';
    exit;
  end;
  FBusy := true;
  FState := wsFinishing;
  try
    Result := FinishRequested(message);
    if Result then begin
      FState := wsFinished;
      FFinished := true;
      CloseHost(mrOK);
      FFinishedEvent.Invoke(self);
    end;
  finally
    if FState = wsFinishing then FState := wsActive;
    FBusy := false;
    FChangedEvent.Invoke(self);
  end;
end;

function TWizard<T>.Cancel(out message : String) : boolean;
begin
  message := '';
  Result := false;
  if FBusy then exit;
  if FState <> wsActive then begin
    Result := FState = wsCancelled;
    exit;
  end;
  CheckStarted;
  if NOT IsCancellable then exit;
  FBusy := true;
  FState := wsCancelling;
  try
    Result := CancelRequested(message) and IsCancellable;
    if Result then begin
      FState := wsCancelled;
      CloseHost(mrCancel);
    end;
  finally
    if FState = wsCancelling then FState := wsActive;
    FBusy := false;
    FChangedEvent.Invoke(self);
  end;
end;

procedure TWizard<T>.UpdatePath(APathUpdateType: TPathUpdateType; const screens : array of TComponentClass);
var
  i, n : SizeInt;
  samePath : boolean;
  screenType : TComponentClass;
begin
  if (FState <> wsActive) or (FBusy and NOT FNavigating) then
    raise Exception.Create('Only an active, idle wizard can change screens');
  if (APathUpdateType = ptReplaceAll) and (Length(screens) = 0) then
    raise Exception.Create('Wizard needs at least 1 screen');
  // Validate the complete request before changing the live path.
  for screenType in screens do
    ValidateScreenType(screenType);
  if FNavigating and Assigned(FCurrentScreen) then
    FModel := FCurrentScreen.Model;
  for screenType in screens do
    if APathUpdateType <> ptRemoveNext then begin
      CreateScreen(screenType);
      if FStarted then InitializeScreen(screenType);
    end;
  case APathUpdateType of
    ptInject: begin
      // An identical contiguous continuation has already been injected.
      samePath := FScreenPath.Count - FCurrentScreenIndex - 1 >= Length(screens);
      if samePath then
        for i := Low(screens) to High(screens) do
          if FScreenPath[FCurrentScreenIndex + 1 + i] <> screens[i] then
            samePath := false;
      if samePath then exit;
      for i := Low(screens) to High(screens) do
        FScreenPath.Insert(FCurrentScreenIndex + 1 + i, screens[i]);
    end;
    ptReplaceAllNext: begin
      samePath := FScreenPath.Count - FCurrentScreenIndex - 1 = Length(screens);
      if samePath then
        for i := Low(screens) to High(screens) do
          if FScreenPath[FCurrentScreenIndex + 1 + i] <> screens[i] then
            samePath := false;
      if samePath then exit;
      FScreenPath.DeleteRange(FCurrentScreenIndex + 1, FScreenPath.Count - FCurrentScreenIndex - 1);
      for i := Low(screens) to High(screens) do
        FScreenPath.Add(screens[i]);
    end;
    ptReplaceAll: begin
      samePath := FScreenPath.Count = Length(screens);
      if samePath then
        for i := Low(screens) to High(screens) do
          if FScreenPath[i] <> screens[i] then
            samePath := false;
      if samePath then exit;
      FScreenPath.Clear;
      for i := Low(screens)to High(screens) do
        FScreenPath.Add(screens[i]);
      if FCurrentScreenIndex >= FScreenPath.Count then
        FCurrentScreenIndex := FScreenPath.Count - 1;
      ResetPathBackups;
    end;
    ptRemoveNext: begin
      for n := FScreenPath.Count - 1 downto FCurrentScreenIndex + 1 do
        for screenType in screens do
          if FScreenPath[n] = screenType then begin
            FScreenPath.Delete(n);
            break;
          end;
    end;
  end;
  if Assigned(FHost) then begin
    FHost.SetContentSize(CalculateFitSize);
    if Assigned(FCurrentScreen) and
       (FCurrentScreen <> FScreenInstances[FScreenPath[FCurrentScreenIndex]]) then
      PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]])
    else
      RefreshNavigation;
  end;
  FChangedEvent.Invoke(self);
end;

procedure TWizard<T>.RemoveScreen(AType : TComponentClass);
var
  index : Integer;
begin
  if (FState <> wsActive) or FBusy then
    raise Exception.Create('Only an active, idle wizard can remove screens');
  index := FScreenPath.IndexOf(AType);
  if index < 0 then exit;
  if FScreenPath.Count = 1 then
    raise Exception.Create('The last wizard screen cannot be removed');
  FScreenPath.Delete(index);
  if index < FCurrentScreenIndex then dec(FCurrentScreenIndex);
  if FCurrentScreenIndex >= FScreenPath.Count then
    FCurrentScreenIndex := FScreenPath.Count - 1;
  ResetPathBackups;
  if Assigned(FHost) then begin
    if FCurrentScreen <> FScreenInstances[FScreenPath[FCurrentScreenIndex]] then
      PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]])
    else
      RefreshNavigation;
  end;
  FChangedEvent.Invoke(self);
end;

function TWizard<T>.GetScreens : TWizardScreenPath;
begin
  Result := {$IFNDEF FPC}TWizardScreenPath({$ENDIF}FScreenPath.ToArray{$IFNDEF FPC}){$ENDIF};
end;

procedure TWizard<T>.SavePath;
begin
  if FScreenPathBackup.ContainsKey(FCurrentScreenIndex) then
    FScreenPathBackup[FCurrentScreenIndex].Free;
  FScreenPathBackup.AddOrSetValue(FCurrentScreenIndex, TList<TComponentClass>.Create(FScreenPath));
end;

procedure TWizard<T>.ResetPathBackups;
var
  backup : TList<TComponentClass>;
  i : Integer;
begin
  for backup in FScreenPathBackup.Values do
    backup.Free;
  FScreenPathBackup.Clear;
  for i := 0 to FCurrentScreenIndex do
    FScreenPathBackup.Add(i, TList<TComponentClass>.Create(FScreenPath));
end;

procedure TWizard<T>.RefreshNavigation;
begin
  if Assigned(FHost) then begin
    FHost.HidePrevious := NOT HasPrevious;
    if HasNext then
      FHost.NextText := NextText
    else
      FHost.NextText := FinishText;
  end;
end;

procedure TWizard<T>.CloseHost(AModalResult : Integer);
begin
  if Assigned(FHost) then
    FHost.ModalResult := AModalResult;
end;

procedure TWizard<T>.ShowHost;
begin
  FHost.ShowModal;
end;

procedure TWizard<T>.ShowError(const message : String);
begin
  ShowMessage(message);
end;

function TWizard<T>.CloseQueryHandler(out message : String) : boolean;
begin
  message := '';
  if FState in [wsFinished, wsCancelled] then
    Result := true
  else
    Result := Cancel(message);
end;

procedure TWizard<T>.PresentScreen(screen : TWizardForm<T>);
begin
  InitializeScreen(TComponentClass(screen.ClassType));
  FCurrentScreen := screen;
  FCurrentScreen.Model := Model;
  RefreshNavigation;
  FHost.SetContent(screen);
  screen.OnPresent;
  RefreshNavigation;
  FChangedEvent.Invoke(self);
end;

procedure TWizard<T>.CheckStarted;
begin
  if NOT FStarted or NOT Assigned(FHost) then
    raise Exception.Create('Wizard has not been started or its host has closed');
end;

procedure TWizard<T>.CheckNotStarted;
begin
  if FStarted then raise Exception.Create('Wizard has already been started');
end;

procedure TWizard<T>.NextHandler(sender : TObject);
begin
  Next;
end;

procedure TWizard<T>.PreviousHandler(sender : TObject);
begin
  Previous;
end;

{%endregion}

{%region TActionWizard }

constructor TActionWizard<T>.Create(AOwner: TComponent; title, finish: String; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
begin
  inherited Create(AOwner, screens);
  self.TitleText := title;
  self.FinishText := finish;
  self.FCancelHandler := cancelFunc;
  self.FFinishHandler := finishFunc;
end;

class procedure TActionWizard<T>.Show(AParent: TForm; title, finish: String; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} bag : T; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
type
  MyWizard = TActionWizard<T>;
var
  wizard : MyWizard;
begin
  wizard := MyWizard.Create(AParent, title, finish, screens, cancelFunc, finishFunc);
  try
    wizard.Start(bag);
  finally
    wizard.Free;
  end;
end;

function TActionWizard<T>.CancelRequested(out message : String) : boolean;
begin
  message := '';
  if Assigned(FCancelHandler) then
    Result := FCancelHandler(Self.CurrentScreenIndex, self.Model, message)
  else Result := true;
end;

function TActionWizard<T>.FinishRequested(out message : String) : boolean;
begin
  message := '';
  if Assigned(FFinishHandler) then
    Result := FFinishHandler(self.Model, message)
  else Result := true;
end;

{%endregion}

end.
