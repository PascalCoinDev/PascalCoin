unit UWizard.Tests;

{$mode delphi}
{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Variants, Forms, Controls, fpcunit, testregistry, UCommon, UWizard, UWizard.TestScreens;

type
  { Forms created without resources keep these tests independent of the designer. }
  TProbeWizardScreen = class(TWizardForm<Integer>)
    protected
      procedure SetParent(NewParent : TWinControl); override;
    public
      InitializedModel : Integer;
      InitializeCount : Integer;
      NextCount : Integer;
      PreviousCount : Integer;
      AllowNext : boolean;
      FailureMessage : String;
      InjectedScreen : TComponentClass;
      NextAction : TNotifyEvent;
      class var DestroyCount : Integer;
      class var VisibleDetachCount : Integer;
      class var HiddenDetachCount : Integer;
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure Initialize; override;
      procedure OnNext; override;
      procedure OnPrevious; override;
      function Validate(out message : String) : boolean; override;
  end;

  TFirstWizardScreen = class(TProbeWizardScreen);
  TSecondWizardScreen = class(TProbeWizardScreen);
  TThirdWizardScreen = class(TProbeWizardScreen);
  TFailingWizardScreen = class(TProbeWizardScreen)
    public
      procedure Initialize; override;
  end;

  TProbeWizard = class(TWizard<Integer>)
    private
      function GetTestHost : TWizardHostForm;
    protected
      procedure ShowHost; override;
      procedure ShowError(const message : String); override;
      function FinishRequested(out message : String) : boolean; override;
      function CancelRequested(out message : String) : boolean; override;
    public
      Run : TNotifyEvent;
      FinishAction : TNotifyEvent;
      CancelAction : TNotifyEvent;
      AllowFinish : boolean;
      AllowCancel : boolean;
      FailureMessage : String;
      FinishCount : Integer;
      CancelCount : Integer;
      LastError : String;
      constructor Create(const screens : array of TComponentClass);
      property TestHost : TWizardHostForm read GetTestHost;
  end;

  TReferenceProbeWizard = class(TActionWizard<TGenericWizardBag>)
    private
      function GetTestHost : TWizardHostForm;
    protected
      procedure ShowHost; override;
    public
      Run : TNotifyEvent;
      property TestHost : TWizardHostForm read GetTestHost;
  end;

  TWizardTests = class(TTestCase)
    private
      FWizard : TProbeWizard;
      FReferenceWizard : TReferenceProbeWizard;
      FFinishedCount : Integer;
      procedure RunWizard(const screens : array of TComponentClass; action : TNotifyEvent);
      procedure VerifyInitializationAndNavigation(Sender : TObject);
      procedure VerifyValidation(Sender : TObject);
      procedure VerifyDynamicBranch(Sender : TObject);
      procedure VerifyFinishVeto(Sender : TObject);
      procedure VerifyFinishException(Sender : TObject);
      procedure VerifyCancelException(Sender : TObject);
      procedure VerifyCancellation(Sender : TObject);
      procedure VerifyCancellationRecheck(Sender : TObject);
      procedure VerifyReentry(Sender : TObject);
      procedure VerifyReplaceAndRemove(Sender : TObject);
      procedure VerifyBacktracking(Sender : TObject);
      procedure VerifyExplicitFinish(Sender : TObject);
      procedure VerifyHostClose(Sender : TObject);
      procedure RejectNestedNavigation(Sender : TObject);
      procedure RejectNestedTerminal(Sender : TObject);
      procedure DisableCancellation(Sender : TObject);
      procedure RaiseCallbackError(Sender : TObject);
      procedure CountFinished(Sender : TObject);
      procedure VerifyReferenceModel(Sender : TObject);
      procedure VerifyHiddenDetachment(Sender : TObject);
      procedure VerifyNativeStringMessages(Sender : TObject);
    protected
      procedure TearDown; override;
    published
      procedure InitializesWithModelAndPreservesValueNavigation;
      procedure ValidationRunsBeforeNextAndInjection;
      procedure DynamicSuccessorIsRequiredAndCanBeRetried;
      procedure RejectedFinishStaysActiveAndAcceptedFinishRunsOnce;
      procedure FailedFinishCanBeRetried;
      procedure FailedCancellationCanBeRetried;
      procedure CancellationHonorsVetoAndTerminalState;
      procedure CancellationRechecksPermissionAfterCallback;
      procedure NavigationAndTerminalCallbacksRejectReentry;
      procedure PathReplacementRemovalAndSnapshots;
      procedure PreviousRestoresBranchBeforeReinjection;
      procedure ExplicitFinishValidatesAndRejectsRemainingScreens;
      procedure HostCloseUsesCancellationButFinishedCloseDoesNot;
      procedure RemovedScreensRemainOwned;
      procedure ActionWizardCaptionsUseConstructorArguments;
      procedure InvalidScreenRequestsPreservePath;
      procedure EmptyConstructionDoesNotLeak;
      procedure UnstartedWizardCannotCompleteWithAnUnassignedModel;
      procedure InitializationFailureReleasesHostAndRetainsScreenOwnership;
      procedure ReferenceModelScreensAcrossUnitsNavigateAndFinish;
      procedure NavigationAndHostCleanupHideScreensBeforeDetaching;
      procedure NativeStringMessagesAndCaptionsPreserveText;
  end;

implementation

constructor TProbeWizardScreen.Create(AOwner : TComponent);
begin
  inherited CreateNew(AOwner);
  AllowNext := true;
end;

destructor TProbeWizardScreen.Destroy;
begin
  inc(DestroyCount);
  inherited Destroy;
end;

procedure TProbeWizardScreen.SetParent(NewParent : TWinControl);
begin
  if (Parent <> nil) and (NewParent = nil) then begin
    if Visible then begin
      inc(VisibleDetachCount);
      // Prevent a failing regression from creating a visible desktop window.
      Hide;
      raise Exception.Create('Wizard screen detached before it was hidden');
    end;
    inc(HiddenDetachCount);
  end;
  inherited SetParent(NewParent);
end;

procedure TProbeWizardScreen.Initialize;
begin
  inc(InitializeCount);
  InitializedModel := Model;
end;

procedure TProbeWizardScreen.OnNext;
begin
  inc(NextCount);
  Model := Model + 1;
  if Assigned(NextAction) then NextAction(self);
  if Assigned(InjectedScreen) then
    UpdatePath(ptInject, [InjectedScreen]);
end;

procedure TProbeWizardScreen.OnPrevious;
begin
  inc(PreviousCount);
  Model := Model + 1;
end;

function TProbeWizardScreen.Validate(out message : String) : boolean;
begin
  message := '';
  Result := AllowNext;
  if NOT Result then begin
    message := FailureMessage;
    if message = '' then message := 'Choose a branch';
  end;
end;

procedure TFailingWizardScreen.Initialize;
begin
  inherited Initialize;
  raise Exception.Create('Cannot initialize screen');
end;

constructor TProbeWizard.Create(const screens : array of TComponentClass);
begin
  inherited Create(nil, screens);
  AllowFinish := true;
  AllowCancel := true;
end;

function TProbeWizard.GetTestHost : TWizardHostForm;
begin
  Result := Host;
end;

procedure TProbeWizard.ShowHost;
begin
  if Assigned(Run) then Run(self);
end;

procedure TProbeWizard.ShowError(const message : String);
begin
  LastError := message;
end;

function TProbeWizard.FinishRequested(out message : String) : boolean;
begin
  inc(FinishCount);
  if Assigned(FinishAction) then FinishAction(self);
  Result := AllowFinish;
  message := '';
  if NOT Result then begin
    message := FailureMessage;
    if message = '' then message := 'Keep editing';
  end;
end;

function TProbeWizard.CancelRequested(out message : String) : boolean;
begin
  inc(CancelCount);
  if Assigned(CancelAction) then CancelAction(self);
  Result := AllowCancel;
  message := '';
  if NOT Result then begin
    message := FailureMessage;
    if message = '' then message := 'Cannot cancel yet';
  end;
end;

function TReferenceProbeWizard.GetTestHost : TWizardHostForm;
begin
  Result := Host;
end;

procedure TReferenceProbeWizard.ShowHost;
begin
  if Assigned(Run) then Run(self);
end;

procedure TWizardTests.TearDown;
begin
  FreeAndNil(FWizard);
  FreeAndNil(FReferenceWizard);
  inherited TearDown;
end;

procedure TWizardTests.RunWizard(const screens : array of TComponentClass; action : TNotifyEvent);
begin
  FWizard := TProbeWizard.Create(screens);
  FWizard.Run := action;
  FWizard.Start(42);
  AssertTrue('Host must be released after Start', FWizard.TestHost = nil);
end;

procedure TWizardTests.InitializesWithModelAndPreservesValueNavigation;
begin
  RunWizard([TFirstWizardScreen, TSecondWizardScreen], VerifyInitializationAndNavigation);
end;

procedure TWizardTests.VerifyInitializationAndNavigation(Sender : TObject);
var
  message : String;
  first, second : TProbeWizardScreen;
begin
  first := TProbeWizardScreen(FWizard.CurrentScreen);
  AssertEquals(42, first.InitializedModel);
  AssertEquals(1, first.InitializeCount);
  AssertEquals(CT_WIZARD_DEFAULT_NEXT, FWizard.TestHost.NextText);
  AssertTrue(FWizard.TestHost.HidePrevious);
  AssertTrue(FWizard.TryNext(message));
  second := TProbeWizardScreen(FWizard.CurrentScreen);
  AssertEquals(42, second.InitializedModel);
  AssertEquals(1, second.InitializeCount);
  AssertEquals(43, FWizard.Model);
  AssertEquals(1, FWizard.CurrentScreenIndex);
  AssertEquals(CT_WIZARD_DEFAULT_FINISH, FWizard.TestHost.NextText);
  AssertFalse(FWizard.TestHost.HidePrevious);
  FWizard.TestHost.FPreviousButton.Click;
  AssertTrue(FWizard.CurrentScreen = first);
  AssertEquals(44, FWizard.Model);
  AssertEquals(1, second.PreviousCount);
  AssertEquals(1, first.InitializeCount);
end;

procedure TWizardTests.ValidationRunsBeforeNextAndInjection;
begin
  RunWizard([TFirstWizardScreen], VerifyValidation);
end;

procedure TWizardTests.VerifyValidation(Sender : TObject);
var
  message : String;
  first : TProbeWizardScreen;
begin
  first := TProbeWizardScreen(FWizard.CurrentScreen);
  first.Traits := [wstAssumeHasNext];
  first.InjectedScreen := TSecondWizardScreen;
  first.AllowNext := false;
  AssertFalse(FWizard.TryNext(message));
  AssertEquals('Choose a branch', message);
  AssertEquals(0, first.NextCount);
  AssertEquals(1, Length(FWizard.Screens));
  AssertEquals(42, FWizard.Model);
  first.AllowNext := true;
  AssertTrue(FWizard.TryNext(message));
  AssertTrue(FWizard.CurrentScreen is TSecondWizardScreen);
  AssertEquals(1, first.NextCount);
  AssertEquals(0, FWizard.FinishCount);
  AssertEquals(43, TProbeWizardScreen(FWizard.CurrentScreen).InitializedModel);
end;

procedure TWizardTests.DynamicSuccessorIsRequiredAndCanBeRetried;
begin
  FWizard := TProbeWizard.Create([TFirstWizardScreen]);
  // Traits are live metadata and must affect the very first displayed button.
  TProbeWizardScreen(FWizard.Components[0]).Traits := [wstAssumeHasNext];
  FWizard.Run := VerifyDynamicBranch;
  FWizard.Start(42);
end;

procedure TWizardTests.VerifyDynamicBranch(Sender : TObject);
var
  message : String;
  first : TProbeWizardScreen;
begin
  first := TProbeWizardScreen(FWizard.CurrentScreen);
  AssertTrue(FWizard.HasNext);
  AssertEquals(CT_WIZARD_DEFAULT_NEXT, FWizard.TestHost.NextText);
  AssertFalse(FWizard.TryNext(message));
  AssertTrue(message <> '');
  AssertEquals(0, FWizard.CurrentScreenIndex);
  AssertEquals(Ord(wsActive), Ord(FWizard.State));
  AssertFalse(FWizard.IsBusy);
  AssertEquals(0, FWizard.FinishCount);
  first.InjectedScreen := TSecondWizardScreen;
  FWizard.TestHost.FNextButton.Click;
  AssertTrue(FWizard.CurrentScreen is TSecondWizardScreen);
  AssertEquals(2, first.NextCount);
  FWizard.TestHost.FNextButton.Click;
  AssertEquals(Ord(wsFinished), Ord(FWizard.State));
end;

procedure TWizardTests.RejectedFinishStaysActiveAndAcceptedFinishRunsOnce;
var
  message : String;
begin
  RunWizard([TFirstWizardScreen], VerifyFinishVeto);
  AssertTrue(FWizard.Finish(message));
  AssertFalse(FWizard.TryNext(message));
  AssertFalse(FWizard.TryPrevious(message));
  AssertEquals(2, FWizard.FinishCount);
end;

procedure TWizardTests.CountFinished(Sender : TObject);
begin
  inc(FFinishedCount);
end;

procedure TWizardTests.VerifyFinishVeto(Sender : TObject);
var
  message : String;
begin
  FWizard.FinishedEvent.Add(CountFinished);
  FWizard.AllowFinish := false;
  FWizard.TestHost.FNextButton.Click;
  AssertEquals('Keep editing', FWizard.LastError);
  AssertEquals(Ord(wsActive), Ord(FWizard.State));
  AssertEquals(0, FWizard.TestHost.ModalResult);
  AssertEquals(0, FFinishedCount);
  FWizard.AllowFinish := true;
  FWizard.TestHost.FNextButton.Click;
  AssertEquals(Ord(wsFinished), Ord(FWizard.State));
  AssertEquals(mrOK, FWizard.TestHost.ModalResult);
  AssertTrue(FWizard.Finish(message));
  AssertFalse(FWizard.Cancel(message));
  AssertEquals(2, FWizard.FinishCount);
  AssertEquals(0, FWizard.CancelCount);
  AssertEquals(1, FFinishedCount);
end;

procedure TWizardTests.RaiseCallbackError(Sender : TObject);
begin
  raise Exception.Create('Retry completion');
end;

procedure TWizardTests.FailedFinishCanBeRetried;
begin
  RunWizard([TFirstWizardScreen], VerifyFinishException);
end;

procedure TWizardTests.VerifyFinishException(Sender : TObject);
var
  message : String;
  raised : boolean;
begin
  FWizard.FinishAction := RaiseCallbackError;
  raised := false;
  try
    FWizard.TryNext(message);
  except
    on E : Exception do begin
      raised := true;
      AssertEquals('Retry completion', E.Message);
    end;
  end;
  AssertTrue(raised);
  AssertFalse(FWizard.IsBusy);
  AssertEquals(Ord(wsActive), Ord(FWizard.State));
  FWizard.FinishAction := nil;
  AssertTrue(FWizard.TryNext(message));
  AssertEquals(2, FWizard.FinishCount);
end;

procedure TWizardTests.FailedCancellationCanBeRetried;
var
  message : String;
begin
  RunWizard([TFirstWizardScreen], VerifyCancelException);
  AssertTrue('Accepted cancellation remains idempotent after the host closes', FWizard.Cancel(message));
  AssertFalse(FWizard.TryNext(message));
  AssertFalse(FWizard.TryPrevious(message));
  AssertEquals(2, FWizard.CancelCount);
end;

procedure TWizardTests.VerifyCancelException(Sender : TObject);
var
  message : String;
  raised : boolean;
begin
  FWizard.CancelAction := RaiseCallbackError;
  raised := false;
  try
    FWizard.Cancel(message);
  except
    on E : Exception do begin
      raised := true;
      AssertEquals('Retry completion', E.Message);
    end;
  end;
  AssertTrue(raised);
  AssertFalse(FWizard.IsBusy);
  AssertEquals(Ord(wsActive), Ord(FWizard.State));
  FWizard.CancelAction := nil;
  AssertTrue(FWizard.Cancel(message));
  AssertTrue(FWizard.Cancel(message));
  AssertEquals(2, FWizard.CancelCount);
end;

procedure TWizardTests.CancellationHonorsVetoAndTerminalState;
begin
  RunWizard([TFirstWizardScreen], VerifyCancellation);
end;

procedure TWizardTests.VerifyCancellation(Sender : TObject);
var
  message : String;
begin
  FWizard.IsCancellable := false;
  AssertFalse(FWizard.Cancel(message));
  AssertEquals(0, FWizard.CancelCount);
  FWizard.IsCancellable := true;
  FWizard.AllowCancel := false;
  AssertFalse(FWizard.Cancel(message));
  AssertEquals('Cannot cancel yet', message);
  AssertEquals(Ord(wsActive), Ord(FWizard.State));
  FWizard.AllowCancel := true;
  AssertTrue(FWizard.Cancel(message));
  AssertTrue(FWizard.Cancel(message));
  AssertFalse(FWizard.Finish(message));
  AssertEquals(2, FWizard.CancelCount);
  AssertEquals(0, FWizard.FinishCount);
  AssertEquals(Ord(wsCancelled), Ord(FWizard.State));
end;

procedure TWizardTests.DisableCancellation(Sender : TObject);
begin
  FWizard.IsCancellable := false;
end;

procedure TWizardTests.CancellationRechecksPermissionAfterCallback;
begin
  RunWizard([TFirstWizardScreen], VerifyCancellationRecheck);
end;

procedure TWizardTests.VerifyCancellationRecheck(Sender : TObject);
var
  message : String;
begin
  FWizard.CancelAction := DisableCancellation;
  AssertFalse(FWizard.Cancel(message));
  AssertEquals(Ord(wsActive), Ord(FWizard.State));
  AssertEquals(0, FWizard.TestHost.ModalResult);
  FWizard.CancelAction := nil;
  FWizard.IsCancellable := true;
  AssertTrue(FWizard.Cancel(message));
end;

procedure TWizardTests.NavigationAndTerminalCallbacksRejectReentry;
begin
  RunWizard([TFirstWizardScreen, TSecondWizardScreen], VerifyReentry);
end;

procedure TWizardTests.RejectNestedNavigation(Sender : TObject);
var
  message : String;
begin
  AssertTrue(FWizard.IsBusy);
  AssertFalse(FWizard.TryNext(message));
  AssertFalse(FWizard.TryPrevious(message));
  AssertFalse(FWizard.Finish(message));
  AssertFalse(FWizard.Cancel(message));
end;

procedure TWizardTests.RejectNestedTerminal(Sender : TObject);
var
  message : String;
begin
  AssertEquals(Ord(wsFinishing), Ord(FWizard.State));
  AssertFalse(FWizard.Finish(message));
  AssertFalse(FWizard.Cancel(message));
end;

procedure TWizardTests.VerifyReentry(Sender : TObject);
var
  message : String;
begin
  TProbeWizardScreen(FWizard.CurrentScreen).NextAction := RejectNestedNavigation;
  AssertTrue(FWizard.TryNext(message));
  AssertEquals(1, FWizard.CurrentScreenIndex);
  FWizard.FinishAction := RejectNestedTerminal;
  AssertTrue(FWizard.TryNext(message));
  AssertEquals(1, FWizard.FinishCount);
  AssertEquals(0, FWizard.CancelCount);
end;

procedure TWizardTests.PathReplacementRemovalAndSnapshots;
begin
  RunWizard([TFirstWizardScreen, TSecondWizardScreen], VerifyReplaceAndRemove);
end;

procedure TWizardTests.VerifyReplaceAndRemove(Sender : TObject);
var
  message : String;
  path : TWizardScreenPath;
begin
  path := FWizard.Screens;
  path[0] := TThirdWizardScreen;
  AssertTrue(FWizard.Screens[0] = TFirstWizardScreen);
  FWizard.UpdatePath(ptInject, [TThirdWizardScreen]);
  FWizard.UpdatePath(ptInject, [TThirdWizardScreen]);
  AssertEquals(3, Length(FWizard.Screens));
  FWizard.UpdatePath(ptRemoveNext, [TThirdWizardScreen]);
  AssertEquals(2, Length(FWizard.Screens));
  AssertTrue(FWizard.TryNext(message));
  FWizard.UpdatePath(ptReplaceAll, [TThirdWizardScreen]);
  AssertEquals(0, FWizard.CurrentScreenIndex);
  AssertTrue(FWizard.CurrentScreen is TThirdWizardScreen);
  AssertFalse(FWizard.HasPrevious);
  AssertFalse(FWizard.HasNext);
end;

procedure TWizardTests.PreviousRestoresBranchBeforeReinjection;
begin
  RunWizard([TFirstWizardScreen], VerifyBacktracking);
end;

procedure TWizardTests.VerifyBacktracking(Sender : TObject);
var
  message : String;
  first : TProbeWizardScreen;
begin
  first := TProbeWizardScreen(FWizard.CurrentScreen);
  first.Traits := [wstAssumeHasNext];
  first.InjectedScreen := TSecondWizardScreen;
  AssertTrue(FWizard.TryNext(message));
  AssertEquals(2, Length(FWizard.Screens));
  // An identical replacement must not discard the saved pre-branch path.
  FWizard.UpdatePath(ptReplaceAll, FWizard.Screens);
  AssertTrue(FWizard.TryPrevious(message));
  AssertEquals(1, Length(FWizard.Screens));
  first.InjectedScreen := TThirdWizardScreen;
  AssertTrue(FWizard.TryNext(message));
  AssertTrue(FWizard.CurrentScreen is TThirdWizardScreen);
  AssertEquals(2, Length(FWizard.Screens));
end;

procedure TWizardTests.ExplicitFinishValidatesAndRejectsRemainingScreens;
begin
  RunWizard([TFirstWizardScreen, TSecondWizardScreen], VerifyExplicitFinish);
end;

procedure TWizardTests.VerifyExplicitFinish(Sender : TObject);
var
  message : String;
begin
  AssertFalse(FWizard.Finish(message));
  AssertEquals(0, FWizard.FinishCount);
  AssertTrue(FWizard.TryNext(message));
  TProbeWizardScreen(FWizard.CurrentScreen).AllowNext := false;
  AssertFalse(FWizard.Finish(message));
  AssertEquals('Choose a branch', message);
  AssertEquals(0, FWizard.FinishCount);
  TProbeWizardScreen(FWizard.CurrentScreen).AllowNext := true;
  AssertTrue(FWizard.Finish(message));
  AssertEquals(1, FWizard.FinishCount);
end;

procedure TWizardTests.HostCloseUsesCancellationButFinishedCloseDoesNot;
begin
  RunWizard([TFirstWizardScreen], VerifyHostClose);
end;

procedure TWizardTests.VerifyHostClose(Sender : TObject);
var
  message : String;
  canClose : boolean;
begin
  FWizard.IsCancellable := false;
  FWizard.TestHost.FormCloseQuery(FWizard.TestHost, canClose);
  AssertFalse(canClose);
  AssertEquals(0, FWizard.CancelCount);
  AssertTrue(FWizard.TryNext(message));
  FWizard.TestHost.FormCloseQuery(FWizard.TestHost, canClose);
  AssertTrue(canClose);
  AssertEquals(0, FWizard.CancelCount);
end;

procedure TWizardTests.RemovedScreensRemainOwned;
var
  before : Integer;
begin
  before := TProbeWizardScreen.DestroyCount;
  FWizard := TProbeWizard.Create([TFirstWizardScreen]);
  FWizard.UpdatePath(ptInject, [TSecondWizardScreen]);
  FWizard.RemoveScreen(TSecondWizardScreen);
  AssertEquals(before, TProbeWizardScreen.DestroyCount);
  FreeAndNil(FWizard);
  AssertEquals(before + 2, TProbeWizardScreen.DestroyCount);
end;

procedure TWizardTests.ActionWizardCaptionsUseConstructorArguments;
var
  wizard : TActionWizard<Integer>;
begin
  wizard := TActionWizard<Integer>.Create(nil, 'Example setup', '&Install', [TFirstWizardScreen], nil, nil);
  try
    AssertEquals('Example setup', wizard.TitleText);
    AssertEquals('&Install', wizard.FinishText);
  finally
    wizard.Free;
  end;
end;

procedure TWizardTests.InvalidScreenRequestsPreservePath;
var
  raised : boolean;
begin
  FWizard := TProbeWizard.Create([TFirstWizardScreen]);
  raised := false;
  try
    FWizard.UpdatePath(ptReplaceAll, []);
  except
    on E : Exception do raised := true;
  end;
  AssertTrue(raised);
  AssertEquals(1, Length(FWizard.Screens));
  raised := false;
  try
    FWizard.UpdatePath(ptReplaceAll, [TComponent]);
  except
    on E : Exception do raised := true;
  end;
  AssertTrue(raised);
  AssertTrue(FWizard.Screens[0] = TFirstWizardScreen);
  raised := false;
  try
    FWizard.UpdatePath(ptReplaceAll, [TReferenceWizardFirstScreen]);
  except
    on E : Exception do raised := true;
  end;
  AssertTrue('Wizard screens with another model type must be rejected', raised);
  AssertTrue(FWizard.Screens[0] = TFirstWizardScreen);
end;

procedure TWizardTests.EmptyConstructionDoesNotLeak;
var
  raised : boolean;
begin
  raised := false;
  try
    FWizard := TProbeWizard.Create([]);
  except
    on E : Exception do raised := true;
  end;
  AssertTrue(raised);
end;

procedure TWizardTests.UnstartedWizardCannotCompleteWithAnUnassignedModel;
var
  message : String;
  raised : boolean;
begin
  FWizard := TProbeWizard.Create([TFirstWizardScreen]);
  raised := false;
  try
    FWizard.Finish(message);
  except
    on E : Exception do raised := true;
  end;
  AssertTrue(raised);
  raised := false;
  try
    FWizard.Cancel(message);
  except
    on E : Exception do raised := true;
  end;
  AssertTrue(raised);
  AssertEquals(0, FWizard.FinishCount);
  AssertEquals(0, FWizard.CancelCount);
end;

procedure TWizardTests.InitializationFailureReleasesHostAndRetainsScreenOwnership;
var
  before : Integer;
  raised : boolean;
begin
  before := TProbeWizardScreen.DestroyCount;
  FWizard := TProbeWizard.Create([TFailingWizardScreen]);
  raised := false;
  try
    FWizard.Start(42);
  except
    on E : Exception do begin
      raised := true;
      AssertEquals('Cannot initialize screen', E.Message);
    end;
  end;
  AssertTrue(raised);
  AssertTrue(FWizard.TestHost = nil);
  AssertEquals(before, TProbeWizardScreen.DestroyCount);
  FreeAndNil(FWizard);
  AssertEquals(before + 1, TProbeWizardScreen.DestroyCount);
end;

procedure TWizardTests.ReferenceModelScreensAcrossUnitsNavigateAndFinish;
var
  model : TGenericWizardBag;
begin
  model := TGenericWizardBag.Create;
  try
    model.Add('Name', 'Reference model');
    FReferenceWizard := TReferenceProbeWizard.Create(nil, 'Cross-unit wizard', '&Done',
      [TReferenceWizardFirstScreen, TReferenceWizardSecondScreen], nil, nil);
    FReferenceWizard.Run := VerifyReferenceModel;
    FReferenceWizard.Start(model);
    AssertEquals(Ord(wsFinished), Ord(FReferenceWizard.State));
    AssertTrue(FReferenceWizard.TestHost = nil);
    FreeAndNil(FReferenceWizard);
    AssertEquals('Reference model', VarToStr(model['Name']));
  finally
    model.Free;
  end;
end;

procedure TWizardTests.VerifyReferenceModel(Sender : TObject);
var
  first : TReferenceWizardScreen;
begin
  first := TReferenceWizardScreen(FReferenceWizard.CurrentScreen);
  AssertEquals('Reference model', first.InitializedName);
  AssertEquals(1, first.InitializeCount);
  FReferenceWizard.TestHost.FNextButton.Click;
  AssertTrue(FReferenceWizard.CurrentScreen is TReferenceWizardSecondScreen);
  AssertEquals('Reference model', TReferenceWizardScreen(FReferenceWizard.CurrentScreen).InitializedName);
  FReferenceWizard.TestHost.FPreviousButton.Click;
  AssertTrue(TObject(FReferenceWizard.CurrentScreen) = first);
  AssertEquals(1, first.InitializeCount);
  FReferenceWizard.TestHost.FNextButton.Click;
  FReferenceWizard.TestHost.FNextButton.Click;
  AssertEquals(Ord(wsFinished), Ord(FReferenceWizard.State));
end;

procedure TWizardTests.NavigationAndHostCleanupHideScreensBeforeDetaching;
var
  visibleBefore, hiddenBefore : Integer;
begin
  visibleBefore := TProbeWizardScreen.VisibleDetachCount;
  hiddenBefore := TProbeWizardScreen.HiddenDetachCount;
  RunWizard([TFirstWizardScreen, TSecondWizardScreen], VerifyHiddenDetachment);
  AssertEquals('Navigation and final host cleanup must never detach a visible form',
    visibleBefore, TProbeWizardScreen.VisibleDetachCount);
  AssertEquals('Next, Previous and final host cleanup each detach a hidden screen',
    hiddenBefore + 3, TProbeWizardScreen.HiddenDetachCount);
end;

procedure TWizardTests.VerifyHiddenDetachment(Sender : TObject);
var
  first, second : TProbeWizardScreen;
begin
  first := TProbeWizardScreen(FWizard.CurrentScreen);
  AssertTrue(first.Visible);
  FWizard.TestHost.FNextButton.Click;
  second := TProbeWizardScreen(FWizard.CurrentScreen);
  AssertFalse(first.Visible);
  AssertTrue(first.Parent = nil);
  AssertTrue(second.Visible);
  FWizard.TestHost.FPreviousButton.Click;
  AssertFalse(second.Visible);
  AssertTrue(second.Parent = nil);
  AssertTrue(first.Visible);
end;

procedure TWizardTests.NativeStringMessagesAndCaptionsPreserveText;
begin
  FWizard := TProbeWizard.Create([TFirstWizardScreen]);
  FWizard.TitleText := 'Résumé — 漢字';
  FWizard.FinishText := FWizard.TitleText;
  FWizard.Run := VerifyNativeStringMessages;
  FWizard.Start(42);
  AssertTrue(FWizard.TestHost = nil);
end;

procedure TWizardTests.VerifyNativeStringMessages(Sender : TObject);
var
  LMessage, LText : String;
  LScreen : TProbeWizardScreen;
begin
  // Native String out parameters must pass captions and errors without ANSI adapters.
  LText := FWizard.TitleText;
  AssertEquals(LText, FWizard.TestHost.TitleText);
  AssertEquals(LText, FWizard.TestHost.NextText);
  LScreen := TProbeWizardScreen(FWizard.CurrentScreen);
  LScreen.AllowNext := False;
  LScreen.FailureMessage := LText;
  AssertFalse(FWizard.Finish(LMessage));
  AssertEquals(LText, LMessage);
  LScreen.AllowNext := True;
  FWizard.AllowFinish := False;
  FWizard.FailureMessage := LText;
  AssertFalse(FWizard.Finish(LMessage));
  AssertEquals(LText, LMessage);
  AssertEquals(1, FWizard.FinishCount);
  FWizard.AllowCancel := False;
  AssertFalse(FWizard.Cancel(LMessage));
  AssertEquals(LText, LMessage);
  FWizard.AllowCancel := True;
  AssertTrue(FWizard.Cancel(LMessage));
end;

initialization
  RegisterTest(TWizardTests);

end.
