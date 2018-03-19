{
  Copyright (c) 2017 Sphere 10 Software

  Author: Herman Schoenfeld <herman@sphere10.com>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

unit UWizard;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, UCommon, Generics.Collections;

const
  CT_WIZARD_DEFAULT_NEXT : AnsiString = '&Next';
  CT_WIZARD_DEFAULT_PREVIOUS : AnsiString = '&Previous';
  CT_WIZARD_DEFAULT_FINISH : AnsiString = '&Finish';
  CT_WIZARD_DEFAULT_TITLE : AnsiString = 'Wizard';

type
  { Forward Declarations }
  TWizardForm<T> = class;

  { Enums }

  TPathUpdateType = (ptInject, ptReplaceAllNext, ptReplaceAll);

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
      TWizardHostFormCloseQueryDelegate = function(out message : AnsiString) : boolean of object;
    protected
      FNextEvent : TNotifyManyEvent;
      FPreviousEvent : TNotifyManyEvent;
      FCloseQueryEvent : TWizardHostFormCloseQueryDelegate;
      procedure SetTitleText(title : AnsiString); virtual;
      function GetTitleText : AnsiString; virtual;
      procedure SetHideNext(hideNext : boolean); virtual;
      function GetHideNext : boolean; virtual;
      procedure SetHidePrevious(hidePrevious : boolean); virtual;
      function GetHidePrevious : boolean; virtual;
      procedure SetNextText(nextText : AnsiString); virtual;
      function GetNextText : AnsiString; virtual;
      procedure SetPreviousText(previousText : AnsiString); virtual;
      function GetPreviousText : AnsiString; virtual;
    public
      property NextEvent : TNotifyManyEvent read FNextEvent write FNextEvent;
      property PreviousEvent : TNotifyManyEvent read FPreviousEvent write FPreviousEvent;
      property CloseQueryEvent : TWizardHostFormCloseQueryDelegate read FCloseQueryEvent write FCloseQueryEvent;
      property HideNext : boolean read GetHideNext write SetHideNext;
      property HidePrevious : boolean read GetHidePrevious write SetHidePrevious;
      property NextText : AnsiString read GetNextText write SetNextText;
      property PreviousText : AnsiString read GetPreviousText write SetPreviousText;
      property TitleText : AnsiString read GetTitleText write SetTitleText;
      procedure SetContentSize(size : TPoint); virtual;
      procedure SetContent(screen : TForm);
  end;

  { TWizardForm is the base class for wizard screens }
  TWizardForm<T> = class(TForm)
    private
      FModel : T;
      //FWizard : TWizard<T>;  // FPG Bug: cyclic generic dependencies, via forward decls, don't work
    protected
      UpdatePath : procedure (APathUpdateType: TPathUpdateType; const screens : array of TComponentClass) of object;    // FPC Bug workaround: calling this is equivalent to FWizard.InjectScreen
      property Model : T read FModel write FModel;

    public
      procedure Initialize; virtual;
      procedure OnPresent; virtual;
      procedure OnPrevious; virtual;
      procedure OnNext; virtual;
      function Validate(out message : AnsiString) : boolean; virtual;
   end;

  { TWizard - Base class for wizards. Encapsulates the entire wizard flow. }
  TWizard<T> = class(TComponent)
    type
      __TScreenType = TWizardForm<T>; // FPC Bug: doesn't support nested generics
      __TList_TComponentClass = TList<TComponentClass>;
    private
        FHost : TWizardHostForm;
        FStarted : Boolean;
        FFinished : Boolean;
        FCurrentScreen : TWizardForm<T>;
        FModel : T;
        FScreenPath : TList<TComponentClass>;
        FScreenPathBackup : TDictionary<SizeInt, __TList_TComponentClass>;
        FScreenInstances : TDictionary<TComponentClass, __TScreenType>;
        FCurrentScreenIndex : Integer;
        FNextText : AnsiString;
        FPreviousText : AnsiString;
        FFinishText : AnsiString;
        FTitleText : AnsiString;
        procedure CheckStarted;
        procedure CheckNotStarted;
        function CalculateFitSize : TPoint;
        procedure NextHandler(sender : TObject);
        procedure PreviousHandler(sender : TObject);
    protected
        function CreateScreen(AType: TComponentClass) : TWizardForm<T>;
        function DetermineHasNext : boolean; virtual;
        function DetermineHasPrevious : boolean; virtual;
        procedure PresentScreen(screen : TWizardForm<T>); virtual;
        function FinishRequested(out message : AnsiString) : boolean; virtual; abstract;
        function CancelRequested(out message : AnsiString) : boolean; virtual; abstract;
    public
        constructor Create(AOwner:TComponent; const screens: array of TComponentClass); overload;
        destructor Destroy; override;
        property CurrentScreen : TWizardForm<T> read FCurrentScreen;
        property Model : T read FModel;
        property HasNext : boolean read DetermineHasNext;
        property HasPrevious : boolean read DetermineHasPrevious;
        property NextText : AnsiString read FNextText write FNextText;
        property PreviousText : AnsiString read FPreviousText write FPreviousText;
        property FinishText : AnsiString read FFinishText write FFinishText;
        property TitleText : AnsiString read FTitleText write FTitleText;
        procedure Start(constref model : T); virtual;
        procedure Next; virtual;
        procedure Previous; virtual;
        procedure UpdatePath(APathUpdateType: TPathUpdateType; const screens : array of TComponentClass); virtual;
  end;


  { TActionWizard - a generic Wizard that can be used without subclassing }
  TActionWizard<T> = class(TWizard<T>)
    public type
      TActionWizardCancelFunc = function(screenIndex : Integer; constref model : T; out message : AnsiString) : boolean of object;
      TActionWizardFinishFunc = function(constref  model : T; out message : AnsiString) : boolean of object;
    private
      FCancelHandler : TActionWizardCancelFunc;
      FFinishHandler : TActionWizardFinishFunc;
    protected
      function CancelRequested(out message : AnsiString) : boolean; override;
      function FinishRequested(out message : AnsiString) : boolean; override;
    public
      constructor Create(AOwner:TComponent; title, finish: AnsiString; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
      class procedure Show(AParent: TForm; title, finish: AnsiString; constref bag : T; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
      property FinishText : AnsiString read FTitleText;
      property TitleText : AnsiString read FFinishText;
  end;

  { TGenericWizardBag - a generic bag for use within wizard }
  TGenericWizardBag = TDictionary<AnsiString, TObject>;

implementation

{$R *.lfm}

uses
  UCommon.UI;

{%region TWizardForm }

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

function TWizardForm<T>.Validate(out message : AnsiString) : boolean;
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
   message : string;
begin
  if Assigned(FCloseQueryEvent) then begin
    CanClose := FCloseQueryEvent(message);
    if message <> '' then
      ShowMessage(message)
  end else
    CanClose := true
end;

procedure TWizardHostForm.SetTitleText(title : AnsiString);
begin
 self.Caption:= title;
end;

function TWizardHostForm.GetTitleText : AnsiString;
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

procedure TWizardHostForm.SetNextText(nextText : AnsiString);
begin
  FNextButton.Caption := nextText;
end;

function TWizardHostForm.GetNextText : AnsiString;
begin
  Result := FNextButton.Caption;
end;

procedure TWizardHostForm.SetPreviousText(previousText : AnsiString);
begin
  FPreviousButton.Caption := previousText;
end;

function TWizardHostForm.GetPreviousText : AnsiString;
begin
  Result := FPreviousButton.Caption;
end;

procedure TWizardHostForm.SetContent(screen : TForm);
begin
  if FWizardScreenPanel.ControlCount > 0 then
    FWizardScreenPanel.RemoveAllControls(false);

  with screen do begin
       Align := alClient;
       BorderStyle := bsNone;
       Parent := self.FWizardScreenPanel;
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
var
   i : integer;
   screen : TWizardForm<T>;
begin
  inherited Create(AOwner);
  if Length(screens) = 0 then
    raise Exception.Create('Wizard needs at least 1 screen');

  //self.FHost = nil; // Created/destroyed on Show
  self.FNextText := CT_WIZARD_DEFAULT_NEXT;
  self.FPreviousText := CT_WIZARD_DEFAULT_PREVIOUS;
  self.FFinishText := CT_WIZARD_DEFAULT_FINISH;
  self.FTitleText := CT_WIZARD_DEFAULT_TITLE;
  self.FStarted := false;
  self.FFinished := false;
  self.FScreenPath := TList<TComponentClass>.Create;
  self.FScreenInstances := TDictionary<TComponentClass, __TScreenType>.Create;

  // Create the screen path
  UpdatePath(ptReplaceAll, screens);
  FScreenPathBackup := TDictionary<SizeInt, __TList_TComponentClass>.Create;
end;

destructor TWizard<T>.Destroy;
var
  i : integer;
  screen : TWizardForm<T>;
  key : TComponentClass;
  backup : TList<TComponentClass>;
begin
  // screens destroyed as sub-components
  for screen in FScreenInstances.Values do begin
    if screen.Owner = nil then
      screen.Free; // only destroy if dangling screen component
  end;
  FScreenInstances.Free;
  FScreenPath.Free;
  for backup in FScreenPathBackup.Values do
    backup.Free;
  FScreenPathBackup.Free;
  inherited Destroy;
  // note: Property bag not destroyed, left for user to destroy
end;

function TWizard<T>.CreateScreen(AType: TComponentClass) : TWizardForm<T>;
begin
  if NOT FScreenInstances.ContainsKey(AType) then begin
    Result := TWizardForm<T>(AType.Create(self));
    if Result = nil then
      raise Exception.Create('Supplied type was not correct TWizardForm<T> type');
    Result.UpdatePath := UpdatePath;
    FScreenInstances.Add(AType, Result);
    Result.Initialize;
  end else Result := FScreenInstances[AType];
end;

function TWizard<T>.CalculateFitSize : TPoint;
var
  maxWidth, maxHeight, i : Integer;
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
   CheckStarted;
   DetermineHasNext := FCurrentScreenIndex < FScreenPath.Count - 1;
end;

function TWizard<T>.DetermineHasPrevious : boolean;
begin
  CheckStarted;
  DetermineHasPrevious := FCurrentScreenIndex > 0;
end;

procedure TWizard<T>.Start(constref model : T);
var
  i : integer;
begin
  CheckNotStarted;
  self.FModel := model;
  self.FHost := TWizardHostForm.Create(Self.GetParentComponent);
  self.FHost.NextEvent.Add(NextHandler);
  self.FHost.PreviousEvent.Add(PreviousHandler);
  self.FHost.CloseQueryEvent := CancelRequested;
  self.FHost.PreviousText := self.PreviousText;
  self.Fhost.NextText := self.NextText;
  self.FHost.TitleText := self.TitleText;
  self.FCurrentScreenIndex := 0;
  self.FStarted := true;
  self.FFinished := false;
  self.FHost.SetContentSize ( CalculateFitSize );
  self.FScreenPathBackup.Add(0, TList<TComponentClass>.Create(FScreenPath));
  self.PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]]);
  //self.FHost.NextEvent.Remove(NextHandler);
  //self.FHost.PreviousEvent.Remove(PreviousHandler);
  self.FHost.ShowModal;
  self.FHost.Free;
  self.FHost := nil;
end;

procedure TWizard<T>.Next;
var
  message : AnsiString;
begin
  CheckStarted;
  if NOT FCurrentScreen.Validate(message) then begin
    ShowMessage (message);
    exit;
  end;
  FCurrentScreen.OnNext;
  FModel := FCurrentScreen.Model; // Restore model in case it's a record
  if HasNext then begin
    inc(FCurrentScreenIndex);
    // Backup current path in case user goes back
    if FScreenPathBackup.ContainsKey(FCurrentScreenIndex) then begin
      FScreenPathBackup[FCurrentScreenIndex].Free;
    end;
    FScreenPathBackup.AddOrSetValue(FCurrentScreenIndex, TList<TComponentClass>.Create(FScreenPath));
    PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]]);
    exit;
  end;

  if NOT FinishRequested(message) then
    ShowMessage(message);
  // else
  // TODO: Fire finished event
  FHost.Close;
end;

procedure TWizard<T>.Previous;
begin
  CheckStarted;
  if not HasPrevious then exit;
  FCurrentScreen.OnPrevious;
  FModel := FCurrentScreen.Model; // Restore model in case it's a record
  dec (FCurrentScreenIndex);

  // Restore backup path when this screen was first displayed
  FScreenPath.Free;
  FScreenPath := TList<TComponentClass>.Create(FScreenPathBackup[FCurrentScreenIndex]);
  PresentScreen(FScreenInstances[FScreenPath[FCurrentScreenIndex]]);
end;

procedure TWizard<T>.UpdatePath(APathUpdateType: TPathUpdateType; const screens : array of TComponentClass);
var
  i, n : SizeInt;
begin
  case APathUpdateType of
    ptInject: begin
      for i := Low(screens) to High(screens) do begin
        n := FCurrentScreenIndex + 1 + i - Low(screens);
        if FScreenPath[n] <> screens[i] then begin
          // only insert screen if not already there
          FScreenPath.Insert(n, screens[i]);
          CreateScreen(screens[i]);
        end;
      end;
    end;
    ptReplaceAllNext: begin
      FScreenPath.DeleteRange(FCurrentScreenIndex + 1, FScreenPath.Count - FCurrentScreenIndex - 1);
      for i := Low(screens) to High(screens) do begin
        FScreenPath.Add(screens[i]);
        CreateScreen(screens[i]);
      end;
    end;
    ptReplaceAll: begin
      FScreenPath.Clear;
      for i := Low(screens)to High(screens) do begin
        FScreenPath.Add(screens[i]);
        CreateScreen(screens[i]);
      end;
    end;
  end;
  if Assigned(FHost) then begin
    FHost.SetContentSize ( CalculateFitSize );
    FHost.NextText := IIF(NOT HasNext, FinishText, NextText);
  end;
end;

procedure TWizard<T>.PresentScreen(screen : TWizardForm<T>);
begin
  FCurrentScreen := screen;
  FCurrentScreen.Model := Model;
  FHost.HidePrevious := NOT HasPrevious;
  FHost.NextText := IIF( NOT HasNext, FinishText, NextText);
  FHost.SetContent(screen);
  screen.OnPresent;
end;

procedure TWizard<T>.CheckStarted;
begin
   if not FStarted then raise Exception.Create('Wizard has not been started');
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

constructor TActionWizard<T>.Create(AOwner: TComponent; title, finish: AnsiString; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
begin
  inherited Create(AOwner, screens);
  self.FTitleText := title;
  self.FFinishText := finishText;
  self.FCancelHandler := cancelFunc;
  self.FFinishHandler := finishFunc;
end;

class procedure TActionWizard<T>.Show(AParent: TForm; title, finish: AnsiString; constref bag : T; const screens : array of TComponentClass; cancelFunc: TActionWizardCancelFunc; finishFunc : TActionWizardFinishFunc);
type
  MyWizard = TActionWizard<T>;
var
  wizard : MyWizard;
begin
  wizard := MyWizard.Create(nil, title, finish, screens, cancelFunc, finishFunc);
  try
    wizard.Start(bag);
  finally
    wizard.Free;
  end;
end;

function TActionWizard<T>.CancelRequested(out message : AnsiString) : boolean;
begin
  if Assigned(FCancelHandler) and NOT FFinished then
    Result := FCancelHandler(Self.FCurrentScreenIndex, self.Model, message)
  else Result := true;
end;

function TActionWizard<T>.FinishRequested(out message : AnsiString) : boolean;
begin
  if Assigned(FFinishHandler) then
    Result := FFinishHandler(self.Model, message)
  else Result := true;
  FFinished := true;
end;

{%endregion}

end.
