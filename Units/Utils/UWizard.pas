{
  Copyright (c) 2017 The PascalCoin Project

  Author: Herman Schoenfeld <herman@sphere10.com>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This code has been donated to The PascalCoin Project by Sphere 10 Software (www.sphere10.com)
  who retains independent Copyright (c) 2017 Sphere 10 Software.

  Additional Credits:
    <contributors add yourselves here>
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
      FBag : T;
      //FWizard : TWizard<T>;  // FPG Bug: cyclic generic dependencies, via forward decls, don't work
    protected
      InjectScreen : procedure (screen : TWizardForm<T>) of object;    // FPC Bug workaround: calling this is equivalent to FWizard.InjectScreen
      property Bag : T read FBag write FBag;

    public
      procedure Initialize; virtual;
      procedure OnPresent; virtual;
      procedure OnPrevious; virtual;
      procedure OnNext; virtual;
      function Validate(out message : AnsiString) : boolean; virtual;
   end;

  { TWizard - Base class for wizards. Encapsulates the entire wizard flow. }
  TWizard<T> = class
    type
      __TScreenType = TWizardForm<T>; // FPC Bug: doesn't support nested generics
      TWizardFormList = TList<__TScreenType>;

    private
        FHost : TWizardHostForm;
        FStarted : Boolean;
        FFinished : Boolean;
        FCurrentScreen : TWizardForm<T>;
        FPropertyBag : T;
        FScreens :  TWizardFormList;
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
        function DetermineHasNext : boolean; virtual;
        function DetermineHasPrevious : boolean; virtual;
        procedure PresentScreen(screen : TWizardForm<T>); virtual;
        function FinishRequested(out message : AnsiString) : boolean; virtual; abstract;
        function CancelRequested(out message : AnsiString) : boolean; virtual; abstract;
    public
        constructor Create(constref propertyBag : T; screens: array of TWizardForm<T>);
        destructor Destroy; override;
        property PropertyBag : T read FPropertyBag;
        property HasNext : boolean read DetermineHasNext;
        property HasPrevious : boolean read DetermineHasPrevious;
        property NextText : AnsiString read FNextText write FNextText;
        property PreviousText : AnsiString read FPreviousText write FPreviousText;
        property FinishText : AnsiString read FFinishText write FFinishText;
        property TitleText : AnsiString read FTitleText write FTitleText;
        procedure Start(AOwner : TComponent); virtual;
        procedure Next; virtual;
        procedure Previous; virtual;
        procedure InjectScreen(screen : TWizardForm<T>); virtual;
        procedure RemoveScreen(screen : TWizardForm<T>); virtual;
  end;

  { TActioWizard Delegate Declarations }
  TActionWizardCancelFunc<T> = function(screenIndex : Integer; constref propertyBag : T; out message : AnsiString) : boolean of object;
  TActionWizardFinishFunc<T> = function(constref  propertyBag : T; out message : AnsiString) : boolean of object;

  { TActionWizard - a generic Wizard that can be used without subclassing }
  TActionWizard<T> = class(specialize TWizard<T>)
    private
      FCancelEvent : TActionWizardCancelFunc<T>;
      FFinishEvent : TActionWizardFinishFunc<T>;
    protected
      function CancelRequested(out message : AnsiString) : boolean; override;
      function FinishRequested(out message : AnsiString) : boolean; override;
    public
      constructor Create(title, finish: AnsiString; constref bag : T; screens : array of TWizardForm<T>; cancelFunc: TActionWizardCancelFunc<T>; finishFunc : TActionWizardFinishFunc<T>);
      class procedure Show(AOwner : TComponent; title, finish: AnsiString; constref bag : T; screens : array of TWizardForm<T>; cancelFunc: TActionWizardCancelFunc<T>; finishFunc : TActionWizardFinishFunc<T>);
      property FinishText : AnsiString read FTitleText;
      property TitleText : AnsiString read FFinishText;
  end;

  { TGenericWizardBag - a generic bag for use within wizard }
  TGenericWizardBag = TDictionary<AnsiString, TObject>;

implementation

{$R *.lfm}

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

constructor TWizard<T>.Create(constref propertyBag : T; screens: array of TWizardForm<T>);
var
   i : integer;
   screen : TWizardForm<T>;
begin
  if Length(screens) = 0 then
    raise Exception.Create('Wizard needs at least 1 screen');

  //self.FHost = nil; // Created/destroyed on Show
  self.FNextText := CT_WIZARD_DEFAULT_NEXT;
  self.FPreviousText := CT_WIZARD_DEFAULT_PREVIOUS;
  self.FFinishText := CT_WIZARD_DEFAULT_FINISH;
  self.FTitleText := CT_WIZARD_DEFAULT_TITLE;
  self.FStarted := false;
  self.FFinished := false;
  self.FPropertyBag := propertyBag;
  self.FScreens := TWizardFormList.Create;
  for i := Low(screens) to High(screens) do begin
    screen := screens[i];
    screen.InjectScreen := InjectScreen;
    FScreens.Add(screen);
  end;
end;

destructor TWizard<T>.Destroy;
var
  i : integer;
begin
  for i:= 0 to FScreens.Count - 1 do begin
    FScreens[i].Destroy;
  end;
  FScreens.Destroy;
  // note: Property bag not destroyed, left for user to destroy
end;

function TWizard<T>.CalculateFitSize : TPoint;
var
  maxWidth, maxHeight, i : Integer;
  screen : TWizardForm<T>;
begin
  maxWidth := 0;
  maxHeight := 0;
  for i := 0 to self.FScreens.Count - 1 do begin
    screen := self.FScreens[i];
    if screen.Width > maxWidth then maxWidth := screen.Width;
    if screen.Height > maxHeight then maxHeight := screen.Height;
  end;
  Result := TPoint.Create(maxWidth, maxHeight);
end;

function TWizard<T>.DetermineHasNext : boolean;
begin
   CheckStarted;
   DetermineHasNext := FCurrentScreenIndex < FScreens.Count - 1;
end;

function TWizard<T>.DetermineHasPrevious : boolean;
begin
  CheckStarted;
  DetermineHasPrevious := FCurrentScreenIndex > 0;
end;

procedure TWizard<T>.Start(AOwner : TComponent);
var
  i : integer;
begin
  CheckNotStarted;
  self.FHost := TWizardHostForm.Create(AOwner);
  self.FHost.NextEvent.Add(NextHandler);
  self.FHost.PreviousEvent.Add(PreviousHandler);
  self.FHost.CloseQueryEvent := CancelRequested;
  self.FHost.PreviousText := self.PreviousText;
  self.Fhost.NextText := self.NextText;
  self.FHost.TitleText := self.TitleText;
  self.FCurrentScreenIndex := 0;
  self.FStarted := true;
  self.FFinished := false;
  for i := 0 to self.FScreens.Count - 1 do begin
    FScreens[i].Bag := self.PropertyBag;
    FScreens[i].Initialize;
  end;
  self.FHost.SetContentSize ( CalculateFitSize );
  self.PresentScreen(FScreens[FCurrentScreenIndex]);
  self.FHost.ShowModal;
  self.FHost.Destroy;
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
  FCurrentScreen.Next;
  if HasNext then begin
    inc(FCurrentScreenIndex);
    PresentScreen(FScreens[FCurrentScreenIndex]);
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
  FCurrentScreen.Previous;
  dec (FCurrentScreenIndex);
  PresentScreen(FScreens[FCurrentScreenIndex]);
end;

procedure TWizard<T>.InjectScreen(screen : TWizardForm<T>);
begin
  CheckStarted;
  screen.Initialize;
  FScreens.Insert(FCurrentScreenIndex + 1, screen );
  self.FHost.SetContentSize ( CalculateFitSize );
  FHost.NextText := IIF(NOT HasNext, FinishText, NextText);
end;

procedure TWizard<T>.RemoveScreen(screen : TWizardForm<T>);
begin
  CheckStarted;
  FScreens.Remove(screen);
  screen.Destroy;
end;

procedure TWizard<T>.PresentScreen(screen : TWizardForm<T>);
begin
  FCurrentScreen := screen;
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

constructor TActionWizard<T>.Create(title, finish: AnsiString; constref bag : T; screens : array of TWizardForm<T>; cancelFunc: TActionWizardCancelFunc<T>; finishFunc : TActionWizardFinishFunc<T>);
begin
  inherited Create(bag, screens);
  self.FTitleText := title;
  self.FFinishText := finishText;
  self.FCancelEvent := cancelFunc;
  self.FFinishEvent := finishFunc;
end;

class procedure TActionWizard<T>.Show(AOwner : TComponent; title, finish: AnsiString; constref bag : T; screens : array of TWizardForm<T>; cancelFunc: TActionWizardCancelFunc<T>; finishFunc : TActionWizardFinishFunc<T>);
type
  MyWizard = TActionWizard<T>;
var
  wizard : MyWizard;
begin
  wizard := MyWizard.Create(title, finish, bag, screens, cancelFunc, finishFunc);
  try
    wizard.Start(AOwner);
  finally
    wizard.Destroy;
  end;
end;

function TActionWizard<T>.CancelRequested(out message : AnsiString) : boolean;
begin
  if Assigned(FCancelEvent) and NOT FFinished then
    Result := FCancelEvent(Self.FCurrentScreenIndex, self.PropertyBag, message)
  else Result := true;
end;

function TActionWizard<T>.FinishRequested(out message : AnsiString) : boolean;
begin
  if Assigned(FFinishEvent) then
    Result := FFinishEvent(self.PropertyBag, message)
  else Result := true;
  FFinished := true;
end;

{%endregion}

end.

