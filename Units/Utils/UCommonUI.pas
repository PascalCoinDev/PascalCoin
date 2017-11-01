{
  Copyright (c) 2017 The PascalCoin Project
  Copyright (c) 2017 Sphere 10 Software

  Author: Herman Schoenfeld <herman@sphere10.com>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This code has been donated to The PascalCoin Project by Sphere 10 Software (www.sphere10.com)
  who retains independent copyright.

  Additional Credits:
    <contributors add yourselves here>
}

unit UCommonUI;


{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls,ExtCtrls,  FGL, Graphics, Generics.Collections, Generics.Defaults, syncobjs;

type
  TApplicationForm = class(TForm)
    private
      FActivatedCount : UInt32;
      FActivateFirstTime : TNotifyEvent;
      FDestroyed : TNotifyEvent;
      FCloseAction : TCloseAction;
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
      procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    published
      property CloseAction : TCloseAction read FCloseAction write FCloseAction;
      property OnActivateFirstTime : TNotifyEvent read FActivateFirstTime write FActivateFirstTime;
      property OnDestroyed : TNotifyEvent read FDestroyed write FDestroyed;
  end;

  { TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
    procedure RemoveAllControls(destroy : boolean);
    procedure AddControlDockCenter(AControl: TWinControl);
  end;

  { TFormHelper }

  TFormHelper = class helper for TForm
    procedure SetSubFormCoordinate(SubForm: TForm);
  end;

  { TImageHelper }

  TImageHelper = class helper for TImage
    procedure SetImageListPicture(AImageList: TImageList; AIndex : SizeInt);
  end;


implementation

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

procedure TApplicationForm.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if (Left <> ALeft) or (Top <> ATop) or (Width <> AWidth) or (Height <> AHeight) then
    InvalidatePreferredSize;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
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
      BorderStyle := bsNone;
      // Needed to avoid infinite WMSize loop
      Constraints.MinHeight := 0;
      Constraints.MaxHeight := 0;
      Constraints.MinWidth := 0;
      Constraints.MaxWidth := 0;
    end;
  end;

  with AControl do begin
    Left := 0;
    Top := 0;
    Width := Self.Width;
    Height := Self.Height;
    Align := alClient;
    Parent := Self;
    Show;
  end;
end;

{%endregion}

{%region TFormHelper}

//Form is shown centered over parent form
//Show the position of the subform
procedure TFormHelper.SetSubFormCoordinate(SubForm: TForm);
var TopLeft:TPoint;
begin
  // TODO this needs to be changed
//  Subform.Position:=poOwnerFormCenter; - test for only use center of from
// On Linux ClientToScreen work only if window show as resut need call from OnAcivate from linux correct show.
// on window shows correct in OnShow
  TopLeft:= Self.ClientToScreen(Point(Self.Left,Self.Top));
  Subform.Top   :=TopLeft.y;
  Subform.Left  :=TopLeft.x;
  Subform.Height:= Self.Height-27;
  Subform.Width := Self.Width-9;
end;

{%endregion}

{%region TImageHelper}

procedure TImageHelper.SetImageListPicture(AImageList: TImageList; AIndex : SizeInt);
begin
  AImageList.GetBitmap(AIndex, Self.Picture.Bitmap);
end;

{%endregion}

end.

