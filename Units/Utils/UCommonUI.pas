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


{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, FGL, Generics.Collections, Generics.Defaults, syncobjs;

type
  TApplicationForm = class(TForm)
    private
      FActivatedCount : UInt32;
      FActivateFirstTime : TNotifyEvent;
      procedure NotifyActivateFirstTime;
    protected
      FUILock : TCriticalSection;
      procedure DoCreate; override;
      procedure DoDestroy; override;
      procedure Activate; override;
      procedure ActivateFirstTime; virtual;
    published
      property OnActivateFirstTime : TNotifyEvent read FActivateFirstTime write FActivateFirstTime;
  end;

  { TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
    procedure RemoveAllControls(destroy : boolean);
  end;

  { TFormHelper }

  TFormHelper = class helper for TForm
    procedure SetSubFormCoordinate(SubForm: TForm);
  end;


implementation

{%region TApplicationForm}

procedure TApplicationForm.DoCreate;
begin
  inherited;
  FUILock := TCriticalSection.Create;
  FActivatedCount := 0;
end;

procedure TApplicationForm.DoDestroy;
begin
  inherited;
  FUILock.Destroy;
  FUILock := nil;
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


procedure TApplicationForm.NotifyActivateFirstTime;
begin
  ActivateFirstTime;
  if Assigned(FActivateFirstTime) then
    FActivateFirstTime(Self);
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

end.

