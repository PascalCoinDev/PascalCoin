unit UWIZOperationPayload_Password;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


{$IFDEF FPC}
{$modeswitch nestedprocvars}
{$ENDIF}

interface

uses
  Types, Math, Controls,
  SysUtils, StdCtrls, ExtCtrls, UWizard, UWIZOperation;

type

  { TWIZOperationPayload_Password }

  TWIZOperationPayload_Password = class(TWizardForm<TWIZOperationsModel>)
    edtPassword: TEdit;
    grpPayload: TGroupBox;
    lblNote: TLabel;
    lblPassword: TLabel;
    paPayload: TPanel;
    procedure FormResize(Sender: TObject);
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: String): boolean; override;
  end;


implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  UWIZOperationPayload_Content;

{ TWIZOperationPayload_Password }

type
  TCaptionGroupBoxAccess = class(TGroupBox);

procedure TWIZOperationPayload_Password.FormResize(Sender: TObject);

  procedure PositionContents(AGroup: TGroupBox);
  var
    LContent: TRect;
    LPadding, LTop, LDelta, I: Integer;
    LControl: TControl;
  begin
    LContent := AGroup.ClientRect;
    TCaptionGroupBoxAccess(AGroup).AdjustClientRect(LContent);
    Canvas.Font.Assign(AGroup.Font);
    LPadding := Max(6, Canvas.TextHeight('0') * 2 div 3);
    LTop := High(Integer);
    for I := 0 to AGroup.ControlCount - 1 do
      LTop := Min(LTop, AGroup.Controls[I].Top);
    LDelta := LContent.Top + LPadding - LTop;
    for I := 0 to AGroup.ControlCount - 1 do begin
      LControl := AGroup.Controls[I];
      LControl.Top := LControl.Top + LDelta;
      if (LControl is TPanel) and (akBottom in LControl.Anchors) then
        LControl.Height := Max(0, LContent.Bottom - LControl.Top - LPadding);
    end;
  end;

begin
  if not HandleAllocated or not Assigned(lblNote) then Exit;
  // VCL reserves its caption inside ClientRect; LCL already excludes it.
  // Use the native content inset so controls stay clear at either font scale.
  PositionContents(grpPayload);
end;

procedure TWIZOperationPayload_Password.OnPresent;
begin
  FormResize(Self);
  edtPassword.SetFocus;
end;

procedure TWIZOperationPayload_Password.OnNext;
begin
  Model.Payload.Password := edtPassword.Text;
  UpdatePath(ptInject, [TWIZOperationPayload_Content]);
end;

function TWIZOperationPayload_Password.Validate(out message: String): boolean;
begin
  Result := True;
  if Length(Trim(edtPassword.Text)) = 0 then
  begin
    message := 'Password Cannot Be Empty';
    Exit(False);
  end;
end;

end.
