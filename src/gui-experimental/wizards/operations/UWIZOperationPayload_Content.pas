unit UWIZOperationPayload_Content;

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

{ Collects an operation message and checks message text length against the limit before the
  next wizard step.
}

{ Public types in this unit

  Type                         | Purpose
  -----------------------------+--------------------------------------------------------------------------
  TWIZOperationPayload_Content | Edits payload content and checks its length against the size limit.
}

interface

uses
  Types, Math, Controls,
  SysUtils, StdCtrls, ExtCtrls, UWizard, UWIZOperation;

type

  { TWIZOperationPayload_Content }

  TWIZOperationPayload_Content = class(TWizardForm<TWIZOperationsModel>)
    grpPayload: TGroupBox;
    Label1: TLabel;
    mmoPayload: TMemo;
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
  UConst,
  UCoreObjects,
  UWIZOperationSigner_Select;

{ TWIZOperationPayload_Content }

type
  TCaptionGroupBoxAccess = class(TGroupBox);

procedure TWIZOperationPayload_Content.FormResize(Sender: TObject);

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
  if not HandleAllocated or not Assigned(Label1) then Exit;
  // VCL reserves its caption inside ClientRect; LCL already excludes it.
  // Use the native content inset so controls stay clear at either font scale.
  PositionContents(grpPayload);
end;

procedure TWIZOperationPayload_Content.OnPresent;
begin
  FormResize(Self);
  mmoPayload.SetFocus;
end;

procedure TWIZOperationPayload_Content.OnNext;
begin
  Model.Payload.Content := mmoPayload.Lines.Text;

  case Model.ExecuteOperationType of
    omtSendPasc, omtBuyAccount:
    begin
      // do nothing
    end;

    omtChangeInfo:
    begin
      // Change Info chooses its optional common payer on the fee page.
    end
    else
    begin
      UpdatePath(ptInject, [TWIZOperationSigner_Select]);
    end;
  end;

end;

function TWIZOperationPayload_Content.Validate(out message: String): boolean;
begin
  Result := True;
  if Length(mmoPayload.Lines.Text) > CT_MaxPayloadSize then
  begin
    message := Format('Payload Size Is Larger Than Max Payload Size Which Is "%u"', [CT_MaxPayloadSize]);
    Exit(False);
  end;
end;

end.
