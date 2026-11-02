unit UWIZChangeAccountInfo_Skipped;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>.
  Distributed under the MIT software license; see LICENSE.
  Explains unchanged batch targets and requires approval before continuing. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

{ Explains which accounts already have the requested type and requires
  approval to skip them.
}

{ Public types in this unit

  Type                          | Purpose
  ------------------------------+--------------------------------------------------------------------------
  TWIZChangeAccountInfo_Skipped | Confirms skipped accounts before a batch type change continues.
}

interface

uses Classes, SysUtils, Types, Math, Forms, Controls, StdCtrls, UWizard, UWIZOperation;

type
  TWIZChangeAccountInfo_Skipped = class(TWizardForm<TWIZOperationsModel>)
    gbSkipped: TGroupBox;
    lblSummary: TLabel;
    lblRemaining: TLabel;
    lblFees: TLabel;
    chkApprove: TCheckBox;
    procedure FormResize(Sender: TObject);
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: String): Boolean; override;
  end;

implementation

uses Graphics, {$IFDEF FPC}LCLType, LCLIntf{$ELSE}Windows{$ENDIF};

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

type
  TGroupBoxContentAccess = class(TGroupBox);

procedure TWIZChangeAccountInfo_Skipped.FormResize(Sender: TObject);
var
  LContent: TRect;
  LGap, LTop, LWidth: Integer;
  LMeasure: TControlCanvas;

  procedure PositionLabel(ALabel: TLabel; ALines: Integer);
  var
    LText: TRect;
    LHeight: Integer;
  begin
    LMeasure.Control := ALabel;
    LMeasure.Font.Assign(ALabel.Font);
    LText := Rect(0, 0, LWidth, 0);
    DrawText(LMeasure.Handle, PChar(ALabel.Caption), Length(ALabel.Caption), LText,
      DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
    LHeight := Max(LMeasure.TextHeight('Mg') * ALines, LText.Bottom - LText.Top);
    ALabel.SetBounds(LContent.Left + LGap, LTop, LWidth, LHeight);
    // Reserve the measured wrapped text, not a temporarily adjusted LCL bound.
    Inc(LTop, LHeight + LGap);
  end;

begin
  if not HandleAllocated or not Assigned(chkApprove) then Exit;
  LMeasure := TControlCanvas.Create;
  // Changing a wrapped LCL label's width can realign its siblings before its
  // new anchor bounds are stored. Commit the complete layout as one update.
  gbSkipped.DisableAlign;
  try
    LContent := gbSkipped.ClientRect;
    TGroupBoxContentAccess(gbSkipped).AdjustClientRect(LContent);
    LMeasure.Control := gbSkipped;
    LMeasure.Font.Assign(gbSkipped.Font);
    LGap := Max(8, LMeasure.TextHeight('Mg') * 2 div 3);
    LTop := LContent.Top + LGap;
    LWidth := Max(1, LContent.Right - LContent.Left - 2 * LGap);
    // Measure each control's actual font and wrapping after native parenting.
    PositionLabel(lblSummary, 3);
    PositionLabel(lblRemaining, 2);
    PositionLabel(lblFees, 2);
    LMeasure.Control := chkApprove;
    LMeasure.Font.Assign(chkApprove.Font);
    chkApprove.SetBounds(LContent.Left + LGap, LTop, LWidth,
      Max(chkApprove.Height, LMeasure.TextHeight('Mg') + 6));
  finally
    try
      gbSkipped.EnableAlign;
    finally
      LMeasure.Free;
    end;
  end;
end;

procedure TWIZChangeAccountInfo_Skipped.OnPresent;
var
  LCount: Integer;
begin
  LCount := Model.ChangeAccountInfo.SkippedCount;
  if LCount = 1 then
    lblSummary.Caption := Format('1 selected account already has type %d and will be skipped.',
      [Model.ChangeAccountInfo.NewType])
  else
    lblSummary.Caption := Format('%d selected accounts already have type %d and will be skipped.',
      [LCount, Model.ChangeAccountInfo.NewType]);
  LCount := Length(Model.ChangeAccountInfo.TargetAccounts);
  if LCount = 0 then
    lblRemaining.Caption := 'No accounts remain to change. Go back and choose another type.'
  else if LCount = 1 then
    lblRemaining.Caption := '1 account will be changed.'
  else
    lblRemaining.Caption := Format('%d accounts will be changed.', [LCount]);
  lblFees.Caption := 'Fees apply only to accounts being updated. No operation fee is charged for skipped changes.';
  // Returning to this decision requires a fresh explicit approval.
  Model.ChangeAccountInfo.SkippedAccountsApproved := False;
  chkApprove.Checked := False;
  chkApprove.Enabled := LCount > 0;
  FormResize(Self);
end;

procedure TWIZChangeAccountInfo_Skipped.OnNext;
begin
  Model.ChangeAccountInfo.SkippedAccountsApproved := chkApprove.Checked;
end;

function TWIZChangeAccountInfo_Skipped.Validate(out message: String): Boolean;
begin
  message := '';
  Result := False;
  if Length(Model.ChangeAccountInfo.TargetAccounts) = 0 then
  begin
    message := 'No accounts remain to change. Go back and choose another type.';
    Exit;
  end;
  if not chkApprove.Checked then
  begin
    message := 'Approve skipping the unchanged accounts before continuing.';
    Exit;
  end;
  Result := True;
end;

end.
