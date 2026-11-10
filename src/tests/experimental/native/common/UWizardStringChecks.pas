unit UWizardStringChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$IFDEF FPC}{$mode delphi}{$H+}{$codepage UTF8}{$ENDIF}

interface

uses Classes;

procedure CheckNativeWizardStrings(ALog: TStrings);

implementation

uses SysUtils, UMemory, UWizard;

const
  CT_NativeText: String = 'Résumé — 漢字';

type
  TStringScreen = class(TWizardForm<Integer>)
  public
    RejectValidation: Boolean;
    constructor Create(AOwner: TComponent); override;
    function Validate(out message: String): Boolean; override;
  end;

  TStringWizard = class(TWizard<Integer>)
  private
    FAllowCancel: Boolean;
    FFinishCalls: Integer;
  protected
    procedure ShowHost; override;
    procedure ShowError(const message: String); override;
    function FinishRequested(out message: String): Boolean; override;
    function CancelRequested(out message: String): Boolean; override;
  public
    constructor Create;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

constructor TStringScreen.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  RejectValidation := True;
end;

function TStringScreen.Validate(out message: String): Boolean;
begin
  message := CT_NativeText;
  Result := not RejectValidation;
end;

constructor TStringWizard.Create;
begin
  inherited Create(nil, [TStringScreen]);
  TitleText := CT_NativeText;
  FinishText := CT_NativeText;
end;

procedure TStringWizard.ShowHost;
var LMessage: String;
begin
  // Exercise real host captions and all out-message paths using native String.
  Require(Host.TitleText = CT_NativeText, 'Wizard title lost native text');
  Require(Host.NextText = CT_NativeText, 'Wizard button lost native text');
  Require(not Finish(LMessage), 'Validation must reject finishing');
  Require((LMessage = CT_NativeText) and (FFinishCalls = 0),
    'Validation message changed or executed the finish callback');
  TStringScreen(CurrentScreen).RejectValidation := False;
  Require(not Finish(LMessage), 'Finish callback must reject finishing');
  Require((LMessage = CT_NativeText) and (FFinishCalls = 1),
    'Finish message changed or its callback ran more than once');
  Require(not Cancel(LMessage), 'Cancel callback must reject cancellation');
  Require(LMessage = CT_NativeText, 'Cancel message lost native text');
  FAllowCancel := True;
  Require(Cancel(LMessage), 'Accepted cancellation did not complete');
end;

procedure TStringWizard.ShowError(const message: String);
begin
  raise Exception.Create('Unexpected wizard dialog: ' + message);
end;

function TStringWizard.FinishRequested(out message: String): Boolean;
begin
  Inc(FFinishCalls);
  message := CT_NativeText;
  Result := False;
end;

function TStringWizard.CancelRequested(out message: String): Boolean;
begin
  message := CT_NativeText;
  Result := FAllowCancel;
end;

procedure CheckNativeWizardStrings(ALog: TStrings);
var LScope: TDisposables; LWizard: TStringWizard;
begin
  LWizard := LScope.AddObject(TStringWizard.Create) as TStringWizard;
  LWizard.Start(1);
  Require(LWizard.State = wsCancelled, 'Wizard did not retain its terminal state');
  ALog.Add('PASS native String wizard captions, validation, finish and cancel messages');
end;

end.
