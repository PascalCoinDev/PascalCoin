unit UWalletConsecutiveSelectionChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>.
  Distributed under the MIT software license; see the accompanying LICENSE. }

{ Native regression for account-selection refreshes through the existing grid
  throttle. Independent selections publish within a short bound; a scroll burst
  fetches only its final account after 250 ms of silence. Records alone are
  synthetic; the wallet handler,
  typed providers, cache, worker and native controls remain wired. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UWalletGridMocks;

procedure CheckConsecutiveNativeSelections(AMain: TMockMainForm; const ALogPath: String);

implementation

uses Windows, Messages, Classes, SysUtils, Types, Variants, Generics.Collections,
  Forms, Controls, ExtCtrls,
  UCommon.UI, UVisualGrid;

type
  TConsecutiveSelectionObserver = class
  private
    FMain: TMockMainForm;
    FLog: TStringList;
    FPreviousFinished: TNotifyEvent;
    FIndex, FExpectedAccount, FBaselineFetches: Integer;
    FInputTick, FPublicationTick, FCardHideTick, FAdmissionTick, FProviderEndTick: UInt64;
    FPublications, FAllPublications, FIncorrectPublications: Integer;
    FWaitingBefore: Boolean;
    FCardReopened: Boolean;
    FSpacing: String;
    function CurrentRowsMatch: Boolean;
    procedure FinishedUpdating(Sender: TObject);
    procedure CheckCard;
    procedure Pump(AMilliseconds: Cardinal);
    procedure NativeClick(ARow: Integer);
    procedure CheckRapidBurst;
  public
    constructor Create(AMain: TMockMainForm);
    destructor Destroy; override;
    procedure Run(const ALogPath: String);
  end;

procedure Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then
    raise Exception.Create(AMessage);
end;

constructor TConsecutiveSelectionObserver.Create(AMain: TMockMainForm);
begin
  inherited Create;
  FMain := AMain;
  FLog := TStringList.Create;
  FPreviousFinished := FMain.MockWallet.OperationsGrid.OnFinishedUpdating;
  FMain.MockWallet.OperationsGrid.OnFinishedUpdating := FinishedUpdating;
end;

destructor TConsecutiveSelectionObserver.Destroy;
begin
  FMain.MockWallet.OperationsGrid.OnFinishedUpdating := FPreviousFinished;
  FLog.Free;
  inherited;
end;

function TConsecutiveSelectionObserver.CurrentRowsMatch: Boolean;
var
  I: Integer;
  LGrid: TVisualGrid;
begin
  Result := False;
  LGrid := FMain.MockWallet.OperationsGrid;
  if (LGrid.RowCount = 0) or
    (Length(FMain.MockWallet.OperationsSource.Accounts) <> 1) or
    (FMain.MockWallet.OperationsSource.Accounts[0] <> Cardinal(FExpectedAccount)) then
    Exit;
  for I := 0 to LGrid.RowCount - 1 do
    if Cardinal(LGrid.Rows[I].AccountNumber) <> Cardinal(FExpectedAccount) then
      Exit;
  Result := True;
end;

procedure TConsecutiveSelectionObserver.CheckCard;
var
  LPanel: TPanel;
  LCircle: TLoadingCircle;
begin
  if (FIndex = 0) or (FPublicationTick = 0) then
    Exit;
  LPanel := TPanel(FMain.MockWallet.OperationsGrid.FindComponent('LoadingOverlay'));
  LCircle := TLoadingCircle(FMain.MockWallet.OperationsGrid.FindComponent('LoadingCircle'));
  Require(Assigned(LPanel) and Assigned(LCircle), 'Real loading components are absent');
  if not LPanel.Visible and not LCircle.Active then
  begin
    if FCardHideTick = 0 then
      FCardHideTick := GetTickCount64;
  end
  else if FCardHideTick <> 0 then
    FCardReopened := True;
end;

procedure TConsecutiveSelectionObserver.FinishedUpdating(Sender: TObject);
var
  LMetrics: TMockFetchMetrics;
  LFirstAccount: String;
begin
  if Assigned(FPreviousFinished) then
    FPreviousFinished(Sender);
  if FIndex = 0 then
    Exit;
  LMetrics := FMain.Model.Metrics(mdkOperations);
  Inc(FAllPublications);
  LFirstAccount := '(empty)';
  if FMain.MockWallet.OperationsGrid.RowCount > 0 then
    LFirstAccount := VarToStr(FMain.MockWallet.OperationsGrid.Rows[0].AccountNumber);
  FLog.Add(Format('PUBLICATION click=%d elapsed_ms=%d rows=%d first_account=%s source_matches=%s fetches=%d waiting=%s',
    [FIndex, GetTickCount64 - FInputTick, FMain.MockWallet.OperationsGrid.RowCount,
     LFirstAccount,
     BoolToStr(CurrentRowsMatch, True), LMetrics.FetchCount,
     BoolToStr(FMain.MockWallet.OperationsGrid.RefreshThrottle.Waiting, True)]));
  if CurrentRowsMatch and (LMetrics.FetchCount > FBaselineFetches) and
    (LMetrics.LastStartedTick >= FInputTick) then
  begin
    Inc(FPublications);
    if FPublicationTick = 0 then
    begin
      FPublicationTick := GetTickCount64;
      FAdmissionTick := LMetrics.LastStartedTick;
      FProviderEndTick := LMetrics.LastCompletedTick;
    end;
    CheckCard;
  end
  else
  begin
    Inc(FIncorrectPublications);
    FLog.Add(Format('CALLBACK_VIOLATION click=%d stale_or_wrong_identity=True', [FIndex]));
  end;
end;

procedure TConsecutiveSelectionObserver.Pump(AMilliseconds: Cardinal);
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    CheckCard;
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TConsecutiveSelectionObserver.NativeClick(ARow: Integer);
var
  LCell: TRect;
  LPoint: LPARAM;
  LSelected: TArray<Variant>;
begin
  LCell := FMain.MockWallet.AccountsGrid.InternalDrawGrid.CellRect(0, ARow + 1);
  LPoint := LPARAM(((LCell.Top + LCell.Bottom) div 2) shl 16) or
    LPARAM((LCell.Left + LCell.Right) div 2);
  SendMessage(FMain.MockWallet.AccountsGrid.InternalDrawGrid.Handle, WM_MOUSEMOVE, 0, LPoint);
  SendMessage(FMain.MockWallet.AccountsGrid.InternalDrawGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPoint);
  SendMessage(FMain.MockWallet.AccountsGrid.InternalDrawGrid.Handle, WM_LBUTTONUP, 0, LPoint);
  LSelected := FMain.MockWallet.AccountsGrid.SelectedRows;
  Require((Length(LSelected) = 1) and (Cardinal(LSelected[0].__KEY) = Cardinal(ARow)),
    'Actual native click did not select precisely its intended account');
end;

procedure TConsecutiveSelectionObserver.CheckRapidBurst;
var I: Integer; LUntil: UInt64; LPanel: TPanel; LMetrics: TMockFetchMetrics;
begin
  FIndex := 21;
  FPublications := 0;
  FAllPublications := 0;
  FIncorrectPublications := 0;
  FPublicationTick := 0;
  FCardHideTick := 0;
  FAdmissionTick := 0;
  FCardReopened := False;
  FBaselineFetches := FMain.Model.Metrics(mdkOperations).FetchCount;
  LPanel := TPanel(FMain.MockWallet.OperationsGrid.FindComponent('LoadingOverlay'));
  Require(Assigned(LPanel),'Real operations loading panel is absent');
  Require(FMain.MockWallet.OperationsGrid.RefreshThrottle.Mode=temNotifyOnEventBurstFinished,
    'Actual wallet operations grid is not using its existing burst-finished throttle');
  for I := 0 to 5 do begin
    FExpectedAccount := I mod 2;
    FInputTick := GetTickCount64;
    NativeClick(FExpectedAccount);
    Pump(80);
    Require(FMain.Model.Metrics(mdkOperations).FetchCount=FBaselineFetches,
      'Operations provider was queried before account selection became quiet');
    Require(FPublicationTick=0,'An intermediate account was published during scrolling');
  end;
  while GetTickCount64-FInputTick<230 do begin
    Pump(2);
    Require(FMain.Model.Metrics(mdkOperations).FetchCount=FBaselineFetches,
      'Selection throttle did not reset its quiet interval on every input');
  end;
  LUntil := GetTickCount64+1500;
  while ((FPublicationTick=0) or (FCardHideTick=0)) and (GetTickCount64<LUntil) do Pump(2);
  Require((FPublicationTick<>0) and CurrentRowsMatch,'Final selected account was not published');
  LMetrics := FMain.Model.Metrics(mdkOperations);
  Require((LMetrics.FetchCount=FBaselineFetches+1) and (FPublications=1) and
    (FAllPublications=1) and (FIncorrectPublications=0),
    'Scroll burst did not coalesce to exactly one final-account fetch/publication');
  Require((FAdmissionTick>=FInputTick+240) and (FAdmissionTick<FInputTick+1000),
    'Latest operations fetch did not follow the configured quiet interval');
  Require((FCardHideTick<>0) and not FCardReopened,
    'Completed latest-account publication retained or reopened its loading card');
  FLog.Add(Format('PASS scroll burst: six native selections, one latest query after %dms, loading hidden after %dms',
    [FAdmissionTick-FInputTick,FCardHideTick-FInputTick]));
end;

procedure TConsecutiveSelectionObserver.Run(const ALogPath: String);
const
  ClickCount = 20;
  ImmediateLimitMilliseconds = 1000;
var
  I, LSlow, LWrongFetchCount, LWrongPublicationCount, LWrongIdentityCount, LWrongCardCount: Integer;
  LUntil, LAdmission, LProviderCompleted, LPublication, LCard: UInt64;
  LMetrics: TMockFetchMetrics;
  LSpacing: String;

  procedure FinalizePreviousClick;
  begin
    LMetrics := FMain.Model.Metrics(mdkOperations);
    Require(FAdmissionTick >= FInputTick, 'Provider admission belongs to an older input');
    Require(FProviderEndTick >= FAdmissionTick, 'Provider timing is inconsistent');
    LAdmission := FAdmissionTick - FInputTick;
    LProviderCompleted := FProviderEndTick - FInputTick;
    LPublication := FPublicationTick - FInputTick;
    LCard := FCardHideTick - FInputTick;
    CheckCard;
    FLog.Add(Format('CLICK_RESULT click=%d account=%d admission_ms=%d provider_end_ms=%d publication_ms=%d card_hide_ms=%d card_reopened=%s fetch_delta=%d current_publications=%d all_publications=%d wrong_identity=%d waiting_before=%s waiting_after=%s spacing=%s',
      [FIndex, FExpectedAccount, LAdmission, LProviderCompleted, LPublication, LCard,
       BoolToStr(FCardReopened, True),
       LMetrics.FetchCount - FBaselineFetches, FPublications, FAllPublications,
       FIncorrectPublications, BoolToStr(FWaitingBefore, True),
       BoolToStr(FMain.MockWallet.OperationsGrid.RefreshThrottle.Waiting, True), FSpacing]));
    if LPublication > ImmediateLimitMilliseconds then
    begin
      Inc(LSlow);
      FLog.Add(Format('FAIL click=%d current history publication exceeded new immediate-selection bound %dms: %dms',
        [FIndex, ImmediateLimitMilliseconds, LPublication]));
    end;
    if LMetrics.FetchCount <> FBaselineFetches + 1 then
      Inc(LWrongFetchCount);
    if (FPublications <> 1) or (FAllPublications <> 1) then
      Inc(LWrongPublicationCount);
    if FIncorrectPublications <> 0 then
      Inc(LWrongIdentityCount);
    if FCardReopened or (FCardHideTick - FPublicationTick > 200) then
      Inc(LWrongCardCount);
    Require(CurrentRowsMatch, 'Published native rows no longer match current selected account after inter-click observation');
  end;
begin
  LSlow := 0;
  LWrongFetchCount := 0;
  LWrongPublicationCount := 0;
  LWrongIdentityCount := 0;
  LWrongCardCount := 0;
  FLog.Add('CONTRACT operations RefreshThrottle burst-finished250ms only; existing loading behavior unchanged; no product selection timer');
  FLog.Add('SCOPE actual main/mock wallet composition and native clicks; only account/history records synthetic; no signing/network/data clone');
  FMain.Model.DelayMilliseconds := 0;
  FMain.MockWallet.OperationsSource.OperationsPerAccount := 268;
  try
    // Initial setup alone may settle to quiet. No per-click loop waits for it.
    LUntil := GetTickCount64 + 10000;
    while (FMain.MockWallet.AccountsGrid.IsUpdating or
      FMain.MockWallet.OperationsGrid.IsUpdating or
      FMain.MockWallet.AccountsGrid.RefreshThrottle.Waiting or
      FMain.MockWallet.OperationsGrid.RefreshThrottle.Waiting) and
      (GetTickCount64 < LUntil) do
      Pump(5);
    Require(not FMain.MockWallet.AccountsGrid.IsUpdating and
      not FMain.MockWallet.OperationsGrid.IsUpdating and
      not FMain.MockWallet.AccountsGrid.RefreshThrottle.Waiting and
      not FMain.MockWallet.OperationsGrid.RefreshThrottle.Waiting,
      'Initial real grids did not settle to quiet');
    for I := 1 to ClickCount do
    begin
      LSpacing := 'consecutive-after-current-publication';
      if (I = 17) or (I = 19) then
      begin
        LSpacing := 'spaced-5200ms-after-current-publication';
        Pump(5200);
      end
      else
        Pump(25);
      // Keep the preceding observer armed throughout spacing. Late callbacks
      // must not disappear merely because no new click has been sent yet.
      if FIndex <> 0 then
        FinalizePreviousClick;
      FIndex := I;
      FSpacing := LSpacing;
      FExpectedAccount := (I - 1) mod 2;
      FPublicationTick := 0;
      FCardHideTick := 0;
      FAdmissionTick := 0;
      FProviderEndTick := 0;
      FPublications := 0;
      FAllPublications := 0;
      FIncorrectPublications := 0;
      FCardReopened := False;
      LMetrics := FMain.Model.Metrics(mdkOperations);
      FBaselineFetches := LMetrics.FetchCount;
      FWaitingBefore := FMain.MockWallet.OperationsGrid.RefreshThrottle.Waiting;
      FInputTick := GetTickCount64;
      FLog.Add(Format('INPUT click=%d account=%d tick=%d waiting_before=%s spacing=%s',
        [I, FExpectedAccount, FInputTick, BoolToStr(FWaitingBefore, True), LSpacing]));
      NativeClick(FExpectedAccount);
      LUntil := GetTickCount64 + 8000;
      while ((FPublicationTick = 0) or (FCardHideTick = 0)) and
        (GetTickCount64 < LUntil) do
        Pump(2);
      Require(FPublicationTick <> 0, 'No current-account publication within eight-second observation bound');
      Require(FCardHideTick <> 0, 'Current-account publication retained its loading card');
      Require(CurrentRowsMatch, 'Published native rows do not match current selected account');
    end;
    // This is one final grace period, not a quiet wait between requested pages.
    // It detects duplicate trailing publications/queries after the last page.
    Pump(5200);
    FinalizePreviousClick;
    FLog.Add(Format('RESULT clicks=%d slow=%d wrong_fetch_count=%d wrong_publication_count=%d wrong_identity_count=%d wrong_card_count=%d',
      [ClickCount, LSlow, LWrongFetchCount, LWrongPublicationCount, LWrongIdentityCount, LWrongCardCount]));
    Require((LSlow = 0) and (LWrongFetchCount = 0) and (LWrongPublicationCount = 0) and
      (LWrongIdentityCount = 0) and (LWrongCardCount = 0),
      'Consecutive native selections failed the newly requested immediate-response policy');
    FLog.Add('PASS 20 independent selections: bounded publication, current rows, one fetch and no stale loading card');
    CheckRapidBurst;
  finally
    FIndex := 0;
    FLog.SaveToFile(ALogPath);
  end;
end;

procedure CheckConsecutiveNativeSelections(AMain: TMockMainForm; const ALogPath: String);
var
  LObserver: TConsecutiveSelectionObserver;
begin
  LObserver := TConsecutiveSelectionObserver.Create(AMain);
  try
    LObserver.Run(ALogPath);
  finally
    LObserver.Free;
  end;
end;

end.
