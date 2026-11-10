unit UWalletSelectionLoadingChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Uses the real native account selection, wallet source binding and grid queue.
  Only the persisted account/history records come from the existing mock model. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UWalletGridMocks;

procedure CheckWalletSelectionLoadingAndBanner(AMain: TMockMainForm; const ALogPath: String);
procedure CheckWalletSelectionCompletion(AMain: TMockMainForm; const ALogPath: String);
procedure CheckWalletRefreshLifecycle(AMain: TMockMainForm; const ALogPath: String);

implementation

uses Windows, Messages, Classes, SysUtils, Types, Variants, Forms, Controls,
  StdCtrls, ExtCtrls, Graphics, UVisualGrid, UCommon.UI, UNode,
  UUserInterface, UCTRLBanner, UCTRLSyncronization, UAccounts;

procedure Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure PumpFor(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure Log(const ALogPath, AMessage: String);
var LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    if FileExists(ALogPath) then LLines.LoadFromFile(ALogPath);
    LLines.Add(FormatDateTime('hh:nn:ss.zzz',Now)+' '+AMessage);
    LLines.SaveToFile(ALogPath);
  finally LLines.Free; end;
end;

procedure ClickAccount(AMain: TMockMainForm; ARow: Integer);
var LCell: TRect; LPoint: LPARAM;
begin
  LCell := AMain.MockWallet.AccountsGrid.InternalDrawGrid.CellRect(0,ARow+1);
  LPoint := LPARAM(((LCell.Top+LCell.Bottom) div 2) shl 16) or
    LPARAM((LCell.Left+LCell.Right) div 2);
  SendMessage(AMain.MockWallet.AccountsGrid.InternalDrawGrid.Handle,
    WM_MOUSEMOVE,0,LPoint);
  SendMessage(AMain.MockWallet.AccountsGrid.InternalDrawGrid.Handle,
    WM_LBUTTONDOWN,MK_LBUTTON,LPoint);
  SendMessage(AMain.MockWallet.AccountsGrid.InternalDrawGrid.Handle,
    WM_LBUTTONUP,0,LPoint);
end;

procedure AwaitGrid(AGrid: TVisualGrid; AWaitForQuiet: Boolean);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64+10000;
  while (AGrid.IsUpdating or (AWaitForQuiet and AGrid.RefreshThrottle.Waiting)) and
    (GetTickCount64<LUntil) do PumpFor(5);
  Require(not AGrid.IsUpdating,'Native provider did not finish');
  if AWaitForQuiet then Require(not AGrid.RefreshThrottle.Waiting,'Native throttle did not finish');
end;

procedure CheckBanner(AMain: TMockMainForm; const ALogPath: String);
var LBanner: TCTRLBanner; I, LLeft, LRight, LWidth: Integer;
  LImage, LWithoutLabel: TBitmap; LOriginalWidth, X, Y, LChangedPixels: Integer;
  LOriginalNetworkActive: Boolean; LPeer: TObject; LUntil: UInt64;

  function PaintBanner: TBitmap;
  begin
    Result := TBitmap.Create;
    Result.PixelFormat := pf24bit;
    Result.SetSize(LBanner.ClientWidth,LBanner.ClientHeight);
    Result.Canvas.Brush.Color := clFuchsia;
    Result.Canvas.FillRect(Types.Rect(0,0,Result.Width,Result.Height));
    // LCL GetFormImage adds PRF_CHECKVISIBLE and can return black on an owned
    // inactive desktop. VCL PaintTo additionally paints graphic children which
    // its plain WM_PRINT handler omits. Both execute actual control painting.
    {$IFDEF FPC}
    SendMessage(LBanner.Handle,WM_PRINT,WPARAM(Result.Canvas.Handle),
      PRF_CLIENT or PRF_CHILDREN or PRF_ERASEBKGND);
    {$ELSE}
    LBanner.PaintTo(Result.Canvas.Handle,0,0);
    {$ENDIF}
  end;

  procedure CheckBounds(const ACheckpoint: String);
  begin
    LBanner.FormResize(LBanner);
    Require(LBanner.NodeStatusLabel.Parent=LBanner,ACheckpoint+': label is not a banner child');
    LLeft := LBanner.lblClientVersion.Left+LBanner.lblClientVersion.Width+LBanner.imLogo.Left;
    LRight := LBanner.cbLanguage.Left-LBanner.imLogo.Left;
    LWidth := LBanner.NodeStatusLabel.Width;
    Require(LBanner.NodeStatusLabel.Visible and (LWidth>0),ACheckpoint+': status label is not visible');
    Require((LBanner.NodeStatusLabel.Left>=LLeft) and
      (LBanner.NodeStatusLabel.Left+LWidth<=LRight),ACheckpoint+': status overlaps brand or language selector');
    Require((LBanner.NodeStatusLabel.Top>=0) and
      (LBanner.NodeStatusLabel.Top+LBanner.NodeStatusLabel.Height<=LBanner.ClientHeight),
      ACheckpoint+': status extends outside banner');
    Require(LBanner.NodeStatusLabel.Left=LLeft,ACheckpoint+': status must start after the brand');
    Require(LBanner.NodeStatusLabel.Alignment=taLeftJustify,
      ACheckpoint+': status text must be left-aligned');
    if TUserInterface.State=uisActive then
      Require(LBanner.NodeStatusLabel.Caption='',ACheckpoint+': normal online status must be blank')
    else
      Require(LBanner.NodeStatusLabel.Caption=TCTRLSyncronization.CurrentNodeStatusText,
        ACheckpoint+': banner text disagrees with synchronization presentation');
    Require(LBanner.NodeStatusLabel.Font.Color=clWhite,ACheckpoint+': status must be white');
    Require(LBanner.NodeStatusLabel.Font.Size>=16,ACheckpoint+': status font must be at least 16 points');
    Log(ALogPath,Format('BANNER %s client=%d label=%d,%d,%d,%d caption=%s',
      [ACheckpoint,LBanner.ClientWidth,LBanner.NodeStatusLabel.Left,LBanner.NodeStatusLabel.Top,
       LBanner.NodeStatusLabel.Width,LBanner.NodeStatusLabel.Height,LBanner.NodeStatusLabel.Caption]));
  end;

begin
  LBanner := nil;
  for I := 0 to AMain.ComponentCount-1 do
    if AMain.Components[I] is TCTRLBanner then LBanner := TCTRLBanner(AMain.Components[I]);
  Require(Assigned(LBanner),'Actual main form has no banner');
  LOriginalNetworkActive := TUserInterface.Node.GetSynchronizationStatus.NetworkActive;
  Require(not LOriginalNetworkActive,'Banner fixture must start with its owned offline node');
  LPeer := TObject.Create;
  try
    // Publish real cached node status without opening sockets. Assigning the
    // UI state directly is invalid: the details view restores offline status.
    TUserInterface.Node.SetSynchronizationNetworkActive(True);
    TUserInterface.Node.SetSynchronizationPeerConnected(LPeer,True);
    LUntil := GetTickCount64+3000;
    while (TUserInterface.State<>uisActive) and (GetTickCount64<LUntil) do PumpFor(5);
    Require(TUserInterface.State=uisActive,'Connected idle node did not publish online state');
    Require(LBanner.NodeStatusLabel.Caption='',
      'Normal online transition did not clear the banner');
    Require(TCTRLSyncronization.CurrentNodeStatusText='Active',
      'Banner-only suppression changed the synchronization details');
  finally
    TUserInterface.Node.SetSynchronizationPeerConnected(LPeer,False);
    TUserInterface.Node.SetSynchronizationNetworkActive(LOriginalNetworkActive);
    LPeer.Free;
    PumpFor(250);
  end;
  Log(ALogPath,'PASS normal online state clears only the banner; status is white, left-aligned and enlarged');
  CheckBounds('normal');
  LOriginalWidth := AMain.Width;
  AMain.Width := 1640;
  PumpFor(100);
  CheckBounds('wide');
  AMain.Width := 1040;
  PumpFor(100);
  CheckBounds('narrow');
  AMain.Width := LOriginalWidth;
  LBanner.ScaleBy(144,96);
  LBanner.FormResize(LBanner);
  PumpFor(100);
  CheckBounds('144dpi');
  LBanner.ScaleBy(96,144);
  LBanner.FormResize(LBanner);
  PumpFor(100);
  CheckBounds('96dpi-return');

  TUserInterface.Node.BeginBlockChainUpdate;
  try
    PumpFor(250);
    Require(LBanner.NodeStatusLabel.Caption='Synchronizing',
      'Banner did not immediately follow the actual node synchronization event');
    CheckBounds('synchronizing-event');
  finally TUserInterface.Node.EndBlockChainUpdate; end;
  PumpFor(250);
  CheckBounds('synchronization-finished-event');
  Require(LBanner.NodeStatusLabel.Caption<>'Synchronizing',
    'Banner retained synchronization after the actual empty scope finished');
  LImage := PaintBanner;
  LWithoutLabel := nil;
  try
    LBanner.NodeStatusLabel.Visible := False;
    try LWithoutLabel := PaintBanner;
    finally LBanner.NodeStatusLabel.Visible := True; end;
    LChangedPixels := 0;
    Y := LBanner.NodeStatusLabel.Top;
    while Y<LBanner.NodeStatusLabel.Top+LBanner.NodeStatusLabel.Height do begin
      X := LBanner.NodeStatusLabel.Left;
      while X<LBanner.NodeStatusLabel.Left+LBanner.NodeStatusLabel.Width do begin
        if LImage.Canvas.Pixels[X,Y]<>LWithoutLabel.Canvas.Pixels[X,Y] then Inc(LChangedPixels);
        Inc(X,2);
      end;
      Inc(Y,2);
    end;
    LImage.SaveToFile(ChangeFileExt(ALogPath,'.banner-native.bmp'));
    LWithoutLabel.SaveToFile(ChangeFileExt(ALogPath,'.banner-without-label.bmp'));
    Require(LChangedPixels>5,'Native status text did not paint; blank/black captures are not visual proof');
  finally LWithoutLabel.Free; LImage.Free; end;
  Log(ALogPath,'PASS banner native parent, left-aligned bounds, white enlarged text, normal/wide/narrow and both DPI directions; event-owned caption; label pixels='+IntToStr(LChangedPixels));
end;

procedure CheckOverviewLoading(AMain: TMockMainForm; const ALogPath: String);
var LUntil: UInt64; LPreviousPASC,LPreviousPASA: String;

  procedure AwaitTotal(const AExpected: String);
  begin
    LUntil := GetTickCount64+9000;
    while (AMain.MockWallet.lblTotalPASA.Caption<>AExpected) and
      (GetTickCount64<LUntil) do PumpFor(5);
    Require(AMain.MockWallet.lblTotalPASA.Caption=AExpected,'Completed overview did not publish '+AExpected);
  end;

begin
  Require((AMain.MockWallet.lblTotalPASC.Caption='-') and
    (AMain.MockWallet.lblTotalPASA.Caption='-'),
    'Overview displayed a number before its first completed balance snapshot');
  Log(ALogPath,'PASS initial Total PASC/PASA remain dashes while the real first summary worker is held');
  AMain.Model.ReleaseBalanceSnapshots;
  AwaitTotal('200');
  LPreviousPASC := AMain.MockWallet.lblTotalPASC.Caption;
  LPreviousPASA := AMain.MockWallet.lblTotalPASA.Caption;
  Require(LPreviousPASC<>'-','First completed nonzero balance remained unknown');
  AMain.Model.HoldBalanceSnapshots;
  try
    AMain.Model.SetData(6000,0);
    TUserInterface.Node.NotifyBlocksChanged;
    PumpFor(100);
    AwaitGrid(AMain.MockWallet.AccountsGrid,True);
    AwaitGrid(AMain.MockWallet.OperationsGrid,True);
    Require((AMain.MockWallet.lblTotalPASC.Caption=LPreviousPASC) and
      (AMain.MockWallet.lblTotalPASA.Caption=LPreviousPASA),
      'A newer held summary replaced the last completed nonzero totals');
    Log(ALogPath,'PASS completed nonzero overview is retained while a newer real summary worker is held');
  finally AMain.Model.ReleaseBalanceSnapshots; end;
  AwaitTotal('0');
  Require(AMain.MockWallet.lblTotalPASC.Caption=TAccountComp.FormatMoney(0),
    'A genuine completed zero balance was displayed as unknown');
  Log(ALogPath,'PASS an actual completed empty summary may publish numeric zero');
  AMain.Model.SetData(6001,200);
  TUserInterface.Node.NotifyBlocksChanged;
  PumpFor(100);
  AwaitGrid(AMain.MockWallet.AccountsGrid,True);
  AwaitGrid(AMain.MockWallet.OperationsGrid,True);
  AwaitTotal('200');
end;

procedure RequireSelectedAccount(AMain: TMockMainForm; AAccount: Cardinal);
var
  I: Integer;
  LAccounts: TArray<Cardinal>;
begin
  LAccounts := AMain.MockWallet.OperationsSource.Accounts;
  Require(not AMain.MockWallet.OperationsSource.UseWalletAccounts and
    (Length(LAccounts) = 1) and (LAccounts[0] = AAccount),
    'Actual selection did not bind precisely its selected account');
  Require(AMain.MockWallet.OperationsGrid.RowCount > 0,
    'The selected account has no accepted native history rows');
  for I := 0 to AMain.MockWallet.OperationsGrid.RowCount - 1 do
    Require(Cardinal(AMain.MockWallet.OperationsGrid.Rows[I].AccountNumber) = AAccount,
      'Accepted history belongs to an obsolete account selection');
end;

procedure CheckWalletSelectionCompletion(AMain: TMockMainForm; const ALogPath: String);
var
  LGrid: TVisualGrid;
  LPanel: TPanel;
  LCircle: TLoadingCircle;
  LBefore, LAccount: Integer;
  LStarted, LUntil: UInt64;
begin
  LGrid := AMain.MockWallet.OperationsGrid;
  AwaitGrid(AMain.MockWallet.AccountsGrid, True);
  AwaitGrid(LGrid, True);
  Require((AMain.MockWallet.AccountsGrid.RefreshThrottle.Mode = temNone) and
    (LGrid.RefreshThrottle.Mode = temNotifyOnEventBurstFinished) and
    (LGrid.RefreshThrottle.Interval.TotalMilliseconds=250),
    'Accounts must admit immediately; operations require 250 ms burst silence');
  AMain.MockWallet.OperationsSource.OperationsPerAccount := 268;
  AMain.Model.DelayMilliseconds := 0;
  LPanel := TPanel(LGrid.FindComponent('LoadingOverlay'));
  LCircle := TLoadingCircle(LGrid.FindComponent('LoadingCircle'));
  for LAccount := 0 to 1 do
  begin
    LBefore := AMain.Model.Metrics(mdkOperations).FetchCount;
    LStarted := GetTickCount64;
    AMain.Model.GateFetches := True;
    try
      ClickAccount(AMain, LAccount);
      PumpFor(100);
      Require(LGrid.RefreshThrottle.Waiting and
        (AMain.Model.Metrics(mdkOperations).FetchCount=LBefore),
        'Native selection queried operations before the 250 ms quiet interval');
      LUntil := GetTickCount64 + 1000;
      while (AMain.Model.Metrics(mdkOperations).FetchCount = LBefore) and
        (GetTickCount64 < LUntil) do
        PumpFor(5);
      Require(LGrid.IsUpdating and
        (AMain.Model.Metrics(mdkOperations).FetchCount = LBefore + 1),
        'Native selection did not admit exactly one provider after its quiet interval');
      Require(GetTickCount64-LStarted>=250,'Native selection bypassed operations quiet admission');
      PumpFor(250);
      Require(LPanel.Visible and LCircle.Active,
        'A genuinely held selected-account query has no animated loading feedback');
      Require(not LGrid.RefreshThrottle.Waiting, 'The operations quiet timer did not finish before its held provider ran');
    finally
      AMain.Model.ReleaseFetches;
    end;
    AwaitGrid(LGrid, True);
    Require(GetTickCount64 - LStarted < 2000,
      'Fast selected-account history waited for an obsolete throttle interval');
    RequireSelectedAccount(AMain, Cardinal(LAccount));
    Require(not LPanel.Visible and not LCircle.Active,
      'Accepted selected-account rows retained automatic loading');
    PumpFor(50);
    Require((AMain.Model.Metrics(mdkOperations).FetchCount = LBefore + 1) and
      not LPanel.Visible and not LCircle.Active,
      'Selection completion duplicated its query or reopened loading');
    Log(ALogPath, Format('PASS native account=%d after 250 ms quiet: 268 operations, accepted page=%d rows, completion=%dms, loading cleared on publication',
      [LAccount, LGrid.RowCount, GetTickCount64 - LStarted]));
  end;
end;

procedure CheckWalletRefreshLifecycle(AMain: TMockMainForm; const ALogPath: String);
var
  A, O: TVisualGrid;
  LA, LO: TMockFetchMetrics;
  LBeforeA, LBeforeO, LRevision, I: Integer;
  LStart, LUntil: UInt64;
  LScopeActive: Boolean;

  function CurrentRevisionVisible: Boolean;
  var
    LPrefix: String;
  begin
    Result := False;
    if A.IsUpdating or O.IsUpdating or A.RefreshThrottle.Waiting or O.RefreshThrottle.Waiting or
      (A.RowCount = 0) or (O.RowCount = 0) then
      Exit;
    LPrefix := 'mock revision=' + IntToStr(LRevision) + ' ';
    Result := (Pos(LPrefix, String(A.Rows[0].Name)) = 1) and
      (Pos(LPrefix, String(O.Rows[0].Description)) = 1);
  end;

  procedure AwaitRevision;
  begin
    LUntil := GetTickCount64 + 3000;
    while not CurrentRevisionVisible and (GetTickCount64 < LUntil) do
      PumpFor(5);
    Require(CurrentRevisionVisible, 'Real wallet grids did not publish the latest backend revision');
    Require(not A.RefreshThrottle.Waiting and not O.RefreshThrottle.Waiting,
      'The current pages did not settle after their configured operations quiet interval');
  end;

  procedure RequireSyncCards;
  begin
    Require(A.RefreshSuspended and O.RefreshSuspended and A.LoadingVisible and O.LoadingVisible,
      'Actual node synchronization did not own both grid pauses/cards');
    Require((TLabel(A.FindComponent('LoadingStatus')).Caption = 'Please Wait') and
      (TLabel(O.FindComponent('LoadingStatus')).Caption = 'Please Wait') and
      (TLabel(A.FindComponent('LoadingSubtitle')).Caption = 'Synchronizing') and
      (TLabel(O.FindComponent('LoadingSubtitle')).Caption = 'Synchronizing'),
      'Node-owned synchronization card text was replaced by ordinary loading');
  end;

begin
  A := AMain.MockWallet.AccountsGrid;
  O := AMain.MockWallet.OperationsGrid;
  AMain.Model.ReleaseFetches;
  AMain.Model.DelayMilliseconds := 0;
  AMain.MockWallet.OperationsSource.OperationsPerAccount := 268;
  AwaitGrid(A, False);
  AwaitGrid(O, False);
  Require((A.RefreshThrottle.Mode = temNone) and (O.RefreshThrottle.Mode = temNotifyOnEventBurstFinished) and
    (O.RefreshThrottle.Interval.TotalMilliseconds=250),
    'Lifecycle checks require immediate accounts and 250 ms quiet operations');
  LBeforeA := AMain.Model.Metrics(mdkAccounts).FetchCount;
  LBeforeO := AMain.Model.Metrics(mdkOperations).FetchCount;
  AMain.Model.Advance(200);
  LRevision := AMain.Model.Revision;
  LStart := GetTickCount64;
  TUserInterface.Node.NotifyBlocksChanged;
  AwaitRevision;
  Require(GetTickCount64 - LStart < 2000, 'An idle backend event waited for five-second admission');
  Require((AMain.Model.Metrics(mdkAccounts).FetchCount > LBeforeA) and
    (AMain.Model.Metrics(mdkOperations).FetchCount > LBeforeO),
    'An idle backend event did not reach both production providers');
  Log(ALogPath, 'PASS idle backend event refreshes accounts immediately and operations after 250 ms quiet');

  // Ordinary backend inputs coalesce while one real provider is held. They do
  // not start parallel workers, and completion admits one latest pending page.
  LBeforeA := AMain.Model.Metrics(mdkAccounts).FetchCount;
  LBeforeO := AMain.Model.Metrics(mdkOperations).FetchCount;
  AMain.Model.GateFetches := True;
  try
    AMain.Model.Advance(200);
    TUserInterface.Node.NotifyBlocksChanged;
    LUntil := GetTickCount64 + 2000;
    repeat
      PumpFor(5);
      LA := AMain.Model.Metrics(mdkAccounts);
      LO := AMain.Model.Metrics(mdkOperations);
    until ((LA.ActiveFetches = 1) and (LO.ActiveFetches = 1)) or (GetTickCount64 >= LUntil);
    Require((LA.ActiveFetches = 1) and (LO.ActiveFetches = 1), 'Both actual providers were not held');
    for I := 1 to 4 do
    begin
      AMain.Model.Advance(200);
      TUserInterface.Node.NotifyBlocksChanged;
      PumpFor(60);
    end;
    LRevision := AMain.Model.Revision;
    PumpFor(150);
    Require((AMain.Model.Metrics(mdkAccounts).FetchCount = LBeforeA + 1) and
      (AMain.Model.Metrics(mdkOperations).FetchCount = LBeforeO + 1),
      'Backend input started another worker before the held provider retired');
  finally
    AMain.Model.ReleaseFetches;
  end;
  AwaitRevision;
  PumpFor(100);
  Require((AMain.Model.Metrics(mdkAccounts).FetchCount = LBeforeA + 2) and
    (AMain.Model.Metrics(mdkOperations).FetchCount = LBeforeO + 2),
    'Held backend inputs did not coalesce to exactly one latest replay');
  Log(ALogPath, 'PASS held old providers retain one latest backend request after operations quiet admission without parallel queries');

  LScopeActive := False;
  try
    TUserInterface.Node.BeginBlockChainUpdate;
    LScopeActive := True;
    LUntil := GetTickCount64 + 2000;
    while (not A.RefreshSuspended or not O.RefreshSuspended) and
      (GetTickCount64 < LUntil) do
      PumpFor(5);
    RequireSyncCards;
    LBeforeA := AMain.Model.Metrics(mdkAccounts).FetchCount;
    LBeforeO := AMain.Model.Metrics(mdkOperations).FetchCount;
    for I := 1 to 6 do
    begin
      AMain.Model.Advance(200);
      TUserInterface.Node.NotifyBlocksChanged;
      PumpFor(60);
      RequireSyncCards;
      Require((AMain.Model.Metrics(mdkAccounts).FetchCount = LBeforeA) and
        (AMain.Model.Metrics(mdkOperations).FetchCount = LBeforeO),
        'A provider queried during the node-owned synchronization pause');
    end;
    LRevision := AMain.Model.Revision;
    O.PageIndex := 1;
    PumpFor(100);
    RequireSyncCards;
    Require((AMain.Model.Metrics(mdkAccounts).FetchCount = LBeforeA) and
      (AMain.Model.Metrics(mdkOperations).FetchCount = LBeforeO),
      'A paused page change queried its provider');
    Log(ALogPath, 'PASS synchronization lifecycle holds both cards and performs zero provider queries despite backend/page inputs');
  finally
    if LScopeActive then
      TUserInterface.Node.EndBlockChainUpdate;
  end;
  LStart := GetTickCount64;
  AwaitRevision;
  Require(GetTickCount64 - LStart < 2000, 'Node completion imposed an obsolete quiet interval');
  Require(not A.RefreshSuspended and not O.RefreshSuspended and not A.LoadingVisible and not O.LoadingVisible,
    'Node completion did not release the actual manual pauses/cards');
  Require(O.PageIndex = 1, 'Synchronization resume lost the latest requested page');
  Require(not TPanel(A.FindComponent('LoadingOverlay')).Visible and
    not TPanel(O.FindComponent('LoadingOverlay')).Visible,
    'Current resumed pages retain an ordinary/manual/error loading card');
  PumpFor(200);
  Log(ALogPath, 'PASS node completion publishes the latest revision/page, settles operations quiet admission and releases synchronization cards');
end;

procedure CheckWalletSelectionLoadingAndBanner(AMain: TMockMainForm; const ALogPath: String);
begin
  CheckOverviewLoading(AMain, ALogPath);
  CheckWalletSelectionCompletion(AMain, ALogPath);
  CheckBanner(AMain, ALogPath);
end;

end.
