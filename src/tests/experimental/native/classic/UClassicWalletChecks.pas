unit UClassicWalletChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

function RunClassicWalletProbe: Integer;

implementation

uses
  Windows, Messages, Classes, SysUtils, SyncObjs, Forms, Graphics, UFRMWallet, UGridUtils,
  UAccounts, UBlockChain, UNode, UNetProtocol, UThread, UCrypto, UWallet,
  USettings, UAppParams, UConst, UPCDataTypes, UBaseTypes;

type
  // Access only: Application creates the unchanged production form class and
  // loads its real DFM/LFM. No renderer, loader or readiness method is replaced.
  TClassicAccess = class(TFRMWallet);
  TClassicAccounts = array of TAccount;
  TClassicGridAccess = class(TAccountsGrid)
  public
    function ReadDisplayedAccount(AAccountNumber: Integer; out AAccount: TAccount;
      out ABlocksCount, AAccountsCount: Integer): Boolean;
  end;
  TClassicChecks = class;

  TClassicBlockTask = class(TPCThread)
  private
    FChecks: TClassicChecks;
  protected
    procedure BCExecute; override;
  public
    Step, ContinueStep, Done: TEvent;
    Completed: LongInt;
    ErrorText: String;
    constructor Create(AChecks: TClassicChecks);
    destructor Destroy; override;
  end;

  TClassicChecks = class(TComponent)
  private
    FLog: TStringList;
    FLogFile, FDirectory, FOldDirectory, FExceptionText: String;
    FPreviousException: TExceptionEvent;
    FForm: TFRMWallet;
    FNode: TNode;
    FBlocks: array[0..4] of TPCOperationsComp;
    FTask: TClassicBlockTask;
    FActivationThread: TThread;
    FActivationHandle: THandle;
    FActivationDone: Boolean;
    FPreviousTerminate: TNotifyEvent;
    FSynchronizationPeer: TObject;
    FEmptyWatchKey: TAccountKey;
    FExpectedFirst, FExpectedSecond, FExpectedAll, FExpectedEmpty: TClassicAccounts;
    procedure Log(const AText: String);
    procedure Pump;
    procedure Await(AEvent: TEvent; AMilliseconds: Cardinal; const AReason: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure OnActivationTerminated(Sender: TObject);
    procedure LoadFixture;
    procedure SeedOwnedSettings;
    procedure CaptureActivation;
    procedure ObserveActivation;
    procedure JoinActivation;
    procedure CheckNoUnexpectedForm;
    procedure CheckFinalAccounts;
    procedure SelectWatchKey(AKeyIndex: Integer);
    procedure CheckDisplayedAccounts(const AExpected: TClassicAccounts;
      const APhase: String; AWhileSynchronizing: Boolean);
    procedure CheckKeySelectionDuringCatchup;
    procedure Capture(const AName: String);
    procedure CloseOwnedForm;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(const ALogFile: String);
    destructor Destroy; override;
    procedure Run;
  end;

procedure Require(ACondition: Boolean; const AReason: String);
begin
  if not ACondition then raise Exception.Create(AReason);
end;

function DesktopName(AHandle: THandle): String;
var LBuffer: array[0..255] of Char; LRequired: DWORD;
begin
  if not GetUserObjectInformation(AHandle, UOI_NAME, @LBuffer[0],
    SizeOf(LBuffer), LRequired) then RaiseLastOSError;
  Result := StrPas(PChar(@LBuffer[0]));
end;

function TClassicGridAccess.ReadDisplayedAccount(AAccountNumber: Integer;
  out AAccount: TAccount; out ABlocksCount, AAccountsCount: Integer): Boolean;
begin
  // Exercise the exact reader used by the unchanged production cell renderer.
  Result := BufferGetAccount(AAccountNumber, AAccount, ABlocksCount, AAccountsCount);
end;

constructor TClassicBlockTask.Create(AChecks: TClassicChecks);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FChecks := AChecks;
  Step := TEvent.Create(nil, True, False, '');
  ContinueStep := TEvent.Create(nil, False, False, '');
  Done := TEvent.Create(nil, True, False, '');
end;

destructor TClassicBlockTask.Destroy;
begin
  Terminate;
  ContinueStep.SetEvent;
  WaitFor;
  Done.Free;
  ContinueStep.Free;
  Step.Free;
  inherited;
end;

procedure TClassicBlockTask.BCExecute;
var I: Integer; LError: String; LOperations: TPCOperationsComp;
begin
  try
    try
      for I := 0 to High(FChecks.FBlocks) do begin
        if Terminated then Exit;
        // As in the normal network decoder, validation needs a transaction
        // bound to the actual current bank. The immutable file-only objects
        // intentionally have no bank or validation shortcut attached.
        LOperations := TPCOperationsComp.Create(FChecks.FNode.Bank);
        try
          LOperations.CopyFrom(FChecks.FBlocks[I]);
          if not FChecks.FNode.AddNewBlockChain(nil, LOperations, LError) then
            raise Exception.Create('Actual historic block ' + IntToStr(I) +
              ' failed at local count=' + IntToStr(FChecks.FNode.Bank.BlocksCount) + ': ' + LError);
        finally LOperations.Free; end;
        InterlockedExchange(Completed, I + 1);
        Step.SetEvent;
        Require(ContinueStep.WaitFor(30000) = wrSignaled,
          'Classic UI did not release its committed-block gate');
      end;
    except on E: Exception do ErrorText := E.ClassName + ': ' + E.Message +
      ' address=' + IntToHex(NativeUInt(ExceptAddr), SizeOf(Pointer) * 2); end;
  finally Done.SetEvent; end;
end;

constructor TClassicChecks.Create(const ALogFile: String);
begin
  inherited Create(nil);
  FLogFile := ExpandFileName(ALogFile);
  FLog := TStringList.Create;
  FSynchronizationPeer := TObject.Create;
  FOldDirectory := TNode.GetPascalCoinDataFolder;
  FDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(FLogFile)) + 'classic-owned-data';
  Require(not DirectoryExists(FDirectory), 'Refusing an existing Classic data directory');
  Require(ForceDirectories(FDirectory), 'Cannot create the owned Classic data directory');
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TClassicChecks.Destroy;
var I: Integer; LUntil: UInt64;
begin
  // Finish test-owned producers before the real form frees its node and bank.
  CaptureActivation;
  if Assigned(FTask) then begin
    FTask.Terminate;
    FTask.ContinueStep.SetEvent;
    LUntil := GetTickCount64 + 15000;
    while (FTask.Done.WaitFor(0) <> wrSignaled) and (GetTickCount64 < LUntil) do begin
      CheckSynchronize(1);
      Application.ProcessMessages;
    end;
    FTask.Free;
  end;
  if Assigned(FActivationThread) and not FActivationDone then
    FActivationThread.Terminate;
  JoinActivation;
  if Assigned(FForm) then begin
    if Assigned(FNode) then FNode.SetSynchronizationNetworkActive(False);
    FForm.Free;
    FRMWallet := nil;
  end;
  if FActivationHandle <> 0 then CloseHandle(FActivationHandle);
  Application.OnException := FPreviousException;
  for I := Low(FBlocks) to High(FBlocks) do FBlocks[I].Free;
  FSynchronizationPeer.Free;
  TNode.SetPascalCoinDataFolder(FOldDirectory);
  FLog.Free;
  inherited;
end;

procedure TClassicChecks.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FForm) then FForm := nil;
end;

procedure TClassicChecks.Log(const AText: String);
begin
  FLog.Add(UIntToStr(GetTickCount64) + ' ' + AText);
  FLog.SaveToFile(FLogFile);
end;

procedure TClassicChecks.OnException(Sender: TObject; E: Exception);
{$IFDEF FPC}var I: Integer;{$ENDIF}
begin
  FExceptionText := E.ClassName + ': ' + E.Message;
  Log('APPLICATION_EXCEPTION ' + FExceptionText + ' address=' + IntToHex(NativeUInt(ExceptAddr), SizeOf(Pointer)*2));
  {$IFDEF FPC}
  Log(BackTraceStrFunc(ExceptAddr));
  for I := 0 to ExceptFrameCount - 1 do Log(BackTraceStrFunc(ExceptFrames[I]));
  {$ENDIF}
end;

procedure TClassicChecks.CheckNoUnexpectedForm;
var I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    Require((Screen.Forms[I] = FForm) or not Screen.Forms[I].Visible,
      'Unexpected visible Classic dialog: ' + Screen.Forms[I].ClassName);
end;

procedure TClassicChecks.Pump;
begin
  CaptureActivation;
  try
    CheckSynchronize(1);
    Application.ProcessMessages;
  finally CaptureActivation; end;
  Require(FExceptionText = '', 'Classic application raised ' + FExceptionText);
  CheckNoUnexpectedForm;
  Sleep(1);
end;

procedure TClassicChecks.Await(AEvent: TEvent; AMilliseconds: Cardinal;
  const AReason: String);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    Pump;
    if AEvent.WaitFor(0) = wrSignaled then Exit;
    if Assigned(FTask) and (FTask.Done.WaitFor(0) = wrSignaled) then
      raise Exception.Create(AReason + '; worker=' + FTask.ErrorText);
  until GetTickCount64 >= LUntil;
  if Assigned(FTask) then
    raise Exception.Create(AReason + '; worker=' + FTask.ErrorText)
  else raise Exception.Create(AReason);
end;

procedure TClassicChecks.LoadFixture;
var LFile: TFileStream; LCount: Cardinal; I: Integer; LError, LFileName: String;
begin
  LFileName := ExpandFileName(GetEnvironmentVariable('PASCAL_CLASSIC_BLOCK_FIXTURE'));
  Require(FileExists(LFileName), 'Supply the public five-block fixture via PASCAL_CLASSIC_BLOCK_FIXTURE');
  LFile := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyNone);
  try
    LFile.ReadBuffer(LCount, SizeOf(LCount));
    Require(LCount = 5, 'Expected the five provenance-checked historic mainnet blocks');
    for I := Low(FBlocks) to High(FBlocks) do begin
      FBlocks[I] := TPCOperationsComp.Create(nil);
      Require(FBlocks[I].LoadBlockFromStream(LFile, LError), LError);
      Require(FBlocks[I].OperationBlock.block = Cardinal(I), 'Historic block order differs');
    end;
    Require(LFile.Position = LFile.Size, 'Unexpected historic fixture suffix');
  finally LFile.Free; end;
end;

procedure TClassicChecks.SeedOwnedSettings;
var LParams: TAppParams; LKeys: TWalletKeysExt; LGeneratedKey: TECPrivateKey;
  I: Integer;
begin
  // The real FormCreate calls TSettings.Load once. Seed its own file directly;
  // never invoke the experimental CLI parser or the user's default data path.
  TNode.SetPascalCoinDataFolder(FDirectory);
  LParams := TAppParams.Create(nil);
  try
    LParams.FileName := IncludeTrailingPathDelimiter(FDirectory) + 'AppParams.prm';
    LParams.ParamByName[CT_PARAM_FirstTime].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(0);
    LParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].SetAsInteger(0);
    LParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString('127.0.0.1:1');
    LParams.ParamByName[CT_PARAM_PeerCache].SetAsString('127.0.0.1:1');
    LParams.ParamByName[CT_PARAM_ShowModalMessages].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(Ord(mpk_Selected));
  finally LParams.Free; end;
  // Derive an unowned public key in memory, then discard its private material
  // before the actual wallet is constructed. Persist only its public half.
  LGeneratedKey := TECPrivateKey.Create;
  try
    LGeneratedKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    FEmptyWatchKey := LGeneratedKey.PublicKey.GetCopy;
    for I := Low(FBlocks) to High(FBlocks) do
      Require(not TAccountComp.EqualAccountKeys(FEmptyWatchKey,
        FBlocks[I].OperationBlock.account_key),
        'The empty public watch key must not own a fixture account');
  finally
    LGeneratedKey.Free;
  end;
  LKeys := TWalletKeysExt.Create(nil);
  try
    LKeys.WalletFileName := IncludeTrailingPathDelimiter(FDirectory) + 'WalletKeys.dat';
    Require(not TAccountComp.EqualAccountKeys(FBlocks[0].OperationBlock.account_key,
      FBlocks[2].OperationBlock.account_key), 'Fixture watch keys must have distinct owners');
    LKeys.AddPublicKey('Historic public watch key A', FBlocks[0].OperationBlock.account_key);
    LKeys.AddPublicKey('Historic public watch key B', FBlocks[2].OperationBlock.account_key);
    LKeys.AddPublicKey('Generated empty public watch key', FEmptyWatchKey);
    Require(LKeys.Count = 3, 'Fixture must contain three distinct public watch keys');
    for I := 0 to LKeys.Count - 1 do
      Require(not LKeys[I].HasPrivateKey, 'Fixture must persist no private key');
  finally LKeys.Free; end;
  TNetData.NetData.NetConnectionsActive := False;
end;

procedure TClassicChecks.OnActivationTerminated(Sender: TObject);
begin
  FActivationDone := True;
  FActivationThread := nil;
  if Assigned(FPreviousTerminate) then FPreviousTerminate(Sender);
end;

procedure TClassicChecks.CaptureActivation;
var LThread: TThread;
begin
  if (FActivationHandle <> 0) or Assigned(FActivationThread) or
    FActivationDone or not Assigned(FForm) then Exit;
  LThread := TThread(TClassicAccess(FForm).FThreadActivate);
  if not Assigned(LThread) then Exit;
  // Capture as soon as Show/activation exposes the worker, before readiness
  // assertions; Show and every pump also capture in finally. The actual
  // fresh Classic loader synchronizes its initialization text before reading
  // storage, then waits at <=1 block. No fixture producer has started yet.
  // Even an initialization failure after that point must finish before the
  // real FormDestroy can free its node. Preserve FreeOnTerminate throughout.
  Require(LThread.FreeOnTerminate, 'Classic activation ownership changed');
  FPreviousTerminate := LThread.OnTerminate;
  LThread.OnTerminate := OnActivationTerminated;
  FActivationThread := LThread;
  Require(DuplicateHandle(GetCurrentProcess, LThread.Handle, GetCurrentProcess,
    @FActivationHandle, SYNCHRONIZE, False, 0), 'Cannot retain the activation completion handle');
end;

procedure TClassicChecks.ObserveActivation;
var LUntil: UInt64;
begin
  CaptureActivation;
  LUntil := GetTickCount64 + 20000;
  repeat
    Pump;
    FNode := TClassicAccess(FForm).FNode;
    if Assigned(FNode) and FNode.NetServer.Active then Break;
  until GetTickCount64 >= LUntil;
  Require(Assigned(FNode) and FNode.NetServer.Active,
    'Actual Classic activation did not finish local restore and start its listener');
  Require(FNode.Bank.BlocksCount = 0, 'Fresh Classic restore was not empty');
  Require(FActivationHandle <> 0, 'The actual Classic activation worker was not observed');
  Require(not TNetData.NetData.NetConnectionsActive, 'Classic activation enabled outbound contacts');
  Require(FNode.NetServer.Port = 0, 'Classic listener did not retain its ephemeral port policy');
  Log('READY actual Classic local restore; outbound disabled; ephemeral listener; real activation worker observed');
end;

procedure TClassicChecks.JoinActivation;
var LUntil: UInt64;
begin
  if (FActivationHandle = 0) and not Assigned(FActivationThread) then Exit;
  LUntil := GetTickCount64 + 10000;
  repeat
    CheckSynchronize(1);
    Application.ProcessMessages;
    if FActivationHandle <> 0 then begin
      if WaitForSingleObject(FActivationHandle, 0) = WAIT_OBJECT_0 then Exit;
    end else if FActivationDone then Exit;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  raise Exception.Create('Actual Classic activation worker did not finish cooperatively');
end;

procedure TClassicChecks.CheckFinalAccounts;
var I, LIndex: Integer; LAccount: TAccount;
begin
  // Capture the confirmed oracle after the real producer has completed. No
  // fixture account, balance or key membership is supplied to the renderer.
  for I := 0 to FNode.Bank.AccountsCount - 1 do begin
    LAccount := FNode.Bank.SafeBox.Account(I);
    if TAccountComp.EqualAccountKeys(LAccount.accountInfo.accountKey,
      FBlocks[0].OperationBlock.account_key) then begin
      LIndex := Length(FExpectedFirst);
      SetLength(FExpectedFirst, LIndex + 1);
      FExpectedFirst[LIndex] := LAccount;
    end else if TAccountComp.EqualAccountKeys(LAccount.accountInfo.accountKey,
      FBlocks[2].OperationBlock.account_key) then begin
      LIndex := Length(FExpectedSecond);
      SetLength(FExpectedSecond, LIndex + 1);
      FExpectedSecond[LIndex] := LAccount;
    end else Continue;
    LIndex := Length(FExpectedAll);
    SetLength(FExpectedAll, LIndex + 1);
    FExpectedAll[LIndex] := LAccount;
  end;
  Require(Length(FExpectedFirst) = CT_AccountsPerBlock,
    'Public watch key A must own its five actual historic accounts');
  Require(Length(FExpectedSecond) = 2 * CT_AccountsPerBlock,
    'Public watch key B must own ten different actual historic accounts');
  Require(Length(FExpectedAll) = 3 * CT_AccountsPerBlock,
    'The two public watch keys must have disjoint confirmed account sets');
  FForm.PageControl.ActivePage := FForm.tsMyAccounts;
  FForm.PageControlChange(FForm.PageControl);
  Require(FForm.cbExploreMyAccounts.Checked, 'Classic must initially filter its own watch keys');
  Require(FForm.WalletKeys.Count = 3, 'Ordinary Classic startup created another wallet key');
  for I := 0 to FForm.WalletKeys.Count - 1 do
    Require(not FForm.WalletKeys[I].HasPrivateKey,
      'Classic fixture unexpectedly contains private material');
  SelectWatchKey(0);
  CheckDisplayedAccounts(FExpectedFirst, 'idle A', False);
  SelectWatchKey(1);
  CheckDisplayedAccounts(FExpectedSecond, 'idle B', False);
  SelectWatchKey(0);
  CheckDisplayedAccounts(FExpectedFirst, 'idle A again', False);
  SelectWatchKey(2);
  CheckDisplayedAccounts(FExpectedEmpty, 'idle empty watch key', False);
  SelectWatchKey(-1);
  CheckDisplayedAccounts(FExpectedAll, 'idle All', False);
  Capture('classic-accounts.bmp');
end;

procedure TClassicChecks.SelectWatchKey(AKeyIndex: Integer);
var I, LWalletIndex: Integer; LKey: TAccountKey;
begin
  Require(Assigned(FForm.cbMyPrivateKeys.OnChange),
    'Classic key dropdown must retain its actual production change handler');
  if AKeyIndex < 0 then begin
    FForm.cbMyPrivateKeys.ItemIndex := 0;
  end else begin
    case AKeyIndex of
      0: LKey := FBlocks[0].OperationBlock.account_key;
      1: LKey := FBlocks[2].OperationBlock.account_key;
      2: LKey := FEmptyWatchKey;
    else
      raise Exception.Create('Unknown Classic fixture watch key');
    end;
    LWalletIndex := FForm.WalletKeys.IndexOfAccountKey(LKey);
    Require(LWalletIndex >= 0, 'The requested public fixture owner is absent from the wallet');
    I := FForm.cbMyPrivateKeys.Items.IndexOfObject(TObject(PtrInt(LWalletIndex)));
    Require(I > 0, 'The actual Classic dropdown omitted a public fixture owner');
    FForm.cbMyPrivateKeys.ItemIndex := I;
  end;
  FForm.cbMyPrivateKeys.OnChange(FForm.cbMyPrivateKeys);
end;

procedure TClassicChecks.CheckDisplayedAccounts(const AExpected: TClassicAccounts;
  const APhase: String; AWhileSynchronizing: Boolean);
var LGrid: TAccountsGrid; LUntil: UInt64; I, LBlocks, LAccounts, LExpectedRows: Integer;
  LMatches: Boolean; LAccount: TAccount; LBalance: Int64;
begin
  LGrid := TClassicAccess(FForm).FAccountsGrid;
  LBalance := 0;
  // Keep the expectation in explicit Int64 arithmetic on both native targets.
  for I := 0 to High(AExpected) do
    LBalance := LBalance + Int64(AExpected[I].balance);
  LExpectedRows := Length(AExpected) + 1;
  if Length(AExpected) = 0 then LExpectedRows := 2;
  LUntil := GetTickCount64 + 10000;
  repeat
    Pump;
    if AWhileSynchronizing then begin
      Require(FNode.GetSynchronizationStatus.State = nssSynchronizing,
        APhase + ': catch-up ended before the selection was displayed');
      Require(not FNode.PendingOperationsAvailable,
        APhase + ': pending data became available before the selection was displayed');
    end;
    LMatches := (AWhileSynchronizing or FNode.PendingOperationsAvailable) and
      not LGrid.IsUpdatingData and
      (LGrid.AccountsCount = Length(AExpected)) and
      (FForm.dgAccounts.RowCount = LExpectedRows) and
      (FForm.lblAccountsCount.Caption = IntToStr(Length(AExpected))) and
      (FForm.lblAccountsBalance.Caption = TAccountComp.FormatMoney(LBalance));
    if LMatches then
      for I := 0 to High(AExpected) do
        if LGrid.AccountNumber(I + 1) <> AExpected[I].account then begin
          LMatches := False;
          Break;
        end;
    if LMatches then Break;
  until GetTickCount64 >= LUntil;
  Require(LMatches, APhase + ': actual Classic selection/totals did not publish; expected=' +
    IntToStr(Length(AExpected)) + ' actual=' + IntToStr(LGrid.AccountsCount) +
    ' count_label=' + FForm.lblAccountsCount.Caption +
    ' balance_label=' + FForm.lblAccountsBalance.Caption);
  Require(LGrid.AccountsBalance = LBalance, APhase + ': selected confirmed balance differs');
  if Length(AExpected) = 0 then
    Require(LGrid.AccountNumber(1) < 0,
      APhase + ': an empty watch key must not fall back to all wallet accounts');
  for I := 0 to High(AExpected) do begin
    LAccount := CT_Account_NUL;
    Require(TClassicGridAccess(LGrid).ReadDisplayedAccount(AExpected[I].account,
      LAccount, LBlocks, LAccounts),
      APhase + ': actual cell reader withheld account data while catch-up was active');
    Require((LAccount.account = AExpected[I].account) and
      (LAccount.balance = AExpected[I].balance) and
      (LAccount.n_operation = AExpected[I].n_operation) and
      TAccountComp.EqualAccountInfos(LAccount.accountInfo, AExpected[I].accountInfo),
      APhase + ': actual cell reader returned another owner, balance or account record');
    Require((LBlocks = FNode.Bank.BlocksCount) and (LAccounts = FNode.Bank.AccountsCount),
      APhase + ': actual cell reader published stale confirmed-tip metadata');
  end;
  FForm.dgAccounts.Invalidate;
  FForm.dgAccounts.Update;
  Log('PASS ' + APhase + ' exact_ids=' + IntToStr(Length(AExpected)) +
    ' balance=' + TAccountComp.FormatMoney(LBalance) +
    ' pending_available=' + BoolToStr(FNode.PendingOperationsAvailable, True));
end;

procedure TClassicChecks.CheckKeySelectionDuringCatchup;
var LUntil: UInt64;
begin
  // A selected remote target exercises the actual deferred-pending state. The
  // peer is an owned identity only: transport remains disabled throughout.
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FSynchronizationPeer, True);
  FNode.SetSynchronizationTarget(FSynchronizationPeer, FNode.Bank.BlocksCount + 20);
  try
    Require((FNode.GetSynchronizationStatus.State = nssSynchronizing) and
      not FNode.PendingOperationsAvailable, 'Catch-up must make the pending overlay unavailable');
    Require(not TNetData.NetData.NetConnectionsActive,
      'A typed catch-up fixture must not enable outbound connections');
    // Existing rows must still paint complete confirmed records, rather than
    // going blank because their pending overlay is temporarily unavailable.
    CheckDisplayedAccounts(FExpectedAll, 'catch-up existing All', True);
    SelectWatchKey(0);
    CheckDisplayedAccounts(FExpectedFirst, 'catch-up A', True);
    SelectWatchKey(1);
    CheckDisplayedAccounts(FExpectedSecond, 'catch-up B', True);
    SelectWatchKey(0);
    CheckDisplayedAccounts(FExpectedFirst, 'catch-up A again', True);
    SelectWatchKey(2);
    CheckDisplayedAccounts(FExpectedEmpty, 'catch-up empty watch key', True);
    SelectWatchKey(-1);
    CheckDisplayedAccounts(FExpectedAll, 'catch-up All', True);
    // No message pump separates these requests. Only the last captured owner
    // may remain after canceled/retired workers publish their completions.
    SelectWatchKey(0);
    SelectWatchKey(1);
    SelectWatchKey(0);
    CheckDisplayedAccounts(FExpectedFirst, 'catch-up rapid A/B/A', True);
    Capture('classic-accounts-catchup.bmp');
  finally
    FNode.SetSynchronizationNetworkActive(False);
  end;
  LUntil := GetTickCount64 + 10000;
  repeat
    Pump;
    if FNode.PendingOperationsAvailable then Break;
  until GetTickCount64 >= LUntil;
  Require(FNode.PendingOperationsAvailable, 'Classic pending overlay did not reconcile after catch-up');
  CheckDisplayedAccounts(FExpectedFirst, 'reconciled latest A', False);
end;

procedure TClassicChecks.Capture(const AName: String);
var LBitmap: TBitmap;
begin
  FForm.Update;
  LBitmap := FForm.GetFormImage;
  try
    Require((LBitmap.Width > 0) and (LBitmap.Height > 0), 'Classic capture is empty');
    LBitmap.SaveToFile(IncludeTrailingPathDelimiter(ExtractFilePath(FLogFile)) + AName);
  finally LBitmap.Free; end;
end;

procedure TClassicChecks.CloseOwnedForm;
var LUntil: UInt64;
begin
  Require(Assigned(FForm), 'Classic form is already absent before native Close');
  Require(PostMessage(FForm.Handle, WM_CLOSE, 0, 0), 'Cannot post native Classic Close');
  LUntil := GetTickCount64 + 10000;
  repeat
    Pump;
    if Application.Terminated then Break;
  until GetTickCount64 >= LUntil;
  Require(Application.Terminated, 'Actual Classic main-form Close did not terminate the application');
  // Classic has no confirmation in its existing close flow. Freeing the actual
  // form runs its unchanged FormDestroy and owned node/network shutdown.
  FForm.Free;
  FRMWallet := nil;
  Require(not TNode.NodeExists and not TNetData.NetDataExists,
    'Classic FormDestroy left its owned core singletons alive');
  Require(FExceptionText = '', 'Classic close raised ' + FExceptionText);
  Log('PASS native Classic Close and unchanged FormDestroy completed; no confirmation invented');
end;

procedure TClassicChecks.Run;
var I: Integer; LUntil: UInt64; LExpectedLabel: String;
begin
  Require(not TNode.NodeExists and not TNetData.NetDataExists, 'Classic probe requires fresh core singletons');
  TCrypto.InitCrypto;
  LoadFixture;
  SeedOwnedSettings;
  Application.CreateForm(TFRMWallet, FRMWallet);
  FForm := FRMWallet;
  FForm.FreeNotification(Self);
  {$IFNDEF FPC}
  if FForm.ClassType <> TFRMWallet then begin
    // Translation must hook BeforeDestruction itself. Patching another VMT
    // slot leaves freed controls registered for unsafe finalization reads.
    Require(PPointer(NativeInt(FForm.ClassType) + vmtBeforeDestruction)^ <>
      PPointer(NativeInt(TFRMWallet) + vmtBeforeDestruction)^,
      'Translated Classic form did not hook its actual BeforeDestruction slot');
    Log('PASS native translated Classic destruction hook');
  end;
  {$ENDIF}
  try FForm.Show;
  finally CaptureActivation; end;
  ObserveActivation;
  FTask := TClassicBlockTask.Create(Self);
  FTask.Start;
  for I := 0 to 4 do begin
    Await(FTask.Step, 20000, 'Historic block task did not reach its next atomic commit');
    Require(FTask.ErrorText = '', FTask.ErrorText);
    Require(InterlockedCompareExchange(FTask.Completed, 0, 0) = I + 1,
      'Historic block task skipped a commit gate');
    Require(FNode.Bank.BlocksCount = Cardinal(I + 1), 'Classic core committed height differs');
    if I >= 1 then begin
      LUntil := GetTickCount64 + 10000;
      repeat
        Pump;
        if FForm.PageControl.Enabled and FActivationDone then Break;
      until GetTickCount64 >= LUntil;
      Require(FForm.PageControl.Enabled and FActivationDone,
        'The real Classic local-load worker did not publish its loaded form');
      if I = 1 then begin
        JoinActivation;
        // Isolate actual block notifications for the later commits. The Classic
        // production timer policy is unchanged; it is disabled only in this test.
        FForm.TimerUpdateStatus.Enabled := False;
        FForm.PageControl.ActivePage := FForm.tsBlockChain;
        FForm.PageControlChange(FForm.PageControl);
      end;
      if I >= 2 then begin
        LExpectedLabel := IntToStr(I + 1) + ' (0..' + IntToStr(I) + ')';
        LUntil := GetTickCount64 + 5000;
        repeat
          Pump;
          if (FForm.lblCurrentBlock.Caption = LExpectedLabel) and
            (FForm.dgBlockChainExplorer.RowCount = I + 2) then Break;
        until GetTickCount64 >= LUntil;
        Require(FForm.lblCurrentBlock.Caption = LExpectedLabel,
          'Classic committed block label did not update through its real Node observer');
        Require(FForm.dgBlockChainExplorer.RowCount = I + 2,
          'Classic real block-grid worker did not publish the committed rows');
      end;
    end;
    Require(not TNetData.NetData.NetConnectionsActive, 'Classic smoke enabled outbound networking');
    Log('COMMIT block=' + IntToStr(I) + ' core_blocks=' + IntToStr(FNode.Bank.BlocksCount) +
      ' label=' + FForm.lblCurrentBlock.Caption + ' block_grid_rows=' + IntToStr(FForm.dgBlockChainExplorer.RowCount));
    FTask.Step.ResetEvent;
    FTask.ContinueStep.SetEvent;
  end;
  Await(FTask.Done, 10000, 'Classic historic block task did not finish');
  Require(FTask.ErrorText = '', FTask.ErrorText);
  FreeAndNil(FTask);
  Require(FNode.Bank.BlocksCount = 5, 'Classic smoke did not preserve all five actual valid blocks');
  Require(FNode.Bank.AccountsCount = 5 * CT_AccountsPerBlock, 'Classic smoke account count differs');
  Require(TNetData.NetData.NetStatistics.ActiveConnections = 0, 'Classic smoke contacted a peer');
  Require(not TClassicAccess(FForm).FPoolMiningServer.Active and
    not TSettings.JsonRpcPortEnabled, 'Classic smoke started an unrelated service');
  Capture('classic-blocks.bmp');
  CheckFinalAccounts;
  CheckKeySelectionDuringCatchup;
  // Queue a real asynchronous filter and close immediately. Native destruction
  // must join any active/retiring reader before releasing its node and wallet.
  SelectWatchKey(1);
  Require(TClassicAccess(FForm).FAccountsGrid.IsUpdatingData,
    'The native Close regression must retain an active or retiring account filter');
  Log('CLOSE filter_request_pending=' +
    BoolToStr(TClassicAccess(FForm).FAccountsGrid.IsUpdatingData, True));
  CloseOwnedForm;
  Log('RESULT 0 Classic actual-form, five valid blocks, three public watch keys, idle/catch-up selection, cell data and natural Close');
end;

function RunClassicWalletProbe: Integer;
var LInput: THandle; LChecks: TClassicChecks;
begin
  Result := 1;
  if ParamCount <> 1 then Exit;
  LInput := OpenInputDesktop(0, False, DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    Require(not SameText(DesktopName(GetThreadDesktop(GetCurrentThreadId)), DesktopName(LInput)),
      'Run the Classic smoke through the owned private-desktop broker');
  finally CloseDesktop(LInput); end;
  Require(not FileExists(ParamStr(1)), 'Refusing an existing Classic probe log');
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Classic compatibility native probe';
  LChecks := TClassicChecks.Create(ParamStr(1));
  try
    try
      LChecks.Run;
      Result := 0;
    except on E: Exception do LChecks.Log('FAIL ' + E.ClassName + ': ' + E.Message); end;
  finally LChecks.Free; end;
end;

end.
