unit UFRMAccountExplorer;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/Releases/2.1.6/Units/Forms/UFRMWallet.pas

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

{$I ..\config.inc}

uses
  Classes, SysUtils, Messages,
  {$IFDEF FPC}FileUtil, LCLIntf, LMessages,{$ELSE}Winapi.Windows,{$ENDIF} Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Grids, Menus,
  UCommon, UCommon.UI, UBaseTypes, UPCDataTypes, UGridUtils, UNode, UAccounts,
  UBlockChain, UExplorerAccountSnapshots;

const
  CM_ExplorerKeysChanged = {$IFDEF FPC}LM_USER{$ELSE}WM_USER{$ENDIF} + 102;

type

  { TFRMAccountExplorer }

  TFRMAccountExplorer = class(TApplicationForm)
    bbAccountsRefresh: TBitBtn;
    bbAccountsRefresh1: TBitBtn;
    bbAccountsRefresh2: TBitBtn;
    bbChangeKeyName: TBitBtn;
    bbChangeKeyName1: TBitBtn;
    bbChangeKeyName2: TBitBtn;
    bbSelectedAccountsOperation: TBitBtn;
    bbSelectedAccountsOperation1: TBitBtn;
    bbSelectedAccountsOperation2: TBitBtn;
    cbExploreMyAccounts: TCheckBox;
    cbExploreMyAccounts1: TCheckBox;
    cbExploreMyAccounts2: TCheckBox;
    cbFilterAccounts: TCheckBox;
    cbFilterAccounts1: TCheckBox;
    cbFilterAccounts2: TCheckBox;
    cbMyPrivateKeys: TComboBox;
    cbMyPrivateKeys1: TComboBox;
    cbMyPrivateKeys2: TComboBox;
    dgAccountOperations: TDrawGrid;
    dgAccountOperations1: TDrawGrid;
    dgAccountOperations2: TDrawGrid;
    dgAccounts: TDrawGrid;
    dgAccounts1: TDrawGrid;
    dgAccounts2: TDrawGrid;
    dgSelectedAccounts: TDrawGrid;
    dgSelectedAccounts1: TDrawGrid;
    dgSelectedAccounts2: TDrawGrid;
    ebFilterAccountByBalanceMax: TEdit;
    ebFilterAccountByBalanceMax1: TEdit;
    ebFilterAccountByBalanceMax2: TEdit;
    ebFilterAccountByBalanceMin: TEdit;
    ebFilterAccountByBalanceMin1: TEdit;
    ebFilterAccountByBalanceMin2: TEdit;
    ebFindAccountNumber: TEdit;
    ebFindAccountNumber1: TEdit;
    ebFindAccountNumber2: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    lblAccountsBalance: TLabel;
    lblAccountsBalance1: TLabel;
    lblAccountsBalance2: TLabel;
    lblAccountsCount: TLabel;
    lblAccountsCount1: TLabel;
    lblAccountsCount2: TLabel;
    lblSelectedAccountsBalance: TLabel;
    lblSelectedAccountsBalance1: TLabel;
    lblSelectedAccountsBalance2: TLabel;
    lblSelectedAccountsCount: TLabel;
    lblSelectedAccountsCount1: TLabel;
    lblSelectedAccountsCount2: TLabel;
    meAccountExplorerMenu : TMainMenu;
    miDecodePayload: TMenuItem;
    miAccountInformation: TMenuItem;
    miAddAccountToSelected: TMenuItem;
    miFindAccount: TMenuItem;
    miFindNextAccountWithHighBalance: TMenuItem;
    miFindPreviousAccountWithHighBalance: TMenuItem;
    miNewOperation: TMenuItem;
    miRemoveAccountFromSelected: TMenuItem;
    miTools: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    pcAccountsOptions: TPageControl;
    pcAccountsOptions1: TPageControl;
    pcAccountsOptions2: TPageControl;
    pnlAccounts: TPanel;
    pnlAccounts1: TPanel;
    pnlAccounts2: TPanel;
    pnlAccountsInfo: TPanel;
    pnlAccountsInfo1: TPanel;
    pnlAccountsInfo2: TPanel;
    pnlMyAccountsTop: TPanel;
    pnlMyAccountsTop1: TPanel;
    pnlMyAccountsTop2: TPanel;
    pnlSelectedAccountsBottom: TPanel;
    pnlSelectedAccountsBottom1: TPanel;
    pnlSelectedAccountsBottom2: TPanel;
    pnlSelectedAccountsLeft: TPanel;
    pnlSelectedAccountsLeft1: TPanel;
    pnlSelectedAccountsLeft2: TPanel;
    pnlSelectedAccountsTop: TPanel;
    pnlSelectedAccountsTop1: TPanel;
    pnlSelectedAccountsTop2: TPanel;
    sbSearchAccount: TSpeedButton;
    sbSearchAccount1: TSpeedButton;
    sbSearchAccount2: TSpeedButton;
    sbSelectedAccountsAdd: TSpeedButton;
    sbSelectedAccountsAdd1: TSpeedButton;
    sbSelectedAccountsAdd2: TSpeedButton;
    sbSelectedAccountsAddAll: TSpeedButton;
    sbSelectedAccountsAddAll1: TSpeedButton;
    sbSelectedAccountsAddAll2: TSpeedButton;
    sbSelectedAccountsDel: TSpeedButton;
    sbSelectedAccountsDel1: TSpeedButton;
    sbSelectedAccountsDel2: TSpeedButton;
    sbSelectedAccountsDelAll: TSpeedButton;
    sbSelectedAccountsDelAll1: TSpeedButton;
    sbSelectedAccountsDelAll2: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    tsAccountOperations: TTabSheet;
    tsAccountOperations1: TTabSheet;
    tsAccountOperations2: TTabSheet;
    tsMultiSelectAccounts: TTabSheet;
    tsMultiSelectAccounts1: TTabSheet;
    tsMultiSelectAccounts2: TTabSheet;
    tsMyAccounts: TTabSheet;
    tsMyAccounts1: TTabSheet;
    tsMyAccounts2: TTabSheet;
    procedure bbAccountsRefreshClick(Sender: TObject);
    procedure bbChangeKeyNameClick(Sender: TObject);
    procedure bbSelectedAccountsOperationClick(Sender: TObject);
    procedure cbExploreMyAccountsChange(Sender: TObject);
    procedure cbFilterAccountsChange(Sender: TObject);
    procedure cbMyPrivateKeysChange(Sender: TObject);
    procedure dgAccountOperationsClick(Sender: TObject);
    procedure dgAccountsClick(Sender: TObject);
    procedure ebFilterAccountByBalanceMinExit(Sender: TObject);
    procedure ebFilterAccountByBalanceMinKeyPress(Sender: TObject; var Key: char);
    procedure ebFindAccountNumberChange(Sender: TObject);
    procedure ebFindAccountNumberExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAccountInformationClick(Sender: TObject);
    procedure miAddAccountToSelectedClick(Sender: TObject);
    procedure miDecodePayloadClick(Sender: TObject);
    procedure miFindNextAccountWithHighBalanceClick(Sender: TObject);
    procedure miFindPreviousAccountWithHighBalanceClick(Sender: TObject);
    procedure miNewOperationClick(Sender: TObject);
    procedure miRemoveAccountFromSelectedClick(Sender: TObject);
    procedure sbSearchAccountClick(Sender: TObject);
    function DoUpdateAccountsFilter: Boolean;
    procedure sbSelectedAccountsAddAllClick(Sender: TObject);
    procedure sbSelectedAccountsAddClick(Sender: TObject);
    procedure sbSelectedAccountsDelAllClick(Sender: TObject);
    procedure sbSelectedAccountsDelClick(Sender: TObject);
  private
    { private declarations }
    FAccountsGrid : TAccountsGrid;
    FAccountOperationsGrid : TOperationsGrid;
    FAccountsSelectedGrid : TAccountsGrid;
    FAccountSnapshots: TExplorerAccountSnapshots;
    FBackendRefresh: TThrottledEvent;
    FBackendPending, FBackendReady, FStopping: Boolean;
    FSelectedAccountsRefreshPending: Boolean;
    FMinAccountBalance : Int64;
    FMaxAccountBalance : Int64;
    FLastAccountsGridInvalidateTC : TTickCount;
    procedure RefreshMyKeysCombo;
    procedure RefreshAccountsGrid(RefreshData : Boolean);
    procedure OnLoaded(Sender: TObject);
    procedure OnAccountsChanged(Sender: TObject);
    procedure OnBlocksChanged(Sender: TObject);
    procedure OnPrivateKeysChanged(Sender: TObject);
    procedure OnAccountsSelectedGridUpdated(Sender: TObject);
    function ReadAccount(Sender: TObject; AAccountNumber: Integer;
      out AAccount: TAccount; out ABlockCount, AAccountCount: Integer): Boolean;
    function ReadSummary(Sender: TObject; out AAccountCount: Integer;
      out ABalance: Int64): Boolean;
    procedure OnSnapshotUpdated(Sender: TObject);
    procedure OnBackendReady(Sender: TObject);
    procedure TryRefreshBackend;
    procedure CM_PrivateKeysChanged(var AMsg: TMessage); message CM_ExplorerKeysChanged;
  protected
    function CreateAccountSnapshots: TExplorerAccountSnapshots; virtual;
    procedure InitializeBackendRefresh; virtual;
    function CanRefreshBackend: Boolean; virtual;
    procedure RefreshBackend; virtual;
    procedure DoShow; override;
    procedure OnViewReady(Sender: TObject);
    procedure ScheduleBackendRefresh;
    property BackendRefreshEvent: TThrottledEvent read FBackendRefresh;
  public
    { public declarations }
    procedure OnSelectedAccountChanged;
    procedure OnAccountsGridUpdatedData(Sender: TObject);
    procedure Refresh;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  UConst, USettings, UWallet, UCrypto, UCoreUtils,
  UFRMAccountSelect, UFRMMemoText, UUserInterface, UPCOrderedLists, UMemory;

{ TFRMAccountExplorer }

{%region Form life-cycle}

procedure TFRMAccountExplorer.FormCreate(Sender: TObject);
begin
  FLastAccountsGridInvalidateTC := TPlatform.GetTickCount;
  FMinAccountBalance := 0;
  FMaxAccountBalance := CT_MaxWalletAmount;
  FAccountSnapshots := CreateAccountSnapshots;
  FAccountSnapshots.OnUpdated := OnSnapshotUpdated;
  FAccountSnapshots.OnCompleted := OnViewReady;
  InitializeBackendRefresh;
  FAccountsGrid := TAccountsGrid.Create(Self);
  FAccountsGrid.OnReadAccount := ReadAccount;
  FAccountsGrid.OnReadSummary := ReadSummary;
  FAccountsGrid.DrawGrid := dgAccounts;
  FAccountsGrid.Node := TUserInterface.Node;
  FAccountsGrid.AllowMultiSelect := True;
  FAccountsGrid.OnUpdated := OnAccountsGridUpdatedData;
  FAccountsGrid.OnAccountsGridUpdatedData := OnAccountsGridUpdatedData;
  // Retired/cancelled filter work can become idle without publishing rows.
  FAccountsGrid.OnRefreshStateChanged := OnViewReady;
  FAccountsSelectedGrid := TAccountsGrid.Create(Self);
  FAccountsSelectedGrid.OnReadAccount := ReadAccount;
  FAccountsSelectedGrid.AccountsGridDatasource := acds_InternalList;
  FAccountsSelectedGrid.DrawGrid :=dgSelectedAccounts;
  FAccountsSelectedGrid.Node := TUserInterface.Node;
  FAccountsSelectedGrid.OnUpdated := OnAccountsSelectedGridUpdated;
  FAccountOperationsGrid := TOperationsGrid.Create(Self);
  FAccountOperationsGrid.AsynchronousUpdates := True;
  FAccountOperationsGrid.AutoRefreshOnNodeEvents := False;
  FAccountOperationsGrid.MustShowAlwaysAnAccount := True;
  FAccountOperationsGrid.AccountNumber := -1;
  FAccountOperationsGrid.DrawGrid := dgAccountOperations;
  FAccountOperationsGrid.Node := TUserInterface.Node;
  pcAccountsOptions.ActivePage := tsAccountOperations;

  // Subscribe to events
  TUserInterface.Loaded.Add(OnLoaded);
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);
  TUserInterface.AccountsChanged.Add(OnAccountsChanged);
  TUserInterface.BlocksChanged.Add(OnBlocksChanged);
  TUserInterface.AppStarted.Add(OnViewReady);
  TUserInterface.StateChanged.Add(OnViewReady);
  Refresh;
end;

procedure TFRMAccountExplorer.FormDestroy(Sender: TObject);
begin
  FStopping := True;
  if Assigned(FBackendRefresh) then FBackendRefresh.Remove(OnBackendReady);
  // Unsubscribe from events
  TUserInterface.Loaded.Remove(OnLoaded);
  TWallet.Keys.OnChanged.Remove(OnPrivateKeysChanged);
  TUserInterface.AccountsChanged.Remove(OnAccountsChanged);
  TUserInterface.BlocksChanged.Remove(OnBlocksChanged);
  TUserInterface.AppStarted.Remove(OnViewReady);
  TUserInterface.StateChanged.Remove(OnViewReady);

  if Assigned(FAccountSnapshots) then begin
    FAccountSnapshots.OnCompleted := nil;
    FAccountSnapshots.Stop;
  end;
  if Assigned(FAccountsGrid) then begin
    FAccountsGrid.OnReadAccount := nil;
    FAccountsGrid.OnReadSummary := nil;
    FAccountsGrid.OnUpdated := nil;
    FAccountsGrid.OnAccountsGridUpdatedData := nil;
    FAccountsGrid.OnRefreshStateChanged := nil;
  end;
  if Assigned(FAccountsSelectedGrid) then begin
    FAccountsSelectedGrid.OnReadAccount := nil;
    FAccountsSelectedGrid.OnUpdated := nil;
  end;

  // Nullify fields
  if Assigned(FAccountOperationsGrid) then FAccountOperationsGrid.Node := Nil;
  if Assigned(FAccountsGrid) then FAccountsGrid.Node := Nil;
  if Assigned(FAccountsSelectedGrid) then FAccountsSelectedGrid.Node := Nil;

  // Note: grids themselves are collected with Self (TComponent dependency)
end;

{%endregion}

{%region Form methods}

procedure TFRMAccountExplorer.Refresh;
begin
  if FStopping or TUserInterface.Stopping then Exit;
  RefreshMyKeysCombo;
  FSelectedAccountsRefreshPending := True;
  FAccountSnapshots.Invalidate;
  RefreshAccountsGrid(true);
end;

function TFRMAccountExplorer.CreateAccountSnapshots: TExplorerAccountSnapshots;
begin
  Result := TExplorerAccountSnapshots.Create(Self, TUserInterface.Node);
end;

function TFRMAccountExplorer.ReadAccount(Sender: TObject; AAccountNumber: Integer;
  out AAccount: TAccount; out ABlockCount, AAccountCount: Integer): Boolean;
begin
  Result := FAccountSnapshots.TryGetAccount(AAccountNumber, AAccount,
    ABlockCount, AAccountCount);
end;

function TFRMAccountExplorer.ReadSummary(Sender: TObject; out AAccountCount: Integer;
  out ABalance: Int64): Boolean;
begin
  Result := FAccountSnapshots.TryGetSummary(AAccountCount, ABalance);
end;

procedure TFRMAccountExplorer.OnSnapshotUpdated(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  if FAccountsGrid.AccountsGridDatasource = acds_Node then
    FAccountsGrid.UpdateData
  else dgAccounts.Invalidate;
  dgSelectedAccounts.Invalidate;
  // A data revision changes selected totals; viewport batches only repaint.
  if FSelectedAccountsRefreshPending then begin
    FSelectedAccountsRefreshPending := False;
    FAccountsSelectedGrid.UpdateData;
  end;
  OnAccountsGridUpdatedData(Self);
  OnSelectedAccountChanged;
  TryRefreshBackend;
end;

procedure TFRMAccountExplorer.InitializeBackendRefresh;
begin
  FBackendRefresh := TThrottledEvent.Create(Self);
  FBackendRefresh.Mode := temNotifyOnEventBurstStartAndFinished;
  FBackendRefresh.Interval := TTimeSpan.FromSeconds(5);
  FBackendRefresh.Add(OnBackendReady);
end;

function TFRMAccountExplorer.CanRefreshBackend: Boolean;
begin
  Result := IsVisible and TUserInterface.Started and
    (not FAccountsGrid.IsUpdatingData) and (not FAccountSnapshots.Updating);
end;

procedure TFRMAccountExplorer.RefreshBackend;
begin
  FSelectedAccountsRefreshPending := True;
  FAccountSnapshots.Invalidate;
  RefreshAccountsGrid(True);
  FAccountOperationsGrid.UpdateAccountOperations;
end;

procedure TFRMAccountExplorer.ScheduleBackendRefresh;
begin
  if FStopping or TUserInterface.Stopping then Exit;
  FBackendPending := True;
  FBackendRefresh.Notify;
end;

procedure TFRMAccountExplorer.OnBackendReady(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  // Only inputs delivered to the owned throttle determine its deadline.
  // A late callback after deferred work was consumed must not create new work.
  if not FBackendPending then Exit;
  FBackendReady := True;
  TryRefreshBackend;
end;

procedure TFRMAccountExplorer.TryRefreshBackend;
begin
  if FStopping or TUserInterface.Stopping or not FBackendPending or not FBackendReady then Exit;
  if not CanRefreshBackend then Exit;
  FBackendPending := False;
  FBackendReady := False;
  RefreshBackend;
end;

procedure TFRMAccountExplorer.DoShow;
begin
  inherited;
  OnViewReady(Self);
end;

procedure TFRMAccountExplorer.OnViewReady(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  // Visibility, application state and source completion consume already admitted
  // work. They never Notify or extend the owned five-second quiet deadline.
  TryRefreshBackend;
end;

{%endregion}

{%region For auxillary methods}

procedure TFRMAccountExplorer.RefreshAccountsGrid(RefreshData : Boolean);
Var accl : TOrderedCardinalList;
  l : TOrderedCardinalList;
  i,j,k : Integer;
  c  : Cardinal;
  LApplyfilter : Boolean;
  acc : TAccount;
  LFilters : TAccountsGridFilter;
begin
  if FStopping or not Assigned(FAccountsGrid) then Exit;

  if Not RefreshData then begin
    if TPlatform.GetElapsedMilliseconds(FLastAccountsGridInvalidateTC)>1000 then begin
      FLastAccountsGridInvalidateTC := TPlatform.GetTickCount;
      dgAccounts.Invalidate;
    end;
    exit;
  end;
  LApplyfilter := (cbFilterAccounts.Checked) and ((FMinAccountBalance>0) Or ((FMaxAccountBalance<CT_MaxWalletAmount) and (FMaxAccountBalance>=0)));
  if (Not cbExploreMyAccounts.Checked) And (not LApplyfilter) then begin
    FAccountsGrid.AccountsGridDatasource := acds_Node;
    FAccountsGrid.UpdateData;
  end else begin
    LFilters := FAccountsGrid.AccountsGridFilter;
    LFilters.MinBalance := FMinAccountBalance;
    LFilters.MaxBalance := FMaxAccountBalance;
    LFilters.UseAccountKey := False;
    LFilters.AccountKey := CT_TECDSA_Public_Nul;
    if cbExploreMyAccounts.Checked then begin
      // Capturing the key does not acquire the index lock held by a broad
      // query. The owned worker resolves its position when that lock is free.
      LFilters.OrderedAccountsKeyList := TWallet.Keys.AccountsKeyList;
      LFilters.indexAccountsKeyList := -1;
      if cbMyPrivateKeys.ItemIndex>0 then begin
        i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
        if (i>=0) And (i<TWallet.Keys.Count) then begin
          LFilters.UseAccountKey := True;
          LFilters.AccountKey := TWallet.Keys[i].AccountKey;
        end;
      end;
    end else begin
      LFilters.OrderedAccountsKeyList := Nil;
      LFilters.indexAccountsKeyList := -1;
    end;
    FAccountsGrid.AccountsGridFilter := LFilters;
    FAccountsGrid.AccountsGridDatasource := acds_NodeFiltered;
  end;

  bbChangeKeyName.Enabled := cbExploreMyAccounts.Checked;
  OnSelectedAccountChanged;
end;

procedure TFRMAccountExplorer.RefreshMyKeysCombo;
Var i,last_i : Integer;
  wk : TWalletKey;
  s : String;
begin
  cbMyPrivateKeys.Enabled := cbExploreMyAccounts.Checked;
  if (cbMyPrivateKeys.ItemIndex>=0) then last_i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex])
  else last_i := -1;
  cbMyPrivateKeys.items.BeginUpdate;
  Try
    cbMyPrivateKeys.Items.Clear;
    For i:=0 to TWallet.Keys.Count-1 do begin
      wk := TWallet.Keys.Key[i];
      if (wk.Name='') then begin
        s := 'Sha256='+TCrypto.ToHexaString( TCrypto.DoSha256( TAccountComp.AccountKey2RawString(wk.AccountKey) ) );
      end else begin
        s := wk.Name;
      end;
      if Not Assigned(wk.PrivateKey) then s := s + '(*)';
      cbMyPrivateKeys.Items.AddObject(s,TObject(i));
    end;
    cbMyPrivateKeys.Sorted := true;
    cbMyPrivateKeys.Sorted := false;
    cbMyPrivateKeys.Items.InsertObject(0,'(All my private keys)',TObject(-1));
  Finally
    cbMyPrivateKeys.Items.EndUpdate;
  End;
  last_i := cbMyPrivateKeys.Items.IndexOfObject(TObject(last_i));
  if last_i<0 then last_i := 0;
  if cbMyPrivateKeys.Items.Count>last_i then cbMyPrivateKeys.ItemIndex := last_i
  else if cbMyPrivateKeys.Items.Count>=0 then cbMyPrivateKeys.ItemIndex := 0;
end;

function TFRMAccountExplorer.DoUpdateAccountsFilter: Boolean;
Var bmin,bmax:Int64;
  doupd : Boolean;
begin
  If Not TAccountComp.TxtToMoney(ebFilterAccountByBalanceMin.Text,bmin) then bmin := 0;
  If not TAccountComp.TxtToMoney(ebFilterAccountByBalanceMax.Text,bmax) then bmax := CT_MaxWalletAmount;
  if (bmax<bmin) or (bmax=0) then bmax := CT_MaxWalletAmount;
  if bmin>bmax then bmin := 0;
  doupd := (bmin<>FMinAccountBalance) Or (bmax<>FMaxAccountBalance);
  FMinAccountBalance := bmin;
  FMaxAccountBalance := bmax;
  if bmin>0 then
    ebFilterAccountByBalanceMin.Text:=TAccountComp.FormatMoney(bmin)
  else ebFilterAccountByBalanceMin.Text := '';
  if bmax<CT_MaxWalletAmount then
    ebFilterAccountByBalanceMax.Text := TAccountComp.FormatMoney(bmax)
  else ebFilterAccountByBalanceMax.Text := '';
  if cbFilterAccounts.Checked then begin
    ebFilterAccountByBalanceMin.ParentFont := true;
    ebFilterAccountByBalanceMax.ParentFont := true;
  end else begin
    ebFilterAccountByBalanceMin.font.Color := clDkGray;
    ebFilterAccountByBalanceMax.font.Color := clDkGray;
  end;
  if doupd then RefreshAccountsGrid(true);
  Result := doupd;
  //end;
end;

{%endregion}

{%region Event Handlers: Blockchain }

procedure TFRMAccountExplorer.OnLoaded(Sender: TObject);
begin
  ScheduleBackendRefresh;
end;

procedure TFRMAccountExplorer.OnAccountsChanged(Sender: TObject);
begin
  ScheduleBackendRefresh;
end;

procedure TFRMAccountExplorer.OnBlocksChanged(Sender: TObject);
begin
  ScheduleBackendRefresh;
end;

procedure TFRMAccountExplorer.OnPrivateKeysChanged(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  if TThread.CurrentThread.ThreadID <> MainThreadID then begin
    // Wallet notifications may originate in RPC. Never read key choices or
    // mutate controls there; native messages disappear with their receiver.
    if HandleAllocated then PostMessage(Handle, CM_ExplorerKeysChanged, 0, 0);
    Exit;
  end;
  Refresh;
end;

procedure TFRMAccountExplorer.CM_PrivateKeysChanged(var AMsg: TMessage);
begin
  OnPrivateKeysChanged(nil);
end;

procedure TFRMAccountExplorer.OnSelectedAccountChanged;
Var accn : Int64;
begin
  accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
  FAccountOperationsGrid.AccountNumber := accn;
end;

procedure TFRMAccountExplorer.OnAccountsGridUpdatedData(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  if FAccountsGrid.IsUpdatingData or
    ((FAccountsGrid.AccountsGridDatasource = acds_Node) and FAccountSnapshots.Updating) then begin
    lblAccountsCount.Caption := '(Calculating)';
    lblAccountsBalance.Caption := '(Calculating)';
  end else begin
    lblAccountsCount.Caption := IntToStr(FAccountsGrid.AccountsCount);
    lblAccountsBalance.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
    // A cached new page may require no snapshot callback. Its completed row
    // replacement must still bind history to the account now selected.
    if Assigned(FAccountOperationsGrid) then OnSelectedAccountChanged;
  end;
  // The account/filter worker has published; retry a previously admitted input.
  OnViewReady(Sender);
end;

{%endregion}

{%region Event Handlers: Combo Boxes}

procedure TFRMAccountExplorer.cbExploreMyAccountsChange(Sender: TObject);
begin
    RefreshAccountsGrid(true);
    RefreshMyKeysCombo;
    OnSelectedAccountChanged;
end;

procedure TFRMAccountExplorer.cbFilterAccountsChange(Sender: TObject);
begin
  If not DoUpdateAccountsFilter then RefreshAccountsGrid(true);
end;

procedure TFRMAccountExplorer.cbMyPrivateKeysChange(Sender: TObject);
begin
  RefreshAccountsGrid(true);
end;

{%endregion}

{%region Event Handlers: Buttons}

procedure TFRMAccountExplorer.bbChangeKeyNameClick(Sender: TObject);
var
  i : Integer;
  name : String;
begin
  if (cbMyPrivateKeys.ItemIndex<0) then  exit;
  i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
  if (i<0) Or (i>=TWallet.Keys.Count) then raise Exception.Create('Must select a Key');
  name := TWallet.Keys.Key[i].Name;
  if InputQuery('Change Key name','Input new name',name) then begin
    TWallet.Keys.SetName(i,name);
  end;
  RefreshMyKeysCombo;
end;

procedure TFRMAccountExplorer.bbAccountsRefreshClick(Sender: TObject);
begin
  FSelectedAccountsRefreshPending := True;
  FAccountSnapshots.Invalidate;
  RefreshAccountsGrid(true);
end;

procedure TFRMAccountExplorer.sbSelectedAccountsAddAllClick(Sender: TObject);
Var lsource,ltarget : TOrderedCardinalList;
  i : Integer;
begin
  lsource := FAccountsGrid.LockAccountsList;
  Try
    ltarget := FAccountsSelectedGrid.LockAccountsList;
    Try
      for i := 0 to lsource.Count-1 do begin
        if TWallet.Keys.IndexOfAccountKey(TUserInterface.Node.Bank.SafeBox.Account(lsource.Get(i)).accountInfo.accountKey)<0 then raise Exception.Create(Format('You cannot operate with account %d because private key not found in your wallet',[lsource.Get(i)]));
        ltarget.Add(lsource.Get(i));
      end;
    Finally
      FAccountsSelectedGrid.UnlockAccountsList;
    End;
  Finally
    FAccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFRMAccountExplorer.sbSelectedAccountsAddClick(Sender: TObject);
Var l, selected : TOrderedCardinalList;
  an : Int64;
  i : Integer;
begin
  an := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if (an<0) then raise Exception.Create('No account selected');
  if TWallet.Keys.IndexOfAccountKey(TUserInterface.Node.Bank.SafeBox.Account(an).accountInfo.accountkey)<0 then
    raise Exception.Create(Format('You cannot add %s account because private key not found in your wallet.'#10+#10+'You''re not the owner!',
      [TAccountComp.AccountNumberToAccountTxtNumber(an)]));
  // Add
  l := FAccountsSelectedGrid.LockAccountsList;
  selected := TOrderedCardinalList.Create;
  Try
    FAccountsGrid.SelectedAccounts(selected);
    for i := 0 to selected.Count-1 do begin
      l.Add(selected.Get(i));
    end;
  Finally
    selected.Free;
    FAccountsSelectedGrid.UnlockAccountsList;
  End;
end;

procedure TFRMAccountExplorer.sbSelectedAccountsDelAllClick(Sender: TObject);
Var l : TOrderedCardinalList;
begin
  l := FAccountsSelectedGrid.LockAccountsList;
  try
    l.Clear;
  finally
    FAccountsSelectedGrid.UnlockAccountsList;
  end;
end;

procedure TFRMAccountExplorer.sbSelectedAccountsDelClick(Sender: TObject);
Var an : Int64;
  l : TOrderedCardinalList;
begin
  l := FAccountsSelectedGrid.LockAccountsList;
  try
    an := FAccountsSelectedGrid.AccountNumber(dgSelectedAccounts.Row);
    if an>=0 then l.Remove(an);
  finally
    FAccountsSelectedGrid.UnlockAccountsList;
  end;
end;

procedure TFRMAccountExplorer.bbSelectedAccountsOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  TUserInterface.CheckNodeIsReady;
  if FAccountsSelectedGrid.AccountsCount<=0 then raise Exception.Create('Must select at least 1 account');
  try
    l := FAccountsSelectedGrid.LockAccountsList;
    TUserInterface.ShowNewOperationDialog(Self, l, TSettings.DefaultFee);
  finally
    FAccountsSelectedGrid.UnlockAccountsList;
  end;
end;

procedure TFRMAccountExplorer.sbSearchAccountClick(Sender: TObject);
Var F : TFRMAccountSelect;
begin
  F := TFRMAccountSelect.Create(Self);
  try
    F.Node := TUserInterface.Node;
    F.WalletKeys := TWallet.Keys;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

{%endregion}

{%region Event Handlers: Text Boxes}

procedure TFRMAccountExplorer.ebFilterAccountByBalanceMinExit(Sender: TObject);
begin
  DoUpdateAccountsFilter;
end;

procedure TFRMAccountExplorer.ebFilterAccountByBalanceMinKeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then DoUpdateAccountsFilter;
end;

procedure TFRMAccountExplorer.ebFindAccountNumberChange(Sender: TObject);
var
  an: Cardinal;
  accountName: TRawBytes;
  matches: TOrderedRawList;
  disposed: TDisposables;
  found: Boolean;
begin
  an := 0;
  if Trim(ebFindAccountNumber.Text)='' then begin
    ebFindAccountNumber.Color := clWindow;
    ebFindAccountNumber.Font.Color := clDkGray;
  end else begin
    found := TAccountComp.AccountTxtNumberToAccountNumber(ebFindAccountNumber.Text, an);
    if not found then begin
      accountName.FromString(ebFindAccountNumber.Text);
      matches := disposed.AddObject(TOrderedRawList.Create) as TOrderedRawList;
      found := TUserInterface.Node.Bank.SafeBox.FindAccountsStartingByName(accountName, matches, 1) > 0;
      if found then an := matches.GetTag(0);
    end;
    if found then begin
      ebFindAccountNumber.Color := clWindow;
      if FAccountsGrid.MoveRowToAccount(an) then
        ebFindAccountNumber.Font.Color := clWindowText
      else ebFindAccountNumber.Font.Color := clRed;
    end else begin
      ebFindAccountNumber.Color := clRed;
      ebFindAccountNumber.Font.Color := clWindowText;
    end;
  end;
end;

procedure TFRMAccountExplorer.ebFindAccountNumberExit(Sender: TObject);
begin
  ebFindAccountNumber.Text := '';
end;

{%endregion}

{%region Event Handlers: Data Grid}

procedure TFRMAccountExplorer.dgAccountOperationsClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(Self, FAccountOperationsGrid.SelectedOperation);
end;

procedure TFRMAccountExplorer.dgAccountsClick(Sender: TObject);
begin
  OnSelectedAccountChanged;
end;

procedure TFRMAccountExplorer.OnAccountsSelectedGridUpdated(Sender: TObject);
begin
  lblSelectedAccountsCount.Caption := Inttostr(FAccountsSelectedGrid.AccountsCount);
  lblSelectedAccountsBalance.Caption := TAccountComp.FormatMoney( FAccountsSelectedGrid.AccountsBalance );
end;

{%endregion}

{%region Events Handlers: Menu Items}

procedure TFRMAccountExplorer.miNewOperationClick(Sender: TObject);
var targetAccounts : TOrderedCardinalList;
begin
  targetAccounts := TOrderedCardinalList.Create;
  try
    If FAccountsGrid.SelectedAccounts(targetAccounts) = 0
      then raise Exception.Create('No row selected');

    TUserInterface.ShowNewOperationDialog(Self, targetAccounts, TSettings.DefaultFee);
  finally
     targetAccounts.Free;
  end;
end;

procedure TFRMAccountExplorer.miFindPreviousAccountWithHighBalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
    an64 := FAccountsGrid.AccountNumber(dgAccounts.Row);
    if an64<0 then an := TUserInterface.Node.Bank.SafeBox.AccountsCount-1
    else an := an64;
    If an>= TUserInterface.Node.Bank.SafeBox.AccountsCount then exit;
    start := TUserInterface.Node.Bank.SafeBox.Account(an);
    while (an>0)  do begin
      if TUserInterface.Node.Bank.SafeBox.Account(an).balance>start.balance then break
      else dec(an);
    end;
    if (TUserInterface.Node.Bank.SafeBox.Account(an).balance>start.balance) then FAccountsGrid.MoveRowToAccount(an)
    else raise Exception.Create('Not found any account lower than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
      TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMAccountExplorer.miFindNextAccountWithHighBalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  an64 := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if an64<0 then an := 0
  else an := an64;
  If an>=TUserInterface.Node.Bank.SafeBox.AccountsCount then exit;
  start := TUserInterface.Node.Bank.SafeBox.Account(an);
  while (an<TUserInterface.Node.Bank.SafeBox.AccountsCount)  do begin
    if TUserInterface.Node.Bank.SafeBox.Account(an).balance>start.balance then break
    else inc(an);
  end;
  if (an<TUserInterface.Node.Bank.SafeBox.AccountsCount) then FAccountsGrid.MoveRowToAccount(an)
  else raise Exception.Create('Not found any account higher than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
    TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMAccountExplorer.miAccountInformationClick(Sender: TObject);
Var F : TFRMMemoText;
  accn : Int64;
  acc : TAccount;
  i : Integer;
  opr : TOperationResume;
begin
  accn := -1;
  opr := CT_TOperationResume_NUL;
  accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if accn<0 then
    raise Exception.Create('Select an account');

  if accn >= TUserInterface.Node.Bank.AccountsCount then
    raise Exception.Create('Account not found');

  acc := TUserInterface.Node.GetAccount(accn);

  i := FAccountOperationsGrid.DrawGrid.Row;
  if (i>0) and (i<=FAccountOperationsGrid.OperationsResume.Count) then begin
    opr := FAccountOperationsGrid.OperationsResume[i-1];
  end;

  If opr.valid then
    TUserInterface.ShowAccountOperationInfoDialog(Self, acc, opr)
  else
    TUserInterface.ShowAccountInfoDialog(Self, acc);
end;

procedure TFRMAccountExplorer.miAddAccountToSelectedClick(Sender: TObject);
begin
   // in memory for not exit program - Application.Exit - auto free mem not need control free manual for this send Self!
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  sbSelectedAccountsAddClick(Sender);
end;

procedure TFRMAccountExplorer.miDecodePayloadClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(Self, TPCOperation.OperationHashAsHexa(FAccountOperationsGrid.SelectedOperation.OperationHash));
end;

procedure TFRMAccountExplorer.miRemoveAccountFromSelectedClick(Sender: TObject);
begin
  Self.pcAccountsOptions.ActivePage := Self.tsMultiSelectAccounts;
  Self.sbSelectedAccountsDelClick(Sender);
end;

{%endregion}

end.

