unit UFRMAccountExplorer;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Grids, Menus, UCommonUI,
  UGridUtils, UNode, UAccounts, UBlockChain;

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
    procedure UpdateAccounts(RefreshData : Boolean);
    procedure OnSelectedAccountsGridUpdated(Sender: TObject);
  private
    { private declarations }
    FUpdating : boolean;
    FAccountsGrid : TAccountsGrid;  //HS
    FOperationsAccountGrid : TOperationsGrid; // HS
    FSelectedAccountsGrid : TAccountsGrid; //HS
    FOrderedAccountsKeyList : TOrderedAccountKeysList; //HS
    FMinAccountBalance : Int64;
    FMaxAccountBalance : Int64;
    Procedure FillAccountInformation(Const Strings : TStrings; Const AccountNumber : Cardinal);
    Procedure FillOperationInformation(Const Strings : TStrings; Const OperationResume : TOperationResume);
    procedure OnPrivateKeysChanged(Sender: TObject);
  public
    { public declarations }
    procedure OnSelectedAccountChanged;
    procedure Refresh;
  end;


implementation

{$R *.lfm}

uses UFRMAccountSelect, UConst, UFRMOperation,
     UWalletKeys, UCrypto, UFRMMemoText, UUserInterface, UCommon;

{ TFRMAccountExplorer }

{%region Form life-cycle}

procedure TFRMAccountExplorer.FormCreate(Sender: TObject);
begin
  FMinAccountBalance := 0;
  FMaxAccountBalance := CT_MaxWalletAmount;
  FOrderedAccountsKeyList := Nil;
  FAccountsGrid := TAccountsGrid.Create(Self);
  FAccountsGrid.DrawGrid := dgAccounts;
  FAccountsGrid.Node := TUserInterface.Node;
  FAccountsGrid.AllowMultiSelect := True;
  FSelectedAccountsGrid := TAccountsGrid.Create(Self);
  FSelectedAccountsGrid.DrawGrid :=dgSelectedAccounts;
  FSelectedAccountsGrid.Node := TUserInterface.Node;
  FSelectedAccountsGrid.OnUpdated := OnSelectedAccountsGridUpdated;
  FOperationsAccountGrid := TOperationsGrid.Create(Self);
  FOperationsAccountGrid.DrawGrid := dgAccountOperations;
  FOperationsAccountGrid.Node := TUserInterface.Node;
  FOperationsAccountGrid.MustShowAlwaysAnAccount := true;
  pcAccountsOptions.ActivePage := tsAccountOperations;

  // Subscribe to wallet events
  TUserInterface.WalletKeys.OnChanged.Add(OnPrivateKeysChanged);

  FUpdating := false;
end;

procedure TFRMAccountExplorer.FormDestroy(Sender: TObject);
begin
  // Unsubscribe from wallet events
  TUserInterface.WalletKeys.OnChanged.Remove(OnPrivateKeysChanged);

  // Nullify fields
  FOperationsAccountGrid.Node := Nil;
  FAccountsGrid.Node := Nil;
  FSelectedAccountsGrid.Node := Nil;
  FAccountsGrid.Node := Nil;

  // Note: grids themselves are collected with Self (TComponent dependency)
end;

{%endregion}

{%region Form methods}

procedure TFRMAccountExplorer.Refresh;
Var i,last_i : Integer;
  wk : TWalletKey;
  s : AnsiString;
begin
  If (Not Assigned(FOrderedAccountsKeyList)) And (Assigned(TUserInterface.Node)) Then begin
    FOrderedAccountsKeyList := TOrderedAccountKeysList.Create(TUserInterface.Node.Bank.SafeBox,false);
  end;
  if (cbMyPrivateKeys.ItemIndex>=0) then last_i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex])
  else last_i := -1;
  cbMyPrivateKeys.items.BeginUpdate;
  Try
    cbMyPrivateKeys.Items.Clear;
    For i:=0 to TUserInterface.WalletKeys.Count-1 do begin
      wk := TUserInterface.WalletKeys.Key[i];
      if assigned(FOrderedAccountsKeyList) then begin
        FOrderedAccountsKeyList.AddAccountKey(wk.AccountKey);
      end;
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


{%endregion}

{%region For auxillary methods}

procedure TFRMAccountExplorer.FillAccountInformation(const Strings: TStrings; Const AccountNumber: Cardinal);
Var account : TAccount;
  s : String;
begin
  account := TUserInterface.Node.Operations.SafeBoxTransaction.Account(AccountNumber);
  if account.name<>'' then s:='Name: '+account.name
  else s:='';
  Strings.Add(Format('Account: %s %s Type:%d',[TAccountComp.AccountNumberToAccountTxtNumber(AccountNumber),s,account.account_type]));
  Strings.Add('');
  Strings.Add(Format('Current balance: %s',[TAccountComp.FormatMoney(account.balance)]));
  Strings.Add('');
  Strings.Add(Format('Updated on block: %d  (%d blocks ago)',[account.updated_block,TUserInterface.Node.Bank.BlocksCount-account.updated_block]));
  Strings.Add(Format('Public key type: %s',[TAccountComp.GetECInfoTxt(account.accountInfo.accountKey.EC_OpenSSL_NID)]));
  Strings.Add(Format('Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.accountKey)]));
  if TAccountComp.IsAccountForSale(account.accountInfo) then begin
    Strings.Add('');
    Strings.Add('** Account is for sale: **');
    Strings.Add(Format('Price: %s',[TAccountComp.FormatMoney(account.accountInfo.price)]));
    Strings.Add(Format('Seller account (where to pay): %s',[TAccountComp.AccountNumberToAccountTxtNumber(account.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForSaleAcceptingTransactions(account.accountInfo) then begin
      Strings.Add('');
      Strings.Add('** Private sale **');
      Strings.Add(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.new_publicKey)]));
      Strings.Add('');
      if TAccountComp.IsAccountLocked(account.accountInfo,TUserInterface.Node.Bank.BlocksCount) then begin
        Strings.Add(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [account.accountInfo.locked_until_block,TUserInterface.Node.Bank.BlocksCount,account.accountInfo.locked_until_block-TUserInterface.Node.Bank.BlocksCount]));
      end else begin
        Strings.Add(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [account.accountInfo.locked_until_block,TUserInterface.Node.Bank.BlocksCount]));
      end;
    end;
  end;
end;

procedure TFRMAccountExplorer.FillOperationInformation(const Strings: TStrings; Const OperationResume: TOperationResume);
begin
  If (not OperationResume.valid) then exit;
  If OperationResume.Block<TUserInterface.Node.Bank.BlocksCount then
    if (OperationResume.NOpInsideBlock>=0) then begin
      Strings.Add(Format('Block: %d/%d',[OperationResume.Block,OperationResume.NOpInsideBlock]))
    end else begin
      Strings.Add(Format('Block: %d',[OperationResume.Block]))
    end
  else Strings.Add('** Pending operation not included on blockchain **');
  Strings.Add(Format('%s',[OperationResume.OperationTxt]));
  Strings.Add(Format('OpType:%d Subtype:%d',[OperationResume.OpType,OperationResume.OpSubtype]));
  Strings.Add(Format('Operation Hash (ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash)]));
  If (OperationResume.OperationHash_OLD<>'') then begin
    Strings.Add(Format('Old Operation Hash (old_ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash_OLD)]));
  end;
  if (OperationResume.OriginalPayload<>'') then begin
    Strings.Add(Format('Payload length:%d',[length(OperationResume.OriginalPayload)]));
    If OperationResume.PrintablePayload<>'' then begin
      Strings.Add(Format('Payload (human): %s',[OperationResume.PrintablePayload]));
    end;
    Strings.Add(Format('Payload (Hexadecimal): %s',[TCrypto.ToHexaString(OperationResume.OriginalPayload)]));
  end;
  If OperationResume.Balance>=0 then begin
    Strings.Add(Format('Final balance: %s',[TAccountComp.FormatMoney(OperationResume.Balance)]));
  end;
end;

procedure TFRMAccountExplorer.UpdateAccounts(RefreshData : Boolean);
Var accl : TOrderedCardinalList;
  l : TOrderedCardinalList;
  i,j,k : Integer;
  c  : Cardinal;
  applyfilter : Boolean;
  acc : TAccount;
begin
//  with FRMWallet do begin
  If Not Assigned(FOrderedAccountsKeyList) Then exit;
  if Not RefreshData then begin
    dgAccounts.Invalidate;
    exit;
  end;
  applyfilter := (cbFilterAccounts.Checked) and ((FMinAccountBalance>0) Or (FMaxAccountBalance<CT_MaxWalletAmount));
  FAccountsGrid.ShowAllAccounts := (Not cbExploreMyAccounts.Checked) And (not applyfilter);
  if Not FAccountsGrid.ShowAllAccounts then begin
    accl := FAccountsGrid.LockAccountsList;
    Try
      accl.Clear;
      if cbExploreMyAccounts.Checked then begin
        if cbMyPrivateKeys.ItemIndex<0 then exit;
        if cbMyPrivateKeys.ItemIndex=0 then begin
          // All keys in the wallet
          for i := 0 to TUserInterface.WalletKeys.Count - 1 do begin
            j := FOrderedAccountsKeyList.IndexOfAccountKey(TUserInterface.WalletKeys[i].AccountKey);
            if (j>=0) then begin
              l := FOrderedAccountsKeyList.AccountKeyList[j];
              for k := 0 to l.Count - 1 do begin
                if applyfilter then begin
                  acc := TUserInterface.Node.Bank.SafeBox.Account(l.Get(k));
                  if (acc.balance>=FMinAccountBalance) And (acc.balance<=FMaxAccountBalance) then accl.Add(acc.account);
                end else accl.Add(l.Get(k));
              end;
            end;
          end;
        end else begin
          i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
          if (i>=0) And (i<TUserInterface.WalletKeys.Count) then begin
            j := FOrderedAccountsKeyList.IndexOfAccountKey(TUserInterface.WalletKeys[i].AccountKey);
            if (j>=0) then begin
              l := FOrderedAccountsKeyList.AccountKeyList[j];
              for k := 0 to l.Count - 1 do begin
                if applyfilter then begin
                  acc := TUserInterface.Node.Bank.SafeBox.Account(l.Get(k));
                  if (acc.balance>=FMinAccountBalance) And (acc.balance<=FMaxAccountBalance) then accl.Add(acc.account);
                end else accl.Add(l.Get(k));
              end;
            end;
          end;
        end;
      end else begin
        // There is a filter... check every account...
        c := 0;
        while (c<TUserInterface.Node.Bank.SafeBox.AccountsCount) do begin
          acc := TUserInterface.Node.Bank.SafeBox.Account(c);
          if (acc.balance>=FMinAccountBalance) And (acc.balance<=FMaxAccountBalance) then accl.Add(acc.account);
          inc(c);
        end;
      end;
    Finally
      FAccountsGrid.UnlockAccountsList;
    End;
    lblAccountsCount.Caption := inttostr(accl.Count);
  end else begin
    lblAccountsCount.Caption := inttostr(TUserInterface.Node.Bank.AccountsCount);
  end;
  bbChangeKeyName.Enabled := cbExploreMyAccounts.Checked;
  // Show Totals:
  lblAccountsBalance.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
  OnSelectedAccountChanged;
//  end
end;

function TFRMAccountExplorer.DoUpdateAccountsFilter: Boolean;
Var bmin,bmax:Int64;
  doupd : Boolean;
begin
//  with FRMWallet do
//  begin
  if FUpdating then exit;
  FUpdating := true;
  Try
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
  Finally
    FUpdating := false;
  End;
  if doupd then UpdateAccounts(true);
  Result := doupd;
  //end;
end;

{%endregion}

{%region Event Handlers: Blockchain }

procedure TFRMAccountExplorer.OnPrivateKeysChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TFRMAccountExplorer.OnSelectedAccountChanged;
Var accn : Int64;
begin
  accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
  FOperationsAccountGrid.AccountNumber := accn;
end;

{%endregion}

{%region Event Handlers: Combo Boxes}

procedure TFRMAccountExplorer.cbExploreMyAccountsChange(Sender: TObject);
begin
//  with FRMWallet do
//  begin
    cbMyPrivateKeys.Enabled := cbExploreMyAccounts.Checked;
    UpdateAccounts(true);
    OnSelectedAccountChanged;
//  end;
end;

procedure TFRMAccountExplorer.cbFilterAccountsChange(Sender: TObject);
begin
  If not DoUpdateAccountsFilter then UpdateAccounts(true);
end;

procedure TFRMAccountExplorer.cbMyPrivateKeysChange(Sender: TObject);
begin
  UpdateAccounts(true);
end;

{%endregion}

{%region Event Handlers: Buttons}

procedure TFRMAccountExplorer.bbChangeKeyNameClick(Sender: TObject);
var i : Integer;
  nameString : String;
begin
  if (cbMyPrivateKeys.ItemIndex<0) then  exit;
  i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
  if (i<0) Or (i>=TUserInterface.WalletKeys.Count) then raise Exception.Create('Must select a Key');
  name := TUserInterface.WalletKeys.Key[i].Name;
  if InputQuery('Change Key name','Input new name',nameString) then begin
    TUserInterface.WalletKeys.SetName(i,name);
  end;
end;

procedure TFRMAccountExplorer.bbAccountsRefreshClick(Sender: TObject);
begin
  UpdateAccounts(true);
end;

procedure TFRMAccountExplorer.sbSelectedAccountsAddAllClick(Sender: TObject);
Var lsource,ltarget : TOrderedCardinalList;
  i : Integer;
begin
//  with FRMWallet do
//  begin
  lsource := FAccountsGrid.LockAccountsList;
  Try
    ltarget := FSelectedAccountsGrid.LockAccountsList;
    Try
      for i := 0 to lsource.Count-1 do begin
        if TUserInterface.WalletKeys.IndexOfAccountKey(TUserInterface.Node.Bank.SafeBox.Account(lsource.Get(i)).accountInfo.accountKey)<0 then raise Exception.Create(Format('You cannot operate with account %d because private key not found in your wallet',[lsource.Get(i)]));
        ltarget.Add(lsource.Get(i));
      end;
    Finally
      FSelectedAccountsGrid.UnlockAccountsList;
    End;
  Finally
    FAccountsGrid.UnlockAccountsList;
  End;

//  end;
end;

procedure TFRMAccountExplorer.sbSelectedAccountsAddClick(Sender: TObject);
Var l, selected : TOrderedCardinalList;
  an : Int64;
  i : Integer;
begin
//  with FRMWallet do
//  begin
  an := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if (an<0) then raise Exception.Create('No account selected');
  if TUserInterface.WalletKeys.IndexOfAccountKey(TUserInterface.Node.Bank.SafeBox.Account(an).accountInfo.accountkey)<0 then
    raise Exception.Create(Format('You cannot add %s account because private key not found in your wallet.'#10+#10+'You''re not the owner!',
      [TAccountComp.AccountNumberToAccountTxtNumber(an)]));
  // Add
  l := FSelectedAccountsGrid.LockAccountsList;
  selected := TOrderedCardinalList.Create;
  Try
    FAccountsGrid.SelectedAccounts(selected);
    for i := 0 to selected.Count-1 do begin
      l.Add(selected.Get(i));
    end;
  Finally
    selected.Free;
    FSelectedAccountsGrid.UnlockAccountsList;
  End;
//  end;
end;

procedure TFRMAccountExplorer.sbSelectedAccountsDelAllClick(Sender: TObject);
Var l : TOrderedCardinalList;
begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    l.Clear;
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFRMAccountExplorer.sbSelectedAccountsDelClick(Sender: TObject);
Var an : Int64;
  l : TOrderedCardinalList;
begin
//  with FRMWallet do
//  begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    an := FSelectedAccountsGrid.AccountNumber(dgSelectedAccounts.Row);
    if an>=0 then l.Remove(an);
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
//  end;
end;

procedure TFRMAccountExplorer.bbSelectedAccountsOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
//  with FRMWallet do
//  begin
  TUserInterface.CheckNodeIsReady;
  if FSelectedAccountsGrid.AccountsCount<=0 then raise Exception.Create('Must select at least 1 account');
  With TFRMOperation.Create(Self) do
  Try
    l := FSelectedAccountsGrid.LockAccountsList;
    try
      SenderAccounts.CopyFrom(l);
    finally
      FSelectedAccountsGrid.UnlockAccountsList;
    end;
    DefaultFee := TUserInterface.AppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
    WalletKeys := TUserInterface.WalletKeys;
    ShowModal;
  Finally
    Free;
  End;
//  end;
end;

procedure TFRMAccountExplorer.sbSearchAccountClick(Sender: TObject);
Var F : TFRMAccountSelect;
begin
  F := TFRMAccountSelect.Create(Self);
  try
    F.Node := TUserInterface.Node;
    F.WalletKeys := TUserInterface.WalletKeys;
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
Var an : Cardinal =0;
begin
  if Trim(ebFindAccountNumber.Text)='' then begin
    ebFindAccountNumber.Color := clWindow;
    ebFindAccountNumber.Font.Color := clDkGray;
  end else if TAccountComp.AccountTxtNumberToAccountNumber(ebFindAccountNumber.Text,an) then begin
    ebFindAccountNumber.Color := clWindow;
    //HS if FRMWallet.FAccountsGrid.MoveRowToAccount(an) then begin
    if FAccountsGrid.MoveRowToAccount(an) then begin
      ebFindAccountNumber.Font.Color := clWindowText;
    end else begin
      ebFindAccountNumber.Font.Color := clRed;
    end;
  end else begin
    // Invalid value
    ebFindAccountNumber.Color := clRed;
    ebFindAccountNumber.Font.Color := clWindowText;
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
//  with FRMWallet do
  FOperationsAccountGrid.ShowModalDecoder(TUserInterface.WalletKeys,TUserInterface.AppParams);
end;

procedure TFRMAccountExplorer.dgAccountsClick(Sender: TObject);
begin
  OnSelectedAccountChanged;
end;

procedure TFRMAccountExplorer.OnSelectedAccountsGridUpdated(Sender: TObject);
begin
  lblSelectedAccountsCount.Caption := Inttostr(FSelectedAccountsGrid.AccountsCount);
  lblSelectedAccountsBalance.Caption := TAccountComp.FormatMoney( FSelectedAccountsGrid.AccountsBalance );
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

    TUserInterface.ShowNewOperationDialog(Self, targetAccounts, TUserInterface.AppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0));
  finally
     targetAccounts.Free;
  end;
end;

procedure TFRMAccountExplorer.miFindPreviousAccountWithHighBalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
//  with FWallet do begin
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
//    end;
end;

procedure TFRMAccountExplorer.miFindNextAccountWithHighBalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
//  with FRMWallet do begin
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
//  end;
end;

procedure TFRMAccountExplorer.miAccountInformationClick(Sender: TObject);
Var F : TFRMMemoText;
  accn : Int64 =-1;
  title : String;
  //account : TAccount;
  strings : TStrings;
  i : Integer;
  opr : TOperationResume;
begin
//  with FRMWallet do begin
  accn := -1;
  title := '';
  strings := TStringList.Create;
  try
    opr := CT_TOperationResume_NUL;
    //HS
    ////AntonB if PageControl.ActivePage=tsOperations then begin
    //  if not (FOperationsExplorerGrid.DrawGrid = nil) then begin
    //  i := FOperationsExplorerGrid.DrawGrid.Row;
    //  if (i>0) and (i<=FOperationsExplorerGrid.OperationsResume.Count) then begin
    //    opr := FOperationsExplorerGrid.OperationsResume.OperationResume[i-1];
    //  end;
    //AntonB end else if PageControl.ActivePage=tsPendingOperations then begin
    //  end else if not (FPendingOperationsGrid.DrawGrid = nil) then begin
    //  i := FPendingOperationsGrid.DrawGrid.Row;
    //  if (i>0) and (i<=FPendingOperationsGrid.OperationsResume.Count) then begin
    //    opr := FPendingOperationsGrid.OperationsResume.OperationResume[i-1];
    //  end;
    //end else //if PageControl.ActivePage=tsMyAccounts then
    begin
      accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
      if accn<0 then raise Exception.Create('Select an account');
      FillAccountInformation(strings,accn);
      title := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(accn)+' info';
      i := FOperationsAccountGrid.DrawGrid.Row;
      if (i>0) and (i<=FOperationsAccountGrid.OperationsResume.Count) then begin
        opr := FOperationsAccountGrid.OperationsResume.OperationResume[i-1];
      end;
    end;
    If (opr.valid) then begin
      if accn>=0 then strings.Add('')
      else title := 'Operation info';
      strings.Add('Operation info:');
      FillOperationInformation(strings,opr);
    end else if accn<0 then Raise Exception.Create('No info available');
    F := TFRMMemoText.Create(Self);
    Try
      F.Caption := title;
      F.Memo.Lines.Assign(strings);
      F.ShowModal;
    Finally
      F.Free;
    End;
  finally
    strings.free;
  end;
//  end;
end;

procedure TFRMAccountExplorer.miAddAccountToSelectedClick(Sender: TObject);
begin
   // in memory for not exit program - Application.Exit - auto free mem not need control free manual for this send Self!
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  sbSelectedAccountsAddClick(Sender);
end;

procedure TFRMAccountExplorer.miDecodePayloadClick(Sender: TObject);
begin
  FOperationsAccountGrid.ShowModalDecoder(TUserInterface.WalletKeys,TUserInterface.AppParams);
end;

procedure TFRMAccountExplorer.miRemoveAccountFromSelectedClick(Sender: TObject);
begin
  Self.pcAccountsOptions.ActivePage := Self.tsMultiSelectAccounts;
  Self.sbSelectedAccountsDelClick(Sender);
end;

{%endregion}


end.

