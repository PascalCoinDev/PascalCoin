unit UFRMAccountSelect;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UAccounts, Grids, StdCtrls, Buttons, ExtCtrls, UWalletKeys, UNode,
  UGridUtils, UConst, UThread;

const
  CT_AS_MyAccounts = $0001;
  CT_AS_OnlyForSale = $0002;

type
  { TSearchThread }
  TSearchValues = Record
    SafeBox : TPCSafeBox;
    inWalletKeys : TWalletKeys;
    inAccountKey : TAccountKey;
    onlyForSale,
    onlyForPublicSale,
    onlyForPrivateSaleToMe : Boolean;
    minBal,maxBal : Int64;
    searchName : AnsiString;
  end;

  TSearchProcedure = procedure(Const searchFound : TCardinalsArray; const searchValues : TSearchValues) of object;

  TSearchThread = Class(TPCThread)
  private
    FIsReadyForSearch: Boolean;
    FOnSearchFinished: TSearchProcedure;
    FDoStopSearch : Boolean;
    FStartSearch : Boolean;
    FAccounts : TCardinalsArray;
    FSearchValues : TSearchValues;
    procedure SetIsReadyForSearch(AValue: Boolean);
  protected
    procedure BCExecute; override;
    Procedure DoNotifySearchFinished;
  public
    property IsReadyForSearch : Boolean read FIsReadyForSearch write SetIsReadyForSearch;
    Property OnSearchFinished : TSearchProcedure read FOnSearchFinished write FOnSearchFinished;
    Procedure DoSearch(Const newSearchValues : TSearchValues);
    Procedure StopSearch;
  end;

  { TFRMAccountSelect }

  TFRMAccountSelect = class(TForm)
    cbAccountsName: TCheckBox;
    ebMinBalance: TEdit;
    ebMaxBalance: TEdit;
    ebAccountName: TEdit;
    Label17: TLabel;
    Label19: TLabel;
    lblAccountsBalance: TLabel;
    lblAccountsCount: TLabel;
    pnlAccountsTop: TPanel;
    cbOnlyForSale: TCheckBox;
    bbSearch: TBitBtn;
    cbOnlyForPrivateSaleToMe: TCheckBox;
    pnlBottom: TPanel;
    dgAccounts: TDrawGrid;
    bbSelect: TBitBtn;
    bbCancel: TBitBtn;
    cbAccountsBalance: TCheckBox;
    cbMyAccounts: TCheckBox;
    cbMyPrivateKeys: TComboBox;
    cbOnlyForPublicSale: TCheckBox;
    procedure bbSearchClick(Sender: TObject);
    procedure cbMyAccountsChange(Sender: TObject);
    procedure cbMyAccountsClick(Sender: TObject);
    procedure cbMyPrivateKeysChange(Sender: TObject);
    procedure ebMaxBalanceExit(Sender: TObject);
    procedure ebMinBalanceExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAllowSelect: Boolean;
    FDefaultAccount: Int64;
    FFilters: Integer;
    FWalletKeys: TWalletKeys;
    FNode: TNode;
    FSafeBox : TPCSafeBox;
    FAccountsGrid : TAccountsGrid;
    FSearchThread : TSearchThread;
    { Private declarations }
    procedure SetAllowSelect(AValue: Boolean);
    procedure SetDefaultAccount(AValue: Int64);
    procedure SetFilters(AValue: Integer);
    procedure SetNode(const Value: TNode);
    procedure SetWalletKeys(const Value: TWalletKeys);
    Procedure SearchFiltered;
    Procedure UpdateControls;
    procedure OnSearchFinished(Const searchFound : TCardinalsArray; const searchValues : TSearchValues);
    procedure OnAccountsGridUpdated(Sender : TObject);
  protected
    FAccounts : TOrderedAccountList;
  public
    { Public declarations }
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
    Property Node : TNode read FNode write SetNode;
    Property Filters : Integer read FFilters write SetFilters;
    Property DefaultAccount : Int64 read FDefaultAccount write SetDefaultAccount;
    Function GetSelected : Int64;
    Property AllowSelect : Boolean read FAllowSelect write SetAllowSelect;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

Uses strutils;

{ TSearchThread }

procedure TSearchThread.SetIsReadyForSearch(AValue: Boolean);
begin
  if FIsReadyForSearch=AValue then Exit;
  FIsReadyForSearch:=AValue;
end;

procedure TSearchThread.BCExecute;
  procedure SearchFilteredInThread;
  Var c,maxC : Cardinal;
    account : TAccount;
    isValid : Boolean;
    validAccKey : Boolean;
    errors : AnsiString;
    i : Integer;
  begin
    SetLength(FAccounts,0);
    c := 0;
    maxC := FSearchValues.SafeBox.AccountsCount-1;
    validAccKey := TAccountComp.IsValidAccountKey(FSearchValues.inAccountKey,errors);
    while (c<=maxC) And (Not Terminated) And (Not FDoStopSearch) do begin
      account := FSearchValues.SafeBox.Account(c);
      isValid := True;
      If validAccKey then begin
        isValid := TAccountComp.EqualAccountKeys(account.accountInfo.accountKey,FSearchValues.inAccountKey);
      end else if (Assigned(FSearchValues.inWalletKeys)) then begin
        isValid := FSearchValues.inWalletKeys.IndexOfAccountKey(account.accountInfo.accountKey)>=0;
      end;
      If isValid And (FSearchValues.onlyForSale) then begin
        isValid := TAccountComp.IsAccountForSale(account.accountInfo);
      end;
      If IsValid and (FSearchValues.onlyForPublicSale) then begin
        isValid := (TAccountComp.IsAccountForSale(account.accountInfo)) And (Not TAccountComp.IsAccountForSaleAcceptingTransactions(account.accountInfo));
      end;
      If IsValid and (FSearchValues.onlyForPrivateSaleToMe) then begin
        isValid := (TAccountComp.IsAccountForSaleAcceptingTransactions(account.accountInfo)) And
          (Assigned(FSearchValues.inWalletKeys)) And (FSearchValues.inWalletKeys.IndexOfAccountKey(account.accountInfo.new_publicKey)>=0);
      end;
      If IsValid then begin
        IsValid := (account.balance>=FSearchValues.minBal) And ((FSearchValues.maxBal<0) Or (account.balance<=FSearchValues.maxBal));
      end;
      If IsValid And (FSearchValues.searchName<>'') then begin
        i := ansipos(FSearchValues.searchName,account.name);
        IsValid := i>0;
      end;
      //
      if IsValid then begin
        setLength(FAccounts,length(FAccounts)+1);
        FAccounts[high(FAccounts)] := c;
      end;
      inc(c);
    end;
    If (Not Terminated) And (Not FDoStopSearch) then begin
      Synchronize(DoNotifySearchFinished);
    end;
  end;
begin
  FIsReadyForSearch := True;
  FDoStopSearch := False;
  FStartSearch := False;
  while (Not Terminated) do begin
    FIsReadyForSearch := True;
    If (FStartSearch) then begin
      FStartSearch := False;
      FIsReadyForSearch:=False;
      FDoStopSearch:=False;
      SearchFilteredInThread;
    end;
  end;
end;

procedure TSearchThread.DoNotifySearchFinished;
begin
  If Assigned(FOnSearchFinished) then begin
    FOnSearchFinished(FAccounts,FSearchValues);
  end;
end;

procedure TSearchThread.DoSearch(const newSearchValues: TSearchValues);
begin
  FDoStopSearch := True;
  FStartSearch := False;
  While (Not FIsReadyForSearch) do sleep(1);
  FSearchValues := newSearchValues;
  FStartSearch := True;
end;

procedure TSearchThread.StopSearch;
begin
  FDoStopSearch := True;
  FStartSearch := False;
  While (Not FIsReadyForSearch) do sleep(1);
end;

{ TFRMAccountSelect }

procedure TFRMAccountSelect.SetDefaultAccount(AValue: Int64);
begin
  if FDefaultAccount=AValue then Exit;
  FDefaultAccount:=AValue;
end;

procedure TFRMAccountSelect.SetAllowSelect(AValue: Boolean);
begin
  FAllowSelect:=AValue;
  bbSelect.Visible:=AValue;
  bbSelect.Enabled:=AValue;
end;

procedure TFRMAccountSelect.SetFilters(AValue: Integer);
begin
  FFilters:=AValue;
  cbMyAccounts.Checked := (AValue and CT_AS_MyAccounts) = CT_AS_MyAccounts;
  cbOnlyForSale.Checked := (AValue and CT_AS_OnlyForSale) = CT_AS_OnlyForSale;
  SearchFiltered;
end;

procedure TFRMAccountSelect.FormCreate(Sender: TObject);
begin
  FSearchThread := TSearchThread.Create(false);
  FSearchThread.OnSearchFinished := OnSearchFinished;
  FAccounts := TOrderedAccountList.Create;
  FWalletKeys := Nil;
  FNode := Nil;
  FSafeBox:=Nil;
  FFilters:=0;
  FDefaultAccount:=-1;
  FAccountsGrid := TAccountsGrid.Create(Self);
  FAccountsGrid.DrawGrid := dgAccounts;
  FAccountsGrid.OnUpdated:=OnAccountsGridUpdated;
  //
  cbMyAccounts.OnClick:=cbMyAccountsClick;
  cbMyPrivateKeys.OnClick:=cbMyAccountsClick;
  cbOnlyForSale.OnClick:=cbMyAccountsClick;
  cbOnlyForPrivateSaleToMe.OnClick:=cbMyAccountsClick;
  cbOnlyForPublicSale.OnClick:=cbMyAccountsClick;
  cbAccountsBalance.OnClick:=cbMyAccountsClick;
  cbAccountsName.OnClick:=cbMyAccountsClick;
  UpdateControls;
  AllowSelect:=False;
end;

procedure TFRMAccountSelect.bbSearchClick(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.cbMyAccountsChange(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.cbMyAccountsClick(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.cbMyPrivateKeysChange(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.ebMaxBalanceExit(Sender: TObject);
begin
  if ebMaxBalance.text<>'' then begin
    cbAccountsBalance.Checked:=True;
    SearchFiltered;
  end;
end;

procedure TFRMAccountSelect.ebMinBalanceExit(Sender: TObject);
begin
  if ebMinBalance.text<>'' then begin
    cbAccountsBalance.Checked:=True;
    SearchFiltered;
  end;
end;

procedure TFRMAccountSelect.FormDestroy(Sender: TObject);
begin
  FSearchThread.Terminate;
  FSearchThread.WaitFor;
  FSearchThread.Free;
  FAccounts.Free;
end;

procedure TFRMAccountSelect.SetNode(const Value: TNode);
begin
  FNode := Value;
  FSafeBox := FNode.Bank.SafeBox;
  UpdateControls;
end;

procedure TFRMAccountSelect.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
  UpdateControls;
  SearchFiltered;
end;

procedure TFRMAccountSelect.SearchFiltered;
Var
  i : Integer;
  searchValues : TSearchValues;
begin
  FSearchThread.StopSearch;
  searchValues.SafeBox := FSafeBox;
  searchValues.inAccountKey := CT_AccountInfo_NUL.accountKey;
  searchValues.inWalletKeys := Nil;
  If (cbMyAccounts.Checked) And (Assigned(FWalletKeys)) then begin
    If (cbMyPrivateKeys.ItemIndex<=0) then begin
      // All
      searchValues.inWalletKeys := FWalletKeys;
    end else begin
      i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
      if (i>=0) and (i<FWalletKeys.Count) then searchValues.inAccountKey := FWalletKeys.Key[i].AccountKey;
    end;
    cbMyPrivateKeys.ParentFont:=True;
  end else begin
    cbMyPrivateKeys.Font.Color := clGray;
  end;
  searchValues.onlyForSale := (cbOnlyForSale.Checked);
  searchValues.onlyForPrivateSaleToMe := (cbOnlyForPrivateSaleToMe.Checked);
  If searchValues.onlyForPrivateSaleToMe then begin
    searchValues.inWalletKeys := FWalletKeys;
  end;
  searchValues.onlyForPublicSale := (cbOnlyForPublicSale.Checked);
  If cbAccountsBalance.Checked then begin
    If not TAccountComp.TxtToMoney(ebMinBalance.Text,searchValues.minBal) then Raise Exception.Create('Invalid Min Balance');
    ebMinBalance.Text:=TAccountComp.FormatMoney(searchValues.minBal);
    If Trim(ebMaxBalance.Text)='' then begin
      ebMaxBalance.Text:='';
      searchValues.maxBal:=-1;
    end else begin
      If not TAccountComp.TxtToMoney(ebMaxBalance.Text,searchValues.maxBal) then Raise Exception.Create('Invalid Max Balance');
      ebMaxBalance.Text:=TAccountComp.FormatMoney(searchValues.maxBal);
    end;
    ebMinBalance.ParentFont:=True;
    ebMaxBalance.ParentFont:=True;
  end else begin
    searchValues.minBal:=0;
    searchValues.maxBal:=-1;
    ebMinBalance.Font.Color:=clGray;
    ebMaxBalance.Font.Color:=clGray;
  end;
  if (cbAccountsName.Checked) then begin
    searchValues.searchName := LowerCase(Trim(ebAccountName.Text));
    ebAccountName.ParentFont:=True;
  end else begin
    searchValues.searchName:='';
    ebAccountName.Font.Color := clGray;
  end;
  If (searchValues.inAccountKey.EC_OpenSSL_NID=0) AND (searchValues.inWalletKeys=Nil) And (searchValues.maxBal<0) And (searchValues.minBal<=0) And
     (Not searchValues.onlyForPrivateSaleToMe) And (Not searchValues.onlyForPublicSale) And (Not searchValues.onlyForSale) And
     (searchValues.searchName='') then begin
    FAccountsGrid.ShowAllAccounts:=True;
    lblAccountsCount.Caption := IntToStr(FAccountsGrid.Node.Bank.SafeBox.AccountsCount);
    lblAccountsBalance.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
  end else begin
    FAccountsGrid.ShowAllAccounts:=False;
    FSearchThread.DoSearch(searchValues);
  end;
end;

procedure TFRMAccountSelect.UpdateControls;
var i : Integer;
begin
  cbMyAccounts.Enabled:=Assigned(FWalletKeys);
  If not Assigned(FWalletKeys) then begin
    cbMyAccounts.Checked:=False;
    cbMyPrivateKeys.Enabled:=False;
    cbMyPrivateKeys.Clear;
  end else begin
    cbMyPrivateKeys.Enabled:=True;
    cbMyPrivateKeys.Items.BeginUpdate;
    try
      cbMyPrivateKeys.Items.Clear;
      For i:=0 to FWalletKeys.Count-1 do begin
        cbMyPrivateKeys.Items.AddObject(FWalletKeys.Key[i].Name,TObject(i));
      end;
      cbMyPrivateKeys.Sorted:=True;
      cbMyPrivateKeys.Sorted:=False;
      cbMyPrivateKeys.items.InsertObject(0,'(All)',TObject(-1));
    finally
      cbMyPrivateKeys.Items.EndUpdate;
    end;
  end;
end;

procedure TFRMAccountSelect.OnSearchFinished(const searchFound: TCardinalsArray; const searchValues: TSearchValues);
Var l : TOrderedCardinalList;
  i, foundpos : Integer;
begin
  foundpos := -1;
  l := FAccountsGrid.LockAccountsList;
  try
    l.Clear;
    for i:=0 to High(searchFound) do begin
      l.Add(searchFound[i]);
      If searchFound[i]=FDefaultAccount then foundpos := i;
    end;
    lblAccountsCount.Caption := inttostr(l.Count);
  finally
    FAccountsGrid.UnlockAccountsList;
  end;
  If foundpos>=0 then begin
    FAccountsGrid.DrawGrid.Row := foundpos + 1;
  end;
end;

procedure TFRMAccountSelect.OnAccountsGridUpdated(Sender: TObject);
begin
  lblAccountsBalance.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
end;

function TFRMAccountSelect.GetSelected: Int64;
Var ocl : TOrderedCardinalList;
begin
  Result := -1;
  ocl := TOrderedCardinalList.Create;
  try
    If FAccountsGrid.SelectedAccounts(ocl)=1 then Result := ocl.Get(0);
  finally
    ocl.Free;
  end;
end;

end.
