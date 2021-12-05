unit UFrameAccountExplorer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TFrameAccountExplorer = class(TFrame)
    Splitter1: TSplitter;
    pnlMyAccountsTop: TPanel;
    Label18: TLabel;
    sbSearchAccount: TSpeedButton;
    cbMyPrivateKeys: TComboBox;
    cbExploreMyAccounts: TCheckBox;
    ebFindAccountNumber: TEdit;
    bbChangeKeyName: TBitBtn;
    cbFilterAccounts: TCheckBox;
    ebFilterAccountByBalanceMin: TEdit;
    ebFilterAccountByBalanceMax: TEdit;
    pnlAccounts: TPanel;
    dgAccounts: TDrawGrid;
    pnlAccountsInfo: TPanel;
    Label17: TLabel;
    Label19: TLabel;
    lblAccountsCount: TLabel;
    lblAccountsBalance: TLabel;
    bbAccountsRefresh: TBitBtn;
    pcAccountsOptions: TPageControl;
    tsAccountOperations: TTabSheet;
    dgAccountOperations: TDrawGrid;
    tsMultiSelectAccounts: TTabSheet;
    dgSelectedAccounts: TDrawGrid;
    pnlSelectedAccountsTop: TPanel;
    Label15: TLabel;
    pnlSelectedAccountsBottom: TPanel;
    Label20: TLabel;
    lblSelectedAccountsCount: TLabel;
    Label22: TLabel;
    lblSelectedAccountsBalance: TLabel;
    pnlSelectedAccountsLeft: TPanel;
    sbSelectedAccountsAdd: TSpeedButton;
    sbSelectedAccountsAddAll: TSpeedButton;
    sbSelectedAccountsDel: TSpeedButton;
    sbSelectedAccountsDelAll: TSpeedButton;
    bbSelectedAccountsOperation: TBitBtn;
    procedure bbAccountsRefreshClick(Sender: TObject);
    procedure cbMyPrivateKeysChange(Sender: TObject);
    procedure dgAccountsClick(Sender: TObject);

    procedure dgAccountsColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure dgAccountsFixedCellClick(Sender: TObject; ACol, ARow: Integer);

    procedure sbSearchAccountClick(Sender: TObject);

    procedure cbExploreMyAccountsClick(Sender: TObject);


    procedure ebFindAccountNumberChange(Sender: TObject);
    procedure ebFindAccountNumberExit(Sender: TObject);

    procedure bbChangeKeyNameClick(Sender: TObject);

    procedure sbSelectedAccountsAddClick(Sender: TObject);
    procedure sbSelectedAccountsAddAllClick(Sender: TObject);
    procedure sbSelectedAccountsDelClick(Sender: TObject);
    procedure sbSelectedAccountsDelAllClick(Sender: TObject);
    procedure bbSelectedAccountsOperationClick(Sender: TObject);

    procedure ebFilterAccountByBalanceMinExit(Sender: TObject);
    procedure ebFilterAccountByBalanceMinKeyPress(Sender: TObject;
      var Key: Char);
    procedure cbFilterAccountsClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  UFRMWallet, UConst, UPCOrderedLists, UFRMOperation,
  USettings, UFRMAccountSelect, UBaseTypes, UAccounts;

constructor TFrameAccountExplorer.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  // cannot set properties here that interact with FRMWallet because
  // FRMWallet not yet created.
  // problem code below, moved back to FRMWallet.FormCreate
{
  pcAccountsOptions.ActivePage := tsAccountOperations;

  // problem code:
  cbExploreMyAccounts.Checked:=True; // By default
}
end;

destructor TFrameAccountExplorer.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameAccountExplorer.bbAccountsRefreshClick(Sender: TObject);
begin
  FRMWallet.UpdateAccounts(true);
end;

procedure TFrameAccountExplorer.bbChangeKeyNameClick(Sender: TObject);
var i : Integer;
  name : String;
begin
  if (cbMyPrivateKeys.ItemIndex<0) then  exit;
  i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
  if (i<0) Or (i>=FRMWallet.WalletKeys.Count) then raise Exception.Create('Must select a Key');
  name := FRMWallet.WalletKeys.Key[i].Name;
  if InputQuery('Change Key name','Input new name',name) then begin
    FRMWallet.WalletKeys.SetName(i,name);
  end;
  FRMWallet.UpdatePrivateKeys;
end;

procedure TFrameAccountExplorer.bbSelectedAccountsOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  FRMWallet.CheckIsReady;
  if FRMWallet.SelectedAccountsGrid.AccountsCount<=0 then raise Exception.Create('Must select at least 1 account');
  With TFRMOperation.Create(Self) do
  Try
    l := FRMWallet.SelectedAccountsGrid.LockAccountsList;
    try
      SenderAccounts.CopyFrom(l);
    finally
      FRMWallet.SelectedAccountsGrid.UnlockAccountsList;
    end;
    DefaultFee := TSettings.DefaultFee;
    WalletKeys := FRMWallet.WalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

procedure TFrameAccountExplorer.cbExploreMyAccountsClick(Sender: TObject);
begin
  cbMyPrivateKeys.Enabled := cbExploreMyAccounts.Checked;
  FRMWallet.UpdateAccounts(true);
  FRMWallet.UpdateOperations;
end;

procedure TFrameAccountExplorer.cbFilterAccountsClick(Sender: TObject);
begin
  If not FRMWallet.DoUpdateAccountsFilter then FRMWallet.UpdateAccounts(true);
end;

procedure TFrameAccountExplorer.cbMyPrivateKeysChange(Sender: TObject);
begin
  FRMWallet.UpdateAccounts(true);
end;

procedure TFrameAccountExplorer.dgAccountsClick(Sender: TObject);
begin
  FRMWallet.UpdateOperations;
end;

procedure TFrameAccountExplorer.dgAccountsColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  TSettings.Save;
end;

procedure TFrameAccountExplorer.dgAccountsFixedCellClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  TSettings.Save;
end;

procedure TFrameAccountExplorer.ebFindAccountNumberChange(Sender: TObject);
Var an : Cardinal;
  LAccountNameRawValue : TRawBytes;
  LErrors : String;
  LAccNames : TOrderedRawList;
begin
  if Trim(ebFindAccountNumber.Text)='' then begin
    ebFindAccountNumber.Color := clWindow;
    ebFindAccountNumber.Font.Color := clDkGray;
  end else if TAccountComp.AccountTxtNumberToAccountNumber(ebFindAccountNumber.Text,an) then begin
    ebFindAccountNumber.Color := clWindow;
    if FRMWallet.AccountsGrid.MoveRowToAccount(an) then begin
      ebFindAccountNumber.Font.Color := clWindowText;
    end else begin
      ebFindAccountNumber.Font.Color := clRed;
    end;
  end else begin
    LAccountNameRawValue.FromString(ebFindAccountNumber.Text);
    LAccNames := TOrderedRawList.Create;
    Try
      if FRMWallet.Node.Bank.SafeBox.FindAccountsStartingByName(LAccountNameRawValue,LAccNames,1)>0 then begin
        an := LAccNames.GetTag(0);
        ebFindAccountNumber.Color := clWindow;
        if FRMWallet.AccountsGrid.MoveRowToAccount(an) then begin
          ebFindAccountNumber.Font.Color := clWindowText;
        end else begin
          ebFindAccountNumber.Font.Color := clRed;
        end;
      end else begin
        // Invalid value
        ebFindAccountNumber.Color := clRed;
        ebFindAccountNumber.Font.Color := clWindowText;
      end;
    Finally
      LAccNames.Free;
    End;
  end;
end;

procedure TFrameAccountExplorer.ebFindAccountNumberExit(Sender: TObject);
begin
  ebFindAccountNumber.Text := '';
end;

procedure TFrameAccountExplorer.sbSearchAccountClick(Sender: TObject);
Var F : TFRMAccountSelect;
begin
  F := TFRMAccountSelect.Create(Self);
  try
    F.Node := FRMWallet.Node;
    F.WalletKeys := FRMWallet.WalletKeys;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TFrameAccountExplorer.sbSelectedAccountsAddAllClick(Sender: TObject);
Var lsource,ltarget : TOrderedCardinalList;
  i : Integer;
begin
  lsource := FRMWallet.AccountsGrid.LockAccountsList;
  Try
    ltarget := FRMWallet.SelectedAccountsGrid.LockAccountsList;
    Try
      for i := 0 to lsource.Count-1 do begin
        if FRMWallet.WalletKeys.IndexOfAccountKey(FRMWallet.Node.Bank.SafeBox.Account(lsource.Get(i)).accountInfo.accountKey)<0 then raise Exception.Create(Format('You cannot operate with account %d because private key not found in your wallet',[lsource.Get(i)]));
        ltarget.Add(lsource.Get(i));
      end;
    Finally
      FRMWallet.SelectedAccountsGrid.UnlockAccountsList;
    End;
  Finally
    FRMWallet.AccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFrameAccountExplorer.sbSelectedAccountsAddClick(Sender: TObject);
Var l, selected : TOrderedCardinalList;
  an : Int64;
  i : Integer;
begin
  an := FRMWallet.AccountsGrid.AccountNumber(dgAccounts.Row);
  if (an<0) then raise Exception.Create('No account selected');
  if FRMWallet.WalletKeys.IndexOfAccountKey(FRMWallet.Node.Bank.SafeBox.Account(an).accountInfo.accountkey)<0 then
    raise Exception.Create(Format('You cannot add %s account because private key not found in your wallet.'#10+#10+'You''re not the owner!',
      [TAccountComp.AccountNumberToAccountTxtNumber(an)]));
  // Add
  l := FRMWallet.SelectedAccountsGrid.LockAccountsList;
  selected := TOrderedCardinalList.Create;
  Try
    FRMWallet.AccountsGrid.SelectedAccounts(selected);
    for i := 0 to selected.Count-1 do begin
      l.Add(selected.Get(i));
    end;
  Finally
    selected.Free;
    FRMWallet.SelectedAccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFrameAccountExplorer.sbSelectedAccountsDelAllClick(Sender: TObject);
Var l : TOrderedCardinalList;
begin
  l := FRMWallet.SelectedAccountsGrid.LockAccountsList;
  try
    l.Clear;
  finally
    FRMWallet.SelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFrameAccountExplorer.sbSelectedAccountsDelClick(Sender: TObject);
Var an : Int64;
  l : TOrderedCardinalList;
begin
  l := FRMWallet.SelectedAccountsGrid.LockAccountsList;
  try
    an := FRMWallet.SelectedAccountsGrid.AccountNumber(dgSelectedAccounts.Row);
    if an>=0 then l.Remove(an);
  finally
    FRMWallet.SelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFrameAccountExplorer.ebFilterAccountByBalanceMinExit(Sender: TObject);
begin
  FRMWallet.DoUpdateAccountsFilter;
end;

procedure TFrameAccountExplorer.ebFilterAccountByBalanceMinKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then FRMWallet.DoUpdateAccountsFilter;
end;



end.
