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
    procedure pnlAccountsInfoClick(Sender: TObject);

    procedure sbSearchAccountClick(Sender: TObject);

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
  UFRMWallet;

constructor TFrameAccountExplorer.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );


end;

destructor TFrameAccountExplorer.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameAccountExplorer.bbAccountsRefreshClick(Sender: TObject);
begin
  UpdateAccounts(true);
end;

procedure TFrameAccountExplorer.bbChangeKeyNameClick(Sender: TObject);
var i : Integer;
  name : String;
begin
  if (cbMyPrivateKeys.ItemIndex<0) then  exit;
  i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
  if (i<0) Or (i>=FWalletKeys.Count) then raise Exception.Create('Must select a Key');
  name := FWalletKeys.Key[i].Name;
  if InputQuery('Change Key name','Input new name',name) then begin
    FWalletKeys.SetName(i,name);
  end;
  UpdatePrivateKeys;
end;

procedure TFrameAccountExplorer.bbSelectedAccountsOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  CheckIsReady;
  if FSelectedAccountsGrid.AccountsCount<=0 then raise Exception.Create('Must select at least 1 account');
  With TFRMOperation.Create(Self) do
  Try
    l := FSelectedAccountsGrid.LockAccountsList;
    try
      SenderAccounts.CopyFrom(l);
    finally
      FSelectedAccountsGrid.UnlockAccountsList;
    end;
    DefaultFee := TSettings.DefaultFee;
    WalletKeys := FWalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

procedure TFrameAccountExplorer.cbExploreMyAccountsClick(Sender: TObject);
begin
  cbMyPrivateKeys.Enabled := cbExploreMyAccounts.Checked;
  UpdateAccounts(true);
  UpdateOperations;
end;

procedure TFrameAccountExplorer.cbFilterAccountsClick(Sender: TObject);
begin
  If not DoUpdateAccountsFilter then UpdateAccounts(true);
end;

procedure TFrameAccountExplorer.cbMyPrivateKeysChange(Sender: TObject);
begin
  UpdateAccounts(true);
end;

procedure TFrameAccountExplorer.dgAccountsClick(Sender: TObject);
begin
  UpdateOperations;
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
    if FAccountsGrid.MoveRowToAccount(an) then begin
      ebFindAccountNumber.Font.Color := clWindowText;
    end else begin
      ebFindAccountNumber.Font.Color := clRed;
    end;
  end else begin
    LAccountNameRawValue.FromString(ebFindAccountNumber.Text);
    LAccNames := TOrderedRawList.Create;
    Try
      if FNode.Bank.SafeBox.FindAccountsStartingByName(LAccountNameRawValue,LAccNames,1)>0 then begin
        an := LAccNames.GetTag(0);
        ebFindAccountNumber.Color := clWindow;
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
    F.Node := FNode;
    F.WalletKeys := FWalletKeys;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TFrameAccountExplorer.sbSelectedAccountsAddAllClick(Sender: TObject);
Var lsource,ltarget : TOrderedCardinalList;
  i : Integer;
begin
  lsource := FAccountsGrid.LockAccountsList;
  Try
    ltarget := FSelectedAccountsGrid.LockAccountsList;
    Try
      for i := 0 to lsource.Count-1 do begin
        if FWalletKeys.IndexOfAccountKey(FNode.Bank.SafeBox.Account(lsource.Get(i)).accountInfo.accountKey)<0 then raise Exception.Create(Format('You cannot operate with account %d because private key not found in your wallet',[lsource.Get(i)]));
        ltarget.Add(lsource.Get(i));
      end;
    Finally
      FSelectedAccountsGrid.UnlockAccountsList;
    End;
  Finally
    FAccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFrameAccountExplorer.sbSelectedAccountsAddClick(Sender: TObject);
Var l, selected : TOrderedCardinalList;
  an : Int64;
  i : Integer;
begin
  an := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if (an<0) then raise Exception.Create('No account selected');
  if FWalletKeys.IndexOfAccountKey(FNode.Bank.SafeBox.Account(an).accountInfo.accountkey)<0 then
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
end;

procedure TFrameAccountExplorer.sbSelectedAccountsDelAllClick(Sender: TObject);
Var l : TOrderedCardinalList;
begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    l.Clear;
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFrameAccountExplorer.sbSelectedAccountsDelClick(Sender: TObject);
Var an : Int64;
  l : TOrderedCardinalList;
begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    an := FSelectedAccountsGrid.AccountNumber(dgSelectedAccounts.Row);
    if an>=0 then l.Remove(an);
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
end;






end.