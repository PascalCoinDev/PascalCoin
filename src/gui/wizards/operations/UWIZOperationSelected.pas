unit UWIZOperationSelected;

{$mode delphi}

{ Copyright (c) 2018 Sphere 10 Software (http://www.sphere10.com/)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Ugochukwu Mmaduekwe - main developer
  Herman Schoenfeld - designer
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCommon.Data, UCellRenderers,
  UWizard, UCoreObjects;

type

  { TWIZOperationSelected }

  TWIZOperationSelected = class(TWizardForm<TExecuteOperationsModel>)
    gpSelectedAccounts: TGroupBox;
    lblTotalBalances: TLabel;
    lblTotalBalanceValue: TLabel;
    paGrid: TPanel;
  private
    FSelectedAccountsGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    function Validate(out message: ansistring): boolean; override;
  end;



implementation

{$R *.lfm}

uses UAccounts, UCoreUtils, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TOperationSelectedDataSource }

  TOperationSelectedDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TExecuteOperationsModel;
  public
    property Model: TExecuteOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

{ TWIZOperationSelected }

procedure TWIZOperationSelected.OnPresent;
var
  LData: TOperationSelectedDataSource;
  LIdx: integer;
  LAccount: TAccount;
  LTotalBalance: int64;
begin
  FSelectedAccountsGrid := TVisualGrid.Create(Self);
  FSelectedAccountsGrid.CanSearch := False;
  FSelectedAccountsGrid.SortMode := smMultiColumn;
  FSelectedAccountsGrid.FetchDataInThread := False;
  FSelectedAccountsGrid.AutoPageSize := True;
  FSelectedAccountsGrid.SelectionType := stNone;
  FSelectedAccountsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  with FSelectedAccountsGrid.AddColumn('Account') do
  begin
    StretchedToFill := True;
    Binding := 'AccountNumber';
    SortBinding := 'AccountNumber';
    DisplayBinding := 'Account';
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FSelectedAccountsGrid.AddColumn('Balance') do
  begin
    Binding := 'BalanceDecimal';
    SortBinding := 'Balance';
    DisplayBinding := 'Balance';
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  LData := TOperationSelectedDataSource.Create(FSelectedAccountsGrid);
  LData.Model := Model;
  FSelectedAccountsGrid.DataSource := LData;
  paGrid.AddControlDockCenter(FSelectedAccountsGrid);

  LTotalBalance := 0;
  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
  begin
    LAccount := Model.Account.SelectedAccounts[LIdx];
    LTotalBalance := LTotalBalance + LAccount.balance;
  end;

  lblTotalBalanceValue.Caption :=
    Format('%s PASC', [TAccountComp.FormatMoney(LTotalBalance)]);
end;

function TWIZOperationSelected.Validate(out message: ansistring): boolean;
var
  LIdx: integer;
  LAccount: TAccount;
begin
  Result := True;

  if Model.ExecuteOperationType = omtEnlistAccountForSale then
    for LIdx := Low(model.Account.SelectedAccounts) to High(model.Account.SelectedAccounts) do
    begin
      LAccount := model.Account.SelectedAccounts[LIdx];
      if TAccountComp.IsAccountForSale(LAccount.accountInfo) then
      begin
        Result := False;
        message := Format('Account "%s" Is Already Enlisted For Sale', [LAccount.AccountString]);
        Exit;
      end;
    end;

  // get signer accounts from selected accounts
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Length(Model.Account.SelectedAccounts), Model.Fee.SingleOperationFee, Model.Account.SelectedAccounts);

  if Length(Model.Signer.SignerCandidates) < 1 then
  begin
    Result := False;
    message := 'No Valid Signer Account Was Found.';
  end;

end;

{ TOperationSelectedDataSource }

procedure TOperationSelectedDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  LIdx: integer;
begin
  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    AContainer.Add(Model.Account.SelectedAccounts[LIdx]);

end;

end.
