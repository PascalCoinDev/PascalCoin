unit UWIZOperationSelected;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, UVisualGrid, UCellRenderers, UWizard, UWIZOperation;

type

  { TWIZOperationSelected }

  TWIZOperationSelected = class(TWizardForm<TWIZOperationsModel>)
    gpSelectedAccount: TGroupBox;
    lblTotalBalances: TLabel;
    lblTotalBalanceValue: TLabel;
    paGrid: TPanel;
  private
    FSelectedAccountsGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;



implementation

{$R *.lfm}

uses
  UCommon,
  USettings,
  UAccounts,
  UCommon.UI,
  UCoreUtils,
  UCoreObjects,
  UCommon.Data,
  UDataSources,
  Generics.Collections;

type

  { TOperationSelectedDataSource }

  TOperationSelectedDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZOperationsModel;
  public
    property Model: TWIZOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

{ TWIZOperationSelected }

procedure TWIZOperationSelected.OnPresent;
var
  LData: TOperationSelectedDataSource;
  LIdx: integer;
  LAccount: TAccount;
  LTotalBalance: int64;
  LCaption: string;
begin
  FSelectedAccountsGrid := TVisualGrid.Create(Self);
  FSelectedAccountsGrid.CanSearch := False;
  FSelectedAccountsGrid.SortMode := smMultiColumn;
  FSelectedAccountsGrid.FetchDataInThread := False;
  FSelectedAccountsGrid.AutoPageSize := True;
  FSelectedAccountsGrid.SelectionType := stNone;
  FSelectedAccountsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  LCaption := 'Selected Account';
  gpSelectedAccount.Caption := IIF(Model.Account.Count > 1, Format('%ss', [LCaption]), Format('%s', [LCaption]));
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

procedure TWIZOperationSelected.OnNext;
var
  LAllUserAccountsExcludingPending: TArray<TAccount>;
begin
  LAllUserAccountsExcludingPending := TCoreTool.GetUserAccounts(False);
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Model.Account.Count, IIF(TSettings.DefaultFee = 0, 1, TSettings.DefaultFee), LAllUserAccountsExcludingPending);
end;

function TWIZOperationSelected.Validate(out message: ansistring): boolean;
var
  LIdx: integer;
  LAccount: TAccount;
begin
  Result := True;

  case Model.ExecuteOperationType of

    omtSendPasc, omtBuyAccount:
    begin
        if not (TCoreTool.AreAccountBalancesGreaterThan(Model.Account.SelectedAccounts, 0, LAccount)) then
        begin
          message := Format('Account %s has zero balance so it cannot be part of this type of operation.', [LAccount.AccountString]);
          Exit(False);
        end;
    end;

    omtEnlistAccountForSale:
      for LIdx := Low(model.Account.SelectedAccounts) to High(model.Account.SelectedAccounts) do
      begin
        LAccount := model.Account.SelectedAccounts[LIdx];
        if TAccountComp.IsAccountForSale(LAccount.accountInfo) then
        begin
          message := Format('Account "%s" is already enlisted for sale', [LAccount.AccountString]);
          Exit(False);
        end;
      end;

    omtDelistAccountFromSale:
      for LIdx := Low(model.Account.SelectedAccounts) to High(model.Account.SelectedAccounts) do
      begin
        LAccount := model.Account.SelectedAccounts[LIdx];
        if not TAccountComp.IsAccountForSale(LAccount.accountInfo) then
        begin
          message := Format('Account "%s" is not enlisted for sale so cannot be delisted', [LAccount.AccountString]);
          Exit(False);
        end;
      end;
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
