unit UWIZChangeKey_ConfirmAccount;

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
  UWizard, UWIZModels;

type

  { TWIZChangeKey_ConfirmAccount }

  TWIZChangeKey_ConfirmAccount = class(TWizardForm<TWIZOperationsModel>)
    gpChangeKey: TGroupBox;
    lblTotalBalances: TLabel;
    lblTotalBalanceValue: TLabel;
    paGrid: TPanel;
  private
    FChangeKeyGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    function Validate(out message: ansistring): boolean; override;
  end;



implementation

{$R *.lfm}

uses UAccounts, UCoreUtils, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountChangeKeyDataSource }

  TAccountChangeKeyDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZOperationsModel;
  public
    property Model: TWIZOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

{ TWIZChangeKey_ConfirmAccount }

procedure TWIZChangeKey_ConfirmAccount.OnPresent;
var
  Data: TAccountChangeKeyDataSource;
  i: integer;
  acc: TAccount;
  totalBalance: int64;
begin
  FChangeKeyGrid := TVisualGrid.Create(Self);
  FChangeKeyGrid.CanSearch := False;
  FChangeKeyGrid.SortMode := smMultiColumn;
  FChangeKeyGrid.FetchDataInThread := False;
  FChangeKeyGrid.AutoPageSize := True;
  FChangeKeyGrid.SelectionType := stNone;
  FChangeKeyGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  with FChangeKeyGrid.AddColumn('Account') do
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
  with FChangeKeyGrid.AddColumn('Balance') do
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
  Data := TAccountChangeKeyDataSource.Create(FChangeKeyGrid);
  Data.Model := Model;
  FChangeKeyGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FChangeKeyGrid);

  totalBalance := 0;
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
  begin
    acc := Model.Account.SelectedAccounts[i];
    totalBalance := totalBalance + acc.balance;
  end;

  lblTotalBalanceValue.Caption :=
    Format('%s PASC', [TAccountComp.FormatMoney(totalBalance)]);
end;

function TWIZChangeKey_ConfirmAccount.Validate(out message: ansistring): boolean;
begin
  Result := True;
  // get signer accounts from selected accounts
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Length(Model.Account.SelectedAccounts), Model.Fee.SingleOperationFee, Model.Account.SelectedAccounts);

  if Length(Model.Signer.SignerCandidates) < 1 then
  begin
    Result := False;
    message := 'no valid signer account was found.';
  end;

end;

{ TAccountChangeKeyDataSource }

procedure TAccountChangeKeyDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
begin
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
  begin
    AContainer.Add(Model.Account.SelectedAccounts[i]);
  end;
end;

end.
