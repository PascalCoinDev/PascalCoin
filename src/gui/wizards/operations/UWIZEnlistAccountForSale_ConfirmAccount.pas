unit UWIZEnlistAccountForSale_ConfirmAccount;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCommon.Data, UCellRenderers, UWizard, UWIZModels;

type

  { TWIZEnlistAccountForSale_ConfirmAccount }

  TWIZEnlistAccountForSale_ConfirmAccount = class(TWizardForm<TWIZOperationsModel>)
    grpEnlistAccountForSale: TGroupBox;
    grpEnlistAccount: TGroupBox;
    paGrid: TPanel;
  private
    FEnlistAccountsGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    function Validate(out message: ansistring): boolean; override;
  end;



implementation

{$R *.lfm}

uses UAccounts, USettings, UCoreUtils, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZOperationsModel;
  public
    property Model: TWIZOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

{ TWIZEnlistAccountForSale_ConfirmAccount }

procedure TWIZEnlistAccountForSale_ConfirmAccount.OnPresent;
var
  Data: TAccountSenderDataSource;
begin
  FEnlistAccountsGrid := TVisualGrid.Create(Self);
  FEnlistAccountsGrid.CanSearch := False;
  FEnlistAccountsGrid.SortMode := smMultiColumn;
  FEnlistAccountsGrid.FetchDataInThread := False;
  FEnlistAccountsGrid.AutoPageSize := True;
  FEnlistAccountsGrid.SelectionType := stNone;
  FEnlistAccountsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone,
    vgoAutoHidePaging];
  with FEnlistAccountsGrid.AddColumn('Account') do
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
  with FEnlistAccountsGrid.AddColumn('Balance') do
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
  Data := TAccountSenderDataSource.Create(FEnlistAccountsGrid);
  Data.Model := Model;
  FEnlistAccountsGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FEnlistAccountsGrid);
end;

function TWIZEnlistAccountForSale_ConfirmAccount.Validate(out message: ansistring): boolean;
var
  i: integer;
begin
  Result := True;
  for i := Low(model.Account.SelectedAccounts) to High(model.Account.SelectedAccounts) do
    if TAccountComp.IsAccountForSale(model.Account.SelectedAccounts[i].accountInfo) then
    begin
      Result := False;
      message := 'Account ' + TAccountComp.AccountNumberToAccountTxtNumber(
        model.Account.SelectedAccounts[i].account) + ' is already enlisted for sale';
      Exit;
    end;

  // get signer accounts from selected accounts
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Length(Model.Account.SelectedAccounts), Model.Fee.SingleOperationFee, Model.Account.SelectedAccounts);

  if Length(Model.Signer.SignerCandidates) < 1 then
  begin
    Result := False;
    message := 'no valid signer account was found.';
  end;
end;

procedure TAccountSenderDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
begin
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    AContainer.Add(Model.Account.SelectedAccounts[i]);
end;


end.
