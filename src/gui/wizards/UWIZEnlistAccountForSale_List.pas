unit UWIZEnlistAccountForSale_List;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCommon.Data, UCellRenderers,
  UWizard, UWIZEnlistAccountForSale, UWIZEnlistAccountForSale_Transaction, UWIZEnlistAccountForSale_PrivateSaleConfig,
  UWIZEnlistAccountForSale_Confirmation;

type

  { TWIZEnlistAccountForSale_List }

  TWIZEnlistAccountForSale_List = class(TWizardForm<TWIZEnlistAccountForSaleModel>)
    grpEnlistAccountForSale: TGroupBox;
    grpTransferAccount: TGroupBox;
    paGrid: TPanel;
  private
    FSendersGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;



implementation

{$R *.lfm}

uses UAccounts, USettings, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZEnlistAccountForSaleModel;
  public
    property Model: TWIZEnlistAccountForSaleModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

procedure TAccountSenderDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
begin
  for i := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
  begin
    AContainer.Add(Model.SelectedAccounts[i]);
  end;
end;

{ TWIZEnlistAccountForSale_List }

procedure TWIZEnlistAccountForSale_List.OnPresent;
var
  Data: TAccountSenderDataSource;
begin
  FSendersGrid := TVisualGrid.Create(Self);
  FSendersGrid.CanSearch := False;
  FSendersGrid.SortMode := smMultiColumn;
  FSendersGrid.FetchDataInThread := False;
  FSendersGrid.AutoPageSize := True;
  FSendersGrid.SelectionType := stNone;
  FSendersGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone,
    vgoAutoHidePaging];
  with FSendersGrid.AddColumn('Account') do
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
  with FSendersGrid.AddColumn('Balance') do
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
  Data := TAccountSenderDataSource.Create(FSendersGrid);
  Data.Model := Model;
  FSendersGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FSendersGrid);
end;

procedure TWIZEnlistAccountForSale_List.OnNext;
begin
  case Model.AccountSaleMode of
  akaPublicSale:
    begin
      UpdatePath(ptReplaceAllNext, [TWIZEnlistAccountForSale_Transaction, TWIZEnlistAccountForSale_Confirmation]);
    end;

   akaPrivateSale:
    begin
      UpdatePath(ptReplaceAllNext, [TWIZEnlistAccountForSale_PrivateSaleConfig, TWIZEnlistAccountForSale_Confirmation]);
    end;

  end;
end;

function TWIZEnlistAccountForSale_List.Validate(out message: ansistring): boolean;
var
  i: Integer;
begin
  Result := True;
   for i := Low(model.SelectedAccounts) to High(model.SelectedAccounts) do
  begin
    if TAccountComp.IsAccountForSale(model.SelectedAccounts[i].accountInfo) then
    begin
      Result := False;
      message := 'Account ' + TAccountComp.AccountNumberToAccountTxtNumber(
        model.SelectedAccounts[i].account) + ' is already enlisted for sale';
      Exit;
    end;
  end;
end;

end.
