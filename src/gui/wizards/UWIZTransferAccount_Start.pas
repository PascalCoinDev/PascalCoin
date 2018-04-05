unit UWIZTransferAccount_Start;

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
  UWizard, UWIZTransferAccount, UWIZTransferAccount_Transaction, UWIZSendPASC_Confirmation;

type

  { TWIZTransferAccount_Start }

  TWIZTransferAccount_Start = class(TWizardForm<TWIZTransferAccountModel>)
    grpTransferAccount: TGroupBox;
    paGrid: TPanel;
  private
    FSendersGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;



implementation

{$R *.lfm}

uses UAccounts, USettings, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZTransferAccountModel;
  public
    property Model: TWIZTransferAccountModel read FModel write FModel;
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

{ TWIZTransferAccount_Start }

procedure TWIZTransferAccount_Start.OnPresent;
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

procedure TWIZTransferAccount_Start.OnNext;
begin
   UpdatePath(ptReplaceAllNext, [TWIZTransferAccount_Transaction, TWIZSendPASC_Confirmation]);
end;

end.
