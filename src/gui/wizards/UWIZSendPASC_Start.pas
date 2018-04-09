unit UWIZSendPASC_Start;

{$mode delphi}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com>: added grid-based layout

}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCommon.Data, UCellRenderers,
  UWizard, UWIZSendPASC, UWIZSendPASC_Transaction, UWIZSendPASC_Confirmation, UWIZModels;

type

  { TWIZSendPASC_Start }

  TWIZSendPASC_Start = class(TWizardForm<TWIZOperationsModel>)
    gpSender: TGroupBox;
    paGrid: TPanel;
  private
    FSendersGrid : TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;



implementation

{$R *.lfm}

uses UAccounts, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
    private
      FModel : TWIZOperationsModel.TSendPASCModel;
    public
      property Model : TWIZOperationsModel.TSendPASCModel read FModel write FModel;
      procedure FetchAll(const AContainer : TList<TAccount>); override;
  end;

{ TWIZSendPASC_Start }

procedure TWIZSendPASC_Start.OnPresent;
var
  data : TAccountSenderDataSource;
begin
  FSendersGrid := TVisualGrid.Create(Self);
  FSendersGrid.CanSearch:= False;
  FSendersGrid.SortMode := smMultiColumn;
  FSendersGrid.FetchDataInThread := False;
  FSendersGrid.AutoPageSize := True;
  FSendersGrid.SelectionType := stNone;
  FSendersGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  with FSendersGrid.AddColumn('Account') do begin
    StretchedToFill := true;
    Binding := 'AccountNumber';
    SortBinding := 'AccountNumber';
    DisplayBinding := 'Account';
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
    Filters:=SORTABLE_NUMERIC_FILTER;
  end;
  with FSendersGrid.AddColumn('Balance') do begin
    Binding := 'BalanceDecimal';
    SortBinding := 'Balance';
    DisplayBinding := 'Balance';
    Width := 100;
    HeaderAlignment:=taRightJustify;
    DataAlignment:=taRightJustify;
    Renderer := TCellRenderers.PASC;
    Filters:=SORTABLE_NUMERIC_FILTER;
  end;
  data := TAccountSenderDataSource.Create(FSendersGrid);
  data.Model := Model.SendPASCModel;
  FSendersGrid.DataSource := data;
  paGrid.AddControlDockCenter(FSendersGrid);
end;

procedure TWIZSendPASC_Start.OnNext;
begin
  UpdatePath(ptReplaceAllNext, [TWIZSendPASC_Transaction, TWIZSendPASC_Confirmation]);
end;

{ TAccountSenderDataSource }

procedure TAccountSenderDataSource.FetchAll(const AContainer : TList<TAccount>);
var
  i: Integer;
begin
  for i := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
  begin
    AContainer.Add( Model.SelectedAccounts[i] );
  end;
end;

end.

