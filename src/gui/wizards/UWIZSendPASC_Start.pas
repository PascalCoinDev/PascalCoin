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
  ExtCtrls, UVisualGrid, UCommon.Data,
  UWizard, UWIZSendPASC, UWIZSendPASC_Transaction, UWIZSendPASC_Confirmation;

type

  { TWIZSendPASC_Start }

  TWIZSendPASC_Start = class(TWizardForm<TWIZSendPASCModel>)
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

uses UAccounts, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TCustomDataSource<TAccount>)
    private
      FModel : TWIZSendPASCModel;
    protected
      function GetColumns : TTableColumns;  override;
    public
      property Model : TWIZSendPASCModel read FModel write FModel;
      function GetSearchCapabilities: TSearchCapabilities; override;
      function GetEntityKey(constref AItem: TAccount) : Variant; override;
      procedure FetchAll(const AContainer : TList<TAccount>); override;
      function GetItemField(constref AItem: TAccount; const AColumnName : utf8string) : Variant; override;
      procedure DehydrateItem(constref AItem: TAccount; var ATableRow: Variant); override;
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
  FSendersGrid.DefaultColumnWidths :=
    TArray<integer>.Create(
    CT_VISUALGRID_STRETCH, // Account
    100                    // Amount
   );
   data := TAccountSenderDataSource.Create(FSendersGrid);
   data.Model := Model;
   FSendersGrid.DataSource := data;
   paGrid.AddControlDockCenter(FSendersGrid);
end;

procedure TWIZSendPASC_Start.OnNext;
begin
  UpdatePath(ptReplaceAllNext, [TWIZSendPASC_Transaction, TWIZSendPASC_Confirmation]);
end;

{ TAccountSenderDataSource }

function TAccountSenderDataSource.GetColumns : TTableColumns;
begin
  Result := TTableColumns.Create('Account', 'Amount');
end;

function TAccountSenderDataSource.GetSearchCapabilities: TSearchCapabilities;
begin
  Result := TSearchCapabilities.Create(
    TSearchCapability.From('Account', SORTABLE_NUMERIC_FILTER),
    TSearchCapability.From('Amount', SORTABLE_TEXT_FILTER)
  );
end;

function TAccountSenderDataSource.GetEntityKey(constref AItem: TAccount) : Variant;
begin
  Result := AItem.account;
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const AColumnName : utf8string) : Variant;
var
  index : Integer;
begin
   if AColumnName = 'Account' then
     Result := AItem.account
   else if AColumnName = 'Amount' then
     Result := TAccountComp.FormatMoneyDecimal(AItem.Balance)
   else raise Exception.Create(Format('Field not found [%s]', [AColumnName]));
end;

procedure TAccountSenderDataSource.DehydrateItem(constref AItem: TAccount; var ATableRow: Variant);
begin
  ATableRow.Account := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account);
  ATableRow.Amount := TAccountComp.FormatMoney(AItem.balance);
end;

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

