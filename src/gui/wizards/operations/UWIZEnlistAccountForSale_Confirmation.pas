unit UWIZEnlistAccountForSale_Confirmation;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard, UWIZEnlistAccountForSale;

type

  { TWIZEnlistAccountForSale_Confirmation }

  TWIZEnlistAccountForSale_Confirmation = class(TWizardForm<TWIZEnlistAccountForSaleModel>)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblSgnAcc: TLabel;
    lblSellerAcc: TLabel;
    paGrid: TPanel;
  private
    FSendersGrid : TVisualGrid;
  public
    procedure OnPresent; override;
  end;


implementation

{$R *.lfm}

uses UAccounts, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
    private
      FModel : TWIZEnlistAccountForSaleModel;
    protected
      function GetColumns : TDataColumns; override;
    public
      property Model : TWIZEnlistAccountForSaleModel read FModel write FModel;
      procedure FetchAll(const AContainer : TList<TAccount>); override;
      function GetItemField(constref AItem: TAccount; const ABindingName : AnsiString) : Variant; override;
  end;

{ TWIZEnlistAccountForSale_Confirmation }

procedure TWIZEnlistAccountForSale_Confirmation.OnPresent;
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
    Binding := 'Account';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;
   with FSendersGrid.AddColumn('Sale Price') do begin
    Binding := 'SalePrice';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC;
  end;
  // with FSendersGrid.AddColumn('New Public Key') do begin
  //  Binding := 'NewPublicKey';
  //  Filters := SORTABLE_TEXT_FILTER;
  //  Width := 100;
  //end;
  with FSendersGrid.AddColumn('Fee') do begin
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;

   data := TAccountSenderDataSource.Create(FSendersGrid);
   data.Model := Model;
   FSendersGrid.DataSource := data;
   paGrid.AddControlDockCenter(FSendersGrid);
   lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.SignerAccount.account);
   lblSellerAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.SellerAccount.account);

end;

{ TAccountSenderDataSource }

function TAccountSenderDataSource.GetColumns : TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('Account'),
    TDataColumn.From('SalePrice'),
    //TDataColumn.From('NewPublicKey'),
    TDataColumn.From('Fee')
  );
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const ABindingName : AnsiString) : Variant;
var
  index : Integer;
begin
   if ABindingName = 'Account' then
     Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account)
   else if ABindingName = 'SalePrice' then
     Result := TAccountComp.FormatMoney(Model.SalePrice)
   //else if ABindingName = 'NewPublicKey' then
   //  Result := Model.NewPublicKey
     else if ABindingName = 'Fee' then
     Result := TAccountComp.FormatMoney(Model.Fee.DefaultFee)
   else raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
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

