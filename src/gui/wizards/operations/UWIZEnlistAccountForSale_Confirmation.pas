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
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard, UWIZModels;

type

  { TWIZEnlistAccountForSale_Confirmation }

  TWIZEnlistAccountForSale_Confirmation = class(TWizardForm<TWIZOperationsModel>)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblSgnAcc: TLabel;
    lblSellerAcc: TLabel;
    paGrid: TPanel;
  private
    FEnlistAccountsGrid : TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;


implementation

{$R *.lfm}

uses UAccounts, UWallet, UUserInterface, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
    private
      FModel : TWIZOperationsModel;
    protected
      function GetColumns : TDataColumns; override;
    public
      property Model : TWIZOperationsModel read FModel write FModel;
      procedure FetchAll(const AContainer : TList<TAccount>); override;
      function GetItemField(constref AItem: TAccount; const ABindingName : AnsiString) : Variant; override;
  end;

{ TWIZEnlistAccountForSale_Confirmation }

procedure TWIZEnlistAccountForSale_Confirmation.OnPresent;
var
  data : TAccountSenderDataSource;
begin
  FEnlistAccountsGrid := TVisualGrid.Create(Self);
  FEnlistAccountsGrid.CanSearch:= False;
  FEnlistAccountsGrid.SortMode := smMultiColumn;
  FEnlistAccountsGrid.FetchDataInThread := False;
  FEnlistAccountsGrid.AutoPageSize := True;
  FEnlistAccountsGrid.SelectionType := stNone;
  FEnlistAccountsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  with FEnlistAccountsGrid.AddColumn('Account') do begin
    Binding := 'Account';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;
   with FEnlistAccountsGrid.AddColumn('Sale Price') do begin
    Binding := 'SalePrice';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC;
  end;

  with FEnlistAccountsGrid.AddColumn('Fee') do begin
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;

   data := TAccountSenderDataSource.Create(FEnlistAccountsGrid);
   data.Model := Model;
   FEnlistAccountsGrid.DataSource := data;
   paGrid.AddControlDockCenter(FEnlistAccountsGrid);
   lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
   lblSellerAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.EnlistAccountForSale.SellerAccount.account);

end;

procedure TWIZEnlistAccountForSale_Confirmation.OnNext;
var
  locked: Boolean;
begin
  locked := (NOT TWallet.Keys.HasPassword) OR (NOT TWallet.Keys.IsValidPassword);
  if locked then
  begin
    TUserInterface.UnlockWallet(Self);
  end;
end;

{ TAccountSenderDataSource }

function TAccountSenderDataSource.GetColumns : TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('Account'),
    TDataColumn.From('SalePrice'),
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
     Result := TAccountComp.FormatMoney(Model.EnlistAccountForSale.SalePrice)
     else if ABindingName = 'Fee' then
     Result := TAccountComp.FormatMoney(Model.Fee.SingleOperationFee)
   else raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
end;


procedure TAccountSenderDataSource.FetchAll(const AContainer : TList<TAccount>);
var
  i: Integer;
begin
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
  begin
    AContainer.Add( Model.Account.SelectedAccounts[i] );
  end;
end;


end.

