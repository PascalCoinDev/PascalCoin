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
    lblPayload: TLabel;
    lblSgnAcc: TLabel;
    lblSellerAcc: TLabel;
    mmoPayload: TMemo;
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
    Binding := 'AccountNumber';
    SortBinding := 'AccountNumber';
    DisplayBinding := 'Display';
    Filters := SORTABLE_NUMERIC_FILTER;
    StretchedToFill := true;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;

   with FEnlistAccountsGrid.AddColumn('Sale Price') do begin
    Binding := 'SalePriceDecimal';
    SortBinding := 'SalePrice';
    DisplayBinding := 'SalePrice';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;

  with FEnlistAccountsGrid.AddColumn('Fee') do begin
    Binding := 'FeeDecimal';
    SortBinding := 'Fee';
    DisplayBinding := 'Fee';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 50;
    Renderer := TCellRenderers.PASC;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;

   data := TAccountSenderDataSource.Create(FEnlistAccountsGrid);
   data.Model := Model;
   FEnlistAccountsGrid.DataSource := data;
   paGrid.AddControlDockCenter(FEnlistAccountsGrid);
   lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
   lblSellerAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.EnlistAccountForSale.SellerAccount.account);
   mmoPayload.Lines.Text := Model.Payload.Content;
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
  Result := TArrayTool<TDataColumn>.Concat([
    Inherited,
    // Additional columns
    TDataColumns.Create(
      TDataColumn.From('SalePrice'),
      TDataColumn.From('SalePriceDecimal'),
      TDataColumn.From('Fee'),
      TDataColumn.From('FeeDecimal')
    )]
  );
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const ABindingName : AnsiString) : Variant;
var
  index : Integer;
begin
   if ABindingName = 'Account' then
     Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account)
   else if ABindingName = 'SalePrice' then
     Result := Model.EnlistAccountForSale.SalePrice
   else if ABindingName = 'SalePriceDecimal' then
     Result := TAccountComp.FormatMoneyDecimal(Model.EnlistAccountForSale.SalePrice)
   else if ABindingName = 'Fee' then
     Result := -Model.Fee.SingleOperationFee
   else if ABindingName = 'FeeDecimal' then
     Result := TAccountComp.FormatMoneyDecimal(-Model.Fee.SingleOperationFee)
   else Result := Inherited GetItemField(AItem, ABindingName);
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
