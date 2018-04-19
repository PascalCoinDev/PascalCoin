unit UWIZSendPASC_Confirmation;

{$mode delphi}
{$modeswitch nestedprocvars}

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
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard, UWIZSendPASC, UWIZModels;

type

  { TWIZSendPASC_Confirmation }

  TWIZSendPASC_Confirmation = class(TWizardForm<TWIZOperationsModel>)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblPayload: TLabel;
    lblDestAcc: TLabel;
    lblSgnAcc: TLabel;
    mmoPayload: TMemo;
    paGrid: TPanel;
  private
    FSendersGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;


implementation

{$R *.lfm}

uses UAccounts, UWallet, UUserInterface, UDataSources, UCommon, UCommon.UI, Generics.Collections, UCoreUtils;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZOperationsModel;
  protected
    function GetColumns: TDataColumns; override;
  public
    property Model: TWIZOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    function GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant; override;
  end;

{ TWIZSendPASC_Confirmation }

procedure TWIZSendPASC_Confirmation.OnPresent;
var
  Data: TAccountSenderDataSource;
begin
  FSendersGrid := TVisualGrid.Create(Self);
  FSendersGrid.CanSearch := False;
  FSendersGrid.SortMode := smMultiColumn;
  FSendersGrid.FetchDataInThread := False;
  FSendersGrid.AutoPageSize := True;
  FSendersGrid.SelectionType := stNone;
  FSendersGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  with FSendersGrid.AddColumn('Sender') do
  begin
    StretchedToFill:=true;
    Binding := 'Account';
    SortBinding := 'AccountNumber';
    DisplayBinding := 'Display';
    Filters := SORTABLE_NUMERIC_FILTER;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;
  with FSendersGrid.AddColumn('Balance') do
  begin
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;
  with FSendersGrid.AddColumn('Amount') do
  begin
    Binding := 'AmountDecimal';
    SortBinding := 'Amount';
    DisplayBinding := 'Amount';
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC_CheckAllBalance;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;
  with FSendersGrid.AddColumn('Fee') do
  begin
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 50;
    Renderer := TCellRenderers.PASC;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;

  Data := TAccountSenderDataSource.Create(FSendersGrid);
  Data.Model := Model;
  FSendersGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FSendersGrid);
  lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
  lblDestAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.SendPASC.DestinationAccount.account);
  mmoPayload.Lines.Text := Model.Payload.Content;
end;

procedure TWIZSendPASC_Confirmation.OnNext;
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

function TAccountSenderDataSource.GetColumns: TDataColumns;
begin
  Result := TArrayTool<TDataColumn>.Concat([
    Inherited,
    // Additional columns
    TDataColumns.Create(
      TDataColumn.From('AllBalance'),
      TDataColumn.From('Amount'),
      TDataColumn.From('AmountDecimal'),
      TDataColumn.From('Fee'),
      TDataColumn.From('FeeDecimal')
    )
  ]);
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant;
var
  index: integer;
begin
  if ABindingName = 'AllBalance' then
    Result := IIF(Model.SendPASC.SendPASCMode = akaAllBalance, Variant(True), Variant(False))
  else if ABindingName = 'Amount' then
    Result := Variant(-Model.SendPASC.SingleAmountToSend)
  else if ABindingName = 'AmountDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(-Model.SendPASC.SingleAmountToSend)
  else if ABindingName = 'Fee' then
    Result := -Model.Fee.SingleOperationFee
  else if ABindingName = 'FeeDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(-Model.Fee.SingleOperationFee)
  else Result := Inherited GetItemField(AItem, ABindingName);
end;


procedure TAccountSenderDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
begin
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
  begin
    AContainer.Add(Model.Account.SelectedAccounts[i]);
  end;
end;


end.
