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
    lblDestAcc: TLabel;
    lblSgnAcc: TLabel;
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

uses UAccounts, UWallet, UUserInterface, UDataSources, UCommon, UCommon.UI, Generics.Collections;

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
    Binding := 'SenderAccount';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;
  with FSendersGrid.AddColumn('Balance') do
  begin
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC;
  end;
  with FSendersGrid.AddColumn('Amount To Send') do
  begin
    Binding := 'AmountToSend';
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;
  with FSendersGrid.AddColumn('Fee') do
  begin
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;

  Data := TAccountSenderDataSource.Create(FSendersGrid);
  Data.Model := Model;
  FSendersGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FSendersGrid);
  lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
  lblDestAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.SendPASC.DestinationAccount.account);
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

function TWIZSendPASC_Confirmation.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if Length(Model.Account.SelectedAccounts) > 1 then
  begin
    if not (Model.Fee.SingleOperationFee > 0) then
    begin
      message := 'insufficient fee for total operation.';
      Result := False;
      Exit;
    end;
  end;
end;

{ TAccountSenderDataSource }

function TAccountSenderDataSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('SenderAccount'),
    TDataColumn.From('Balance'),
    TDataColumn.From('AmountToSend'),
    TDataColumn.From('Fee')
    );
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant;
var
  index: integer;
begin
  if ABindingName = 'SenderAccount' then
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account)
  else if ABindingName = 'Balance' then
    Result := TAccountComp.FormatMoney(AItem.Balance)
  else if ABindingName = 'AmountToSend' then
    Result := IIF(Model.SendPASC.SendPASCMode = akaAllBalance, 'ALL BALANCE', TAccountComp.FormatMoney(Model.SendPASC.SingleAmountToSend))
  else if ABindingName = 'Fee' then
    Result := TAccountComp.FormatMoney(Model.Fee.SingleOperationFee)
  else
    raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
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
