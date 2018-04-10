unit UWIZChangeAccountPrivateKey_Confirmation;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard,
  UWIZChangeAccountPrivateKey, UWIZModels;

type

  { TWIZChangeAccountPrivateKey_Confirmation }

  TWIZChangeAccountPrivateKey_Confirmation = class(TWizardForm<TWIZOperationsModel>)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lblSgnAcc: TLabel;
    paGrid: TPanel;
  private
    FSendersGrid: TVisualGrid;
  public
    procedure OnPresent; override;
  end;


implementation

{$R *.lfm}

uses UAccounts, UCrypto, UDataSources, UCommon, UCommon.UI, Generics.Collections;

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
    function GetItemField(constref AItem: TAccount;
      const ABindingName: ansistring): variant; override;
  end;

{ TWIZChangeAccountPrivateKey_Confirmation }

procedure TWIZChangeAccountPrivateKey_Confirmation.OnPresent;
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
    Binding := 'Account';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;
  with FSendersGrid.AddColumn('New Private Key Name') do
  begin
    Binding := 'NewPrivateKey';
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
  lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.SignerModel.SignerAccount.account);
end;

{ TAccountSenderDataSource }

function TAccountSenderDataSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('Account'),
    TDataColumn.From('NewPrivateKey'),
    TDataColumn.From('Fee')
    );
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant;
var
  index: integer;
begin
  if ABindingName = 'Account' then
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account)
  else if ABindingName = 'NewPrivateKey' then
  begin
    Result := IIF(Model.ChangeAccountPrivateKeyModel.NewWalletKey.Name = '',
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(Model.ChangeAccountPrivateKeyModel.NewWalletKey.AccountKey)), Model.ChangeAccountPrivateKeyModel.NewWalletKey.Name);
    if not Assigned(Model.ChangeAccountPrivateKeyModel.NewWalletKey.PrivateKey) then
    begin
      Result := Result + '(*)';
    end;
  end
  else if ABindingName = 'Fee' then
    Result := TAccountComp.FormatMoney(Model.FeeModel.DefaultFee)
  else
    raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
end;


procedure TAccountSenderDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
begin
  for i := Low(Model.ChangeAccountPrivateKeyModel.SelectedAccounts) to High(Model.ChangeAccountPrivateKeyModel.SelectedAccounts) do
  begin
    AContainer.Add(Model.ChangeAccountPrivateKeyModel.SelectedAccounts[i]);
  end;
end;


end.
