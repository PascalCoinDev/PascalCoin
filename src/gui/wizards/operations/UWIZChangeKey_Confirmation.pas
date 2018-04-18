unit UWIZChangeKey_Confirmation;

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
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard, UWIZChangeKey, UWIZModels;

type

  { TWIZChangeKey_Confirmation }

  TWIZChangeKey_Confirmation = class(TWizardForm<TWIZOperationsModel>)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lblPayload: TLabel;
    lblSgnAcc: TLabel;
    mmoPayload: TMemo;
    paGrid: TPanel;
  private
    FChangeKeyGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;


implementation

{$R *.lfm}

uses UAccounts, UWallet, UUserInterface, UCrypto, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountChangeKeyDataSource }

  TAccountChangeKeyDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZOperationsModel;
  protected
    function GetColumns: TDataColumns; override;
  public
    property Model: TWIZOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    function GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant; override;
  end;

{ TWIZChangeKey_Confirmation }

procedure TWIZChangeKey_Confirmation.OnPresent;
var
  Data: TAccountChangeKeyDataSource;
begin
  FChangeKeyGrid := TVisualGrid.Create(Self);
  FChangeKeyGrid.CanSearch := False;
  FChangeKeyGrid.SortMode := smMultiColumn;
  FChangeKeyGrid.FetchDataInThread := False;
  FChangeKeyGrid.AutoPageSize := True;
  FChangeKeyGrid.SelectionType := stNone;
  FChangeKeyGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];

  with FChangeKeyGrid.AddColumn('Account') do
  begin
    Binding := 'Account';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;

  with FChangeKeyGrid.AddColumn('Current Key') do
  begin
    Binding := 'CurrentKey';
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;

  with FChangeKeyGrid.AddColumn('New Key') do
  begin
    Binding := 'NewKey';
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;

  with FChangeKeyGrid.AddColumn('Fee') do
  begin
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;

  Data := TAccountChangeKeyDataSource.Create(FChangeKeyGrid);
  Data.Model := Model;
  FChangeKeyGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FChangeKeyGrid);
  lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
  mmoPayload.Lines.Text := Model.Payload.Content;
end;

procedure TWIZChangeKey_Confirmation.OnNext;
var
  locked: boolean;
begin
  locked := (not TWallet.Keys.HasPassword) or (not TWallet.Keys.IsValidPassword);
  if locked then
    TUserInterface.UnlockWallet(Self);
end;

{ TAccountChangeKeyDataSource }

function TAccountChangeKeyDataSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('Account'),
    TDataColumn.From('CurrentKey'),
    TDataColumn.From('NewKey'),
    TDataColumn.From('Fee')
    );
end;

function TAccountChangeKeyDataSource.GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant;
var
  index: integer;
begin
  if ABindingName = 'Account' then
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account)
  else if ABindingName = 'Fee' then
    Result := TAccountComp.FormatMoney(Model.Fee.SingleOperationFee)
  else
    case Model.ChangeKey.ChangeKeyMode of
      akaTransferAccountOwnership:
        if ABindingName = 'CurrentKey' then
          Result := TAccountComp.AccountPublicKeyExport(AItem.accountInfo.accountKey)
        else if ABindingName = 'NewKey' then
          Result := TAccountComp.AccountPublicKeyExport(Model.TransferAccount.AccountKey)
        else
          raise Exception.Create(Format('Field not found [%s]', [ABindingName]));

      akaChangeAccountPrivateKey:
        if ABindingName = 'CurrentKey' then
          { TODO : Check how to get the wallet name an account is in }
          Result := '??? unknown'
        else if ABindingName = 'NewKey' then
        begin
          Result := IIF(Model.ChangeAccountPrivateKey.NewWalletKey.Name = '',
            TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(
            Model.ChangeAccountPrivateKey.NewWalletKey.AccountKey)), Model.ChangeAccountPrivateKey.NewWalletKey.Name);
          if not Assigned(Model.ChangeAccountPrivateKey.NewWalletKey.PrivateKey) then
            Result := Result + '(*)';
        end
        else
          raise Exception.Create(Format('Field not found [%s]', [ABindingName]));

    end;

end;


procedure TAccountChangeKeyDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
begin
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    AContainer.Add(Model.Account.SelectedAccounts[i]);

end;


end.
