unit UWIZOperationConfirmation;

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
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard, UDataObjects;

type

  { TWIZOperationConfirmation }

  TWIZOperationConfirmation = class(TWizardForm<TExecuteOperationsModel>)
    GroupBox1: TGroupBox;
    lblSignerAccount: TLabel;
    lblBeneficiaryAccount: TLabel;
    lblPayload: TLabel;
    lblSgnAcc: TLabel;
    lblBenAcc: TLabel;
    mmoPayload: TMemo;
    paGrid: TPanel;
  private
    FConfirmationGrid: TVisualGrid;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;


implementation

{$R *.lfm}

uses UAccounts, UCrypto, UConst, UWallet, UUserInterface, UDataSources, UCommon, UCommon.UI, Generics.Collections, UCoreUtils;

type

  { TOperationConfirmationDataSource }

  TOperationConfirmationDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TExecuteOperationsModel;

  protected
    function GetColumns: TDataColumns; override;
  public
    property Model: TExecuteOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    function GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant; override;
  end;

{ TWIZOperationConfirmation }

procedure TWIZOperationConfirmation.OnPresent;
var
  Data: TOperationConfirmationDataSource;
begin
  FConfirmationGrid := TVisualGrid.Create(Self);
  FConfirmationGrid.CanSearch := False;
  FConfirmationGrid.SortMode := smMultiColumn;
  FConfirmationGrid.FetchDataInThread := False;
  FConfirmationGrid.AutoPageSize := True;
  FConfirmationGrid.SelectionType := stNone;
  FConfirmationGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  with FConfirmationGrid.AddColumn('Sender') do
  begin
    StretchedToFill := True;
    Binding := 'Account';
    SortBinding := 'AccountNumber';
    DisplayBinding := 'Display';
    Filters := SORTABLE_NUMERIC_FILTER;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;
  with FConfirmationGrid.AddColumn('Recipient') do
  begin
    StretchedToFill := True;
    Filters := SORTABLE_TEXT_FILTER;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;
  with FConfirmationGrid.AddColumn('Operation') do
  begin
    StretchedToFill := True;
    Filters := SORTABLE_TEXT_FILTER;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;
  with FConfirmationGrid.AddColumn('Balance') do
  begin
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;
  with FConfirmationGrid.AddColumn('Fee') do
  begin
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 50;
    Renderer := TCellRenderers.PASC;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
  end;

  Data := TOperationConfirmationDataSource.Create(FConfirmationGrid);
  Data.Model := Model;
  FConfirmationGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FConfirmationGrid);
  lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
  if not (Model.ModelType = omtEnlistAccountForSale) then
  begin
    lblBeneficiaryAccount.Visible := False;
    lblBenAcc.Visible := False;
  end
  else
  begin
    lblBeneficiaryAccount.Visible := True;
    lblBenAcc.Visible := True;
    lblBenAcc.Caption := Model.EnlistAccountForSale.SellerAccount.AccountString;
  end;
  mmoPayload.Lines.Text := Model.Payload.Content;
end;

procedure TWIZOperationConfirmation.OnNext;
var
  locked: boolean;
begin
  locked := (not TWallet.Keys.HasPassword) or (not TWallet.Keys.IsValidPassword);
  if locked then
    TUserInterface.UnlockWallet(Self);
end;

{ TOperationConfirmationDataSource }

function TOperationConfirmationDataSource.GetColumns: TDataColumns;
begin
  Result := TArrayTool<TDataColumn>.Concat([
     Inherited,
    // Additional columns
    TDataColumns.Create(
    TDataColumn.From('Operation'),
    TDataColumn.From('Recipient'),
    TDataColumn.From('Fee')
    )
    ]);
end;

function TOperationConfirmationDataSource.GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant;
var
  LAmount: string;
  LCurrentAccountKey, LDestinationAccountKey: TAccountKey;
begin

  case Model.ChangeKey.ChangeKeyMode of
    akaTransferAccountOwnership:
    begin
      LCurrentAccountKey := AItem.accountInfo.accountKey;
      LDestinationAccountKey := Model.TransferAccount.AccountKey;
    end;
    akaChangeAccountPrivateKey:
    begin
      LCurrentAccountKey := AItem.accountInfo.accountKey;
      LDestinationAccountKey := Model.ChangeAccountPrivateKey.NewWalletKey.AccountKey;
    end

    else
      raise ENotSupportedException.Create('ChangeKeyMode');
  end;

  if ABindingName = 'Operation' then
    case Model.ModelType of
      omtSendPasc:
      begin
        Result := IIF(Model.SendPASC.SendPASCMode = akaAllBalance, 'All Balance', Format('%s ', [TAccountComp.FormatMoney(Model.SendPASC.SingleAmountToSend)]));
        Result := Format('%s %s', [TOperationsManager.GetOperationShortText(CT_Op_Transaction, CT_OpSubtype_TransactionSender), Result]);
      end;
      omtChangeKey:
        Result := Format('%s', [TOperationsManager.GetOperationShortText(CT_Op_ChangeKeySigned, CT_OpSubtype_ChangeKey)]);
      omtEnlistAccountForSale:
        Result := Format('%s', [TOperationsManager.GetOperationShortText(CT_Op_ListAccountForSale, IIF(Model.EnlistAccountForSale.AccountSaleMode = akaPrivateSale, CT_OpSubtype_ListAccountForPrivateSale, CT_OpSubtype_ListAccountForPublicSale))]);

    end
  else if ABindingName = 'Recipient' then
    case Model.ModelType of
      omtSendPasc:
        Result := Model.SendPASC.DestinationAccount.AccountString;
      omtChangeKey:
        case Model.ChangeKey.ChangeKeyMode of
          akaTransferAccountOwnership: Result := TAccountComp.AccountPublicKeyExport(Model.TransferAccount.AccountKey);
          akaChangeAccountPrivateKey:
          begin
            Result := IIF(
              Model.ChangeAccountPrivateKey.NewWalletKey.Name = '',
              TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(Model.ChangeAccountPrivateKey.NewWalletKey.AccountKey)),
              Model.ChangeAccountPrivateKey.NewWalletKey.Name
              );
            if not Assigned(Model.ChangeAccountPrivateKey.NewWalletKey.PrivateKey) then
              Result := Result + '(*)';
          end
          else
            raise ENotSupportedException.Create('ChangeKeyMode');
        end;
      omtEnlistAccountForSale:
        Result := IIF(Model.EnlistAccountForSale.AccountSaleMode = akaPrivateSale, TAccountComp.AccountPublicKeyExport(Model.EnlistAccountForSale.NewOwnerPublicKey), '');

    end
  else if ABindingName = 'Fee' then
    Result := -Model.Fee.SingleOperationFee
  else
    Result := inherited GetItemField(AItem, ABindingName);
end;


procedure TOperationConfirmationDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
begin
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    AContainer.Add(Model.Account.SelectedAccounts[i]);
end;


end.
