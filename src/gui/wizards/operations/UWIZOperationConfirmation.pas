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
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard, UWIZOperation, UCoreObjects;

type

  { TWIZOperationConfirmation }

  TWIZOperationConfirmation = class(TWizardForm<TWIZOperationsModel>)
    gpConfirmOperation: TGroupBox;
    lblPayload: TLabel;
    lblPayload1: TLabel;
    lblPayload2: TLabel;
    lblSignerAccount: TLabel;
    lblBeneficiaryAccount: TLabel;
    lblSgnAcc: TLabel;
    lblBenAcc: TLabel;
    mmoPayload: TMemo;
    mmoPayload1: TMemo;
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
    FModel: TWIZOperationsModel;

  protected
    function GetColumns: TDataColumns; override;
  public
    property Model: TWIZOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    function GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant; override;
  end;

{ TWIZOperationConfirmation }

procedure TWIZOperationConfirmation.OnPresent;
var
  LData: TOperationConfirmationDataSource;
  LCaption: string;
begin

  FConfirmationGrid := TVisualGrid.Create(Self);
  FConfirmationGrid.CanSearch := False;
  FConfirmationGrid.SortMode := smMultiColumn;
  FConfirmationGrid.FetchDataInThread := False;
  FConfirmationGrid.AutoPageSize := True;
  FConfirmationGrid.SelectionType := stNone;
  FConfirmationGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  LCaption := 'Confirm Operation';
  gpConfirmOperation.Caption := IIF(Length(Model.Account.SelectedAccounts) > 1, Format('%ss', [LCaption]), Format('%s', [LCaption]));
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

  LData := TOperationConfirmationDataSource.Create(FConfirmationGrid);
  LData.Model := Model;
  FConfirmationGrid.DataSource := LData;
  paGrid.AddControlDockCenter(FConfirmationGrid);
  lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
  if not (Model.ExecuteOperationType = omtEnlistAccountForSale) then
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


  if not Model.Payload.HasPayload then
  begin
    lblPayload.Visible := False;
    mmoPayload.Visible := False;
    paGrid.SetBounds(paGrid.Left, paGrid.Top, paGrid.Width, gpConfirmOperation.Height - 50);
  end
  else
  begin
    lblPayload.Visible := True;
    mmoPayload.Visible := True;
    mmoPayload.Lines.Text := Model.Payload.Content;
    paGrid.SetBounds(paGrid.Left, paGrid.Top, paGrid.Width, gpConfirmOperation.Height - mmoPayload.Height - lblPayload.Height - 60);
  end;
end;

procedure TWIZOperationConfirmation.OnNext;
var
  LLocked: boolean;
begin
  LLocked := (not TWallet.Keys.HasPassword) or (not TWallet.Keys.IsValidPassword);
  if LLocked then
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
    case Model.ExecuteOperationType of
      omtSendPasc:
      begin
        Result := IIF(Model.SendPASC.SendPASCMode = akaAllBalance, 'All Balance', Format('%s ', [TAccountComp.FormatMoney(Model.SendPASC.SingleAmountToSend)]));
        Result := Format('%s %s', [TCoreTool.GetOperationShortText(CT_Op_Transaction, CT_OpSubtype_TransactionSender), Result]);
      end;
      omtChangeKey:
        Result := Format('%s', [TCoreTool.GetOperationShortText(CT_Op_ChangeKeySigned, CT_OpSubtype_ChangeKey)]);
      omtEnlistAccountForSale:
        Result := Format('%s', [TCoreTool.GetOperationShortText(CT_Op_ListAccountForSale, IIF(Model.EnlistAccountForSale.AccountSaleMode = akaPrivateSale, CT_OpSubtype_ListAccountForPrivateSale, CT_OpSubtype_ListAccountForPublicSale))]);

    end
  else if ABindingName = 'Recipient' then
    case Model.ExecuteOperationType of
      omtSendPasc:
      begin
        Result := Model.SendPASC.DestinationAccount.AccountString;
        Result := TCellRenderers.OperationShortHash(Result);
      end;
      omtChangeKey:
        case Model.ChangeKey.ChangeKeyMode of
          akaTransferAccountOwnership:
          begin
            Result := TAccountComp.AccountPublicKeyExport(Model.TransferAccount.AccountKey);
            Result := TCellRenderers.OperationShortHash(Result);
          end;
          akaChangeAccountPrivateKey:
          begin
            Result := IIF(
              Model.ChangeAccountPrivateKey.NewWalletKey.Name = '',
              TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(Model.ChangeAccountPrivateKey.NewWalletKey.AccountKey)),
              Model.ChangeAccountPrivateKey.NewWalletKey.Name
              );
            if not Assigned(Model.ChangeAccountPrivateKey.NewWalletKey.PrivateKey) then
              Result := Result + '(*)';
            Result := TCellRenderers.OperationShortHash(Result);
          end
          else
            raise ENotSupportedException.Create('ChangeKeyMode');
        end;
      omtEnlistAccountForSale:
      begin
        Result := IIF(Model.EnlistAccountForSale.AccountSaleMode = akaPrivateSale, TAccountComp.AccountPublicKeyExport(Model.EnlistAccountForSale.NewOwnerPublicKey), '');
        Result := TCellRenderers.OperationShortHash(Result);
      end;
    end
  else if ABindingName = 'Fee' then
    Result := -Model.Fee.SingleOperationFee
  else
    Result := inherited GetItemField(AItem, ABindingName);
end;


procedure TOperationConfirmationDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  LIdx: integer;
begin
  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    AContainer.Add(Model.Account.SelectedAccounts[LIdx]);
end;


end.
