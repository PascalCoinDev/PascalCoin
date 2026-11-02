unit UWIZOperationConfirmation;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


{$IFDEF FPC}
{$modeswitch nestedprocvars}
{$ENDIF}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, UVisualGrid, UCellRenderers, UWizard, UWIZOperation;

type

  { TWIZOperationConfirmation }

  TWIZOperationConfirmation = class(TWizardForm<TWIZOperationsModel>)
    gpConfirmOperation: TGroupBox;
    lblPayload: TLabel;
    lblPayload1: TLabel;
    lblPayload2: TLabel;
    lblBeneficiaryAccount: TLabel;
    lblBenAcc: TLabel;
    mmoPayload: TMemo;
    mmoPayload1: TMemo;
    paGrid: TPanel;
  private
    FConfirmationGrid: TVisualGrid;
    procedure UnlockWallet(Sender: TObject);
  public
    procedure OnPresent; override;
    function Validate(out message: String): Boolean; override;
  end;


implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  UConst,
  UCrypto,
  UWallet,
  UCommon,
  UAccounts,
  UPCDataTypes,
  UCoreUtils,
  UCoreObjects,
  UCommon.UI,
  UCommon.Data,
  UDataSources,
  UUserInterface,
  Generics.Collections;

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
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TAccount; const ABindingName: String): variant; override;
  end;

{ TWIZOperationConfirmation }

procedure TWIZOperationConfirmation.OnPresent;
var
  LData: TOperationConfirmationDataSource;
  LCaption: string;
  LTargetCount: Integer;
begin

  lblBeneficiaryAccount.Visible := False;
  lblBenAcc.Visible := False;

  FreeAndNil(FConfirmationGrid);
  FConfirmationGrid := TVisualGrid.Create(Self);
  FConfirmationGrid.CanSearch := False;
  FConfirmationGrid.SortMode := smMultiColumn;
  FConfirmationGrid.FetchDataInThread := False;
  FConfirmationGrid.AutoPageSize := True;
  FConfirmationGrid.SelectionType := stNone;
  FConfirmationGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging];
  LCaption := 'Confirm Operation';
  LTargetCount := Model.Account.Count;
  if Model.ExecuteOperationType = omtChangeInfo then
    LTargetCount := Length(Model.ChangeAccountInfo.TargetAccounts);
  gpConfirmOperation.Caption := IIF(LTargetCount > 1, Format('%ss', [LCaption]), Format('%s', [LCaption]));
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
  with FConfirmationGrid.AddColumn('Signer') do
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
  // Synchronous fetching can measure native cells, so establish parenting first.
  paGrid.AddControlDockCenter(FConfirmationGrid);
  FConfirmationGrid.DataSource := LData;

  case Model.ExecuteOperationType of
    omtEnlistAccountForSale:
    begin
      lblBeneficiaryAccount.Visible := True;
      lblBenAcc.Visible := True;
      lblBenAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.EnlistAccountForSale.SellerAccount.account);
    end;
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

procedure TWIZOperationConfirmation.UnlockWallet(Sender: TObject);
begin
  TUserInterface.UnlockWallet(Self);
end;

function TWIZOperationConfirmation.Validate(out message: String): Boolean;
begin
  Result := Model.TryUnlockWallet(TWallet.Keys, UnlockWallet, message);
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
    TDataColumn.From('Signer'),
    TDataColumn.From('Fee')
    )
    ]);
end;

function TOperationConfirmationDataSource.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TAccount; const ABindingName: String): variant;
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
      omtDelistAccountFromSale:
        Result := Format('%s', [TCoreTool.GetOperationShortText(CT_Op_DelistAccount, CT_OpSubtype_DelistAccount)]);
      omtChangeInfo:
        Result := Format('%s', [TCoreTool.GetOperationShortText(CT_Op_ChangeAccountInfo, CT_OpSubtype_ChangeAccountInfo)]);
      omtBuyAccount:
        Result := Format('%s', [TCoreTool.GetOperationShortText(CT_Op_BuyAccount, CT_OpSubtype_BuyAccountBuyer)]);

    end
  else if ABindingName = 'Recipient' then
    case Model.ExecuteOperationType of
      omtSendPasc:
      begin
        Result := TAccountComp.AccountNumberToAccountTxtNumber(Model.SendPASC.DestinationAccount.account);
        Result := TCellRenderers.OperationShortHash(Result);
      end;
      omtChangeKey:
        case Model.ChangeKey.ChangeKeyMode of
          akaTransferAccountOwnership:
          begin
            Result := TAccountComp.AccountPublicKeyExport(LDestinationAccountKey);
            Result := TCellRenderers.OperationShortHash(Result);
          end;
          akaChangeAccountPrivateKey:
          begin
            Result := IIF(
              Model.ChangeAccountPrivateKey.NewWalletKey.Name = '',
              TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(LDestinationAccountKey)),
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
      omtDelistAccountFromSale, omtChangeInfo:
      begin
        Result := '';
      end;
      omtBuyAccount:
      begin
        Result := TAccountComp.AccountPublicKeyExport(Model.BuyAccount.NewOwnerPublicKey);
        Result := TCellRenderers.OperationShortHash(Result);
      end
    end
  else if ABindingName = 'Signer' then
    if Model.ExecuteOperationType = omtChangeInfo then
    begin
      if Model.ChangeAccountInfo.UseSingleFeePayer then
        Result := TAccountComp.AccountNumberToAccountTxtNumber(Model.ChangeAccountInfo.FeePayerAccount.account)
      else
        Result := inherited GetItemField(AItem, 'Account');
    end
    else if (Model.Signer.OperationSigningMode = akaSecondary) and
      (AItem.balance >= UInt64(Model.Fee.SingleOperationFee)) then
      Result := inherited GetItemField(AItem, 'Account')
    else
    case Model.ExecuteOperationType of
      omtSendPasc, omtBuyAccount:
      begin
        Result := inherited GetItemField(AItem, 'Account');
      end;
      else
      begin
        Result := TAccountComp.AccountNumberToAccountTxtNumber(Model.Signer.SignerAccount.account);
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
  LAccounts: TArray<TAccount>;
begin
  LAccounts := Model.Account.SelectedAccounts;
  if Model.ExecuteOperationType = omtChangeInfo then
    LAccounts := Model.ChangeAccountInfo.TargetAccounts;
  // Confirmation and its fee rows contain only the approved updates.
  for LIdx := Low(LAccounts) to High(LAccounts) do
    AContainer.Add(LAccounts[LIdx]);
end;


end.
