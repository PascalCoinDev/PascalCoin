unit UWIZSendPASC_Confirmation;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCommon.Data, UWizard, UWIZSendPASC;

type

  { TWIZSendPASC_Confirmation }

  TWIZSendPASC_Confirmation = class(TWizardForm<TWIZSendPASCModel>)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblDestAcc: TLabel;
    lblSgnAcc: TLabel;
    paGrid: TPanel;
  private
    FSendersGrid : TVisualGrid;
  public
    procedure OnPresent; override;
  end;


implementation

{$R *.lfm}

uses UAccounts, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TCustomDataSource<TAccount>)
    private
      FModel : TWIZSendPASCModel;
    protected
      function GetColumns : TTableColumns;  override;
    public
      property Model : TWIZSendPASCModel read FModel write FModel;
      function GetSearchCapabilities: TSearchCapabilities; override;
      function GetEntityKey(constref AItem: TAccount) : Variant; override;
      procedure FetchAll(const AContainer : TList<TAccount>); override;
      function GetItemField(constref AItem: TAccount; const AColumnName : utf8string) : Variant; override;
      procedure DehydrateItem(constref AItem: TAccount; var ATableRow: Variant); override;
  end;

{ TWIZSendPASC_Confirmation }

procedure TWIZSendPASC_Confirmation.OnPresent;
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
  FSendersGrid.DefaultColumnWidths :=
    TArray<integer>.Create(
    100, // Sender Account
    100, // Balance
    100, // AmountToSend
    100 // Fee
   );
   data := TAccountSenderDataSource.Create(FSendersGrid);
   data.Model := Model;
   FSendersGrid.DataSource := data;
   paGrid.AddControlDockCenter(FSendersGrid);
   lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.SignerAccount.account);
   lblDestAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.DestinationAccount.account);
end;

{ TAccountSenderDataSource }

function TAccountSenderDataSource.GetColumns : TTableColumns;
begin
  Result := TTableColumns.Create('SenderAccount', 'Balance', 'AmountToSend', 'Fee');
end;

function TAccountSenderDataSource.GetSearchCapabilities: TSearchCapabilities;
begin
  Result := TSearchCapabilities.Create(
    TSearchCapability.From('SenderAccount', SORTABLE_NUMERIC_FILTER),
    TSearchCapability.From('Balance', SORTABLE_TEXT_FILTER),
    TSearchCapability.From('AmountToSend', SORTABLE_TEXT_FILTER),
    TSearchCapability.From('Fee', SORTABLE_TEXT_FILTER)
  );
end;

function TAccountSenderDataSource.GetEntityKey(constref AItem: TAccount) : Variant;
begin
  Result := AItem.account;
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const AColumnName : utf8string) : Variant;
var
  index : Integer;
begin
   if AColumnName = 'SenderAccount' then
     Result := AItem.account
   else if AColumnName = 'Balance' then
     Result := TAccountComp.FormatMoneyDecimal(AItem.Balance)
   else if AColumnName = 'AmountToSend' then
     Result := Model.AmountToSend
     else if AColumnName = 'Fee' then
     Result := TAccountComp.FormatMoney(Model.DefaultFee)
   else raise Exception.Create(Format('Field not found [%s]', [AColumnName]));
end;

procedure TAccountSenderDataSource.DehydrateItem(constref AItem: TAccount; var ATableRow: Variant);
begin
  ATableRow.SenderAccount := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account);
  ATableRow.Balance := TAccountComp.FormatMoney(AItem.balance);
  ATableRow.AmountToSend := Model.AmountToSend;
  ATableRow.Fee := TAccountComp.FormatMoney(Model.DefaultFee);
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

