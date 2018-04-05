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
  ExtCtrls, UVisualGrid, UCellRenderers, UCommon.Data, UWizard, UWIZSendPASC;

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

uses UAccounts, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
    private
      FModel : TWIZSendPASCModel;
    protected
      function GetColumns : TDataColumns; override;
    public
      property Model : TWIZSendPASCModel read FModel write FModel;
      procedure FetchAll(const AContainer : TList<TAccount>); override;
      function GetItemField(constref AItem: TAccount; const ABindingName : AnsiString) : Variant; override;
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
  with FSendersGrid.AddColumn('Sender') do begin
    Binding := 'SenderAccount';
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
  end;
  with FSendersGrid.AddColumn('Balance') do begin
    Filters := SORTABLE_NUMERIC_FILTER;
    Width := 100;
    Renderer := TCellRenderers.PASC;
  end;
  with FSendersGrid.AddColumn('Amount To Send') do begin
    Binding := 'AmountToSend';
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;
  with FSendersGrid.AddColumn('Fee') do begin
    Filters := SORTABLE_TEXT_FILTER;
    Width := 100;
  end;

   data := TAccountSenderDataSource.Create(FSendersGrid);
   data.Model := Model;
   FSendersGrid.DataSource := data;
   paGrid.AddControlDockCenter(FSendersGrid);
   lblSgnAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.SignerAccount.account);
   lblDestAcc.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Model.DestinationAccount.account);
end;

{ TAccountSenderDataSource }

function TAccountSenderDataSource.GetColumns : TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('SenderAccount'),
    TDataColumn.From('Balance'),
    TDataColumn.From('AmountToSend'),
    TDataColumn.From('Fee')
  );
end;

function TAccountSenderDataSource.GetItemField(constref AItem: TAccount; const ABindingName : AnsiString) : Variant;
var
  index : Integer;
begin
   if ABindingName = 'SenderAccount' then
     Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.account)
   else if ABindingName = 'Balance' then
     Result := TAccountComp.FormatMoney(AItem.Balance)
   else if ABindingName = 'AmountToSend' then
     Result := Model.AmountToSend
     else if ABindingName = 'Fee' then
     Result := TAccountComp.FormatMoney(Model.DefaultFee)
   else raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
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

