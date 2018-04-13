unit UWIZSendPASC_ConfirmSender;

{$mode delphi}

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
  ExtCtrls, UVisualGrid, UCommon.Data, UCellRenderers,
  UWizard, UWIZSendPASC, UWIZSendPASC_EnterRecipient, UWIZSendPASC_Confirmation, UWIZModels;

type

  { TWIZSendPASC_ConfirmSender }

  TWIZSendPASC_ConfirmSender = class(TWizardForm<TWIZOperationsModel>)
    gpSender: TGroupBox;
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

uses UAccounts, UCoreUtils, UDataSources, UCommon, UCommon.UI, Generics.Collections;

type

  { TAccountSenderDataSource }

  TAccountSenderDataSource = class(TAccountsDataSourceBase)
  private
    FModel: TWIZOperationsModel;
  public
    property Model: TWIZOperationsModel read FModel write FModel;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

{ TWIZSendPASC_ConfirmSender }

procedure TWIZSendPASC_ConfirmSender.OnPresent;
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
  with FSendersGrid.AddColumn('Account') do
  begin
    StretchedToFill := True;
    Binding := 'AccountNumber';
    SortBinding := 'AccountNumber';
    DisplayBinding := 'Account';
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FSendersGrid.AddColumn('Balance') do
  begin
    Binding := 'BalanceDecimal';
    SortBinding := 'Balance';
    DisplayBinding := 'Balance';
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  Data := TAccountSenderDataSource.Create(FSendersGrid);
  Data.Model := Model;
  FSendersGrid.DataSource := Data;
  paGrid.AddControlDockCenter(FSendersGrid);
end;

procedure TWIZSendPASC_ConfirmSender.OnNext;
begin
  UpdatePath(ptReplaceAllNext, [TWIZSendPASC_EnterRecipient, TWIZSendPASC_Confirmation]);
end;

function TWIZSendPASC_ConfirmSender.Validate(out message: ansistring): boolean;
begin
  Result := True;
  // get signer accounts from selected accounts
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Length(Model.Account.SelectedAccounts), Model.Account.SelectedAccounts);

  if Length(Model.Signer.SignerCandidates) < 1 then
  begin
    Result := False;
    message := 'no valid signer account was found.';
  end;

end;

{ TAccountSenderDataSource }

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
