unit UCTRLWallet;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UVisualGrid, UCommonUI;

type

  { TCTRLWallet }

  TCTRLWalletAccountView = (wavAllAccounts, wavMyAccounts, wavFirstAccount);
  TCTRLWallet = class(TApplicationForm)
    cbMyAccViews: TComboBox;
    cbRecentOpsSelectedAccOnly: TCheckBox;
    GroupBox1: TGroupBox;
    gpMyAccounts: TGroupBox;
    gpRecentOps: TGroupBox;
    paMyAccContent: TPanel;
    procedure cbMyAccViewsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAccountsView : TCTRLWalletAccountView;
    FAccountsGrid : TVisualGrid;
    procedure SetAccountsView(view: TCTRLWalletAccountView);
  protected
    procedure ActivateFirstTime; override;
  public
    property AccountsView : TCTRLWalletAccountView read FAccountsView write SetAccountsView;
  end;

implementation

{$R *.lfm}

uses UGrids;

{ TCTRLWallet }

procedure TCTRLWallet.FormCreate(Sender: TObject);
begin
  FAccountsGrid := TVisualGrid.Create(Self);
  FAccountsGrid.DataSource := TMyAccountDataSource.Create(FAccountsGrid);
  AccountsView := wavMyAccounts;
end;

procedure TCTRLWallet.ActivateFirstTime;
begin
end;

procedure TCTRLWallet.SetAccountsView(view: TCTRLWalletAccountView);
begin
  paMyAccContent.RemoveAllControls(false);
  case view of
     wavAllAccounts: raise Exception.Create('Not implemented');
     wavMyAccounts: paMyAccContent.AddControlDockCenter(FAccountsGrid);
     wavFirstAccount: raise Exception.Create('Not implemented');
  end;
end;

procedure TCTRLWallet.cbMyAccViewsChange(Sender: TObject);
begin
  case cbMyAccViews.ItemIndex of
     0: AccountsView := wavAllAccounts;
     1: AccountsView := wavMyAccounts;
     2: AccountsView := wavFirstAccount;
  end;
end;

end.

