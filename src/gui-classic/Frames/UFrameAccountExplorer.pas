unit UFrameAccountExplorer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TFrameAccountExplorer = class(TFrame)
    Splitter1: TSplitter;
    pnlMyAccountsTop: TPanel;
    Label18: TLabel;
    sbSearchAccount: TSpeedButton;
    cbMyPrivateKeys: TComboBox;
    cbExploreMyAccounts: TCheckBox;
    ebFindAccountNumber: TEdit;
    bbChangeKeyName: TBitBtn;
    cbFilterAccounts: TCheckBox;
    ebFilterAccountByBalanceMin: TEdit;
    ebFilterAccountByBalanceMax: TEdit;
    pnlAccounts: TPanel;
    dgAccounts: TDrawGrid;
    pnlAccountsInfo: TPanel;
    Label17: TLabel;
    Label19: TLabel;
    lblAccountsCount: TLabel;
    lblAccountsBalance: TLabel;
    bbAccountsRefresh: TBitBtn;
    pcAccountsOptions: TPageControl;
    tsAccountOperations: TTabSheet;
    dgAccountOperations: TDrawGrid;
    tsMultiSelectAccounts: TTabSheet;
    dgSelectedAccounts: TDrawGrid;
    pnlSelectedAccountsTop: TPanel;
    Label15: TLabel;
    pnlSelectedAccountsBottom: TPanel;
    Label20: TLabel;
    lblSelectedAccountsCount: TLabel;
    Label22: TLabel;
    lblSelectedAccountsBalance: TLabel;
    pnlSelectedAccountsLeft: TPanel;
    sbSelectedAccountsAdd: TSpeedButton;
    sbSelectedAccountsAddAll: TSpeedButton;
    sbSelectedAccountsDel: TSpeedButton;
    sbSelectedAccountsDelAll: TSpeedButton;
    bbSelectedAccountsOperation: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
