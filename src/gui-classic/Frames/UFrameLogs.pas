unit UFrameLogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrameLogs = class(TFrame)
    memoLogs: TMemo;
    pnlTopLogs: TPanel;
    cbShowDebugLogs: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

constructor TFrameLogs.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

end;

destructor TFrameLogs.Destroy;
begin

  inherited Destroy;
end;

end.
