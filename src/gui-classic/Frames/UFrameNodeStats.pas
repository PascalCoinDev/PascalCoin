unit UFrameNodeStats;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrameNodeStats = class(TFrame)
    PanelTop: TPanel;
    Splitter1: TSplitter;
    PanelMiddle: TPanel;
    PanelBottom: TPanel;
    Splitter2: TSplitter;
    Panel4: TPanel;
    memoNetConnections: TMemo;
    Panel1: TPanel;
    memoNetBlackLists: TMemo;
    Panel2: TPanel;
    memoNetServers: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

constructor TFrameNodeStats.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  memoNetConnections.Lines.Clear;
  memoNetServers.Lines.Clear;
  memoNetBlackLists.Lines.Clear;
end;

destructor TFrameNodeStats.Destroy;
begin

  inherited Destroy;
end;

end.
