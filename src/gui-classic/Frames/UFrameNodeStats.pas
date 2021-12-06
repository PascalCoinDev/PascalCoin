unit UFrameNodeStats;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrameNodeStats = class(TFrame)
    memoNetConnections: TMemo;
    memoNetServers: TMemo;
    memoNetBlackLists: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter2: TSplitter;
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
