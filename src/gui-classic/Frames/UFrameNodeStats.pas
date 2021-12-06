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

    procedure OnNetBlackListUpdated(Sender : TObject);

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  UNetProtocol,
  Utime,
  UConst,
  {$IFNDEF FPC}
  System.Generics.Collections
  {$ELSE}
  Generics.Collections
  {$ENDIF};

constructor TFrameNodeStats.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  memoNetConnections.Lines.Clear;
  memoNetServers.Lines.Clear;
  memoNetBlackLists.Lines.Clear;

  TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;
end;

destructor TFrameNodeStats.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameNodeStats.OnNetBlackListUpdated(Sender: TObject);
Const CT_TRUE_FALSE : Array[Boolean] Of AnsiString = ('FALSE','TRUE');
Var i,j,n : integer;
 P : PNodeServerAddress;
 l : TList<Pointer>;
 strings : TStrings;
begin
  l := TNetData.NetData.NodeServersAddresses.LockList;
  try
    strings := memoNetBlackLists.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('BlackList Updated: '+DateTimeToStr(now)+' by TID:'+IntToHex(PtrInt(TThread.CurrentThread.ThreadID),8));
      j := 0; n:=0;
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        if (P^.is_blacklisted) then begin
          inc(n);
          if Not P^.its_myself then begin
            inc(j);
            strings.Add(Format('Blacklist IP:%s:%d LastConnection:%s Reason: %s',
              [
               P^.ip,P^.port,
               DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection))),P^.BlackListText]));
          end;
        end;
      end;
      Strings.Add(Format('Total Blacklisted IPs: %d (Total %d)',[j,n]));
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.NodeServersAddresses.UnlockList;
  end;
end;




end.
