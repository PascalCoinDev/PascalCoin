unit UFrameMessages;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  UNetProtocol;

type
  TFrameMessages = class(TFrame)
    PanelBottom: TPanel;
    Splitter1: TSplitter;
    PanelTop: TPanel;
    PanelLeft: TPanel;
    Splitter2: TSplitter;
    PanelRight: TPanel;
    memoMessageToSend: TMemo;
    lbNetConnections: TListBox;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Label14: TLabel;
    bbSendAMessage: TButton;
    Panel1: TPanel;
    memoMessages: TMemo;
    procedure bbSendAMessageClick(Sender: TObject);
  private
    { Private declarations }
    FMessagesUnreadCount : Integer;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateAvailableConnections;

    procedure OnNodeMessageEvent(NetConnection : TNetConnection; MessageData : String);

    property MessagesUnreadCount : integer read FMessagesUnreadCount write FMessagesUnreadCount;
  end;

implementation

{$R *.dfm}

uses
  UFRMWallet, USettings, UCrypto,
  {$IFNDEF FPC}
  System.Generics.Collections
  {$ELSE}
  Generics.Collections
  {$ENDIF};

constructor TFrameMessages.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  memoMessages.Lines.Clear;
  memoMessageToSend.Lines.Clear;

  FMessagesUnreadCount := 0;
end;

destructor TFrameMessages.Destroy;
begin


  inherited Destroy;
end;

procedure TFrameMessages.bbSendAMessageClick(Sender: TObject);
Var basem,m : String;
  them, errors : String;
  i,n : Integer;
  nc : TNetConnection;
begin
  FRMWallet.CheckIsReady;
  if (lbNetConnections.SelCount<=0) Or (lbNetConnections.ItemIndex<0) then raise Exception.Create('Select at least one connection');
  if lbNetConnections.SelCount<=0 then n := 1
  else n := lbNetConnections.SelCount;

  basem := memoMessageToSend.Lines.Text;
  m := '';
  // Clear non valid characters:
  for i := Low(basem) to High(basem) do begin
    if basem[i] in [#32..#127] then m := m + basem[i]
    else m:=m+'.';
  end;

  if trim(m)='' then raise Exception.Create('No message');

  if Application.MessageBox(PChaR('Send this message to '+inttostr(n)+' nodes?'+#10+
    'NOTE: Sending unauthorized messages will be considered spam and you will be banned'+#10+
    #10+
    'Message: '+#10+
    m),PChar(Application.Title),MB_ICONQUESTION+MB_YESNO+MB_DEFBUTTON1)<>IdYes then exit;
  them := m;
  if n>1 then begin
    for i := 0 to lbNetConnections.Items.Count - 1 do begin
      if lbNetConnections.Selected[i] then begin
        nc := TNetConnection(lbNetconnections.Items.Objects[i]);
        if TNetData.NetData.ConnectionExistsAndActive(nc) then begin
          FRMWallet.Node.SendNodeMessage(nc,m,errors);
          memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
        end;
      end;
    end;
  end else begin
    nc := TNetConnection(lbNetconnections.Items.Objects[lbNetconnections.ItemIndex]);
    if TNetData.NetData.ConnectionExistsAndActive(nc) then begin
      FRMWallet.Node.SendNodeMessage(nc,m,errors);
      memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
    end;
  end;

  Application.MessageBox(PChaR('Message sent to '+inttostr(n)+' nodes'+#10+
    'Message: '+#10+m),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
end;

procedure TFrameMessages.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: String);
Var s : String;
begin
  inc(FMessagesUnreadCount);
  if Assigned(NetConnection) then begin
    s := DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr;
    memoMessages.Lines.Add(DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr+' Length '+inttostr(Length(MessageData))+' bytes');
    memoMessages.Lines.Add('RECEIVED> '+MessageData);
    if TSettings.ShowModalMessages then begin
      s := DateTimeToStr(now)+' Message from '+NetConnection.ClientRemoteAddr+#10+
         'Length '+inttostr(length(MessageData))+' bytes'+#10+#10;
      if TCrypto.IsHumanReadable(TEncoding.ANSI.GetBytes(MessageData)) then begin
         s := s + MessageData;
      end else begin
         s := s +'Value in hexadecimal:'+#10+
              TCrypto.ToHexaString(TEncoding.ANSI.GetBytes(MessageData));
      end;
      Application.MessageBox(PChar(s),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
    end;
  end else begin
    memoMessages.Lines.Add(DateTimeToStr(now)+' Internal message: '+MessageData);
  end;
  if FMessagesUnreadCount>1 then
    FRMWallet.FrameInfo.lblReceivedMessages.Caption := Format('You have received %d messages',[FMessagesUnreadCount])
  else
    FRMWallet.FrameInfo.lblReceivedMessages.Caption := 'You have received 1 message';
  FRMWallet.FrameInfo.lblReceivedMessages.Visible := true;
end;

procedure TFrameMessages.UpdateAvailableConnections;
Var i : integer;
 NC : TNetConnection;
 l : TList<TNetConnection>;
begin
  if Not TNetData.NetData.NetConnections.TryLockList(100,l) then exit;
  try
    lbNetConnections.Items.BeginUpdate;
    Try
      lbNetConnections.Items.Clear;
      for i := 0 to l.Count - 1 do begin
        NC := l[i];
        if NC.Connected then begin
          if NC is TNetServerClient then begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Client: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end else begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Server: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end;
        end;
      end;
    Finally
      lbNetConnections.Items.EndUpdate;
    End;
  finally
    TNetData.NetData.NetConnections.UnlockList;
  end;
end;

end.
