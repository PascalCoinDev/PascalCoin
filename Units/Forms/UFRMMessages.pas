unit UFRMMessages;

{$mode delphi}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus,
  UNode, UNetProtocol, UCrypto, UFRMWallet,UConst;

type

  { TFRMMessages }

  TFRMMessages = class(TForm)
    bbSendAMessage: TButton;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lbNetConnections: TListBox;
    memoMessages: TMemo;
    memoMessageToSend: TMemo;
    procedure bbSendAMessageClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMessagesUnreadCount : Integer;
    { private declarations }
    procedure UpdateAvailableConnections;

  public
    { public declarations }
    Procedure OnNodeMessageEvent(NetConnection : TNetConnection; MessageData : TRawBytes);
  end;

var
  FRMMessages: TFRMMessages = nil;

implementation
uses UFRMSyncronizationDialog, UUserInterface;// after implementation!
{$R *.lfm}

{ TFRMMessages }

procedure TFRMMessages.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: TRawBytes);
Var s : String;
begin
  inc(FMessagesUnreadCount);
  if Assigned(NetConnection) then begin
    s := DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr;
    memoMessages.Lines.Add(DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr+' Length '+inttostr(Length(MessageData))+' bytes');
    memoMessages.Lines.Add('RECEIVED> '+MessageData);
    //HS if FRMWallet.FAppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false) then begin
    if TUserInterface.AppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false) then begin
      s := DateTimeToStr(now)+' Message from '+NetConnection.ClientRemoteAddr+#10+
         'Length '+inttostr(length(MessageData))+' bytes'+#10+#10;
      if TCrypto.IsHumanReadable(MessageData) then begin
         s := s + MessageData;
      end else begin
         s := s +'Value in hexadecimal:'+#10+
              TCrypto.ToHexaString(MessageData);
      end;
      Application.MessageBox(PChar(s),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
    end;
  end else begin
    memoMessages.Lines.Add(DateTimeToStr(now)+' Internal message: '+MessageData);
  end;
  if FMessagesUnreadCount>1 then TUserInterface.MessagesNotificationText := Format('You have received %d messages',[FMessagesUnreadCount])
  else TUserInterface.MessagesNotificationText := 'You have received 1 message';
end;

procedure TFRMMessages.UpdateAvailableConnections;
Var i : integer;
 NC : TNetConnection;
 l : TList;
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

procedure TFRMMessages.bbSendAMessageClick(Sender: TObject);
Var basem,m : String;
  them, errors : AnsiString;
  i,n : Integer;
  nc : TNetConnection;
begin
  TUserInterface.CheckNodeIsReady;//CheckIsReady;
  if (lbNetConnections.SelCount<=0) Or (lbNetConnections.ItemIndex<0) then raise Exception.Create('Select at least one connection');
  if lbNetConnections.SelCount<=0 then n := 1
  else n := lbNetConnections.SelCount;

  basem := memoMessageToSend.Lines.Text;
  m := '';
  // Clear non valid characters:
  for i := 1 to length(basem) do begin
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
          TUserInterface.Node.SendNodeMessage(nc,m,errors);
          memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
        end;
      end;
    end;
  end else begin
    nc := TNetConnection(lbNetconnections.Items.Objects[lbNetconnections.ItemIndex]);
    if TNetData.NetData.ConnectionExistsAndActive(nc) then begin
      TUserInterface.Node.SendNodeMessage(nc,m,errors);
      memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
    end;
  end;

  Application.MessageBox(PChaR('Message sent to '+inttostr(n)+' nodes'+#10+
    'Message: '+#10+m),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
end;

procedure TFRMMessages.FormActivate(Sender: TObject);
begin
  UpdateAvailableConnections;
  //HS FNodeNotifyEvents.Node := TUserInterface.Node;
  TUserInterface.MessagesNotificationText := '';
end;

procedure TFRMMessages.FormCreate(Sender: TObject);
begin
  //HS FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  //HS FNodeNotifyEvents.OnBlocksChanged := TUserInterface.Wallet.OnNewAccount; //TODO: may be need move this function
  //HS FNodeNotifyEvents.OnNodeMessageEvent := OnNodeMessageEvent;
end;

procedure TFRMMessages.FormDestroy(Sender: TObject);
begin
  //HS FNodeNotifyEvents.Node := Nil;
  //HS FreeAndNil(FNodeNotifyEvents);
end;

end.
