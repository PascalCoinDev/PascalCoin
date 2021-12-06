unit UFrameNodeStats;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  UFRMWalletUserMessages;

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

    FMustProcessNetConnectionUpdated : Boolean;

    procedure OnNetConnectionsUpdated(Sender : TObject);
    procedure OnNetBlackListUpdated(Sender : TObject);
    procedure OnNetNodeServersUpdated(Sender: TObject);

    procedure CM_NetConnectionUpdated(var Msg: TMessage); message CM_PC_NetConnectionUpdated;


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

  FMustProcessNetConnectionUpdated := false;

  TNetData.NetData.OnNetConnectionsUpdated := OnNetConnectionsUpdated;
  TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;
  TNetData.NetData.OnNodeServersUpdated := OnNetNodeServersUpdated;
end;

destructor TFrameNodeStats.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameNodeStats.OnNetConnectionsUpdated(Sender: TObject);
begin
  if FMustProcessNetConnectionUpdated then exit;
  FMustProcessNetConnectionUpdated := true;
  PostMessage(Self.Handle,CM_PC_NetConnectionUpdated,0,0);
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

procedure TFrameNodeStats.OnNetNodeServersUpdated(Sender: TObject);
Var i : integer;
 P : PNodeServerAddress;
 l : TList<Pointer>;
 strings : TStrings;
 s : String;
begin
  l := TNetData.NetData.NodeServersAddresses.LockList;
  try
    strings := memoNetServers.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('NodeServers Updated: '+DateTimeToStr(now) +' Count: '+inttostr(l.Count));
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        if Not (P^.is_blacklisted) then begin
          s := Format('Server IP:%s:%d',[P^.ip,P^.port]);
          if (P^.last_connection_by_me>0) then begin
            s := s + ' [Server] ';
          end;

          if Assigned(P.netConnection) then begin
            If P.last_connection>0 then  s := s+ ' ** ACTIVE **'
            else s := s+' ** TRYING TO CONNECT **';
          end;
          if P.its_myself then begin
            s := s+' ** NOT VALID ** '+P.BlackListText;
          end;
          if P.last_connection>0 then begin
            s := s + ' Last connection: '+DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection)));
          end;
          if P.last_connection_by_server>0 then begin
            s := s + ' Last server connection: '+DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection_by_server)));
          end;
          if (P.last_attempt_to_connect>0) then begin
            s := s + ' Last attempt to connect: '+DateTimeToStr(P^.last_attempt_to_connect);
          end;
          if (P.total_failed_attemps_to_connect>0) then begin
            s := s + ' (Attempts: '+inttostr(P^.total_failed_attemps_to_connect)+')';
          end;

          strings.Add(s);
        end;
      end;
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.NodeServersAddresses.UnlockList;
  end;
end;


procedure TFrameNodeStats.CM_NetConnectionUpdated(var Msg: TMessage);
Const CT_BooleanToString : Array[Boolean] of String = ('False','True');
Var i : integer;
 NC : TNetConnection;
 l : TList<TNetConnection>;
 sClientApp, sLastConnTime : String;
 strings, sNSC, sRS, sDisc : TStrings;
 hh,nn,ss,ms : Word;
begin
  Try
    if Not TNetData.NetData.NetConnections.TryLockList(100,l) then exit;
    try
      strings := memoNetConnections.Lines;
      sNSC := TStringList.Create;
      sRS := TStringList.Create;
      sDisc := TStringList.Create;
      strings.BeginUpdate;
      Try
        for i := 0 to l.Count - 1 do begin
          NC := l[i];
          If NC.Client.BytesReceived>0 then begin
            sClientApp := '['+IntToStr(NC.NetProtocolVersion.protocol_version)+'-'+IntToStr(NC.NetProtocolVersion.protocol_available)+'] '+NC.ClientAppVersion;
          end else begin
            sClientApp := '(no data)';
          end;

          if NC.Connected then begin
            if NC.Client.LastCommunicationTime>1000 then begin
              DecodeTime(now - NC.Client.LastCommunicationTime,hh,nn,ss,ms);
              if (hh=0) and (nn=0) And (ss<10) then begin
                sLastConnTime := ' - Last comunication <10 sec.';
              end else begin
                sLastConnTime := Format(' - Last comunication %.2dm%.2ds',[(hh*60)+nn,ss]);
              end;
            end else begin
              sLastConnTime := '';
            end;
            if NC is TNetServerClient then begin
              sNSC.Add(Format('Client: IP:%s Block:%d Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.RemoteOperationBlock.block,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]));
            end else begin
              if NC.IsMyselfServer then sNSC.Add(Format('MySelf IP:%s Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]))
              else begin
                sRS.Add(Format('Remote Server: IP:%s Block:%d Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.RemoteOperationBlock.block,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]));
              end;
            end;
          end else begin
            if NC is TNetServerClient then begin
              sDisc.Add(Format('Disconnected client: IP:%s - %s',[NC.ClientRemoteAddr,sClientApp]));
            end else if NC.IsMyselfServer then begin
              sDisc.Add(Format('Disconnected MySelf IP:%s - %s',[NC.ClientRemoteAddr,sClientApp]));
            end else begin
              sDisc.Add(Format('Disconnected Remote Server: IP:%s %s - %s',[NC.ClientRemoteAddr,CT_BooleanToString[NC.Connected],sClientApp]));
            end;
          end;
        end;
        strings.Clear;
        strings.Add(Format('Connections Updated %s Clients:%d Servers:%d (valid servers:%d)',[DateTimeToStr(now),sNSC.Count,sRS.Count,TNetData.NetData.NetStatistics.ServersConnectionsWithResponse]));
        strings.AddStrings(sRS);
        strings.AddStrings(sNSC);
        if sDisc.Count>0 then begin
          strings.Add('');
          strings.Add('Disconnected connections: '+Inttostr(sDisc.Count));
          strings.AddStrings(sDisc);
        end;
      Finally
        strings.EndUpdate;
        sNSC.Free;
        sRS.Free;
        sDisc.Free;
      End;
      //CheckMining;
    finally
      TNetData.NetData.NetConnections.UnlockList;
    end;
  Finally
    FMustProcessNetConnectionUpdated := false;
  End;
end;




end.
