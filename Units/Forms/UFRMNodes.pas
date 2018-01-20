unit UFRMNodes;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Portions of methods were copied from original UFRMWallet.pas, Copyright (c) Albert Molina 2016.
}

interface

{$I ./../PascalCoin/config.inc}

uses
  LCLIntf, LCLType,
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, LMessages, UCommonUI,
  ULog,  UBlockChain, UNode, Menus,  UNetProtocol;

Const
  CM_PC_NetConnectionUpdated = WM_USER + 1;
  CM_PC_BlackListUpdated = WM_USER + 2;
  CM_PC_NetNodeServersUpdated = WM_USER + 3;

type

  { TFRMNodes }

  TFRMNodes = class(TApplicationForm)
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    memoNetBlackLists: TMemo;
    memoNetConnections: TMemo;
    memoNetServers: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure CM_NetConnectionUpdated(var Msg: TMessage); message CM_PC_NetConnectionUpdated;
    procedure CM_BlackListUpdated(var Msg: TMessage); message CM_PC_BlackListUpdated;
    procedure CM_NetNodeServersUpdated(var Msg: TMessage); message CM_PC_NetNodeServersUpdated;
  public
    { public declarations }

    // TODO - refactor this out with TNotifyManyEvent so form subscribes directly to event
    procedure OnNetConnectionsUpdated;
    procedure OnNetBlackListUpdated;
    procedure OnNetNodeServersUpdated;
  end;

implementation

Uses UTime;

{$R *.lfm}

{ TFRMNodes }

procedure TFRMNodes.FormCreate(Sender: TObject);
begin
  OnNetConnectionsUpdated;
  OnNetNodeServersUpdated;
  OnNetBlackListUpdated;
end;


procedure TFRMNodes.CM_NetConnectionUpdated(var Msg: TMessage);
Const CT_BooleanToString : Array[Boolean] of String = ('False','True');
Var i : integer;
 NC : TNetConnection;
 l : TList;
 sClientApp, sLastConnTime : String;
 strings, sNSC, sRS, sDisc : TStrings;
 hh,nn,ss,ms : Word;
begin
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
end;

procedure TFRMNodes.OnNetConnectionsUpdated;
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_NetConnectionUpdated,0,0);
end;

procedure TFRMNodes.CM_NetNodeServersUpdated(var Msg: TMessage);
Var i : integer;
 P : PNodeServerAddress;
 l : TList;
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

procedure TFRMNodes.OnNetNodeServersUpdated;
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_NetNodeServersUpdated,0,0);
end;

procedure TFRMNodes.CM_BlackListUpdated(var Msg: TMessage);
Const CT_TRUE_FALSE : Array[Boolean] Of AnsiString = ('FALSE','TRUE');
Var i,j,n : integer;
 P : PNodeServerAddress;
 l : TList;
 strings : TStrings;
begin
  l := TNetData.NetData.NodeServersAddresses.LockList;
  try
    strings := memoNetBlackLists.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('BlackList Updated: %s by TID: %p', [DateTimeToStr(now), TThread.CurrentThread.ThreadID]);
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

procedure TFRMNodes.OnNetBlackListUpdated;
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_BlackListUpdated,0,0);
end;

end.
