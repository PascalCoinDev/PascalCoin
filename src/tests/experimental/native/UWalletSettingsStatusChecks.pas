unit UWalletSettingsStatusChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Settings-only footer regression using the real controller subscription order.
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UFRMMainForm;

// Requires a fresh owned process with networking restricted to the unavailable
// fixed loopback peer, an ephemeral listener, and RPC/mining disabled.
function CheckSettingsOnlyPortStatus(AMain: TFRMMainForm): String;

implementation

uses Classes, SysUtils, UCommon, USettings, UUserInterface, UNetProtocol;

type
  TSettingsStatusObserver = class
  public
    StateEvents, StatisticsEvents: Integer;
    procedure StateChanged(Sender: TObject);
    procedure StatisticsChanged(Sender: TObject);
  end;

procedure TSettingsStatusObserver.StateChanged(Sender: TObject);
begin
  Inc(StateEvents);
end;

procedure TSettingsStatusObserver.StatisticsChanged(Sender: TObject);
begin
  Inc(StatisticsEvents);
end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('Settings-only footer: ' + AMessage);
end;

function CheckSettingsOnlyPortStatus(AMain: TFRMMainForm): String;
var
  LObserver: TSettingsStatusObserver;
  LPort: Word;
  LState: TUserInterfaceState;
  LCaption: String;
begin
  Require(TUserInterface.Started and not TUserInterface.Offline, 'requires the actual online settings branch');
  Require(TSettings.TryConnectOnlyWithThisFixedServers = '127.0.0.1:1', 'fixed loopback peer policy is missing');
  Require(TSettings.PeerCache = '', 'requires an empty saved peer cache');
  Require(not TSettings.JsonRpcPortEnabled and not TSettings.JsonRpcMinerServerActive, 'RPC/mining must be disabled');
  Require(TNetData.NetData.NetStatistics.ActiveConnections = 0, 'requires zero connected peers');
  Require(TUserInterface.Node.NetServer.Active, 'requires an active owned ephemeral listener');
  LPort := TSettings.InternetServerPort;
  Require(LPort = 0, 'requires the configured ephemeral listener port');
  LState := TUserInterface.State;
  LObserver := TSettingsStatusObserver.Create;
  TUserInterface.StateChanged.Add(LObserver.StateChanged);
  TUserInterface.NetStatisticsChanged.Add(LObserver.StatisticsChanged);
  try
    // Do not pump messages or invoke any UI event here. The controller's
    // settings receiver must apply the value before MainForm reads it.
    TSettings.InternetServerPort := 43219;
    TSettings.Save;
    LCaption := AMain.sbStatusBar.Panels[0].Text;
    Require(TUserInterface.Node.NetServer.Port = 43219, 'controller did not apply the new configured port');
    Require(LCaption = 'Active (port: 43219)', 'native footer retained the old listener port');
    Require(TUserInterface.State = LState, 'the test changed UI enum state');
    Require((LObserver.StateEvents = 0) and (LObserver.StatisticsEvents = 0),
      'another state/statistics notification masked the settings-only update');
    TSettings.InternetServerPort := LPort;
    TSettings.Save;
    Require(AMain.sbStatusBar.Panels[0].Text = 'Active (port: 0)', 'restoring settings did not update the native footer');
    Require((TUserInterface.State = LState) and (LObserver.StateEvents = 0) and
      (LObserver.StatisticsEvents = 0), 'restoring settings required another UI event');
    Result := 'SETTINGS-ONLY PORT: 0 -> 43219 -> 0; immediate native footer; unchanged state; zero state/statistics callbacks; no message/timer dispatch';
  finally
    TUserInterface.StateChanged.Remove(LObserver.StateChanged);
    TUserInterface.NetStatisticsChanged.Remove(LObserver.StatisticsChanged);
    LObserver.Free;
    if TSettings.InternetServerPort <> LPort then begin
      TSettings.InternetServerPort := LPort;
      TSettings.Save;
    end;
  end;
end;

end.
