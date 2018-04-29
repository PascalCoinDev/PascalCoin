unit USettings;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements
      Herman Schoenfeld (herman@sphere10.com) - main author
      Albert Molina - original
}

{$I config.inc}

interface

uses
  UAppParams, UBaseTypes;

const
  // App Params
  CT_PARAM_GridAccountsStream = 'GridAccountsStreamV2';
  CT_PARAM_DefaultFee = 'DefaultFee';
  CT_PARAM_InternetServerPort = 'InternetServerPort';
  {$IFDEF TESTNET}CT_PARAM_AutomaticMineWhenConnectedToNodes = 'AutomaticMineWhenConnectedToNodes';{$ENDIF}
  CT_PARAM_MinerPrivateKeyType = 'MinerPrivateKeyType';
  CT_PARAM_MinerPrivateKeySelectedPublicKey = 'MinerPrivateKeySelectedPublicKey';
  CT_PARAM_SaveLogFiles = 'SaveLogFiles';
  CT_PARAM_SaveDebugLogs = 'SaveDebugLogs';
  CT_PARAM_ShowLogs = 'ShowLogs';
  CT_PARAM_MinerName = 'MinerName';
  CT_PARAM_RunCount = 'RunCount';
  CT_PARAM_FirstTime = 'FirstTime';
  CT_PARAM_ShowModalMessages = 'ShowModalMessages';
  {$IFDEF TESTNET}CT_PARAM_MaxCPUs = 'MaxCPUs'; {$ENDIF} //deprecated
  CT_PARAM_PeerCache = 'PeerCache';
  CT_PARAM_TryToConnectOnlyWithThisFixedServers = 'TryToConnectOnlyWithFixedServers';
  CT_PARAM_JSONRPCMinerServerPort = 'JSONRPCMinerServerPort';
  CT_PARAM_JSONRPCMinerServerActive = 'JSONRPCMinerServerActive';
  CT_PARAM_JSONRPCEnabled = 'JSONRPCEnabled';
  CT_PARAM_JSONRPCAllowedIPs = 'JSONRPCAllowedIPs';
  CT_PARAM_HashRateAvgBlocksCount = 'HashRateAvgBlocksCount';
  CT_PARAM_ShowHashRateAs = 'ShowHashRateAs';

type

  { TMinerPrivateKeyType }

  TMinerPrivateKeyType = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  { TSettings }

  TSettings = class
    private
      class var FOnChanged : TNotifyEventToMany;
      class var FAppParams : TAppParams;
      class function GetInternetServerPort : Integer; static;
      class procedure SetInternetServerPort(AInt:Integer); static;
      class function GetRpcPortEnabled : boolean; static;
      class procedure SetRpcPortEnabled(ABool: boolean); static;
      class function GetDefaultFee : Int64; static;
      class procedure SetDefaultFee(AInt64: Int64); static;
      class function GetMinerPrivateKeyType : TMinerPrivateKeyType; static;
      class procedure SetMinerPrivateKeyType(AType: TMinerPrivateKeyType); static;
      class function GetMinerSelectedPrivateKey : string; static;
      class procedure SetMinerSelectedPrivateKey(AKey:string); static;
      class function GetMinerServerRpcActive : boolean; static;
      class procedure SetMinerServerRpcActive(ABool: Boolean); static;
      class function GetMinerServerRpcPort : Integer; static;
      class procedure SetMinerServerRpcPort(APort: Integer); static;
      class function GetSaveLogFiles : boolean; static;
      class procedure SetSaveLogFiles(ABool: boolean); static;
      class function GetShowLogs : boolean; static;
      class procedure SetShowLogs(ABool: boolean); static;
      class function GetSaveDebugLogs : boolean; static;
      class procedure SetSaveDebugLogs(ABool: boolean); static;
      class function GetMinerName : string; static;
      class procedure SetMinerName(AName: string); static;
      class function GetRunCount : Integer; static;
      class procedure SetRunCount(AInt: Integer); static;
      class function GetShowModalMessages : boolean; static;
      class procedure SetShowModalMessages(ABool: boolean); static;
      class function GetRpcAllowedIPs : string; static;
      class procedure SetRpcAllowedIPs(AString: string); static;
      class function GetPeerCache : string; static;
      class procedure SetPeerCache(AString: string); static;
      class function GetTryConnectOnlyWithThisFixedServers : string; static;
      class procedure SetTryConnectOnlyWithThisFixedServers(AString: string); static;
      class procedure CheckLoaded;
      class procedure NotifyOnChanged;
    public
      class procedure Load;
      class procedure Save;
      class property OnChanged : TNotifyEventToMany read FOnChanged;
      class property InternetServerPort : Integer read GetInternetServerPort write SetInternetServerPort;
      class property RpcPortEnabled : boolean read GetRpcPortEnabled write SetRpcPortEnabled;
      class property RpcAllowedIPs : string read GetRpcAllowedIPs write SetRpcAllowedIPs;
      class property DefaultFee : Int64 read GetDefaultFee write SetDefaultFee;
      class property MinerPrivateKeyType : TMinerPrivateKeyType read GetMinerPrivateKeyType write SetMinerPrivateKeyType;
      class property MinerSelectedPrivateKey : string read GetMinerSelectedPrivateKey write SetMinerSelectedPrivateKey;
      class property MinerServerRpcActive : boolean read GetMinerServerRpcActive write SetMinerServerRpcActive;
      class property MinerServerRpcPort : Integer read GetMinerServerRpcPort write SetMinerServerRpcPort;
      class property SaveLogFiles : boolean read GetSaveLogFiles write SetSaveLogFiles;
      class property ShowLogs : boolean read GetShowLogs write SetShowLogs;
      class property SaveDebugLogs : boolean read GetSaveDebugLogs write SetSaveDebugLogs;
      class property MinerName : string read GetMinerName write SetMinerName;
      class property RunCount : Integer read GetRunCount write SetRunCount;
      class property ShowModalMessages : boolean read GetShowModalMessages write SetShowModalMessages;
      class property PeerCache : string read GetPeerCache write SetPeerCache;
      class property TryConnectOnlyWithThisFixedServers : string read GetTryConnectOnlyWithThisFixedServers write SetTryConnectOnlyWithThisFixedServers;
      class property AppParams : TAppParams read FAppParams;
  end;

implementation

uses
  Classes, SysUtils, UConst, UFolderHelper;


{ TSettings }

class procedure TSettings.Load;
begin
  FAppParams := TAppParams.Create(nil);
  FAppParams.FileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'AppParams.prm';
end;

class procedure TSettings.Save;
begin
  //TODO Update FAppParams to optionally save on set value, and make FApp.Save public and verify all AppParams updates in client code
end;

class function TSettings.GetInternetServerPort : Integer;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
end;

class procedure TSettings.SetInternetServerPort(AInt:Integer);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(AInt);
end;

class function TSettings.GetRpcPortEnabled : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(false);
end;

class procedure TSettings.SetRpcPortEnabled(ABool: boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(ABool);
end;

class function TSettings.GetRpcAllowedIPs : string;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1;');
end;

class procedure TSettings.SetRpcAllowedIPs(AString: string);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].SetAsString(AString);
end;

class function TSettings.GetDefaultFee : Int64;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
end;

class procedure TSettings.SetDefaultFee(AInt64: Int64);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_DefaultFee].SetAsInt64(AInt64);
end;

class function TSettings.GetMinerServerRpcActive : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
end;

class procedure TSettings.SetMinerServerRpcActive(ABool: Boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(ABool);
end;

class function TSettings.GetMinerServerRpcPort : Integer;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port);
end;

class procedure TSettings.SetMinerServerRpcPort(APort: Integer);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].SetAsInteger(APort)
end;

class function TSettings.GetMinerPrivateKeyType : TMinerPrivateKeyType;
begin
  CheckLoaded;
  Result := TMinerPrivateKeyType(FAppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].GetAsInteger(Integer(mpk_Random)))
end;

class procedure TSettings.SetMinerPrivateKeyType(AType: TMinerPrivateKeyType);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(Integer(AType));
end;

class function TSettings.GetMinerSelectedPrivateKey : string;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].GetAsString('');
end;

class procedure TSettings.SetMinerSelectedPrivateKey(AKey:string);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].SetAsString(AKey);
end;

class function TSettings.GetSaveLogFiles : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_SaveLogFiles].GetAsBoolean(false);
end;

class procedure TSettings.SetSaveLogFiles(ABool: boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(ABool);
end;

class function TSettings.GetShowLogs : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_ShowLogs].GetAsBoolean(false);
end;

class procedure TSettings.SetShowLogs(ABool: boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_ShowLogs].SetAsBoolean(ABool);
end;

class function TSettings.GetSaveDebugLogs : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_SaveDebugLogs].GetAsBoolean(false);
end;

class procedure TSettings.SetSaveDebugLogs(ABool: boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_SaveDebugLogs].SetAsBoolean(ABool);
end;

class function TSettings.GetMinerName : string;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString('Anonymous Miner')
end;

class procedure TSettings.SetMinerName(AName: string);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_MinerName].SetAsString(AName);
end;

class function TSettings.GetRunCount : Integer;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_RunCount].GetAsInteger(0)
end;

class procedure TSettings.SetRunCount(AInt: Integer);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_RunCount].SetAsInteger(AInt)
end;

class function TSettings.GetShowModalMessages : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false);
end;

class procedure TSettings.SetShowModalMessages(ABool: boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_ShowModalMessages].SetAsBoolean(ABool);
end;

class function TSettings.GetPeerCache : string;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_PeerCache].GetAsString('');
end;

class procedure TSettings.SetPeerCache(AString: string);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_PeerCache].SetAsString(AString);
end;

class function TSettings.GetTryConnectOnlyWithThisFixedServers : string;
begin
  CheckLoaded;
  Result := Trim(FAppParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].GetAsString(''));
end;

class procedure TSettings.SetTryConnectOnlyWithThisFixedServers(AString: string);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString(Trim(AString));
end;

class procedure TSettings.CheckLoaded;
begin
  if not Assigned(FAppParams) then
    raise Exception.Create('Application settings have not been loaded');
end;

class procedure TSettings.NotifyOnChanged;
begin
  FOnChanged.Invoke(nil);
end;

initialization
  TSettings.FAppParams := nil;
  TSettings.FOnChanged := TNotifyEventToMany.Create;

finalization
  if Assigned(TSettings.FAppParams) then
    FreeAndNil(TSettings.FAppParams);
  if Assigned(TSettings.FOnChanged) then
    FreeAndNil(TSettings.FOnChanged);

end.



