unit USettings;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements
  - Albert Molina: this unit just wraps PascalCoin settings designed by Albert

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{$I ./../config.inc}

interface

uses
  UAppParams, UBaseTypes, UCommon;

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
  CT_PARAM_MaxPayToKeyPurchasePrice = 'MaxPayToKeyPurchasePrice';
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
  CT_PARAM_AllowDownloadNewCheckpointIfOlderThan = 'AllowDownloadNewCheckpointIfOlderThan';
  CT_PARAM_MinFutureBlocksToDownloadNewSafebox = 'MinFutureBlocksToDownloadNewSafebox';
  CT_PARAM_UILanguage = 'UILanguage';

type

  { TMinerPrivateKeyType }

  TMinerPrivateKeyType = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  { TShowHashRateAs }

  TShowHashRateAs = (hr_Unit, hr_Kilo, hr_Mega, hr_Giga, hr_Tera, hr_Peta, hr_Exa);

  { TSettings }

  TSettings = class
    private
      class var FOnChanged : TNotifyManyEvent;
      class var FAppParams : TAppParams;
      class function GetAllowDownloadNewCheckpointIfOlderThan: Boolean; static;
      class function GetInternetServerPort : Integer; static;
      class function GetMinFutureBlocksToDownloadNewSafebox: Integer; static;
      class procedure SetAllowDownloadNewCheckpointIfOlderThan(ABool: Boolean); static;
      class procedure SetInternetServerPort(AInt:Integer); static;
      class function GetJsonRpcPortEnabled : boolean; static;
      class procedure SetMinFutureBlocksToDownloadNewSafebox(AInt: Integer); static;
      class procedure SetJsonRpcPortEnabled(ABool: boolean); static;
      class function GetDefaultFee : Int64; static;
      class procedure SetDefaultFee(AInt64: Int64); static;
      class function GetMinerPrivateKeyType : TMinerPrivateKeyType; static;
      class procedure SetMinerPrivateKeyType(AType: TMinerPrivateKeyType); static;
      class function GetMinerSelectedPublicKey : TRawBytes; static;
      class procedure SetMinerSelectedPublicKey(AKey:TRawBytes); static;
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
      class function GetFirstTime : Boolean; static;
      class procedure SetFirstTime(ABool: Boolean); static;
      class function GetMaxPayToKeyPurchasePrice : UInt64; static;
      class procedure SetMaxPayToKeyPurchasePrice(AVal: UInt64); static;
      class function GetShowModalMessages : boolean; static;
      class procedure SetShowModalMessages(ABool: boolean); static;
      class function GetJsonRpcAllowedIPs : string; static;
      class procedure SetJsonRpcAllowedIPs(AString: string); static;
      class function GetJsonRpcMinerServerActive : boolean; static;
      class procedure SetJsonRpcMinerServerActive(ABool: Boolean); static;
      class function GetMinerServerRpcPort : Integer; static;
      class procedure SetMinerServerRpcPort(APort: Integer); static;
      class function GetHashRateAvgBlocksCount : Integer; static;
      class procedure SetHashRateAvgBlocksCount(AInt: Integer); static;
      class function GetShowHashRateAs : TShowHashRateAs; static;
      class procedure SetShowHashRateAs(AVal: TShowHashRateAs); static;
      class function GetPeerCache : string; static;
      class procedure SetPeerCache(AString: string); static;
      class function GetTryConnectOnlyWithThisFixedServers : string; static;
      class procedure SetTryConnectOnlyWithThisFixedServers(AString: string); static;
      class procedure CheckNotLoaded;
      class procedure CheckLoaded;
      class procedure NotifyOnChanged;
    public
      class procedure Load;
      class procedure Save;
      class property OnChanged : TNotifyManyEvent read FOnChanged;
      class property InternetServerPort : Integer read GetInternetServerPort write SetInternetServerPort;
      class property JsonRpcPortEnabled : boolean read GetJsonRpcPortEnabled write SetJsonRpcPortEnabled;
      class property JsonRpcAllowedIPs : string read GetJsonRpcAllowedIPs write SetJsonRpcAllowedIPs;
      class property JsonRpcMinerServerActive : boolean read GetJsonRpcMinerServerActive write SetJsonRpcMinerServerActive;
      class property JsonRpcMinerServerPort : Integer read GetMinerServerRpcPort write SetMinerServerRpcPort;
      class property HashRateAvgBlocksCount : Integer read GetHashRateAvgBlocksCount write SetHashRateAvgBlocksCount;
      class property ShowHashRateAs : TShowHashRateAs read GetShowHashRateAs write SetShowHashRateAs;
      class property DefaultFee : Int64 read GetDefaultFee write SetDefaultFee;
      class property MinerPrivateKeyType : TMinerPrivateKeyType read GetMinerPrivateKeyType write SetMinerPrivateKeyType;
      class property MinerSelectedPublicKey : TRawBytes read GetMinerSelectedPublicKey write SetMinerSelectedPublicKey;
      class property SaveLogFiles : boolean read GetSaveLogFiles write SetSaveLogFiles;
      class property ShowLogs : boolean read GetShowLogs write SetShowLogs;
      class property SaveDebugLogs : boolean read GetSaveDebugLogs write SetSaveDebugLogs;
      class property MinerName : string read GetMinerName write SetMinerName;
      class property RunCount : Integer read GetRunCount write SetRunCount;
      class property FirstTime : Boolean read GetFirstTime write SetFirstTime;
      class property MaxPayToKeyPurchasePrice : UInt64 read GetMaxPayToKeyPurchasePrice write SetMaxPayToKeyPurchasePrice;
      class property ShowModalMessages : boolean read GetShowModalMessages write SetShowModalMessages;
      class property PeerCache : string read GetPeerCache write SetPeerCache;
      class property TryConnectOnlyWithThisFixedServers : string read GetTryConnectOnlyWithThisFixedServers write SetTryConnectOnlyWithThisFixedServers;
      class property MinFutureBlocksToDownloadNewSafebox : Integer read GetMinFutureBlocksToDownloadNewSafebox write SetMinFutureBlocksToDownloadNewSafebox;
      class property AllowDownloadNewCheckpointIfOlderThan : Boolean read GetAllowDownloadNewCheckpointIfOlderThan write SetAllowDownloadNewCheckpointIfOlderThan;
      class property AppParams : TAppParams read FAppParams;
  end;

implementation

uses
  Classes, SysUtils, UConst, UNode;


{ TSettings }

class procedure TSettings.Load;
begin
  CheckNotLoaded;
  FAppParams := TAppParams.Create(nil);
  FAppParams.FileName := TNode.GetPascalCoinDataFolder+PathDelim+'AppParams.prm';

  If FAppParams.FindParam(CT_PARAM_MinerName)=Nil then begin
    // New configuration... assigning a new random value
    FAppParams.ParamByName[CT_PARAM_MinerName].SetAsString('New Node '+DateTimeToStr(Now)+' - '+ CT_ClientAppVersion);
  end;

end;

class procedure TSettings.Save;
begin
  //TODO Update FAppParams to optionally save on set value, and make FApp.Save public and verify all AppParams updates in client code
  CheckLoaded;
  OnChanged.Invoke(Nil);
end;

class function TSettings.GetInternetServerPort : Integer;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
end;

class function TSettings.GetAllowDownloadNewCheckpointIfOlderThan: Boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].GetAsBoolean(False);
end;

class function TSettings.GetMinFutureBlocksToDownloadNewSafebox: Integer;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewSafebox].GetAsInteger(0);
end;

class procedure TSettings.SetAllowDownloadNewCheckpointIfOlderThan(ABool: Boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].SetAsBoolean(ABool);
end;

class procedure TSettings.SetInternetServerPort(AInt:Integer);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(AInt);
end;

class procedure TSettings.SetMinFutureBlocksToDownloadNewSafebox(AInt: Integer);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewSafebox].SetAsInteger(AInt);
end;

class function TSettings.GetJsonRpcPortEnabled : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(false);
end;

class procedure TSettings.SetJsonRpcPortEnabled(ABool: boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(ABool);
end;

class function TSettings.GetJsonRpcAllowedIPs : string;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1;');
end;

class procedure TSettings.SetJsonRpcAllowedIPs(AString: string);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].SetAsString(AString);
end;

class function TSettings.GetHashRateAvgBlocksCount : Integer;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_HashRateAvgBlocksCount].GetAsInteger(50)
end;

class procedure TSettings.SetHashRateAvgBlocksCount(AInt: Integer);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_HashRateAvgBlocksCount].SetAsInteger(AInt);
end;

class function TSettings.GetShowHashRateAs : TShowHashRateAs;
begin
  CheckLoaded;
  Result := TShowHashRateAs(FAppParams.ParamByName[CT_PARAM_ShowHashRateAs].GetAsInteger(Integer({$IFDEF TESTNET}hr_Mega{$ELSE}hr_Tera{$ENDIF})));
end;

class procedure TSettings.SetShowHashRateAs(AVal: TShowHashRateAs);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_ShowHashRateAs].SetAsInteger(Integer(AVal));
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

class function TSettings.GetJsonRpcMinerServerActive : boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
end;

class procedure TSettings.SetJsonRpcMinerServerActive(ABool: Boolean);
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

class function TSettings.GetMinerSelectedPublicKey : TRawBytes;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].GetAsTBytes(Nil);
end;

class procedure TSettings.SetMinerSelectedPublicKey(AKey:TRawBytes);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].SetAsTBytes(AKey);
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

class function TSettings.GetFirsttime : Boolean;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_FirstTime].GetAsBoolean(true);
end;

class procedure TSettings.SetFirstTime(ABool: Boolean);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_FirstTime].SetAsBoolean(ABool);
end;

class function TSettings.GetMaxPayToKeyPurchasePrice : UInt64;
begin
  CheckLoaded;
  Result := FAppParams.ParamByName[CT_PARAM_MaxPayToKeyPurchasePrice].GetAsUInt64(5000);
end;

class procedure TSettings.SetMaxPayToKeyPurchasePrice(AVal: UInt64);
begin
  CheckLoaded;
  FAppParams.ParamByName[CT_PARAM_MaxPayToKeyPurchasePrice].SetAsUInt64(AVal);
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

class procedure TSettings.CheckNotLoaded;
begin
  if Assigned(FAppParams) then
    raise Exception.Create('Application settings have already been loaded');
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

finalization
  if Assigned(TSettings.FAppParams) then
    FreeAndNil(TSettings.FAppParams);
end.



