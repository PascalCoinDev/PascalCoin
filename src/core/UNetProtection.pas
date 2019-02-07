unit UNetProtection;

{ Copyright (c) 2018 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{
  This unit will include objects usefull for control statistical
  information about nodes IP's to decide if a IP is a scammer or is abusive.

  Information will be stored in a "TIpInfos" object class, thread protected,
  that will store statistical info for each IP and will return
  info based on Limits for decision

  Can be used both for a Node (p2p) and also for a JSON-RPC server
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses SysUtils, Classes, UJSONFunctions, UThread, ULog, UTime,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};


Type
  TIpInfo = Record
    ip : String;
    json : TPCJSONObject;
  End;

  TLimitLifetime = Record
    MaxBackSeconds : Integer;
    MaxCalls : Integer;
    MaxSize : Integer;
    class function Create(AMaxBackSeconds,AMaxCalls,AMaxSize : Integer) : TLimitLifetime; static;
  End;
  TLimitsLifetimeArray = TArray<TLimitLifetime>;

  TIpInfos = Class
  private
    FThreadList : TPCThreadList<Pointer>;
    FMaxStatsLifetime: Integer;
    FMaxStatsCount: Integer;
    FDeletedStatsCount: Int64;
    function Find(lockedList : TList<Pointer>; const ip : String; var Index: Integer): Boolean;
    procedure SetMaxStatsLifetime(const Value: Integer);
    procedure CleanLastStatsByUpdatedTimestamp(minTimestamp : Integer);
    procedure SetMaxStatsCount(const Value: Integer);
  public
    Constructor Create;
    Destructor Destroy; override;
    function Lock(const AIp : String; MarkAsUpdated : Boolean) : TPCJSONObject; overload;
    function Lock(index : Integer) : TIpInfo; overload;
    procedure UpdateIpInfo(const AIp, netTransferType, opType : String; dataSize : Integer);
    function ReachesLimits(const AIp, netTransferType, opType : String; dataSize : Integer; const limits : TLimitsLifetimeArray) : Boolean;
    function Update_And_ReachesLimits(const AIp, netTransferType, opType : String; dataSize : Integer; DoUpdate: Boolean; const limits : TLimitsLifetimeArray) : Boolean;
    procedure LogDisconnect(const AIp, AReason : String; IsMyself : Boolean);
    procedure Unlock;
    procedure Clear;
    function Count : Integer;
    property MaxStatsLifetime : Integer read FMaxStatsLifetime write SetMaxStatsLifetime;
    property MaxStatsCount : Integer read FMaxStatsCount write SetMaxStatsCount;
  End;

implementation

{ TIpInfos }

Type PIpInfo = ^TIpInfo;

procedure TIpInfos.CleanLastStatsByUpdatedTimestamp(minTimestamp: Integer);
var jsonOpType, relJsonOpType, relJsonNetTransferType : TPCJSONObject;
  lasts : TPCJSONArray;
  iIp, i,j,k : Integer;
  list : TList<Pointer>;
  p : PIpInfo;
begin
  list := FThreadList.LockList;
  Try
    for iIp := 0 to list.Count-1 do begin
      p := list[iIp];
      jsonOpType := p^.json.GetAsObject('optype');
      for i := 0 to jsonOpType.Count-1 do begin
        if (jsonOpType.GetNameValue(i).Value is TPCJSONObject) then begin
          relJsonOpType := TPCJSONObject( jsonOpType.GetNameValue(i).Value );
          for j := 0 to relJsonOpType.Count-1 do begin
            if (relJsonOpType.GetNameValue(j).Value is TPCJSONObject) then begin
              relJsonNetTransferType := TPCJSONObject( relJsonOpType.GetNameValue(j).Value );
              lasts := relJsonNetTransferType.GetAsArray('last');
              for k := lasts.Count-1 downto 0 do begin
                if lasts.GetAsObject(k).AsInteger('updated_ts',0) < minTimestamp then begin
                  inc(FDeletedStatsCount);
                  lasts.Delete(k);
                end;
              end;
              // Clean by max
              while (lasts.Count>FMaxStatsCount) And (FMaxStatsCount>=0) do begin
                inc(FDeletedStatsCount);
                lasts.Delete(0);
              end;
            end;
          end;
        end;
      end;
    end;
  Finally
    FThreadList.UnlockList;
  End;
end;

procedure TIpInfos.Clear;
var p : PIpInfo;
  i : Integer;
  list : TList<Pointer>;
begin
  list := FThreadList.LockList;
  Try
    for i := 0 to list.Count-1 do begin
      p := PIpInfo(list[i]);
      p^.json.Free;
      Dispose(p);
    end;
    FDeletedStatsCount := 0;
    list.Clear;
  Finally
    FThreadList.UnlockList;
  End;
end;

function TIpInfos.Count: Integer;
begin
  Result := FThreadList.LockList.Count;
  Unlock;
end;

constructor TIpInfos.Create;
begin
  FThreadList := TPCThreadList<Pointer>.Create(Self.ClassName);
  FMaxStatsLifetime := 60*60*24; // Last values by 24 hours by default
  FMaxStatsCount := 1000; // Max 1000 last stats by default
  FDeletedStatsCount := 0;
end;

destructor TIpInfos.Destroy;
begin
  Clear;
  FreeAndNil(FThreadList);
  inherited;
end;

function TIpInfos.Find(lockedList : TList<Pointer>; const ip: String; var Index: Integer): Boolean;
var L, H, I, C: Integer;
  PN : PIpInfo;
begin
  Result := False;
  L := 0;
  H := lockedList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    PN := lockedList.Items[I];
    C := CompareStr( PN^.ip, ip );
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TIpInfos.Lock(index: Integer): TIpInfo;
var list : TList<Pointer>;
begin
  list := FThreadList.LockList;
  if (list.Count>index) then begin
    Result := PIpInfo(list[index])^;
  end else begin
    Result.ip := '';
    Result.json := Nil;
  end;
end;

procedure TIpInfos.LogDisconnect(const AIp, AReason : String; IsMyself : Boolean);
var json, objinfo : TPCJSONObject;
  arr : TPCJSONArray;
  msg : String;
  i : Integer;
  currts : Integer;
  lastUpdatedTs : Integer;
begin
  // Disconnect will store at /disconnect in objects
  currts := UnivDateTimeToUnix( DateTime2UnivDateTime(now) );
  json := Lock(AIp,True);
  Try
    arr := json.GetAsArray('disconnect');
    objinfo := arr.GetAsObject(arr.Count);
    objinfo.GetAsVariant('updated_ts').Value := currts;
    objinfo.GetAsVariant('reason').Value := AReason;
    objinfo.GetAsVariant('myself').Value := IsMySelf;
    // Clean:
    for i :=arr.Count-1 downto 0 do begin
      lastUpdatedTs := arr.GetAsObject(i).AsInteger('updated_ts',0);
      if lastUpdatedTs+FMaxStatsLifetime < currts then begin
        arr.Delete(i);
      end;
    end;
    while (arr.Count>FMaxStatsCount) do arr.Delete(0);
  Finally
    Unlock;
  End;
end;

function TIpInfos.Lock(const AIp: String; MarkAsUpdated: Boolean): TPCJSONObject;
var list : TList<Pointer>;
  i : Integer;
  p : PIpInfo;
begin
  list := FThreadList.LockList;
  if Not Find(list,AIp,i) then begin
    new(p);
    p^.ip := AIp;
    p^.json := TPCJSONObject.Create;
    list.Insert(i,p);
  end else p := list[i];
  Result := p^.json;
  if MarkAsUpdated then begin
    Result.GetAsObject('stats').GetAsVariant('updated_ts').Value := UnivDateTimeToUnix( DateTime2UnivDateTime(now) );
  end;
end;

function TIpInfos.ReachesLimits(const AIp, netTransferType, opType: String; dataSize : Integer; const limits : TLimitsLifetimeArray): Boolean;
begin
  Result := Update_And_ReachesLimits(AIp,netTransferType,opType,dataSize,False,limits);
end;

procedure TIpInfos.SetMaxStatsCount(const Value: Integer);
var currts : Integer;
begin
  if FMaxStatsCount=Value then Exit;
  if Value<=0 then FMaxStatsCount := 1
  else FMaxStatsCount := Value;
  currts := UnivDateTimeToUnix( DateTime2UnivDateTime(now) );
  CleanLastStatsByUpdatedTimestamp(currts - FMaxStatsLifetime);
end;

procedure TIpInfos.SetMaxStatsLifetime(const Value: Integer);
var currts : Integer;
begin
  if FMaxStatsLifetime=Value then Exit;
  if Value<=0 then
    FMaxStatsLifetime := 1
  else FMaxStatsLifetime := Value;
  currts := UnivDateTimeToUnix( DateTime2UnivDateTime(now) );
  CleanLastStatsByUpdatedTimestamp(currts - FMaxStatsLifetime);
end;

procedure TIpInfos.Unlock;
begin
  FThreadList.UnlockList;
end;

procedure TIpInfos.UpdateIpInfo(const AIp, netTransferType, opType: String; dataSize: Integer);
begin
  Update_And_ReachesLimits(AIp,netTransferType,opType,dataSize,True,Nil);
end;

function TIpInfos.Update_And_ReachesLimits(const AIp, netTransferType, opType: String; dataSize: Integer; DoUpdate: Boolean; const limits: TLimitsLifetimeArray): Boolean;
var json, relJson : TPCJSONObject;
  lasts : TPCJSONArray;
  currts : Integer;
  i, j, lastUpdatedTs : Integer;
  countLimitsValues : TLimitsLifetimeArray;
begin
  Result := False;
  setLength(countLimitsValues,Length(limits));
  for j :=Low(countLimitsValues) to High(countLimitsValues) do begin
    countLimitsValues[j].MaxBackSeconds := limits[j].MaxBackSeconds;
    countLimitsValues[j].MaxCalls := 0;
    countLimitsValues[j].MaxSize := 0;
  end;

  currts := UnivDateTimeToUnix( DateTime2UnivDateTime(now) );
  json := Lock(AIp,DoUpdate);
  Try
    if (DoUpdate) then begin
      relJson := json.GetAsObject('optype').GetAsObject(optype).GetAsObject(netTransferType);
      // Inc totals
      relJson.GetAsVariant('count').Value := relJson.AsInteger('count',0)+1;
      relJson.GetAsVariant('datasize').Value := relJson.AsInt64('datasize',0)+dataSize;
    end else begin
      if Assigned(json.GetAsObject('optype').GetAsObject(optype).FindName(netTransferType)) then
        relJson := json.GetAsObject('optype').GetAsObject(optype).GetAsObject(netTransferType)
      else relJson := Nil;
    end;
    // last values
    if Assigned(relJson) then begin
      lasts := relJson.GetAsArray('last');
      // Clean last values based on MaxStatsLifetime and look at limits
      for i := lasts.Count-1 downto 0 do begin
        lastUpdatedTs := lasts.GetAsObject(i).AsInteger('updated_ts',0);
        if lastUpdatedTs+FMaxStatsLifetime < currts then begin
          inc(FDeletedStatsCount);
          lasts.Delete(i);
        end else if lastUpdatedTs > currts then begin
          // Detected a TimeZone time change ( Daylight saving time ) because lastUpdatedTs > Current
          // Nothing to do
        end else begin
          for j := Low(limits) to High(limits) do begin
            if (limits[j].MaxBackSeconds>0) And (lastUpdatedTs+limits[j].MaxBackSeconds>=currts) then begin
              inc(countLimitsValues[j].MaxCalls);
              inc(countLimitsValues[j].MaxSize,lasts.GetAsObject(i).AsInt64('datasize',0));
              if ((limits[j].MaxCalls>0) And (countLimitsValues[j].MaxCalls>limits[j].MaxCalls))
                  or
                 ((limits[j].MaxSize>0) And (countLimitsValues[j].MaxSize>limits[j].MaxSize)) then begin
                 json.GetAsArray('reached').GetAsVariant(json.GetAsArray('reached').Count).Value :=
                   Format('%s Reached limit calls:%d>%d or Size:%d>%d for IP:%s NetTransferType:%s OpType:%s',
                   [FormatDateTime('yyyy-mm-dd hh:nn:ss',Now),countLimitsValues[j].MaxCalls,limits[j].MaxCalls,countLimitsValues[j].MaxSize,limits[j].MaxSize,AIp,netTransferType,opType]);
                 TLog.NewLog(ltDebug,ClassName,Format('Reached limit calls:%d>%d or Size:%d>%d for IP:%s NetTransferType:%s OpType:%s',
                   [countLimitsValues[j].MaxCalls,limits[j].MaxCalls,countLimitsValues[j].MaxSize,limits[j].MaxSize,AIp,netTransferType,opType]));
                 Result := True;
              end;
            end;
          end;
        end;
      end;
      if (DoUpdate) then begin
        // Save to last values
        relJson := TPCJSONObject.Create;
        relJson.GetAsVariant('updated_ts').Value := currts;
        relJson.GetAsVariant('datasize').Value := dataSize;
        lasts.Insert(lasts.Count,relJson);
        // Clean by max
        while (lasts.Count>FMaxStatsCount) And (FMaxStatsCount>=0) do begin
          inc(FDeletedStatsCount);
          lasts.Delete(0);
        end;
      end;
    end;
  Finally
    Unlock;
  End;
  setLength(countLimitsValues,0);
end;

{ TLimitLifetime }

class function TLimitLifetime.Create(AMaxBackSeconds, AMaxCalls, AMaxSize: Integer): TLimitLifetime;
begin
  Result.MaxBackSeconds := AMaxBackSeconds;
  Result.MaxCalls := AMaxCalls;
  Result.MaxSize := AMaxSize;
end;

end.
