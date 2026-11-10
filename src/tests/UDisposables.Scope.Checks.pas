unit UDisposables.Scope.Checks;

{ Copyright (c) 2026 Herman Schoenfeld <https://www.sphere10.com>
  Shared Delphi/FPC checks for TDisposables managed ownership. }

{$IFDEF FPC}{$mode Delphi}{$H+}{$ENDIF}

interface

const DisposablesScopeCheckCount = 15;

procedure CheckNormalReturn;
procedure CheckEarlyExit;
procedure CheckExceptionUnwind;
procedure CheckInnerCatchRetainsBag;
procedure CheckEmptyBags;
procedure CheckGrowthReverseOrder;
procedure CheckRemoveObjectOwnership;
procedure CheckClassFieldAutomatic;
procedure CheckNestedRecordAutomatic;
procedure CheckMixedResources;
procedure CheckReallocMemPreservesData;
procedure CheckRemoveMemOwnership;
procedure CheckExplicitFreeMem;
procedure CheckUnregisteredReallocDoesNotTakeOwnership;
procedure CheckCleanupFailureContinues;
procedure RunDisposablesScopeChecks;

implementation

uses SysUtils, UMemory;

type
  ECheckedDisposeFailure = class(Exception);
  TTrackedObject = class
  private
    FID: Integer;
    FFail: Boolean;
  public
    constructor Create(AID: Integer; AFail: Boolean = False);
    destructor Destroy; override;
  end;
  TBagOwner = class
  public
    Bag: TDisposables;
  end;
  TBagRecord = record
    Bag: TDisposables;
  end;
  TMemoryObserver = class(TTrackedObject)
  private
    FData: PByte;
  public
    constructor Create(AData: PByte);
    destructor Destroy; override;
  end;

var
  LiveCount: Integer;
  Events: array of Integer;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure BeginCheck;
begin
  Require(LiveCount = 0, 'Previous check left a live object');
  SetLength(Events, 0);
end;

procedure ExpectCount(ACount: Integer);
begin
  Require(Length(Events) = ACount, 'Unexpected cleanup count');
end;

constructor TTrackedObject.Create(AID: Integer; AFail: Boolean);
begin
  inherited Create;
  FID := AID;
  FFail := AFail;
  Inc(LiveCount);
end;

destructor TTrackedObject.Destroy;
var LIndex: Integer;
begin
  try
    LIndex := Length(Events);
    SetLength(Events, LIndex + 1);
    Events[LIndex] := FID;
    Dec(LiveCount);
    try
      if FFail then
        raise ECheckedDisposeFailure.CreateFmt('dispose %d', [FID]);
    finally
      inherited;
    end;
  except
    // This deliberately failing test destructor owns its instance cleanup.
    // A raising destructor skips its normal instance-release epilogue.
    FreeInstance;
    raise;
  end;
end;

constructor TMemoryObserver.Create(AData: PByte);
begin
  inherited Create(1);
  FData := AData;
end;

destructor TMemoryObserver.Destroy;
begin
  try
    Require(FData^ = 42, 'Object cleanup must precede its earlier registered memory');
  finally
    inherited;
  end;
end;

procedure CheckNormalReturn;
  procedure Run;
  var Bag: TDisposables;
  begin
    Bag.AddObject(TTrackedObject.Create(1));
    Bag.AddObject(TTrackedObject.Create(2));
    Require(LiveCount = 2, 'Normal scope cleaned up before return');
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(2);
  Require(LiveCount = 0, 'Normal return leaked objects');
end;

procedure CheckEarlyExit;
  procedure Run;
  var Bag: TDisposables;
  begin
    Bag.AddObject(TTrackedObject.Create(1));
    Exit;
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(1);
  Require(LiveCount = 0, 'Exit skipped bag cleanup');
end;

procedure CheckExceptionUnwind;
  procedure Run;
  var Bag: TDisposables;
  begin
    Bag.AddObject(TTrackedObject.Create(1));
    raise EAbort.Create('body failure');
  end;
var Caught: Boolean;
begin
  BeginCheck;
  Caught := False;
  try Run;
  except
    on E: EAbort do begin
      Caught := True;
      Require(E.Message = 'body failure', 'Bag changed the body exception');
    end;
  end;
  Require(Caught, 'Body exception did not propagate');
  ExpectCount(1);
  Require(LiveCount = 0, 'Exception unwinding skipped bag cleanup');
end;

procedure CheckInnerCatchRetainsBag;
  procedure Run;
  var Bag: TDisposables;
  begin
    Bag.AddObject(TTrackedObject.Create(1));
    try raise EAbort.Create('caught inside scope');
    except on E: EAbort do ; end;
    ExpectCount(0);
    Bag.AddObject(TTrackedObject.Create(2));
    Require(LiveCount = 2, 'Inner handler prematurely released the routine bag');
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(2);
end;

procedure CheckEmptyBags;
  procedure Run;
  var Bag: TDisposables;
  begin
    Bag.RemoveObject(nil);
    Bag.AddObject(nil);
    Bag.RemoveMem(nil);
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(0);
end;

procedure CheckGrowthReverseOrder;
  procedure Run;
  var Bag: TDisposables; i: Integer;
  begin
    for i := 1 to 65 do Bag.AddObject(TTrackedObject.Create(i));
  end;
var i: Integer;
begin
  BeginCheck;
  Run;
  ExpectCount(65);
  for i := 0 to 64 do
    Require(Events[i] = 65 - i, 'Growth changed reverse registration order');
end;

procedure CheckRemoveObjectOwnership;
var Transferred: TTrackedObject;
  procedure Run;
  var Bag: TDisposables;
  begin
    Transferred := Bag[TTrackedObject.Create(1)] as TTrackedObject;
    Bag.RemoveObject(Transferred);
  end;
begin
  BeginCheck;
  Transferred := nil;
  try
    Run;
    ExpectCount(0);
    Require(LiveCount = 1, 'RemoveObject disposed a transferred resource');
  finally
    Transferred.Free;
  end;
  ExpectCount(1);
end;

procedure CheckClassFieldAutomatic;
var Owner: TBagOwner;
begin
  BeginCheck;
  Owner := TBagOwner.Create;
  Owner.Bag.AddObject(TTrackedObject.Create(1));
  Owner.Bag.AddObject(TTrackedObject.Create(2));
  Owner.Free;
  ExpectCount(2);
  Require((Events[0] = 2) and (Events[1] = 1), 'Class field did not clean up automatically');
end;

procedure CheckNestedRecordAutomatic;
  procedure Run;
  var Container: TBagRecord;
  begin
    Container.Bag.AddObject(TTrackedObject.Create(1));
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(1);
end;

procedure CheckMixedResources;
  procedure Run;
  var Bag: TDisposables; Data: Pointer;
  begin
    System.GetMem(Data, 8);
    Bag.AddMem(Data);
    PByte(Data)^ := 42;
    Bag.AddObject(TMemoryObserver.Create(PByte(Data)));
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(1);
end;

procedure CheckReallocMemPreservesData;
  procedure Run;
  var Bag: TDisposables; Data: Pointer; i: Integer;
  begin
    Bag.GetMem(Data, 8);
    for i := 0 to 7 do PByte(NativeUInt(Data) + NativeUInt(i))^ := i + 1;
    Bag.ReallocMem(Data, 128);
    for i := 0 to 7 do
      Require(PByte(NativeUInt(Data) + NativeUInt(i))^ = i + 1, 'Reallocation lost memory contents');
    PByte(Data)^ := 42;
    Bag.ReallocMem(Data, 4);
    Require(PByte(Data)^ = 42, 'Shrinking lost memory contents');
    Bag.ReallocMem(Data, 0);
    Require(Data = nil, 'Zero-size reallocation did not release registered memory');
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(0);
end;

procedure CheckRemoveMemOwnership;
var Data: Pointer;
  procedure Run;
  var Bag: TDisposables;
  begin
    Bag.GetMem(Data, 8);
    PByte(Data)^ := 42;
    Bag.RemoveMem(Data);
  end;
begin
  BeginCheck;
  Data := nil;
  try
    Run;
    Require(PByte(Data)^ = 42, 'RemoveMem did not transfer memory to the caller');
  finally
    System.FreeMem(Data);
  end;
end;

procedure CheckExplicitFreeMem;
  procedure Run;
  var Bag: TDisposables; Data, Original: Pointer;
  begin
    Bag.GetMem(Data, 8);
    Original := Data;
    Bag.FreeMem(Data);
    Require(Data = Original, 'FreeMem changed the established raw-pointer behavior');
    Data := nil;
  end;
begin
  BeginCheck;
  Run;
  ExpectCount(0);
end;

procedure CheckUnregisteredReallocDoesNotTakeOwnership;
var Bag: TDisposables; Data, Original: Pointer;
begin
  BeginCheck;
  System.GetMem(Data, 8);
  try
    Original := Data;
    PByte(Data)^ := 42;
    Bag.ReallocMem(Data, 128);
    Require(Data = Original, 'ReallocMem changed unregistered memory');
    Require(PByte(Data)^ = 42, 'Unregistered memory contents changed');
  finally
    System.FreeMem(Data);
  end;
end;

procedure CheckCleanupFailureContinues;
  procedure Run;
  var Bag: TDisposables;
  begin
    Bag.AddObject(TTrackedObject.Create(1));
    Bag.AddObject(TTrackedObject.Create(2, True));
    Bag.AddObject(TTrackedObject.Create(3, True));
  end;
var Caught: Boolean;
begin
  BeginCheck;
  Caught := False;
  try Run;
  except
    on E: ECheckedDisposeFailure do begin
      Caught := True;
      Require(E.Message = 'dispose 3', 'Cleanup did not preserve the first failure');
    end;
  end;
  Require(Caught, 'Cleanup failure did not propagate');
  ExpectCount(3);
  Require((Events[0] = 3) and (Events[1] = 2) and (Events[2] = 1), 'Failure stopped reverse cleanup');
  Require(LiveCount = 0, 'Failure left objects undisposed');
end;

procedure RunDisposablesScopeChecks;
begin
  CheckNormalReturn;
  CheckEarlyExit;
  CheckExceptionUnwind;
  CheckInnerCatchRetainsBag;
  CheckEmptyBags;
  CheckGrowthReverseOrder;
  CheckRemoveObjectOwnership;
  CheckClassFieldAutomatic;
  CheckNestedRecordAutomatic;
  CheckMixedResources;
  CheckReallocMemPreservesData;
  CheckRemoveMemOwnership;
  CheckExplicitFreeMem;
  CheckUnregisteredReallocDoesNotTakeOwnership;
  CheckCleanupFailureContinues;
end;

end.
