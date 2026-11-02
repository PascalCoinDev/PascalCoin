unit UScope.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license, see LICENSE. }

{$IFDEF FPC}{$MODE Delphi}{$H+}{$ENDIF}

interface

const ScopeCheckCount = 22;
procedure RunScopeCheck(AIndex: Integer);
function ScopeCheckName(AIndex: Integer): String;

implementation

uses Classes, SysUtils, UCommon;

type
  ECallbackFailure = class(Exception);
  EGetterFailure = class(Exception);
  EBodyFailure = class(Exception);
  TCallbackTarget = class;
  TState = class
    Targets: array of TCallbackTarget;
    Created, Freed, Calls: Integer;
    RaiseOnCallback: Boolean;
    Order: String;
    destructor Destroy; override;
  end;
  TCallbackTarget = class
  private
    FState: TState;
    FID, FSlot: Integer;
    FRegistered: Boolean;
  public
    constructor Create(AState: TState; AID: Integer);
    destructor Destroy; override;
    procedure Cleanup;
    function Callback(AFail: Boolean = False): TThreadMethod;
  end;
  TScopeOwner = class
  private
    FScope: TScope;
  public
    constructor Create(AState: TState);
  end;
  TScopeRecordOwner = record
    Scope: TScope;
  end;

procedure Check(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

destructor TState.Destroy;
var I: Integer;
begin
  for I := 0 to High(Targets) do Targets[I].Free;
  inherited;
end;

constructor TCallbackTarget.Create(AState: TState; AID: Integer);
begin
  inherited Create;
  FState := AState;
  FID := AID;
  FSlot := Length(FState.Targets);
  SetLength(FState.Targets, FSlot + 1);
  FState.Targets[FSlot] := Self;
  FRegistered := True;
  Inc(FState.Created);
end;

destructor TCallbackTarget.Destroy;
begin
  if FRegistered then begin
    FState.Targets[FSlot] := nil;
    Inc(FState.Freed);
  end;
  inherited;
end;

procedure TCallbackTarget.Cleanup;
begin
  Inc(FState.Calls);
  if FState.Order <> '' then FState.Order := FState.Order + ',';
  FState.Order := FState.Order + IntToStr(FID);
  if FState.RaiseOnCallback then
    raise ECallbackFailure.Create('Expected cleanup callback failure');
end;

function TCallbackTarget.Callback(AFail: Boolean): TThreadMethod;
begin
  if AFail then raise EGetterFailure.Create('Expected callback argument failure');
  Result := Cleanup;
end;

function NewScope(AState: TState; AID: Integer): TScope;
var Target: TCallbackTarget;
begin
  Target := TCallbackTarget.Create(AState, AID);
  Result := TScope.Create(Target.Cleanup);
end;

procedure InitScope(var ADestination: TScope; AState: TState; AID: Integer);
begin
  // End compiler-result copies before deliberately releasing a last owner.
  ADestination := NewScope(AState, AID);
end;

constructor TScopeOwner.Create(AState: TState);
begin
  inherited Create;
  FScope := NewScope(AState, 1);
end;

procedure OwnNormally(AState: TState);
var Scope: TScope;
begin
  Scope := NewScope(AState, 1);
  // Deliberately never read Scope; external state must still prove retention.
  Check(AState.Calls = 0, 'Callback ran before the protected body');
end;

procedure OwnAndExit(AState: TState);
var Scope: TScope;
begin
  Scope := NewScope(AState, 1);
  Check(AState.Calls = 0, 'Callback ran before Exit');
  Exit;
end;

procedure OwnAndRaise(AState: TState);
var Scope: TScope;
begin
  Scope := NewScope(AState, 1);
  Check(AState.Calls = 0, 'Callback ran before the body exception');
  raise EBodyFailure.Create('Expected body failure');
end;

procedure OwnAndCatchInside(AState: TState);
var Scope: TScope;
begin
  try
    Scope := NewScope(AState, 1);
    raise EBodyFailure.Create('Expected handled body failure');
  except
    on E: EBodyFailure do
      Check(AState.Calls = 0, 'Inner handler lost the routine owner');
  end;
  Check(AState.Calls = 0, 'Callback ran before routine exit after inner handler');
end;

function ReturnLocalCopy(AState: TState): TScope;
var Local: TScope;
begin
  Local := NewScope(AState, 1);
  Result := Local;
end;

procedure OwnReturnedCopy(AState: TState);
var Scope: TScope;
begin
  Scope := ReturnLocalCopy(AState);
  Check(AState.Calls = 0, 'Returned copy released its callback early');
end;

procedure OwnIndependentLocals(AState: TState);
var First, Second: TScope;
begin
  First := NewScope(AState, 1);
  Second := NewScope(AState, 2);
  Check((AState.Created = 2) and (AState.Calls = 0), 'Independent local callbacks did not stay deferred');
end;

procedure OwnRecordField(AState: TState);
var Owner: TScopeRecordOwner;
begin
  Owner.Scope := NewScope(AState, 1);
  Check(AState.Calls = 0, 'Containing record did not retain its scope');
end;

function ScopeCheckName(AIndex: Integer): String;
begin
  case AIndex of
    1: Result := 'DefaultScopeIsEmpty';
    2: Result := 'NilCallbackRejected';
    3: Result := 'CreationDefersCallback';
    4: Result := 'LocalScopeReleasesOnReturn';
    5: Result := 'LocalScopeReleasesOnExit';
    6: Result := 'LocalScopeReleasesOnException';
    7: Result := 'InnerHandlerRetainsLocalScope';
    8: Result := 'ReturnedRecordRetainsCallback';
    9: Result := 'CopiesInvokeOnlyLastOwner';
    10: Result := 'OverwriteInvokesPreviousCallback';
    11: Result := 'OverwriteKeepsAliasedPreviousCallback';
    12: Result := 'ClassFieldFinalizesCallback';
    13: Result := 'ArrayCopiesShareCallback';
    14: Result := 'CallbackExceptionEmptiesOwner';
    15: Result := 'CallbackExceptionDuringOverwrite';
    16: Result := 'FailedCopyAssignmentPreservesSource';
    17: Result := 'SelfAssignmentPreservesOwner';
    18: Result := 'FreeMethodDisposesOwnedTarget';
    19: Result := 'BorrowedTargetRemainsCallerOwned';
    20: Result := 'FailedCallbackGetterKeepsPreviousOwner';
    21: Result := 'IndependentLocalsOwnIndependentCallbacks';
    22: Result := 'RecordFieldFinalizesCallback';
  else raise EArgumentOutOfRangeException.Create('AIndex'); end;
end;

procedure CheckWithState(AIndex: Integer; State: TState);
var
  Scope, Alias: TScope;
  Owners: TArray<TScope>;
  Owner: TScopeOwner;
  Target: TCallbackTarget;
  NilMethod: TThreadMethod;
  Raised: Boolean;
begin
  Owner := nil;
  try
    case AIndex of
      1: begin
        Check(Scope.IsEmpty, 'Default scope must be empty');
        Scope.Reset; Scope.Reset;
        Check(Scope.IsEmpty and (State.Calls = 0), 'Empty Reset must be harmless');
      end;
      2: begin
        NilMethod := nil; Raised := False;
        try Scope := TScope.Create(NilMethod);
        except on E: EArgumentNilException do Raised := True; end;
        Check(Raised and Scope.IsEmpty, 'Nil callback must be rejected without an owner');
      end;
      3: begin
        InitScope(Scope, State, 1);
        Check(State.Calls = 0, 'Create invoked cleanup instead of registering it');
        Scope.Reset; Check(State.Calls = 1, 'Final owner did not invoke deferred callback');
      end;
      4: begin
        OwnNormally(State);
        Check(State.Calls = 1, 'Normal return did not invoke cleanup');
      end;
      5: begin
        OwnAndExit(State);
        Check(State.Calls = 1, 'Exit did not invoke cleanup');
      end;
      6: begin
        Raised := False;
        try OwnAndRaise(State);
        except on E: EBodyFailure do Raised := True; end;
        Check(Raised and (State.Calls = 1), 'Unwind must invoke cleanup and preserve body failure');
      end;
      7: begin
        OwnAndCatchInside(State);
        Check(State.Calls = 1, 'Routine owner did not finalize');
      end;
      8: begin
        OwnReturnedCopy(State);
        Check(State.Calls = 1, 'Returned copy did not finalize once');
      end;
      9: begin
        InitScope(Scope, State, 1); Alias := Scope; Scope.Reset;
        Check(Scope.IsEmpty and not Alias.IsEmpty, 'Reset changed another owner');
        Check(State.Calls = 0, 'Reset invoked a live alias');
        Alias.Reset; Alias.Reset; Check(State.Calls = 1, 'Last alias must invoke callback once');
      end;
      10: begin
        InitScope(Scope, State, 1); InitScope(Scope, State, 2);
        Check(State.Order = '1', 'Overwrite did not invoke previous callback');
        Scope.Reset; Check(State.Order = '1,2', 'Replacement lost its callback');
      end;
      11: begin
        InitScope(Scope, State, 1); Alias := Scope; InitScope(Scope, State, 2);
        Check(State.Calls = 0, 'Overwrite invoked the aliased previous callback');
        Alias.Reset; Check(State.Order = '1', 'Previous callback lost its last owner');
        Scope.Reset; Check(State.Order = '1,2', 'Replacement callback did not run');
      end;
      12: begin
        Owner := TScopeOwner.Create(State); Check(State.Calls = 0, 'Class field lost its scope');
        FreeAndNil(Owner); Check(State.Calls = 1, 'Class destruction did not finalize scope');
      end;
      13: begin
        SetLength(Owners, 3);
        InitScope(Owners[0], State, 1); Owners[1] := Owners[0]; InitScope(Owners[2], State, 2);
        Owners[0].Reset; Check(State.Calls = 0, 'Array alias lost its callback');
        SetLength(Owners, 1); Check(State.Calls = 2, 'Array shrink did not release discarded owners');
      end;
      14, 15: begin
        InitScope(Scope, State, 1); State.RaiseOnCallback := True; Raised := False;
        try
          if AIndex = 14 then Scope.Reset else Scope := Default(TScope);
        except on E: ECallbackFailure do Raised := True; end;
        State.RaiseOnCallback := False;
        Check(Raised and Scope.IsEmpty, 'Callback failure must propagate with owner detached');
        Check(State.Calls = 1, 'Throwing callback must run once');
        Scope.Reset; Check(State.Calls = 1, 'Reset after failure invoked callback again');
      end;
      16: begin
        InitScope(Scope, State, 1); InitScope(Alias, State, 2);
        State.RaiseOnCallback := True; Raised := False;
        try Scope := Alias;
        except on E: ECallbackFailure do Raised := True; end;
        State.RaiseOnCallback := False;
        Check(Raised and Scope.IsEmpty and not Alias.IsEmpty, 'Failed copy must preserve source and clear destination');
        Check(State.Order = '1', 'Failed copy invoked source callback');
        Alias.Reset; Check(State.Order = '1,2', 'Source lost its independent callback');
      end;
      17: begin
        InitScope(Scope, State, 1); Scope := Scope; Alias := Scope; Scope := Alias;
        Check(State.Calls = 0, 'Self/shared assignment invoked cleanup');
        Alias.Reset; Check(State.Calls = 0, 'Remaining alias lost ownership');
        Scope.Reset; Check(State.Calls = 1, 'Self/shared assignment duplicated cleanup');
      end;
      18: begin
        Target := TCallbackTarget.Create(State, 1);
        Scope := TScope.Create(Target.Free);
        Check(State.Freed = 0, 'Registering Free destroyed the target immediately');
        // The outer check observes destruction after this routine and any
        // compiler-created result copies have completed their cleanup.
      end;
      19: begin
        InitScope(Scope, State, 1); Scope.Reset;
        Check((State.Calls = 1) and (State.Freed = 0), 'Scope must not free a borrowed callback target');
        State.Targets[0].Free; Check(State.Freed = 1, 'Caller lost ownership of borrowed target');
      end;
      20: begin
        InitScope(Scope, State, 1); Target := TCallbackTarget.Create(State, 2); Raised := False;
        try Scope := TScope.Create(Target.Callback(True));
        except on E: EGetterFailure do Raised := True; end;
        Check(Raised and not Scope.IsEmpty, 'Argument failure discarded previous owner');
        Check(State.Calls = 0, 'Argument failure invoked a callback');
        Scope.Reset; Check(State.Order = '1', 'Previous callback was not retained');
      end;
      21: begin
        OwnIndependentLocals(State);
        Check(State.Calls = 2, 'Independent locals lost callbacks');
      end;
      22: begin
        OwnRecordField(State);
        Check(State.Calls = 1, 'Containing record did not finalize scope');
      end;
    else raise EArgumentOutOfRangeException.Create('AIndex'); end;
  finally
    State.RaiseOnCallback := False;
    Owner.Free; SetLength(Owners, 0); Alias.Reset; Scope.Reset;
  end;
end;

procedure RunScopeCheck(AIndex: Integer);
var State: TState;
begin
  State := TState.Create;
  try
    CheckWithState(AIndex, State);
    if AIndex = 18 then Check(State.Freed = 1, 'Free callback did not destroy its target once');
  finally
    State.Free;
  end;
end;

end.
