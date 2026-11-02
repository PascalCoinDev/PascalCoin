unit UChangeAccountInfoReplayTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses fpcunit, testregistry;

type
  TChangeAccountInfoReplayTests = class(TTestCase)
  published
    procedure SelfTargetRetainsFeeNonceSealAndBaseIsolation;
    procedure DistinctTargetRetainsSeparateSignerState;
    procedure PendingEditsAndRollbackRetainExactAccountState;
    procedure InvalidNonceCannotMutateAnAccount;
  end;

implementation

uses UChangeAccountInfoReplayChecks;

procedure TChangeAccountInfoReplayTests.SelfTargetRetainsFeeNonceSealAndBaseIsolation;
begin
  CheckChangeInfoSelfTarget;
end;

procedure TChangeAccountInfoReplayTests.DistinctTargetRetainsSeparateSignerState;
begin
  CheckChangeInfoDistinctTarget;
end;

procedure TChangeAccountInfoReplayTests.PendingEditsAndRollbackRetainExactAccountState;
begin
  CheckChangeInfoRepeatedAndRollback;
end;

procedure TChangeAccountInfoReplayTests.InvalidNonceCannotMutateAnAccount;
begin
  CheckChangeInfoRejectsWithoutMutation;
end;

initialization
  RegisterTest(TChangeAccountInfoReplayTests);

end.
