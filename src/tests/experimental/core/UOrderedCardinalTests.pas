unit UOrderedCardinalTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses fpcunit, testregistry, UOrderedCardinalChecks;

type
  TOrderedCardinalTests = class(TTestCase)
  published
    procedure FullCardinalBoundaryPairs;
    procedure ActualListInsertionFindRemovalPermutations;
    procedure ActualListDuplicateAndMissingValues;
  end;

implementation

procedure TOrderedCardinalTests.FullCardinalBoundaryPairs;
begin
  CheckOrderedCardinal(occBoundaryPairs);
end;

procedure TOrderedCardinalTests.ActualListInsertionFindRemovalPermutations;
begin
  CheckOrderedCardinal(occInsertionPermutations);
end;

procedure TOrderedCardinalTests.ActualListDuplicateAndMissingValues;
begin
  CheckOrderedCardinal(occDuplicateAndMissingValues);
end;

initialization
  RegisterTest(TOrderedCardinalTests);
end.
