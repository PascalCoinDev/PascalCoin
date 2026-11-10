unit UOrderedCardinalChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Full-range ordering through the existing AbstractMem collection used by
  affected-account history. No replacement collection or comparator is tested. }

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

type
  TOrderedCardinalCheck = (occBoundaryPairs, occInsertionPermutations,
    occDuplicateAndMissingValues);

procedure CheckOrderedCardinal(ACheck: TOrderedCardinalCheck);
function RunOrderedCardinalChecks(const ALogFile: String): Integer;

implementation

uses Classes, SysUtils, UOrderedList;

const
  CValues: array[0..4] of Cardinal = (0, 1, $7FFFFFFF, $80000000, $FFFFFFFF);
  CNames: array[TOrderedCardinalCheck] of String =
    ('all 25 full-Cardinal boundary pairs',
     'all 120 actual-list insertion/find/removal permutations',
     'actual-list duplicate policy and missing-value insertion positions');

type
  TBoundaryOrder = array[0..4] of Integer;
  TBoundaryMembership = array[0..4] of Boolean;

procedure Require(ACondition: Boolean; const AReason: String);
begin
  if not ACondition then raise Exception.Create(AReason);
end;

procedure CheckBoundaryPairs;
var I, J, LExpected, LActual: Integer;
begin
  for I := Low(CValues) to High(CValues) do
    for J := Low(CValues) to High(CValues) do begin
      // These constants are independently ordered by their specification,
      // including the signed/unsigned boundary. Do not subtract the values.
      LExpected := 0;
      if I < J then LExpected := -1 else if I > J then LExpected := 1;
      LActual := TComparison_Cardinal(CValues[I], CValues[J]);
      Require(((LExpected = 0) and (LActual = 0)) or
        ((LExpected < 0) and (LActual < 0)) or
        ((LExpected > 0) and (LActual > 0)), Format('Compare(%s,%s) expected sign %d, got %d',
        [UIntToStr(CValues[I]), UIntToStr(CValues[J]), LExpected, LActual]));
    end;
end;

procedure CheckMembers(AList: TOrderedList<Cardinal>; const APresent: TBoundaryMembership);
var I, LRow, LFound: Integer;
begin
  LRow := 0;
  for I := Low(CValues) to High(CValues) do
    if APresent[I] then begin
      Require(LRow < AList.Count, 'Actual list omitted an inserted boundary value');
      Require(AList[LRow] = CValues[I], 'Actual list is not in full Cardinal order');
      Require(AList.Find(CValues[I], LFound) and (LFound = LRow),
        'Actual list Find did not locate the ordered boundary value');
      Require(AList.IndexOf(CValues[I]) = LRow, 'Actual list IndexOf disagrees with Find');
      Inc(LRow);
    end else begin
      Require(not AList.Find(CValues[I], LFound), 'Actual list retained a removed boundary value');
      Require(AList.IndexOf(CValues[I]) = -1, 'Actual list IndexOf found an absent value');
    end;
  Require(AList.Count = LRow, 'Actual list contains extra values');
end;

procedure CheckPermutations;
var LOrder: TBoundaryOrder; LPermutationCount, I: Integer;

  procedure RunPermutation;
  var LList: TOrderedList<Cardinal>; LPresent: TBoundaryMembership; J: Integer;
  begin
    FillChar(LPresent, SizeOf(LPresent), 0);
    LList := TOrderedList<Cardinal>.Create(False, TComparison_Cardinal);
    try
      for J := Low(LOrder) to High(LOrder) do begin
        Require(LList.Add(CValues[LOrder[J]]) >= 0, 'A distinct boundary insertion was rejected');
        LPresent[LOrder[J]] := True;
        CheckMembers(LList, LPresent);
      end;
      for J := High(LOrder) downto Low(LOrder) do begin
        LList.Remove(CValues[LOrder[J]]);
        LPresent[LOrder[J]] := False;
        CheckMembers(LList, LPresent);
      end;
      Require(LList.Count = 0, 'Actual list did not empty after all boundary removals');
    finally LList.Free; end;
    Inc(LPermutationCount);
  end;

  procedure Permute(APosition: Integer);
  var J, LTemporary: Integer;
  begin
    if APosition > High(LOrder) then begin
      RunPermutation;
      Exit;
    end;
    for J := APosition to High(LOrder) do begin
      LTemporary := LOrder[APosition]; LOrder[APosition] := LOrder[J]; LOrder[J] := LTemporary;
      Permute(APosition + 1);
      LTemporary := LOrder[APosition]; LOrder[APosition] := LOrder[J]; LOrder[J] := LTemporary;
    end;
  end;

begin
  LPermutationCount := 0;
  for I := Low(LOrder) to High(LOrder) do LOrder[I] := I;
  Permute(0);
  Require(LPermutationCount = 120, 'The actual-list fixture did not execute all five-value permutations');
end;

procedure CheckDuplicateAndMissingValues;
var LList: TOrderedList<Cardinal>; I, LFound: Integer;
begin
  LList := TOrderedList<Cardinal>.Create(False, TComparison_Cardinal);
  try
    for I := High(CValues) downto Low(CValues) do LList.Add(CValues[I]);
    for I := Low(CValues) to High(CValues) do
      Require(LList.Add(CValues[I]) = -1, 'Disallowed duplicate insertion changed policy');
    Require(LList.Count = Length(CValues), 'Disallowed duplicates changed the list count');
    Require(not LList.Find(2, LFound) and (LFound = 2), 'Missing low value has the wrong insertion position');
    Require(not LList.Find($80000001, LFound) and (LFound = 4), 'Missing unsigned value has the wrong insertion position');
    LList.Remove(2);
    Require(LList.Count = Length(CValues), 'Removing a missing value changed the list');
  finally LList.Free; end;
  LList := TOrderedList<Cardinal>.Create(True, TComparison_Cardinal);
  try
    for I := High(CValues) downto Low(CValues) do begin
      LList.Add(CValues[I]); LList.Add(CValues[I]);
    end;
    Require(LList.Count = 2 * Length(CValues), 'Allowed duplicates were not retained');
    for I := Low(CValues) to High(CValues) do begin
      Require((LList[2 * I] = CValues[I]) and (LList[2 * I + 1] = CValues[I]),
        'Allowed duplicates lost full Cardinal ordering');
    end;
    LList.Remove(High(Cardinal));
    Require((LList.Count = 9) and LList.Find(High(Cardinal), LFound), 'Single removal deleted all allowed duplicates');
    LList.Remove(High(Cardinal), True);
    Require((LList.Count = 8) and not LList.Find(High(Cardinal), LFound), 'Remove-all left an unsigned duplicate');
    LList.Clear;
    Require((LList.Count = 0) and not LList.Find(0, LFound) and (LFound = 0),
      'Actual empty-list Find/Clear behavior changed');
  finally LList.Free; end;
end;

procedure CheckOrderedCardinal(ACheck: TOrderedCardinalCheck);
begin
  case ACheck of
    occBoundaryPairs: CheckBoundaryPairs;
    occInsertionPermutations: CheckPermutations;
    occDuplicateAndMissingValues: CheckDuplicateAndMissingValues;
  end;
end;

function RunOrderedCardinalChecks(const ALogFile: String): Integer;
var LLog: TStringList; LCheck: TOrderedCardinalCheck;
begin
  Result := 0;
  LLog := TStringList.Create;
  try
    for LCheck := Low(TOrderedCardinalCheck) to High(TOrderedCardinalCheck) do
      try
        CheckOrderedCardinal(LCheck);
        LLog.Add('PASS ' + CNames[LCheck]);
      except on E: Exception do begin
        Result := 1;
        LLog.Add('FAIL ' + CNames[LCheck] + ': ' + E.ClassName + ': ' + E.Message);
      end; end;
    LLog.Add('RESULT ' + IntToStr(Result));
    LLog.SaveToFile(ALogFile);
  finally LLog.Free; end;
end;

end.
