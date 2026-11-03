unit UBufferedAdoptionTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$MODE Delphi}{$H+}

interface

uses fpcunit, testregistry;

type
  TBufferedAdoptionTests = class(TTestCase)
  published
    procedure PendingProtocol5RoundTripAndReplacement;
    procedure PendingProtocol5CancellationAndRetry;
    procedure HistoryReadSaveDeleteAndReopen;
    procedure HistoryHeaderGroupBoundary;
    procedure RandomHashCanonicalTable;
    procedure RandomHashInvalidMagic;
    procedure RandomHashTruncatedSeal;
    procedure RandomHashMismatchedSeal;
    procedure MalformedHistoryReadOnlyPreservesBytes;
    procedure PendingProtocol6RoundTripAndReplacement;
    procedure PendingProtocol6CancellationAndRetry;
  end;

implementation

uses UBufferedAdoptionChecks;

procedure TBufferedAdoptionTests.PendingProtocol5RoundTripAndReplacement;
begin
  CheckBufferedAdoption(1);
end;

procedure TBufferedAdoptionTests.PendingProtocol5CancellationAndRetry;
begin
  CheckBufferedAdoption(2);
end;

procedure TBufferedAdoptionTests.HistoryReadSaveDeleteAndReopen;
begin
  CheckBufferedAdoption(3);
end;

procedure TBufferedAdoptionTests.HistoryHeaderGroupBoundary;
begin
  CheckBufferedAdoption(4);
end;

procedure TBufferedAdoptionTests.RandomHashCanonicalTable;
begin
  CheckBufferedAdoption(5);
end;

procedure TBufferedAdoptionTests.RandomHashInvalidMagic;
begin
  CheckBufferedAdoption(6);
end;

procedure TBufferedAdoptionTests.RandomHashTruncatedSeal;
begin
  CheckBufferedAdoption(7);
end;

procedure TBufferedAdoptionTests.RandomHashMismatchedSeal;
begin
  CheckBufferedAdoption(8);
end;

procedure TBufferedAdoptionTests.MalformedHistoryReadOnlyPreservesBytes;
begin
  CheckBufferedAdoption(9);
end;

procedure TBufferedAdoptionTests.PendingProtocol6RoundTripAndReplacement;
begin
  CheckBufferedAdoption(10);
end;

procedure TBufferedAdoptionTests.PendingProtocol6CancellationAndRetry;
begin
  CheckBufferedAdoption(11);
end;

initialization
  RegisterTest(TBufferedAdoptionTests);
end.
