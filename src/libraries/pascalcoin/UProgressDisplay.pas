unit UProgressDisplay;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Presentation of typed node progress for GUI, console and RPC clients.
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}

interface

uses SysUtils, UAccounts;

// Recreate the producer's original text separately from each application's
// percentage/prefix decoration; raw counters and metadata remain unchanged.
function FormatProgressMessage(const AProgress: TProgressInfo): String;
function FormatProgressInfo(const AProgress: TProgressInfo): String;
function FormatProgressLogInfo(const AProgress: TProgressInfo): String;

implementation

function FormatProgressMessage(const AProgress: TProgressInfo): String;
begin
  case AProgress.Stage of
    psNone: Result := '';
    psReadingSafebox:
      Result := Format('Reading Safebox block %d/%d',
        [AProgress.BlockNumber, AProgress.BlockCount]);
    psValidatingOperationBlocks:
      Result := Format('Validating OperationBlock info %d/%d',
        [AProgress.CurrentPosition, AProgress.TotalCount]);
    psCheckingSafeboxIntegrity: Result := 'Checking Safebox integrity';
    psReadingBlockBatch:
      Result := Format('Reading blocks from %d to %d with %d operations',
        [AProgress.StartBlock, AProgress.EndBlock, AProgress.OperationsCount]);
    psReadingBlockchain:
      Result := Format('Reading blockchain block %d/%d',
        [AProgress.BlockNumber, AProgress.BlockCount]);
    psLoadingPendingOperations: Result := 'Loading saved pending operations';
    psReadingOperations: Result := Format('Reading operations %d/%d', [AProgress.CurrentPosition, AProgress.TotalCount]);
    psPreparingOperations: Result := Format('Preparing operations %d/%d', [AProgress.CurrentPosition, AProgress.TotalCount]);
    psValidatingSignatures: Result := Format('Validating signatures %d/%d', [AProgress.CurrentPosition, AProgress.TotalCount]);
    psApplyingOperations: Result := Format('Applying operations %d/%d', [AProgress.CurrentPosition, AProgress.TotalCount]);
    psFinalizingOperations: Result := Format('Finalizing operations %d/%d', [AProgress.CurrentPosition, AProgress.TotalCount]);
    psFinalizingRestore: Result := 'Finalizing local restore...';
    psDownloadingSafeboxChunks:
      Result := Format('Downloading Safebox chunks %d/%d',
        [AProgress.CurrentPosition, AProgress.ChunksCount]);
    psSavingSafebox:
      Result := Format('Saving Safebox %d chunks to %s',
        [AProgress.ChunksCount, ExtractFileName(AProgress.FileName)]);
    psReceivedSafebox:
      Result := Format('Received new safebox with %d blocks from %s',
        [AProgress.BlockCount, AProgress.PeerAddress]);
    psDownloadingBlockchain:
      Result := Format('Downloading block %d from %s',
        [AProgress.BlockNumber, AProgress.PeerAddress]);
  else
    Result := '';
  end;
end;

function FormatProgressInfo(const AProgress: TProgressInfo): String;
var
  LPercentage: Extended;
  LNetworkPercentage: Boolean;
  LPercentText: String;
begin
  Result := FormatProgressMessage(AProgress);
  if Result = '' then Exit;
  // These two values were already complete network status strings. They did
  // not pass through a progress callback's percentage/separator wrapper.
  if AProgress.Stage in [psReceivedSafebox, psDownloadingBlockchain] then Exit;

  LPercentText := '';
  if AProgress.TotalCount > 0 then begin
    // Keep the original arithmetic order wherever the Int64 multiply is safe.
    // For wider counters use numeric real conversion, never a bit-level cast.
    // The original callbacks displayed the raw ratio without clamping.
    if (AProgress.CurrentPosition >= Low(Int64) div 100) and
      (AProgress.CurrentPosition <= High(Int64) div 100) then
      LPercentage := AProgress.CurrentPosition * 100 / AProgress.TotalCount
    else begin
      LPercentage := AProgress.CurrentPosition;
      LPercentage := LPercentage / AProgress.TotalCount * 100;
    end;
    LNetworkPercentage := (AProgress.Context = pcCheckingNewSafebox) or
      (AProgress.Stage in [psDownloadingSafeboxChunks, psSavingSafebox]);
    if LNetworkPercentage then LPercentText := FormatFloat('0.00', LPercentage) + '%'
    else LPercentText := Format('%.2f', [LPercentage]) + '%';
  end;
  // The separator is intentional even for an unknown total: both original
  // GUI callbacks and network wrappers retained that trailing space.
  Result := Result + ' ' + LPercentText;
  if AProgress.Context = pcCheckingNewSafebox then
    Result := 'Checking new safebox: ' + Result;
end;

function FormatProgressLogInfo(const AProgress: TProgressInfo): String;
begin
  if AProgress.Stage = psNone then Exit('');
  // The daemon logged raw callback positions and text, not the GUI percentage.
  Result := Format('Progress (%d/%d): %s', [AProgress.CurrentPosition,
    AProgress.TotalCount, FormatProgressMessage(AProgress)]);
end;

end.
