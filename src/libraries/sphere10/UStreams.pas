unit UStreams;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>.

  Stream decorators with bounded read-ahead and explicit ownership.
  Distributed under the MIT license; see LICENSE.

  Public types in this unit

  Type            | Purpose
  ----------------+------------------------------------------------------------
  TBufferedStream | Buffers reads, with optional length caching and ownership.
}

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses Classes, SysUtils;

type
  { Reads ahead into one fixed-size buffer. Writes are forwarded immediately.
    Access the input only through this decorator while it is in use. The input
    determines whether seeking, sizing and writing are supported; sequential
    reads do not require those capabilities. This class adds no synchronization.

    Read-ahead can put WrappedStream.Position beyond this stream's Position.
    FlushUnreadData explicitly restores that cursor when seeking is supported.
    Destruction never seeks: an owned input is freed, while a borrowed input is
    left at its physical cursor. Ownership transfers only after Create succeeds.

    EnableReadBuffering and EnableStreamLengthCaching both default to True.
    Length caching retains one successful Size query; disable it for inputs that
    can grow independently. Writes/resizes invalidate the retained length before
    attempting mutation. Construction never queries the input's size or position.

    Disabling read buffering forwards reads/writes directly and releases
    the read-ahead memory. BufferSize retains the configured capacity for the
    next enable. Unread bytes must first be consumed or returned to the input by
    FlushUnreadData; disabling never attempts an implicit rewind. Length caching
    works independently of read buffering, including when read buffering is off. }
  TBufferedStream = class(TStream)
    private
      FStream: TStream;
      FBufferedData: TBytes;
      FBufferSize: Integer;
      FBufferOffset: Integer;
      FBufferCount: Integer;
      FPosition: Int64;
      FPositionKnown: Boolean;
      FOwnsStream: Boolean;
      FEnableReadBuffering: Boolean;
      FEnableStreamLengthCaching: Boolean;
      FCachedStreamLength: Int64;
      FSizeKnown: Boolean;
      function GetBufferSize: Integer;
      procedure SetEnableReadBuffering(AValue: Boolean);
      procedure SetEnableStreamLengthCaching(AValue: Boolean);
      procedure DiscardReadBuffer;
      procedure InvalidateAfterFailure;
      function AddOffset(APosition, AOffset: Int64): Int64;
      function TrySeekInBuffer(APosition: Int64): Boolean;
    protected
      {$IFNDEF FPC}{$IF DECLARED(TStreamCapabilities)}
      function GetCapabilities: TStreamCapabilities; override;
      {$ENDIF}{$ENDIF}
      function GetSize: Int64; override;
      procedure SetSize(NewSize: Longint); override;
      procedure SetSize(const NewSize: Int64); override;
    public
      constructor Create(AStream: TStream; ABufferSize: Integer = 65536; AOwnsStream: Boolean = False; AEnableStreamLengthCaching: Boolean = True; AEnableReadBuffering: Boolean = True);
      destructor Destroy; override;
      function Read(var Buffer; Count: Longint): Longint; overload; override;
      function Write(const Buffer; Count: Longint): Longint; overload; override;
      function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
      // Convenience methods set both flags. DisableBuffering throws before
      // changing either flag if unread bytes have not been explicitly flushed.
      procedure EnableBuffering;
      procedure DisableBuffering;
      // Returns unread bytes to the input's cursor; it does not discard them.
      // An unsupported rewind throws and keeps the pre-read buffer intact.
      procedure FlushUnreadData;
      // Compatibility name for FlushUnreadData.
      procedure SynchronizePosition;
      property WrappedStream: TStream read FStream;
      property BufferSize: Integer read GetBufferSize;
      property OwnsStream: Boolean read FOwnsStream;
      property EnableStreamLengthCaching: Boolean read FEnableStreamLengthCaching write SetEnableStreamLengthCaching;
      property EnableReadBuffering: Boolean read FEnableReadBuffering write SetEnableReadBuffering;
  end;

implementation

constructor TBufferedStream.Create(AStream: TStream; ABufferSize: Integer; AOwnsStream: Boolean; AEnableStreamLengthCaching: Boolean; AEnableReadBuffering: Boolean);
begin
  inherited Create;
  if AStream = nil then
    raise EArgumentNilException.Create('AStream must not be nil.');
  if ABufferSize <= 0 then
    raise EArgumentOutOfRangeException.Create('ABufferSize must be positive.');
  FBufferSize := ABufferSize;
  if AEnableReadBuffering then SetLength(FBufferedData, FBufferSize);
  // Do not probe Position/Size here: a forward-only stream may still be read.
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FEnableStreamLengthCaching := AEnableStreamLengthCaching;
  FEnableReadBuffering := AEnableReadBuffering;
end;

destructor TBufferedStream.Destroy;
begin
  try
    if FOwnsStream then
      FreeAndNil(FStream);
  finally
    inherited Destroy;
  end;
end;

function TBufferedStream.GetBufferSize: Integer;
begin
  Result := FBufferSize;
end;

procedure TBufferedStream.EnableBuffering;
begin
  // Allocation happens first; a failure leaves both settings unchanged.
  SetEnableReadBuffering(True);
  SetEnableStreamLengthCaching(True);
end;

procedure TBufferedStream.DisableBuffering;
begin
  // Reject unread bytes before changing either setting or releasing memory.
  SetEnableReadBuffering(False);
  SetEnableStreamLengthCaching(False);
end;

procedure TBufferedStream.SetEnableReadBuffering(AValue: Boolean);
begin
  if AValue = FEnableReadBuffering then Exit;
  // Switching modes must never perform an implicit seek or lose unread bytes.
  // The caller explicitly chooses when to attempt an unsupported rewind.
  if not AValue and (FBufferCount > FBufferOffset) then
    raise EStreamError.Create('Call FlushUnreadData before disabling buffering with unread bytes.');
  // Allocate before publishing the enabled state, so allocation failure leaves
  // the direct stream usable. Disabling releases the entire pre-read allocation.
  if AValue then
    SetLength(FBufferedData, FBufferSize)
  else
    SetLength(FBufferedData, 0);
  DiscardReadBuffer;
  FPositionKnown := False;
  FSizeKnown := False;
  FEnableReadBuffering := AValue;
end;

procedure TBufferedStream.SetEnableStreamLengthCaching(AValue: Boolean);
begin
  if AValue = FEnableStreamLengthCaching then Exit;
  // Only length metadata changes here; unread bytes and the cursor stay intact.
  FSizeKnown := False;
  FEnableStreamLengthCaching := AValue;
end;

procedure TBufferedStream.DiscardReadBuffer;
begin
  FBufferOffset := 0;
  FBufferCount := 0;
end;

procedure TBufferedStream.InvalidateAfterFailure;
begin
  // A failing input may already have advanced, including before returning a
  // negative count. Obtain its actual position only on a later query.
  DiscardReadBuffer;
  FPositionKnown := False;
end;

function TBufferedStream.AddOffset(APosition, AOffset: Int64): Int64;
var LOverflow: Boolean;
begin
  LOverflow := False;
  if AOffset > 0 then
    LOverflow := APosition > High(Int64) - AOffset
  else if AOffset < 0 then
    LOverflow := APosition < Low(Int64) - AOffset;
  if LOverflow then
    raise ERangeError.Create('Stream position exceeds the Int64 range.');
  Result := APosition + AOffset;
end;

function TBufferedStream.TrySeekInBuffer(APosition: Int64): Boolean;
var LStart: Int64;
begin
  Result := False;
  if not FPositionKnown then Exit;
  LStart := FPosition - FBufferOffset;
  if APosition < LStart then Exit;
  if APosition - LStart > FBufferCount then Exit;
  FBufferOffset := Integer(APosition - LStart);
  FPosition := APosition;
  Result := True;
end;

function TBufferedStream.Read(var Buffer; Count: Longint): Longint;
var LDestination: PByte; LAvailable, LCopy, LRead, LRemaining: Integer; LReadFromSource: Boolean;
begin
  if Count < 0 then
    raise EArgumentOutOfRangeException.Create('Count must not be negative.');
  if Count = 0 then Exit(0);
  if not FEnableReadBuffering then Exit(FStream.Read(Buffer, Count));
  Result := 0;
  LDestination := @Buffer;
  LReadFromSource := False;
  while Result < Count do begin
    LAvailable := FBufferCount - FBufferOffset;
    LRemaining := Count - Result;
    if LAvailable > 0 then begin
      LCopy := LAvailable;
      if LCopy > LRemaining then LCopy := LRemaining;
      Move(FBufferedData[FBufferOffset], LDestination^, LCopy);
      Inc(FBufferOffset, LCopy);
      Inc(Result, LCopy);
      Inc(LDestination, LCopy);
      if FPositionKnown then Inc(FPosition, LCopy);
    end else begin
      // Read returns available bytes. A second input read could block waiting
      // for a network peer, even after this call has already received data.
      if LReadFromSource then Exit;
      DiscardReadBuffer;
      LReadFromSource := True;
      try
        // Large reads need no intermediate copy or second full-size allocation.
        if LRemaining >= Length(FBufferedData) then begin
          LRead := FStream.Read(LDestination^, LRemaining);
          if LRead > LRemaining then
            raise EStreamError.Create('The input returned more bytes than requested.');
          if LRead <= 0 then begin
            if LRead < 0 then begin
              InvalidateAfterFailure;
              Result := LRead;
            end;
            Exit;
          end;
          Inc(Result, LRead);
          Inc(LDestination, LRead);
          if FPositionKnown then Inc(FPosition, LRead);
        end else begin
          LRead := FStream.Read(FBufferedData[0], Length(FBufferedData));
          if LRead > Length(FBufferedData) then
            raise EStreamError.Create('The input returned more bytes than requested.');
          if LRead <= 0 then begin
            if LRead < 0 then begin
              InvalidateAfterFailure;
              Result := LRead;
            end;
            Exit;
          end;
          FBufferCount := LRead;
        end;
      except
        InvalidateAfterFailure;
        raise;
      end;
    end;
  end;
end;

procedure TBufferedStream.FlushUnreadData;
var LUnread: Integer; LPosition: Int64;
begin
  LUnread := FBufferCount - FBufferOffset;
  if LUnread > 0 then begin
    // On failure keep the unread bytes. In particular, an unsupported seek must
    // not turn a rejected read-to-write transition into lost buffered data.
    LPosition := FStream.Seek(-Int64(LUnread), soCurrent);
    if LPosition < 0 then
      raise EStreamError.Create('The input could not synchronize its position.');
    FPosition := LPosition;
    FPositionKnown := True;
  end;
  DiscardReadBuffer;
end;

procedure TBufferedStream.SynchronizePosition;
begin
  FlushUnreadData;
end;

function TBufferedStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count < 0 then
    raise EArgumentOutOfRangeException.Create('Count must not be negative.');
  if Count = 0 then Exit(0);
  // Even a short or failed write may change the input's length.
  FSizeKnown := False;
  if not FEnableReadBuffering then Exit(FStream.Write(Buffer, Count));
  SynchronizePosition;
  try
    Result := FStream.Write(Buffer, Count);
    if Result > Count then
      raise EStreamError.Create('The input wrote more bytes than requested.');
    if Result < 0 then InvalidateAfterFailure;
    if FPositionKnown and (Result > 0) then Inc(FPosition, Result);
  except
    InvalidateAfterFailure;
    raise;
  end;
end;

function TBufferedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var LTarget, LAdjusted: Int64;
begin
  if not FEnableReadBuffering then Exit(FStream.Seek(Offset, Origin));
  if (Origin = soCurrent) and (Offset = 0) then begin
    if not FPositionKnown then begin
      // Obtain an absolute cursor once. Ordinary reads and repeated Position
      // checks subsequently use the logical cursor without asking the input.
      LTarget := FStream.Seek(0, soCurrent);
      // Some handle streams report a failed seek with -1 instead of raising.
      // Preserve that sentinel and keep unread data available to Read.
      if LTarget < 0 then Exit(LTarget);
      FPosition := LTarget - (FBufferCount - FBufferOffset);
      FPositionKnown := True;
    end;
    Exit(FPosition);
  end;
  case Origin of
    soBeginning: begin
      LTarget := Offset;
      if TrySeekInBuffer(LTarget) then Exit(LTarget);
      LAdjusted := Offset;
    end;
    soCurrent: begin
      if FPositionKnown then begin
        LTarget := AddOffset(FPosition, Offset);
        if TrySeekInBuffer(LTarget) then Exit(LTarget);
      end;
      LAdjusted := AddOffset(Offset, -Int64(FBufferCount - FBufferOffset));
    end;
    soEnd: LAdjusted := Offset;
  else
    raise EArgumentOutOfRangeException.Create('Invalid seek origin.');
  end;
  try
    Result := FStream.Seek(LAdjusted, Origin);
    if Result < 0 then begin
      FPositionKnown := False;
      Exit;
    end;
    DiscardReadBuffer;
    FPosition := Result;
    FPositionKnown := True;
  except
    // Unsupported seeks must not consume unread bytes from a forward-only
    // input. Its failure state is defined by the input; keep the cached bytes
    // and obtain an absolute position again only if a later caller requests it.
    FPositionKnown := False;
    raise;
  end;
end;

{$IFNDEF FPC}{$IF DECLARED(TStreamCapabilities)}
function TBufferedStream.GetCapabilities: TStreamCapabilities;
begin
  // Modern Delphi uses these flags for ReadBuffer/WriteBuffer OS diagnostics
  // and CopyFrom preallocation. The wrapped stream still owns those behaviors.
  Result := FStream.Capabilities;
end;
{$ENDIF}{$ENDIF}

function TBufferedStream.GetSize: Int64;
begin
  // Length metadata is independent of the byte buffer, including direct reads.
  if FEnableStreamLengthCaching and FSizeKnown then Exit(FCachedStreamLength);
  try
    Result := FStream.Size;
    if Result < 0 then
      FPositionKnown := False
    else if FEnableStreamLengthCaching then begin
      FCachedStreamLength := Result;
      FSizeKnown := True;
    end;
  except
    // A size query may be unsupported even when sequential reads are valid.
    // Keep unread data rather than silently skipping it after that exception.
    FPositionKnown := False;
    raise;
  end;
end;

procedure TBufferedStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TBufferedStream.SetSize(const NewSize: Int64);
begin
  if NewSize < 0 then
    raise EArgumentOutOfRangeException.Create('NewSize must not be negative.');
  // Direct-mode mutations must also invalidate independently cached length.
  FSizeKnown := False;
  if not FEnableReadBuffering then begin
    FStream.Size := NewSize;
    Exit;
  end;
  // Query the actual length afterward: an input may ignore or partly apply it.
  SynchronizePosition;
  try
    FStream.Size := NewSize;
    // Input implementations differ on whether resizing moves their cursor.
    FPositionKnown := False;
  except
    InvalidateAfterFailure;
    raise;
  end;
end;

end.
