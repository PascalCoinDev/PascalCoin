unit UStreams.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://www.sphere10.com>
  Independent buffered-stream contracts shared by FPCUnit and native Delphi.
  Distributed under the MIT software license; see LICENSE. }

{$IFDEF FPC}{$MODE Delphi}{$H+}{$ENDIF}
{$IFNDEF FPC}{$IF SizeOf(NativeInt)>SizeOf(Longint)}{$DEFINE STREAM_TEST_NATIVE_COUNTS}{$IFEND}{$ENDIF}

interface

const BufferedStreamCheckCount = 101;
procedure RunBufferedStreamCheck(AIndex: Integer);
function BufferedStreamCheckName(AIndex: Integer): String;

implementation

uses Classes, SysUtils, UStreams;

type
  ESourceFailure = class(EStreamError);
  PDisposeCount = ^Integer;

  TObservedStream = class(TMemoryStream)
  private
    FDisposeCount: PDisposeCount;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const ANewSize: Int64); override;
  public
    ReadCalls, WriteCalls, SeekCalls, SizeCalls: Integer;
    LargestRead, MaxRead, MaxWrite: Longint;
    FailRead, FailWrite, FailSeek, FailSize, FailResize: Boolean;
    MoveBeforeReadFailure, MoveBeforeWriteFailure, MoveBeforeSeekFailure: Boolean;
    NegativeReadResult, NegativeWriteResult, RejectSecondRead: Boolean;
    constructor Create(ADisposeCount: PDisposeCount = nil);
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; overload; override;
    function Write(const ABuffer; ACount: Longint): Longint; overload; override;
    {$IFDEF STREAM_TEST_NATIVE_COUNTS}
    function Read(var ABuffer; ACount: NativeInt): NativeInt; overload; override;
    function Write(const ABuffer; ACount: NativeInt): NativeInt; overload; override;
    {$ENDIF}
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    procedure ResetCounts;
  end;

  TSparseStream = class(TStream)
  private
    FSize, FPosition: Int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const ANewSize: Int64); override;
  public
    ReadCalls, LargestRead: Integer;
    constructor Create;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
  end;

  TForwardStream = class(TStream)
  private
    FData: TBytes;
    FPosition: Integer;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const ANewSize: Int64); override;
  public
    ReadCalls, WriteCalls, SeekCalls, SizeCalls, ResizeCalls: Integer;
    LastWritePosition: Integer;
    ReturnNegativeSeek, ReturnNegativeSize: Boolean;
    constructor Create;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
  end;

  TSizeObservedStream = class(TObservedStream)
  protected
    function GetSize: Int64; override;
    procedure SetSize(const ANewSize: Int64); override;
  public
    ResizeCalls: Integer;
    NegativeSize, IgnoreResize, FailResizeAfterMutation: Boolean;
    procedure ResetCounts; reintroduce;
  end;

  TCountedSparseStream = class(TSparseStream)
  protected
    function GetSize: Int64; override;
  public
    SizeCalls: Integer;
  end;

  TInjectedReadFailureStream = class(TStream)
  public
    Failure: Exception;
    ReadCalls: Integer;
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
  end;

procedure Check(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('Buffered stream: '+AMessage);
end;

constructor TObservedStream.Create(ADisposeCount: PDisposeCount);
begin
  inherited Create;
  FDisposeCount := ADisposeCount;
end;

destructor TObservedStream.Destroy;
begin
  if Assigned(FDisposeCount) then Inc(FDisposeCount^);
  inherited;
end;

procedure TObservedStream.ResetCounts;
begin
  ReadCalls := 0;
  WriteCalls := 0;
  SeekCalls := 0;
  SizeCalls := 0;
  LargestRead := 0;
end;

function TObservedStream.GetSize: Int64;
begin
  Inc(SizeCalls);
  if FailSize then raise ESourceFailure.Create('source-size');
  Result := inherited GetSize;
end;

procedure TObservedStream.SetSize(const ANewSize: Int64);
begin
  if FailResize then raise ESourceFailure.Create('source-resize');
  inherited SetSize(ANewSize);
end;

function TObservedStream.Read(var ABuffer; ACount: Longint): Longint;
begin
  {$IFDEF STREAM_TEST_NATIVE_COUNTS}
  Result := Longint(Read(ABuffer,NativeInt(ACount)));
  {$ELSE}
  Inc(ReadCalls);
  if ACount>LargestRead then LargestRead := ACount;
  if RejectSecondRead and (ReadCalls>1) then raise ESourceFailure.Create('unexpected-second-read');
  if NegativeReadResult then begin
    if ACount>=2 then inherited Read(ABuffer,2);
    Exit(-1);
  end;
  if FailRead then begin
    if MoveBeforeReadFailure and (ACount>=2) then inherited Read(ABuffer,2);
    raise ESourceFailure.Create('source-read');
  end;
  if (MaxRead>0) and (ACount>MaxRead) then ACount := MaxRead;
  Result := inherited Read(ABuffer,ACount);
  {$ENDIF}
end;

function TObservedStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  {$IFDEF STREAM_TEST_NATIVE_COUNTS}
  Result := Longint(Write(ABuffer,NativeInt(ACount)));
  {$ELSE}
  Inc(WriteCalls);
  if NegativeWriteResult then begin
    if ACount>=2 then inherited Write(ABuffer,2);
    Exit(-1);
  end;
  if FailWrite then begin
    if MoveBeforeWriteFailure and (ACount>=2) then inherited Write(ABuffer,2);
    raise ESourceFailure.Create('source-write');
  end;
  if (MaxWrite>0) and (ACount>MaxWrite) then ACount := MaxWrite;
  Result := inherited Write(ABuffer,ACount);
  {$ENDIF}
end;

{$IFDEF STREAM_TEST_NATIVE_COUNTS}
function TObservedStream.Read(var ABuffer; ACount: NativeInt): NativeInt;
begin
  // Delphi64 TMemoryStream implements this virtual overload directly; its
  // Longint overload calls back into it. Count each physical read only once.
  Inc(ReadCalls);
  if ACount>LargestRead then LargestRead := Longint(ACount);
  if RejectSecondRead and (ReadCalls>1) then raise ESourceFailure.Create('unexpected-second-read');
  if NegativeReadResult then begin
    if ACount>=2 then inherited Read(ABuffer,NativeInt(2));
    Exit(-1);
  end;
  if FailRead then begin
    if MoveBeforeReadFailure and (ACount>=2) then inherited Read(ABuffer,NativeInt(2));
    raise ESourceFailure.Create('source-read');
  end;
  if (MaxRead>0) and (ACount>MaxRead) then ACount := MaxRead;
  Result := inherited Read(ABuffer,ACount);
end;

function TObservedStream.Write(const ABuffer; ACount: NativeInt): NativeInt;
begin
  Inc(WriteCalls);
  if NegativeWriteResult then begin
    if ACount>=2 then inherited Write(ABuffer,NativeInt(2));
    Exit(-1);
  end;
  if FailWrite then begin
    if MoveBeforeWriteFailure and (ACount>=2) then inherited Write(ABuffer,NativeInt(2));
    raise ESourceFailure.Create('source-write');
  end;
  if (MaxWrite>0) and (ACount>MaxWrite) then ACount := MaxWrite;
  Result := inherited Write(ABuffer,ACount);
end;
{$ENDIF}

function TObservedStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  Inc(SeekCalls);
  if FailSeek then begin
    if MoveBeforeSeekFailure then inherited Seek(Int64(13),soBeginning);
    raise ESourceFailure.Create('source-seek');
  end;
  Result := inherited Seek(AOffset,AOrigin);
end;

constructor TSparseStream.Create;
begin
  inherited Create;
  FSize := (Int64(1) shl 40)+123;
end;

function TSparseStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TSparseStream.SetSize(const ANewSize: Int64);
begin
  Check(ANewSize>=0,'negative sparse size');
  FSize := ANewSize;
  if FPosition>FSize then FPosition := FSize;
end;

function TSparseStream.Read(var ABuffer; ACount: Longint): Longint;
var I: Integer;
  P: PByte;
begin
  Inc(ReadCalls);
  if ACount>LargestRead then LargestRead := ACount;
  Result := ACount;
  if FPosition>=FSize then Result := 0
  else if FSize-FPosition<Result then Result := Integer(FSize-FPosition);
  P := @ABuffer;
  for I := 0 to Result-1 do begin
    P^ := Byte((FPosition xor (FPosition shr 8)) and 255);
    Inc(P);
    Inc(FPosition);
  end;
end;

function TSparseStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  Result := ACount;
  Inc(FPosition,Result);
  if FPosition>FSize then FSize := FPosition;
end;

function TSparseStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  case AOrigin of
    soBeginning: Result := AOffset;
    soCurrent: Result := FPosition+AOffset;
    soEnd: Result := FSize+AOffset;
  else
    raise EStreamError.Create('Invalid sparse seek origin');
  end;
  Check(Result>=0,'negative sparse position');
  FPosition := Result;
end;

function Pattern(ACount: Integer): TBytes;
var I: Integer;
begin
  SetLength(Result,ACount);
  for I := 0 to ACount-1 do Result[I] := Byte(I mod 251);
end;

procedure Seed(AStream: TStream; const AData: TBytes);
begin
  if Length(AData)>0 then AStream.WriteBuffer(AData[0],Length(AData));
  AStream.Position := 0;
end;

constructor TForwardStream.Create;
begin
  inherited Create;
  FData := Pattern(24);
end;

function TForwardStream.GetSize: Int64;
begin
  Inc(SizeCalls);
  if ReturnNegativeSize then Exit(-1);
  raise ESourceFailure.Create('forward-size');
end;

procedure TForwardStream.SetSize(const ANewSize: Int64);
begin
  Inc(ResizeCalls);
  raise ESourceFailure.Create('forward-resize');
end;

function TForwardStream.Read(var ABuffer; ACount: Longint): Longint;
begin
  Inc(ReadCalls);
  Result := ACount;
  if Result>Length(FData)-FPosition then Result := Length(FData)-FPosition;
  if Result>0 then Move(FData[FPosition],ABuffer,Result);
  Inc(FPosition,Result);
end;

function TForwardStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  Inc(WriteCalls);
  LastWritePosition := FPosition;
  Result := ACount;
  if FPosition+Result>Length(FData) then SetLength(FData,FPosition+Result);
  if Result>0 then Move(ABuffer,FData[FPosition],Result);
  Inc(FPosition,Result);
end;

function TForwardStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  Inc(SeekCalls);
  if ReturnNegativeSeek then Exit(-1);
  raise ESourceFailure.Create('forward-seek');
end;

function TSizeObservedStream.GetSize: Int64;
begin
  if NegativeSize then begin
    Inc(SizeCalls);
    Exit(-1);
  end;
  Result := inherited GetSize;
end;

procedure TSizeObservedStream.ResetCounts;
begin
  inherited ResetCounts;
  ResizeCalls := 0;
end;

procedure TSizeObservedStream.SetSize(const ANewSize: Int64);
begin
  Inc(ResizeCalls);
  if IgnoreResize then Exit;
  inherited SetSize(ANewSize);
  if FailResizeAfterMutation then raise ESourceFailure.Create('source-resize-after-mutation');
end;

function TCountedSparseStream.GetSize: Int64;
begin
  Inc(SizeCalls);
  Result := inherited GetSize;
end;

destructor TInjectedReadFailureStream.Destroy;
begin
  Failure.Free;
  inherited Destroy;
end;

function TInjectedReadFailureStream.Read(var ABuffer; ACount: Longint): Longint;
var LFailure: Exception;
begin
  Inc(ReadCalls);
  LFailure := Failure;
  Failure := nil;
  raise LFailure;
end;

function TInjectedReadFailureStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  raise ESourceFailure.Create('injected-write-unsupported');
end;

function TInjectedReadFailureStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  raise ESourceFailure.Create('injected-seek-unsupported');
end;

procedure CheckPattern(AStream: TStream; ACount, AStart: Integer);
var I, LRead: Integer;
  LBytes: TBytes;
begin
  SetLength(LBytes,ACount);
  if ACount=0 then Exit;
  LRead := AStream.Read(LBytes[0],ACount);
  Check(LRead=ACount,'unexpected read length');
  for I := 0 to ACount-1 do
    Check(LBytes[I]=Byte((AStart+I) mod 251),'incorrect byte at '+IntToStr(AStart+I));
end;

procedure CheckMemoryEqual(ASource, AGold: TMemoryStream);
begin
  Check(ASource.Size=AGold.Size,'source contents have a different size');
  if AGold.Size>0 then
    Check(CompareMem(ASource.Memory,AGold.Memory,Integer(AGold.Size)),'source contents differ from independent TMemoryStream');
end;

procedure CheckArguments(AInvalidBuffer: Boolean);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LRaised: Boolean;
  LDisposed, I, LSize: Integer;
begin
  if not AInvalidBuffer then begin
    LRaised := False;
    LBuffered := nil;
    try
      try
        LBuffered := TBufferedStream.Create(nil);
      except
        on E: EArgumentException do LRaised := True;
      end;
      Check(LRaised,'nil source was accepted');
    finally
      LBuffered.Free;
    end;
    Exit;
  end;
  for I := 0 to 3 do begin
    LDisposed := 0;
    LSource := TObservedStream.Create(@LDisposed);
    LBuffered := nil;
    LRaised := False;
    LSize := -(I mod 2);
    try
      try
        LBuffered := TBufferedStream.Create(LSource,LSize,I>=2);
      except
        on E: EArgumentException do LRaised := True;
      end;
      Check(LRaised,'nonpositive buffer size was accepted');
      Check(LDisposed=0,'failed argument validation took source ownership');
    finally
      LBuffered.Free;
      if LDisposed=0 then LSource.Free;
    end;
  end;
end;

procedure CheckOwnership(AOwns: Boolean; AReadFails: Boolean);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LDisposed: Integer;
  B: Byte;
  {$IFNDEF FPC}{$IF DECLARED(TStreamCapabilities)}
  LHandle: THandleStream;
  LHandleBuffered: TBufferedStream;
  {$IFEND}{$ENDIF}
begin
  LDisposed := 0;
  LSource := TObservedStream.Create(@LDisposed);
  LBuffered := nil;
  try
    Seed(LSource,Pattern(10));
    if AOwns then LBuffered := TBufferedStream.Create(LSource,3,True)
    else LBuffered := TBufferedStream.Create(LSource);
    Check((LBuffered.WrappedStream=LSource) and (LBuffered.OwnsStream=AOwns),
      'wrapped stream or ownership property changed');
    if AOwns then Check(LBuffered.BufferSize=3,'explicit buffer size changed')
    else Check(LBuffered.BufferSize=65536,'default buffer size changed');
    {$IFNDEF FPC}{$IF DECLARED(TStreamCapabilities)}
    Check(LBuffered.Capabilities=LSource.Capabilities,'memory-stream capability flags were not forwarded');
    if not AOwns then begin
      // Query metadata only; this handle wrapper performs no OS I/O and does
      // not own the supplied handle. Its flags differ from TMemoryStream.
      LHandle := THandleStream.Create(0);
      try
        LHandleBuffered := TBufferedStream.Create(LHandle,3);
        try
          Check(LHandleBuffered.Capabilities=LHandle.Capabilities,
            'handle-stream OS-error capability was not forwarded');
        finally
          LHandleBuffered.Free;
        end;
      finally
        LHandle.Free;
      end;
    end;
    {$IFEND}{$ENDIF}
    if AReadFails then begin
      LSource.FailRead := True;
      try
        LBuffered.Read(B,1);
        Check(False,'expected owned source failure');
      except
        on E: ESourceFailure do Check(E.Message='source-read','changed source exception');
      end;
      LSource.FailRead := False;
    end else CheckPattern(LBuffered,1,0);
    FreeAndNil(LBuffered);
    if AOwns then Check(LDisposed=1,'owned source was not disposed exactly once')
    else begin
      Check(LDisposed=0,'borrowed source was disposed');
      LSource.Position := 0;
      CheckPattern(LSource,10,0);
    end;
  finally
    LBuffered.Free;
    if LDisposed=0 then LSource.Free;
  end;
  Check(LDisposed=1,'source disposal count is incorrect');
end;

procedure CheckReadScenario(AKind: Integer);
const Sizes: array[0..3] of Integer = (1,2,3,65536);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LBytes: TBytes;
  LSize, I, J, N, LTotal, LRequested: Integer;
  B: Byte;
  LRaised: Boolean;
  {$IFNDEF FPC}
  LOffsetBytes: TBytes;
  {$IF SizeOf(NativeInt)>SizeOf(Longint)}
  LNativeCount: NativeInt;
  {$IFEND}{$ENDIF}
begin
  for J := Low(Sizes) to High(Sizes) do begin
    LSize := Sizes[J];
    LSource := TObservedStream.Create;
    try
      if AKind=3 then LBytes := nil else LBytes := Pattern(2*LSize+17);
      Seed(LSource,LBytes);
      if AKind=0 then LSource.Position := 7;
      LSource.ResetCounts;
      LBuffered := TBufferedStream.Create(LSource,LSize);
      try
        B := 123;
        case AKind of
          0: begin
            Check(LBuffered.Position=7,'lost nonzero initial position');
            CheckPattern(LBuffered,9,7);
            Check(LBuffered.Position=16,'wrong logical position after initial-offset read');
          end;
          1: begin
            CheckPattern(LBuffered,1,0);
            Check(LBuffered.Position=1,'logical cursor followed physical read-ahead');
            Check((LSource.Position>=1) and (LSource.Position<=LSize),'unbounded physical read-ahead');
            Check(LBuffered.Size=Length(LBytes),'Size followed buffer contents');
          end;
          2: begin
            for I := 0 to Length(LBytes)-1 do begin
              Check(LBuffered.Read(B,1)=1,'single-byte read ended early');
              Check(B=LBytes[I],'single-byte read changed data');
            end;
            Check(LSource.ReadCalls<=(Length(LBytes)+LSize-1) div LSize,'single-byte reads did not collapse wrapped reads');
          end;
          3: begin
            B := 123;
            Check((LBuffered.Size=0) and (LBuffered.Position=0),'empty stream state changed');
            Check(LBuffered.Read(B,1)=0,'empty read did not return EOF');
            Check((B=123) and (LBuffered.Position=0),'EOF changed buffer/cursor');
          end;
          4: begin
            CheckPattern(LBuffered,Length(LBytes),0);
            Check(LBuffered.Position=Length(LBytes),'exact EOF cursor mismatch');
            B := 123;
            Check(LBuffered.Read(B,1)=0,'EOF did not return zero');
            Check((B=123) and (LBuffered.Position=Length(LBytes)),'EOF advanced cursor or overwrote buffer');
          end;
          5: begin
            LSource.ResetCounts;
            B := 123;
            Check(LBuffered.Read(B,0)=0,'zero read returned data');
            Check(LBuffered.Write(B,0)=0,'zero write returned data');
            Check((LSource.ReadCalls=0) and (LSource.WriteCalls=0),'zero count performed source I/O');
            Check((B=123) and (LBuffered.Position=0),'zero count changed state');
          end;
          6: begin
            LRaised := False;
            try
              LBuffered.Read(B,-1);
            except
              on E: EArgumentException do LRaised := True;
            end;
            Check(LRaised,'negative read count was accepted');
            LRaised := False;
            try
              LBuffered.Write(B,-1);
            except
              on E: EArgumentException do LRaised := True;
            end;
            Check(LRaised,'negative write count was accepted');
            Check((LSource.ReadCalls=0) and (LSource.WriteCalls=0) and (LBuffered.Position=0),'negative count touched the source');
          end;
          7: begin
            SetLength(LBytes,LSize+5);
            LBuffered.ReadBuffer(LBytes[0],Length(LBytes));
            for I := 0 to Length(LBytes)-1 do Check(LBytes[I]=Byte(I mod 251),'ReadBuffer boundary mismatch');
            {$IFNDEF FPC}
            LBuffered.Position := 0;
            SetLength(LOffsetBytes,7);
            FillChar(LOffsetBytes[0],Length(LOffsetBytes),123);
            Check(LBuffered.Read(LOffsetBytes,2,3)=3,'derived TBytes Read overload count');
            Check((LOffsetBytes[0]=123) and (LOffsetBytes[1]=123) and
              (LOffsetBytes[2]=0) and (LOffsetBytes[3]=1) and (LOffsetBytes[4]=2) and
              (LOffsetBytes[5]=123),'derived TBytes Read ignored offset/boundaries');
            {$IF SizeOf(NativeInt)>SizeOf(Longint)}
            LNativeCount := 2;
            Check(LBuffered.Read(LOffsetBytes[0],LNativeCount)=LNativeCount,'derived NativeInt Read overload count');
            Check((LOffsetBytes[0]=3) and (LOffsetBytes[1]=4),'derived NativeInt Read bypassed logical cursor');
            {$IFEND}{$ENDIF}
          end;
          8: begin
            LSource.MaxRead := 1;
            SetLength(LBytes,11);
            LTotal := 0;
            repeat
              LRequested := 1+(LTotal mod Length(LBytes));
              N := LBuffered.Read(LBytes[0],LRequested);
              Check((N>=0) and (N<=LRequested),'invalid short-read length');
              for I := 0 to N-1 do Check(LBytes[I]=Byte((LTotal+I) mod 251),'short reads lost or repeated data');
              Inc(LTotal,N);
            until N=0;
            Check(LTotal=2*LSize+17,'short source read was treated as permanent EOF');
          end;
        end;
      finally
        LBuffered.Free;
      end;
    finally
      LSource.Free;
    end;
  end;
end;

procedure CheckSeekScenario(AKind: Integer);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LReads, LSeeks: Integer;
  B: Byte;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(100));
    LBuffered := TBufferedStream.Create(LSource,16);
    try
      CheckPattern(LBuffered,1,0);
      case AKind of
        0: begin
          Check(LBuffered.Seek(20,soBeginning)=20,'absolute seek result');
          CheckPattern(LBuffered,3,20);
          Check(LBuffered.Seek(-2,soCurrent)=21,'relative seek result');
          CheckPattern(LBuffered,2,21);
          Check(LBuffered.Seek(-3,soEnd)=97,'end seek result');
          CheckPattern(LBuffered,3,97);
          Check(LBuffered.Seek(Longint(9),Word(0))=9,'legacy32-bit Seek overload result');
          CheckPattern(LBuffered,2,9);
        end;
        1: begin
          Check(LBuffered.Position=1,'cannot anchor the retained window');
          LReads := LSource.ReadCalls;
          LSeeks := LSource.SeekCalls;
          LBuffered.Position := 8;
          CheckPattern(LBuffered,1,8);
          LBuffered.Position := 0;
          CheckPattern(LBuffered,1,0);
          LBuffered.Position := 15;
          CheckPattern(LBuffered,1,15);
          Check((LSource.ReadCalls=LReads) and (LSource.SeekCalls=LSeeks),
            'seeking within a known retained window performed underlying I/O');
          CheckPattern(LBuffered,1,16);
        end;
        2: begin
          LBuffered.Position := 73;
          CheckPattern(LBuffered,7,73);
          LBuffered.Position := 2;
          CheckPattern(LBuffered,5,2);
        end;
        3: begin
          Check(LBuffered.Seek(200,soBeginning)=200,'beyond-EOF seek result');
          Check((LBuffered.Position=200) and (LBuffered.Size=100),'beyond-EOF seek resized source');
          Check(LBuffered.Read(B,1)=0,'beyond-EOF read returned data');
          Check(LBuffered.Position=200,'beyond-EOF read moved cursor');
          LBuffered.Seek(-2,soEnd);
          CheckPattern(LBuffered,2,98);
        end;
      end;
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckMutation(AKind: Integer);
var LSource: TObservedStream;
  LGold: TMemoryStream;
  LBuffered: TBufferedStream;
  LBytes: TBytes;
  LReadBytes: array[0..4] of Byte;
  B: Byte;
  I: Integer;
  {$IFNDEF FPC}{$IF SizeOf(NativeInt)>SizeOf(Longint)}
  LNativeCount: NativeInt;
  {$IFEND}{$ENDIF}
begin
  LSource := TObservedStream.Create;
  LGold := TMemoryStream.Create;
  try
    Seed(LSource,Pattern(20));
    Seed(LGold,Pattern(20));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      CheckPattern(LBuffered,1,0);
      LGold.Position := 1;
      LBytes := Pattern(5);
      for I := 0 to High(LBytes) do LBytes[I] := Byte(240+I);
      case AKind of
        0: begin
          Check(LBuffered.Write(LBytes[0],3)=3,'write count changed');
          LGold.WriteBuffer(LBytes[0],3);
          {$IFNDEF FPC}
          Check(LBuffered.Write(LBytes,1,3)=3,'derived TBytes Write overload count');
          LGold.WriteBuffer(LBytes[1],3);
          {$IF SizeOf(NativeInt)>SizeOf(Longint)}
          LNativeCount := 2;
          Check(LBuffered.Write(LBytes[0],LNativeCount)=LNativeCount,'derived NativeInt Write overload count');
          LGold.WriteBuffer(LBytes[0],LNativeCount);
          {$IFEND}{$ENDIF}
        end;
        1: begin
          LBuffered.Position := 0;
          LGold.Position := 0;
          LBuffered.WriteBuffer(LBytes[0],5);
          LGold.WriteBuffer(LBytes[0],5);
          LBuffered.Position := 0;
          LBuffered.ReadBuffer(LReadBytes,SizeOf(LReadBytes));
          Check(CompareMem(@LReadBytes,@LBytes[0],SizeOf(LReadBytes)),'write left stale bytes in retained window');
          LGold.Position := 5;
        end;
        2: begin
          LSource.MaxWrite := 2;
          Check(LBuffered.Write(LBytes[0],5)=2,'short write count was hidden');
          LGold.WriteBuffer(LBytes[0],2);
          LSource.MaxWrite := 0;
        end;
        3: begin
          LBuffered.Position := 7;
          LGold.Position := 7;
          LBuffered.Size := 3;
          LGold.Size := 3;
        end;
        4: begin
          LBuffered.Size := 30;
          LGold.Size := 30;
          LBuffered.Position := 20;
          LGold.Position := 20;
          LBuffered.WriteBuffer(LBytes[0],5);
          LGold.WriteBuffer(LBytes[0],5);
          LBuffered.WriteBuffer(LBytes[0],5);
          LGold.WriteBuffer(LBytes[0],5);
        end;
      end;
      Check(LBuffered.Position=LGold.Position,'mutation left incorrect logical cursor');
      Check(LBuffered.Size=LGold.Size,'mutation left incorrect size');
      CheckMemoryEqual(LSource,LGold);
      LBuffered.Position := 0;
      LGold.Position := 0;
      while LGold.Position<LGold.Size do begin
        LGold.ReadBuffer(B,1);
        LBuffered.ReadBuffer(LBytes[0],1);
        Check(B=LBytes[0],'mutation left stale buffered data');
      end;
    finally
      LBuffered.Free;
    end;
  finally
    LGold.Free;
    LSource.Free;
  end;
end;

procedure CheckSourceFailure(AKind: Integer);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LRaised: Boolean;
  B: Byte;
  LExpected: String;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(20));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      B := 123;
      if AKind<>0 then CheckPattern(LBuffered,1,0);
      LRaised := False;
      case AKind of
        0: begin
          LSource.FailRead := True;
          LExpected := 'source-read';
        end;
        1,4: begin
          LSource.FailWrite := True;
          LExpected := 'source-write';
        end;
        2: begin
          LSource.FailResize := True;
          LExpected := 'source-resize';
        end;
        3: begin
          LSource.FailSize := True;
          LExpected := 'source-size';
        end;
      end;
      try
        case AKind of
          0: LBuffered.Read(B,1);
          1,4: LBuffered.Write(B,1);
          2: LBuffered.Size := 3;
          3: Check(LBuffered.Size=20,'size query result');
        end;
      except
        on E: ESourceFailure do begin
          Check(E.Message=LExpected,'wrapped exception identity/message changed');
          LRaised := True;
        end;
      end;
      Check(LRaised,'wrapped failure did not propagate');
      LSource.FailRead := False;
      LSource.FailWrite := False;
      LSource.FailResize := False;
      LSource.FailSize := False;
      if AKind=0 then Check(LBuffered.Position=0,'failed read advanced cursor')
      else Check(LBuffered.Position=1,'failed mutation advanced cursor');
      Check(LBuffered.Size=20,'failed operation changed size');
      if AKind=0 then CheckPattern(LBuffered,5,0)
      else CheckPattern(LBuffered,5,1);
    finally
      LSource.FailRead := False;
      LSource.FailWrite := False;
      LSource.FailResize := False;
      LSource.FailSize := False;
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckSparse;
var LSource: TSparseStream;
  LBuffered: TBufferedStream;
  LStart: Int64;
  I: Integer;
  LBytes: array[0..16] of Byte;
begin
  LSource := TSparseStream.Create;
  try
    LStart := (Int64(1) shl 33)+123;
    LSource.Position := LStart;
    LBuffered := TBufferedStream.Create(LSource,3);
    try
      Check(LBuffered.Position=LStart,'initial64-bit cursor truncated');
      Check(LBuffered.Size=(Int64(1) shl 40)+123,'64-bit size truncated');
      LBuffered.ReadBuffer(LBytes,SizeOf(LBytes));
      for I := 0 to High(LBytes) do
        Check(LBytes[I]=Byte(((LStart+I) xor ((LStart+I) shr 8)) and 255),'sparse64-bit read mismatch');
      Check(LBuffered.Seek(-2,soEnd)=(Int64(1) shl 40)+121,'64-bit end seek truncated');
      Check(LBuffered.Read(LBytes,SizeOf(LBytes))=2,'sparse EOF count mismatch');
      Check(LSource.LargestRead<=SizeOf(LBytes),'read-ahead copied a huge sparse source');
      Check(LSource.ReadCalls<30,'small sparse reads scanned the source');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

function NextRandom(var AState: UInt32; AMaximum: Integer): Integer;
begin
  AState := AState xor (AState shl 13);
  AState := AState xor (AState shr 17);
  AState := AState xor (AState shl 5);
  Result := Integer(AState and $7FFFFFFF) mod AMaximum;
end;

procedure CheckMixed;
const Sizes: array[0..3] of Integer = (1,2,3,65536);
var LSource: TObservedStream;
  LGold: TMemoryStream;
  LBuffered: TBufferedStream;
  LState: UInt32;
  A, B: array[0..22] of Byte;
  I, J, K, LKind, LCount, LReadA, LReadB: Integer;
  LOffset: Int64;
begin
  for J := Low(Sizes) to High(Sizes) do begin
    LSource := TObservedStream.Create;
    LGold := TMemoryStream.Create;
    try
      Seed(LSource,Pattern(128));
      Seed(LGold,Pattern(128));
      LBuffered := TBufferedStream.Create(LSource,Sizes[J]);
      try
        LState := $12345678;
        for I := 1 to 1000 do begin
          LKind := NextRandom(LState,7);
          LCount := NextRandom(LState,Length(A)+1);
          case LKind of
            0,1: begin
              FillChar(A,SizeOf(A),113);
              FillChar(B,SizeOf(B),113);
              LReadA := LBuffered.Read(A,LCount);
              LReadB := LGold.Read(B,LCount);
              Check((LReadA=LReadB) and CompareMem(@A,@B,SizeOf(A)),'differential read mismatch at step '+IntToStr(I));
            end;
            2: begin
              LOffset := NextRandom(LState,Integer(LGold.Size)+1);
              Check(LBuffered.Seek(LOffset,soBeginning)=LGold.Seek(LOffset,soBeginning),'differential beginning seek');
            end;
            3: begin
              LOffset := -NextRandom(LState,Integer(LGold.Size)+1);
              Check(LBuffered.Seek(LOffset,soEnd)=LGold.Seek(LOffset,soEnd),'differential end seek');
            end;
            4: begin
              for K := 0 to LCount-1 do A[K] := Byte(NextRandom(LState,256));
              Check(LBuffered.Write(A,LCount)=LGold.Write(A,LCount),'differential write count');
            end;
            5: begin
              LOffset := NextRandom(LState,Integer(LGold.Size)+1);
              LBuffered.Size := LOffset;
              LGold.Size := LOffset;
            end;
            6: begin
              LOffset := NextRandom(LState,Integer(LGold.Size)+1);
              LBuffered.Position := LOffset;
              LGold.Position := LOffset;
            end;
          end;
          Check(LBuffered.Position=LGold.Position,'differential cursor mismatch at step '+IntToStr(I));
          Check(LBuffered.Size=LGold.Size,'differential size mismatch at step '+IntToStr(I));
          CheckMemoryEqual(LSource,LGold);
        end;
      finally
        LBuffered.Free;
      end;
    finally
      LGold.Free;
      LSource.Free;
    end;
  end;
end;

procedure CheckWriteOnly;
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LBytes: TBytes;
begin
  LSource := TObservedStream.Create;
  try
    LSource.FailRead := True;
    LBuffered := TBufferedStream.Create(LSource,3);
    try
      LBytes := Pattern(10);
      LBuffered.WriteBuffer(LBytes[0],Length(LBytes));
      Check((LSource.ReadCalls=0) and (LBuffered.Position=10) and (LBuffered.Size=10),'write-through performed hidden reads');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckForwardOnly(AKind: Integer);
var LSource: TForwardStream;
  LBuffered: TBufferedStream;
  B: Byte;
  I, LRaised: Integer;
begin
  LSource := TForwardStream.Create;
  try
    if AKind=3 then LBuffered := TBufferedStream.Create(LSource,1)
    else LBuffered := TBufferedStream.Create(LSource,8);
    try
      B := 92;
      Check((LSource.SeekCalls=0) and (LSource.SizeCalls=0),
        'construction probed unsupported source capabilities');
      if AKind=4 then begin
        B := 91;
        Check(LBuffered.Write(B,1)=1,'forward-only direct write failed');
        Check((LSource.WriteCalls=1) and (LSource.LastWritePosition=0) and (LSource.SeekCalls=0),
          'direct write required seeking or used a wrong cursor');
        Exit;
      end;
      CheckPattern(LBuffered,1,0);
      if AKind=0 then begin
        CheckPattern(LBuffered,23,1);
        Check(LBuffered.Read(B,1)=0,'forward-only EOF incorrect');
        Check((LSource.SeekCalls=0) and (LSource.SizeCalls=0),'forward-only reads required seeking or size');
      end else if AKind=1 then begin
        LRaised := 0;
        for I := 0 to 2 do begin
          try
            case I of
              0: Check(LBuffered.Position=1,'forward position query');
              1: LBuffered.Seek(0,soBeginning);
              2: Check(LBuffered.Size=24,'forward size query');
            end;
          except
            on E: ESourceFailure do begin
              if I=2 then Check(E.Message='forward-size','changed unsupported Size exception')
              else Check(E.Message='forward-seek','changed unsupported Position/Seek exception');
              Inc(LRaised);
            end;
          end;
          CheckPattern(LBuffered,1,I+1);
        end;
        Check(LRaised=3,'unsupported source capability was silently invented');
        CheckPattern(LBuffered,3,4);
        Check(LSource.ReadCalls=1,'unsupported capability query discarded unread data');
      end else if AKind=3 then begin
        B := 92;
        Check(LBuffered.Write(B,1)=1,'fully consumed forward buffer could not write');
        Check((LSource.SeekCalls=0) and (LSource.WriteCalls=1) and (LSource.LastWritePosition=1),
          'fully consumed buffer write required seeking');
      end else begin
        LRaised := 0;
        try
          case AKind of
            2: LBuffered.Write(B,1);
            5: LBuffered.SynchronizePosition;
            6: LBuffered.Size := 10;
          end;
        except
          on E: ESourceFailure do begin
            Check(E.Message='forward-seek','changed unread-buffer synchronization failure');
            Inc(LRaised);
          end;
        end;
        Check(LRaised=1,'unread forward buffer was silently discarded');
        Check((LSource.WriteCalls=0) and (LSource.ResizeCalls=0),
          'unsupported rewind mutated the underlying stream');
        CheckPattern(LBuffered,3,1);
        Check(LSource.ReadCalls=1,'failed rewind discarded retained bytes');
      end;
    finally
      I := LSource.SeekCalls;
      LBuffered.Free;
      Check(LSource.SeekCalls=I,'destruction implicitly sought a forward-only source');
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckExplicitSynchronization(AFail: Boolean);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LRaised: Boolean;
  LReads: Integer;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      CheckPattern(LBuffered,1,0);
      Check(LSource.Position=8,'expected real read-ahead before synchronization');
      LReads := LSource.ReadCalls;
      LSource.FailSeek := AFail;
      LRaised := False;
      try
        LBuffered.SynchronizePosition;
      except
        on E: ESourceFailure do begin
          Check(E.Message='source-seek','changed explicit synchronization exception');
          LRaised := True;
        end;
      end;
      Check(LRaised=AFail,'explicit synchronization failure contract changed');
      LSource.FailSeek := False;
      Check(LBuffered.Position=1,'explicit synchronization changed logical position');
      if not AFail then Check(LSource.Position=1,'explicit synchronization did not align wrapped position');
      CheckPattern(LBuffered,3,1);
      if AFail then Check(LSource.ReadCalls=LReads,'failed synchronization discarded valid retained bytes');
    finally
      LSource.FailSeek := False;
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckBorrowedDestruction;
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LPhysical: Int64;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      Check(LBuffered.WrappedStream=LSource,'WrappedStream property changed');
      Check((LBuffered.BufferSize=8) and not LBuffered.OwnsStream,'constructor properties changed');
      CheckPattern(LBuffered,1,0);
      LPhysical := LSource.Position;
      LSource.ResetCounts;
      LSource.FailSeek := True;
    finally
      // Disposal releases ownership only. SynchronizePosition is explicit so
      // borrowed pipes remain usable and cleanup never hides an I/O exception.
      LBuffered.Free;
    end;
    Check(LSource.SeekCalls=0,'borrowed destruction sought implicitly');
    LSource.FailSeek := False;
    Check((LPhysical=8) and (LSource.Position=LPhysical),'borrowed destruction rewound physical read-ahead');
  finally
    LSource.Free;
  end;
end;

procedure CheckPartialFailure(AKind: Integer);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LBytes: array[0..4] of Byte;
  LRaised: Boolean;
  LExpected: String;
  LPosition: Integer;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      Check(LBuffered.Position=0,'initial partial-failure cursor');
      FillChar(LBytes,SizeOf(LBytes),99);
      LPosition := 0;
      LExpected := '';
      case AKind of
        0: begin
          LSource.FailRead := True;
          LSource.MoveBeforeReadFailure := True;
          LExpected := 'source-read';
          LPosition := 2;
        end;
        1: begin
          CheckPattern(LBuffered,1,0);
          LSource.FailWrite := True;
          LSource.MoveBeforeWriteFailure := True;
          LExpected := 'source-write';
          LPosition := 3;
        end;
        2: begin
          CheckPattern(LBuffered,1,0);
          LSource.FailSeek := True;
          LSource.MoveBeforeSeekFailure := True;
          LExpected := 'source-seek';
          LPosition := 13;
        end;
      end;
      LRaised := False;
      try
        case AKind of
          0: LBuffered.Read(LBytes,SizeOf(LBytes));
          1: LBuffered.Write(LBytes,SizeOf(LBytes));
          2: LBuffered.Seek(17,soBeginning);
        end;
      except
        on E: ESourceFailure do begin
          Check(E.Message=LExpected,'partial-I/O source exception was replaced');
          LRaised := True;
        end;
      end;
      Check(LRaised,'partial-I/O exception did not propagate');
      LSource.FailRead := False;
      LSource.FailWrite := False;
      LSource.FailSeek := False;
      if AKind=2 then begin
        // An arbitrary failing seek may already have moved the source. Retain
        // pending bytes, then restore an absolute anchor before using Position.
        CheckPattern(LBuffered,3,1);
        LBuffered.Seek(LPosition,soBeginning);
      end;
      Check(LBuffered.Position=LPosition,'failed I/O retained a stale logical cursor');
      CheckPattern(LBuffered,3,LPosition);
    finally
      LSource.FailRead := False;
      LSource.FailWrite := False;
      LSource.FailSeek := False;
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckNegativeSeekResult(AKind: Integer);
var LSource: TForwardStream;
  LBuffered: TBufferedStream;
  LSeeks, I: Integer;
  LRaised: Boolean;
  B: Byte;
begin
  LSource := TForwardStream.Create;
  try
    LSource.ReturnNegativeSeek := True;
    LSource.ReturnNegativeSize := True;
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      B := 119;
      CheckPattern(LBuffered,1,0);
      case AKind of
        0: begin
          LSeeks := LSource.SeekCalls;
          Check(LBuffered.Position=-1,'negative Position was adjusted by unread data');
          CheckPattern(LBuffered,1,1);
          Check(LBuffered.Position=-1,'failed Position was cached as a real cursor');
          Check(LSource.SeekCalls=LSeeks+2,'negative Position suppressed a later capability query');
          CheckPattern(LBuffered,3,2);
        end;
        1: begin
          Check(LBuffered.Seek(0,soBeginning)=-1,'negative Seek result was changed');
          CheckPattern(LBuffered,3,1);
          Check(LBuffered.Size=-1,'negative Size result was changed');
          CheckPattern(LBuffered,3,4);
          Check(LSource.ReadCalls=1,'negative capability result discarded buffered bytes');
        end;
        2: begin
          for I := 0 to 1 do begin
            LRaised := False;
            try
              if I=0 then LBuffered.Write(B,1)
              else LBuffered.Size := 5;
            except
              on E: EStreamError do LRaised := True;
            end;
            Check(LRaised,'negative synchronization result allowed a direction change');
            Check((LSource.WriteCalls=0) and (LSource.ResizeCalls=0),
              'negative synchronization result mutated its source');
          end;
          CheckPattern(LBuffered,3,1);
        end;
      end;
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckNegativeIoResult(AWrite: Boolean);
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LBytes: array[0..4] of Byte;
  LExpectedPosition, LSeeks: Integer;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      Check(LBuffered.Position=0,'cannot anchor negative-I/O fixture');
      FillChar(LBytes,SizeOf(LBytes),99);
      if AWrite then begin
        CheckPattern(LBuffered,1,0);
        LSource.NegativeWriteResult := True;
        Check(LBuffered.Write(LBytes,SizeOf(LBytes))=-1,'negative Write result was changed');
        LSource.NegativeWriteResult := False;
        LExpectedPosition := 3;
      end else begin
        LSource.NegativeReadResult := True;
        LSeeks := LSource.SeekCalls;
        Check(LBuffered.Read(LBytes,SizeOf(LBytes))=-1,'negative Read result was changed');
        Check(LSource.SeekCalls=LSeeks,'negative read performed recovery I/O');
        LSource.NegativeReadResult := False;
        LExpectedPosition := 2;
      end;
      Check(LBuffered.Position=LExpectedPosition,'negative I/O retained a stale logical cursor');
      CheckPattern(LBuffered,3,LExpectedPosition);
    finally
      LSource.NegativeReadResult := False;
      LSource.NegativeWriteResult := False;
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckSingleSourceRead;
var LSource: TObservedStream;
  LBuffered: TBufferedStream;
  LBytes: array[0..4095] of Byte;
  I: Integer;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(256));
    LSource.MaxRead := 30;
    LSource.ResetCounts;
    LSource.RejectSecondRead := True;
    LBuffered := TBufferedStream.Create(LSource,64);
    try
      // The next source read would block a peer. Throw deterministically if it
      // is attempted so this test proves one-call behavior without timing.
      Check(LBuffered.Read(LBytes,SizeOf(LBytes))=30,'positive short read did not return available bytes');
      Check(LSource.ReadCalls=1,'one caller Read attempted multiple source reads');
      for I := 0 to 29 do Check(LBytes[I]=I,'positive short read changed data');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckUncachedSizeIsDynamic;
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,False);
    try
      Check(not LBuffered.EnableStreamLengthCaching,'explicitly disabled length caching was ignored');
      Check((LSource.SizeCalls=0) and (LSource.SeekCalls=0),'construction probed metadata');
      Check(LBuffered.Size=24,'uncached size was incorrect');
      LSource.Size := 31;
      Check(LBuffered.Size=31,'uncached size did not observe source growth');
      Check(LSource.SizeCalls=2,'uncached size did not forward both queries');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSizeQueriesOnce(AEmpty: Boolean);
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
  LExpected, LTotal: Int64;
  I, LInitialSizeSeeks: Integer;
  B: Byte;
begin
  LSource := TSizeObservedStream.Create;
  try
    if not AEmpty then Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True);
    try
      Check(LBuffered.EnableStreamLengthCaching and not LBuffered.OwnsStream,'constructor flags were not retained independently');
      Check((LSource.SizeCalls=0) and (LSource.SeekCalls=0),'cached construction probed metadata');
      if AEmpty then LExpected := 0 else LExpected := 24;
      // Delphi may seek and restore inside the first physical Size query.
      LTotal := LBuffered.Size;
      LInitialSizeSeeks := LSource.SeekCalls;
      for I := 2 to 1000000 do Inc(LTotal,LBuffered.Size);
      Check(LTotal=LExpected*1000000,'cached size queries changed the result');
      Check(LSource.SizeCalls=1,'cached size made more than one physical query');
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=LInitialSizeSeeks),'cached queries read input or moved the source');
      Check(LSource.Position=0,'size queries changed the source cursor');
      if not AEmpty then begin
        // Opting in means that out-of-band growth is deliberately not polled.
        LSource.Size := 31;
        Check(LBuffered.Size=24,'cached length unexpectedly polled external growth');
        Check(LSource.SizeCalls=1,'cached length queried after external growth');
        B := 99;
        LBuffered.WriteBuffer(B,1);
        Check(LSource.SizeCalls=1,'write proactively queried length');
        Check(LBuffered.Size=31,'mutation did not reload source length');
        Check(LSource.SizeCalls=2,'mutation did not invalidate the cached length');
      end;
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedConstructionDoesNotProbe;
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
  LRaised: Boolean;
  LUnusedSize: Int64;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LSource.FailSize := True;
    LSource.FailSeek := True;
    LBuffered := TBufferedStream.Create(LSource,8,False,True);
    try
      Check((LSource.SizeCalls=0) and (LSource.SeekCalls=0),'cached constructor probed unsupported metadata');
      CheckPattern(LBuffered,1,0);
      LRaised := False;
      try
        LUnusedSize := LBuffered.Size;
      except
        on E: ESourceFailure do LRaised := E.Message='source-size';
      end;
      Check(LRaised,'first cached Size query did not preserve the source failure');
      CheckPattern(LBuffered,3,1);
      Check(LSource.ReadCalls=1,'metadata failure lost unread buffered data');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSizeExceptionIsRetried(AReadBuffering: Boolean = True);
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
  LRaised: Boolean;
  LUnusedSize: Int64;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,AReadBuffering);
    try
      LSource.FailSize := True;
      LRaised := False;
      try
        LUnusedSize := LBuffered.Size;
      except
        on E: ESourceFailure do LRaised := E.Message='source-size';
      end;
      Check(LRaised and (LSource.SizeCalls=1),'failed Size query was hidden');
      LSource.FailSize := False;
      Check(LBuffered.Size=24,'Size query did not retry after failure');
      Check(LSource.SizeCalls=2,'failed length was cached');
      LSource.FailSize := True;
      Check(LBuffered.Size=24,'known cached length queried a failing source');
      Check(LSource.SizeCalls=2,'known cached length performed additional I/O');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedNegativeSizeIsRetried(AReadBuffering: Boolean = True);
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LSource.NegativeSize := True;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,AReadBuffering);
    try
      Check((LBuffered.Size=-1) and (LBuffered.Size=-1),'negative size sentinel was changed');
      Check(LSource.SizeCalls=2,'negative length was cached');
      LSource.NegativeSize := False;
      Check(LBuffered.Size=24,'valid size did not replace a negative result');
      LSource.NegativeSize := True;
      Check(LBuffered.Size=24,'valid cached size was not retained');
      Check(LSource.SizeCalls=3,'cached size queried the source again');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSizeAfterOverwriteAndAppend(AReadBuffering: Boolean = True);
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
  LBytes: array[0..3] of Byte;
begin
  FillChar(LBytes,SizeOf(LBytes),99);
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,AReadBuffering);
    try
      Check(LBuffered.Size=24,'cannot seed length cache');
      CheckPattern(LBuffered,1,0);
      LBuffered.WriteBuffer(LBytes,SizeOf(LBytes));
      Check(LSource.SizeCalls=1,'overwrite proactively queried Size');
      Check(LBuffered.Size=24,'overwrite changed the source length');
      Check(LSource.SizeCalls=2,'overwrite retained a possibly stale size');
      LBuffered.Seek(Int64(0),soEnd);
      LBuffered.WriteBuffer(LBytes,3);
      Check(LSource.SizeCalls=2,'append proactively queried Size');
      Check(LBuffered.Size=27,'append did not refresh the cached size');
      Check(LSource.SizeCalls=3,'append did not invalidate size');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSizeAfterWriteFailure(AMode: Integer; AReadBuffering: Boolean = True);
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
  LBytes: array[0..4] of Byte;
  LRaised: Boolean;
begin
  FillChar(LBytes,SizeOf(LBytes),99);
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,AReadBuffering);
    try
      Check(LBuffered.Size=24,'cannot seed failing-write length cache');
      LBuffered.Seek(Int64(0),soEnd);
      case AMode of
        0: LSource.MaxWrite := 2;
        1: LSource.NegativeWriteResult := True;
        2: begin
          LSource.FailWrite := True;
          LSource.MoveBeforeWriteFailure := True;
        end;
      end;
      LRaised := False;
      try
        case AMode of
          0: Check(LBuffered.Write(LBytes,SizeOf(LBytes))=2,'short write result changed');
          1: Check(LBuffered.Write(LBytes,SizeOf(LBytes))=-1,'negative write result changed');
          2: LBuffered.Write(LBytes,SizeOf(LBytes));
        end;
      except
        on E: ESourceFailure do LRaised := E.Message='source-write';
      end;
      Check((AMode<>2) or LRaised,'partial write exception was changed');
      Check(LSource.SizeCalls=1,'failing write performed hidden Size I/O');
      LSource.FailWrite := False;
      LSource.NegativeWriteResult := False;
      Check(LBuffered.Size=26,'partial write retained the pre-write cached length');
      Check(LSource.SizeCalls=2,'partial write did not force a new Size query');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSizeAfterResize(AReadBuffering: Boolean = True);
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,AReadBuffering);
    try
      Check(LBuffered.Size=24,'cannot seed resize length cache');
      CheckPattern(LBuffered,1,0);
      LBuffered.Size := 7;
      Check(LSource.SizeCalls=1,'truncate proactively queried Size');
      Check(LBuffered.Size=7,'truncate retained the old cached size');
      LBuffered.Size := 31;
      Check(LSource.SizeCalls=2,'expansion proactively queried Size');
      Check(LBuffered.Size=31,'expansion retained the old cached size');
      Check((LSource.SizeCalls=3) and (LSource.ResizeCalls=2),'resize did not reload each actual source length');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSizeAfterFailedAndIgnoredResize(AReadBuffering: Boolean = True);
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
  LRaised: Boolean;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,AReadBuffering);
    try
      Check(LBuffered.Size=24,'cannot seed partial-resize length cache');
      LSource.FailResizeAfterMutation := True;
      LRaised := False;
      try
        LBuffered.Size := 9;
      except
        on E: ESourceFailure do LRaised := E.Message='source-resize-after-mutation';
      end;
      Check(LRaised,'partial resize exception was changed');
      Check(LSource.SizeCalls=1,'failing resize queried metadata during unwind');
      LSource.FailResizeAfterMutation := False;
      Check(LBuffered.Size=9,'failed partial resize retained stale size');
      LSource.IgnoreResize := True;
      LBuffered.Size := 99;
      Check(LSource.SizeCalls=2,'ignored resize proactively queried Size');
      Check(LBuffered.Size=9,'requested size was cached without source confirmation');
      Check((LSource.SizeCalls=3) and (LSource.ResizeCalls=2),'failed/no-op resize did not requery actual length');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSizeAfterFailedSynchronization;
var LSource: TSizeObservedStream;
  LBuffered: TBufferedStream;
  B: Byte;
  I: Integer;
  LRaised: Boolean;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True);
    try
      Check(LBuffered.Size=24,'cannot seed rejected-mutation length cache');
      CheckPattern(LBuffered,1,0);
      LSource.FailSeek := True;
      B := 99;
      for I := 0 to 1 do begin
        LRaised := False;
        try
          if I=0 then LBuffered.Write(B,1)
          else LBuffered.Size := 9;
        except
          on E: ESourceFailure do LRaised := E.Message='source-seek';
        end;
        Check(LRaised,'failed synchronization was hidden by size caching');
      end;
      Check((LSource.WriteCalls=0) and (LSource.ResizeCalls=0),'rejected direction change mutated source');
      Check(LSource.SizeCalls=1,'rejected direction change queried source size');
      CheckPattern(LBuffered,3,1);
      Check(LSource.ReadCalls=1,'rejected direction change discarded unread data');
      LSource.FailSeek := False;
      Check(LBuffered.Size=24,'rejected mutation left an incorrect size');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckCachedSparseLengthAndZeroWrite(AReadBuffering: Boolean = True);
var LSource: TCountedSparseStream;
  LBuffered: TBufferedStream;
  LExpected: Int64;
  B: Byte;
begin
  LSource := TCountedSparseStream.Create;
  try
    LExpected := (Int64(1) shl 40)+123;
    LBuffered := TBufferedStream.Create(LSource,16,False,True,AReadBuffering);
    try
      Check((LBuffered.Size=LExpected) and (LBuffered.Size=LExpected),'cached sparse size was truncated');
      Check(LSource.SizeCalls=1,'sparse length was not cached');
      LBuffered.Position := LExpected-5;
      Check(LBuffered.Read(B,1)=1,'sparse cached stream was not readable');
      Check(LBuffered.Write(B,0)=0,'zero write result changed');
      Check(LBuffered.Size=LExpected,'read/zero-write changed cached length');
      Check(LSource.SizeCalls=1,'read/zero-write invalidated the length cache');
      Check(LSource.ReadCalls=1,'cached sparse read queried the input repeatedly');
      if AReadBuffering then
        Check(LSource.LargestRead=16,'buffered sparse read lost its configured capacity')
      else Check(LSource.LargestRead=1,'length-only caching enabled byte read-ahead');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDefaultBufferingEnabled;
var LSource: TObservedStream; LBuffered: TBufferedStream;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      Check(LBuffered.EnableReadBuffering and LBuffered.EnableStreamLengthCaching,'buffering flags were not both enabled by default');
      Check((LBuffered.Size=24) and (LBuffered.Size=24) and (LSource.SizeCalls=1),'default length cache was not reused');
      CheckPattern(LBuffered,1,0);
      Check((LSource.ReadCalls=1) and (LSource.LargestRead=8),'default buffering no longer reads ahead');
      CheckPattern(LBuffered,7,1);
      Check(LSource.ReadCalls=1,'default buffer did not serve retained bytes');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisabledConstructorDirectReads;
var LSource: TObservedStream; LBuffered: TBufferedStream;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      Check(not LBuffered.EnableReadBuffering,'constructor ignored disabled buffering');
      Check(LBuffered.BufferSize=8,'disabled constructor lost configured buffer size');
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'disabled constructor probed the input');
      CheckPattern(LBuffered,3,0);
      Check((LSource.ReadCalls=1) and (LSource.LargestRead=3),'disabled read changed the requested count');
      CheckPattern(LBuffered,14,3);
      Check((LSource.ReadCalls=2) and (LSource.LargestRead=14),'disabled large read was buffered or split');
      Check((LSource.SeekCalls=0) and (LSource.SizeCalls=0),'disabled reads queried metadata');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisabledShortReadsAndEOF;
var LSource: TObservedStream; LBuffered: TBufferedStream; LBytes: array[0..31] of Byte; LCount: Integer;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LSource.MaxRead := 3;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      LCount := LBuffered.Read(LBytes[0],Longint(10));
      Check((LCount=3) and (LBytes[0]=0) and (LBytes[2]=2),'disabled short read changed source data/count');
      Check((LSource.ReadCalls=1) and (LSource.LargestRead=10),'disabled short read retried the input');
      LSource.MaxRead := 0;
      CheckPattern(LBuffered,21,3);
      Check(LBuffered.Read(LBytes[0],Longint(10))=0,'disabled EOF did not return zero');
      Check(LSource.ReadCalls=3,'disabled short read or EOF made extra attempts');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisabledForwardOnlyReads;
var LSource: TForwardStream; LBuffered: TBufferedStream;
begin
  LSource := TForwardStream.Create;
  try
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      CheckPattern(LBuffered,1,0);
      CheckPattern(LBuffered,5,1);
      Check((LSource.ReadCalls=2) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'disabled forward reads required unsupported capabilities');
      LBuffered.EnableReadBuffering := True;
      Check((LSource.SeekCalls=0) and (LSource.SizeCalls=0),'enabling forward input probed metadata');
      CheckPattern(LBuffered,1,6);
      CheckPattern(LBuffered,7,7);
      Check(LSource.ReadCalls=3,'reenabled forward input did not read ahead');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisableUnreadRejectedWithoutIO;
var LSource: TObservedStream; LBuffered: TBufferedStream; LRaised: Boolean;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      CheckPattern(LBuffered,1,0);
      LSource.ResetCounts;
      LRaised := False;
      try
        LBuffered.EnableReadBuffering := False;
      except
        on E: EStreamError do LRaised := True;
      end;
      Check(LRaised and LBuffered.EnableReadBuffering,'disabling unread data was not rejected atomically');
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0) and (LSource.WriteCalls=0),'rejected disable performed source I/O');
      CheckPattern(LBuffered,7,1);
      Check(LSource.ReadCalls=0,'rejected disable lost retained bytes');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckFlushUnreadDataAllowsDisabling;
var LSource: TObservedStream; LBuffered: TBufferedStream;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      CheckPattern(LBuffered,1,0);
      LSource.ResetCounts;
      LBuffered.FlushUnreadData;
      Check((LSource.SeekCalls=1) and (LSource.ReadCalls=0),'explicit flush did not perform just the required rewind');
      Check(LBuffered.EnableReadBuffering,'flushing changed the buffering mode');
      LBuffered.EnableReadBuffering := False;
      Check((not LBuffered.EnableReadBuffering) and (LSource.SeekCalls=1),'disable after flush performed extra I/O');
      CheckPattern(LBuffered,1,1);
      Check((LSource.ReadCalls=1) and (LSource.LargestRead=1),'flushed cursor did not continue in direct mode');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckEmptyAndRepeatedTogglesDoNoIO;
var LSource: TObservedStream; LBuffered: TBufferedStream; I: Integer;
begin
  LSource := TObservedStream.Create;
  try
    LSource.FailRead := True;
    LSource.FailSeek := True;
    LSource.FailSize := True;
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      for I := 1 to 4 do begin
        LBuffered.EnableReadBuffering := True;
        LBuffered.EnableReadBuffering := False;
        LBuffered.EnableReadBuffering := False;
        LBuffered.FlushUnreadData;
        LBuffered.EnableReadBuffering := True;
      end;
      Check(LBuffered.EnableReadBuffering,'empty toggles lost the requested final mode');
      Check(LBuffered.BufferSize=8,'toggling discarded the configured buffer capacity');
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'empty/no-op toggle or flush probed the input');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckReenablePreservesDirectCursor;
var LSource: TObservedStream; LBuffered: TBufferedStream;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.Position := 5;
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      CheckPattern(LBuffered,3,5);
      LBuffered.EnableReadBuffering := True;
      Check(LSource.SeekCalls=0,'reenabling queried the source cursor');
      CheckPattern(LBuffered,1,8);
      CheckPattern(LBuffered,7,9);
      Check(LSource.ReadCalls=2,'reenabling did not retain one buffer');
      LSource.ResetCounts;
      LBuffered.FlushUnreadData;
      LBuffered.EnableReadBuffering := False;
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0),'consumed-buffer disable/flush performed I/O');
      CheckPattern(LBuffered,2,16);
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisabledSparseSeekOrigins;
var LSource: TSparseStream; LBuffered: TBufferedStream; LStart, LLength: Int64; LBytes: array[0..1] of Byte; I: Integer;
begin
  LSource := TSparseStream.Create;
  try
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      LStart := (Int64(1) shl 40)+17;
      LLength := LSource.Size;
      Check(LBuffered.Seek(LStart,soBeginning)=LStart,'disabled beginning seek truncated a 64-bit offset');
      Check(LBuffered.Read(LBytes[0],Longint(2))=2,'disabled sparse read failed');
      for I := 0 to 1 do
        Check(LBytes[I]=Byte(((LStart+I) xor ((LStart+I) shr 8)) and 255),'disabled sparse bytes differ');
      Check(LBuffered.Seek(-1,soCurrent)=LStart+1,'disabled current seek was incorrect');
      Check(LBuffered.Seek(-2,soEnd)=LLength-2,'disabled end seek was incorrect');
      Check(LBuffered.Read(LBytes[0],Longint(2))=2,'disabled sparse end read failed');
      Check(LSource.LargestRead=2,'disabled sparse read allocated/read ahead');
      Check(LBuffered.Position=LLength,'disabled Position was not the source cursor');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisabledWriteAndResize;
var LSource: TObservedStream; LGold: TMemoryStream; LBuffered: TBufferedStream; LData: array[0..1] of Byte; LRaised: Boolean;
begin
  LSource := TObservedStream.Create;
  LGold := TMemoryStream.Create;
  try
    Seed(LSource,Pattern(24));
    Seed(LGold,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      LBuffered.Position := 3;
      LGold.Position := 3;
      LData[0] := 201;
      LData[1] := 202;
      Check(LBuffered.Write(LData[0],Longint(2))=LGold.Write(LData[0],Longint(2)),'disabled write count differs');
      Check(LSource.ReadCalls=0,'disabled write read the input');
      LBuffered.Size := 12;
      LGold.Size := 12;
      Check(LBuffered.Position=LSource.Position,'disabled resize changed source cursor semantics');
      CheckMemoryEqual(LSource,LGold);
      LBuffered.Position := 11;
      Check(LBuffered.Read(LData[0],Longint(2))=1,'disabled truncate did not affect following reads');
      LSource.FailWrite := True;
      LRaised := False;
      try
        LBuffered.Write(LData[0],Longint(1));
      except
        on E: ESourceFailure do LRaised := E.Message='source-write';
      end;
      Check(LRaised,'disabled write changed the source exception');
      LSource.FailWrite := False;
      LSource.FailResize := True;
      LRaised := False;
      try
        LBuffered.Size := 7;
      except
        on E: ESourceFailure do LRaised := E.Message='source-resize';
      end;
      Check(LRaised,'disabled resize changed the source exception');
      LSource.FailResize := False;
    finally
      LBuffered.Free;
    end;
  finally
    LGold.Free;
    LSource.Free;
  end;
end;

procedure CheckDisabledReadExceptionIdentity;
var LSource: TInjectedReadFailureStream; LBuffered: TBufferedStream; LExpected: Exception; LRaised: Boolean; B: Byte;
begin
  LSource := TInjectedReadFailureStream.Create;
  try
    LExpected := ESourceFailure.Create('same-exception-object');
    LSource.Failure := LExpected;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      LRaised := False;
      try
        LBuffered.Read(B,Longint(1));
      except
        on E: Exception do begin
          LRaised := True;
          Check(E=LExpected,'disabled read replaced the exception object');
        end;
      end;
      Check(LRaised and (LSource.ReadCalls=1),'disabled read failed to forward exactly one source attempt');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisabledNegativePartialRead;
var LSource: TObservedStream; LBuffered: TBufferedStream; LData: array[0..3] of Byte;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      LSource.NegativeReadResult := True;
      Check(LBuffered.Read(LData[0],Longint(4))=-1,'disabled read changed a negative source result');
      Check(LSource.ReadCalls=1,'negative disabled read retried the source');
      LSource.NegativeReadResult := False;
      Check(LBuffered.Position=2,'disabled read concealed partial source advancement');
      LBuffered.EnableReadBuffering := True;
      CheckPattern(LBuffered,2,2);
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckFailedFlushPreservesUnread;
var LSource: TObservedStream; LBuffered: TBufferedStream; LRaised: Boolean;
begin
  LSource := TObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      CheckPattern(LBuffered,1,0);
      LSource.ResetCounts;
      LSource.FailSeek := True;
      LRaised := False;
      try
        LBuffered.FlushUnreadData;
      except
        on E: ESourceFailure do LRaised := E.Message='source-seek';
      end;
      Check(LRaised and LBuffered.EnableReadBuffering,'failed flush changed exception or mode');
      Check((LSource.SeekCalls=1) and (LSource.ReadCalls=0),'failed flush performed extra I/O');
      LRaised := False;
      try
        LBuffered.EnableReadBuffering := False;
      except
        on E: EStreamError do LRaised := True;
      end;
      Check(LRaised and (LSource.SeekCalls=1),'disable after failed flush attempted an automatic rewind');
      CheckPattern(LBuffered,7,1);
      Check(LSource.ReadCalls=0,'failed flush discarded unread bytes');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckForwardOnlyFlushPreservesUnread(ANegativeSeek: Boolean);
var LSource: TForwardStream; LBuffered: TBufferedStream; LRaised: Boolean;
begin
  LSource := TForwardStream.Create;
  try
    LSource.ReturnNegativeSeek := ANegativeSeek;
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      CheckPattern(LBuffered,1,0);
      LRaised := False;
      try
        LBuffered.EnableReadBuffering := False;
      except
        on E: EStreamError do LRaised := True;
      end;
      Check(LRaised and (LSource.SeekCalls=0),'unread forward disable touched the source');
      LRaised := False;
      try
        LBuffered.FlushUnreadData;
      except
        on E: EStreamError do LRaised := True;
      end;
      Check(LRaised and LBuffered.EnableReadBuffering,'failed forward flush changed mode');
      Check((LSource.SeekCalls=1) and (LSource.ReadCalls=1),'forward flush retried/read input');
      CheckPattern(LBuffered,7,1);
      Check(LSource.ReadCalls=1,'failed forward flush lost retained bytes');
      LBuffered.FlushUnreadData;
      LBuffered.EnableReadBuffering := False;
      Check(LSource.SeekCalls=1,'empty forward flush/disable attempted seeking');
      CheckPattern(LBuffered,1,8);
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDisabledReadBufferingCachesLength;
var LSource: TSizeObservedStream; LBuffered: TBufferedStream;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,False);
    try
      Check(LBuffered.EnableStreamLengthCaching and not LBuffered.EnableReadBuffering,'constructor flags were not independent');
      Check((LSource.SizeCalls=0) and (LSource.SeekCalls=0),'disabled cached constructor probed metadata');
      Check(LBuffered.Size=24,'disabled size query was incorrect');
      Check(LBuffered.Size=24,'direct-mode cached length changed');
      Check(LSource.SizeCalls=1,'disabled read buffering bypassed length caching');
      LSource.Size := 31;
      Check(LBuffered.Size=24,'length caching polled external growth in direct mode');
      Check(LSource.SizeCalls=1,'direct-mode cache queried the source again');
      LBuffered.EnableStreamLengthCaching := False;
      Check(LBuffered.Size=31,'disabling length caching did not query the source');
      Check(LSource.SizeCalls=2,'disabled length caching did not forward Size');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckReenableInvalidatesSizeAndPosition;
var LSource: TSizeObservedStream; LBuffered: TBufferedStream; LCalls: Integer;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True);
    try
      Check((LBuffered.Size=24) and (LBuffered.Position=0),'initial cached metadata was incorrect');
      LBuffered.EnableReadBuffering := False;
      LBuffered.Position := 11;
      LSource.Size := 31;
      Check(LBuffered.Size=31,'disabled size retained the old enabled cache');
      LSource.ResetCounts;
      LBuffered.EnableReadBuffering := True;
      Check((LSource.SizeCalls=0) and (LSource.SeekCalls=0),'reenabling proactively queried metadata');
      Check(LBuffered.Position=11,'reenabling retained the old logical cursor');
      Check(LBuffered.Size=31,'reenabling retained the old cached length');
      LCalls := LSource.SizeCalls;
      Check((LBuffered.Size=31) and (LSource.SizeCalls=LCalls),'reenabled size cache was not reused');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckToggledOwnership;
var LSource: TObservedStream; LBuffered: TBufferedStream; LDisposed, I: Integer; LOwns: Boolean;
begin
  for I := 0 to 1 do begin
    LDisposed := 0;
    LOwns := I=1;
    LSource := TObservedStream.Create(@LDisposed);
    try
      Seed(LSource,Pattern(24));
      LBuffered := TBufferedStream.Create(LSource,8,LOwns,False,False);
      try
        CheckPattern(LBuffered,1,0);
        LBuffered.EnableReadBuffering := True;
        CheckPattern(LBuffered,1,1);
        LBuffered.FlushUnreadData;
        LBuffered.EnableReadBuffering := False;
        Check(LBuffered.OwnsStream=LOwns,'toggle changed stream ownership');
      finally
        LBuffered.Free;
        if LOwns then LSource := nil;
      end;
      if LOwns then Check(LDisposed=1,'owned toggled source was not disposed once')
      else Check(LDisposed=0,'borrowed toggled source was disposed');
    finally
      LSource.Free;
    end;
    Check(LDisposed=1,'toggled source lifetime leaked or disposed twice');
  end;
end;

procedure CheckAlternatingModesMatchMemoryStream;
var LSource: TObservedStream; LGold: TMemoryStream; LBuffered: TBufferedStream; LActual, LExpected: array[0..15] of Byte; I, LCount, LRead, LGoldRead, LPosition: Integer;
begin
  LSource := TObservedStream.Create;
  LGold := TMemoryStream.Create;
  try
    Seed(LSource,Pattern(64));
    Seed(LGold,Pattern(64));
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      for I := 0 to 49 do begin
        if (I mod 3)=0 then begin
          if LBuffered.EnableReadBuffering then LBuffered.FlushUnreadData;
          LBuffered.EnableReadBuffering := not LBuffered.EnableReadBuffering;
        end;
        if (I mod 7)=0 then begin
          LPosition := (I*13) mod 64;
          LBuffered.Position := LPosition;
          LGold.Position := LPosition;
        end;
        LCount := (I mod 9)+1;
        LRead := LBuffered.Read(LActual[0],Longint(LCount));
        LGoldRead := LGold.Read(LExpected[0],Longint(LCount));
        Check(LRead=LGoldRead,'alternating modes returned a different read count');
        if LRead>0 then Check(CompareMem(@LActual[0],@LExpected[0],LRead),'alternating modes changed bytes');
        Check(LBuffered.Position=LGold.Position,'alternating modes changed logical position');
      end;
    finally
      LBuffered.Free;
    end;
  finally
    LGold.Free;
    LSource.Free;
  end;
end;

procedure CheckDisabledArgumentValidation;
var LSource: TObservedStream; LBuffered: TBufferedStream; B: Byte; LRaised: Boolean;
begin
  LSource := TObservedStream.Create;
  try
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      B := 77;
      Check((LBuffered.Read(B,Longint(0))=0) and (LBuffered.Write(B,Longint(0))=0),'disabled zero counts changed results');
      LRaised := False;
      try
        LBuffered.Read(B,Longint(-1));
      except
        on E: EArgumentOutOfRangeException do LRaised := True;
      end;
      Check(LRaised,'disabled negative read was accepted');
      LRaised := False;
      try
        LBuffered.Write(B,Longint(-1));
      except
        on E: EArgumentOutOfRangeException do LRaised := True;
      end;
      Check(LRaised,'disabled negative write was accepted');
      LRaised := False;
      try
        LBuffered.Size := -1;
      except
        on E: EArgumentOutOfRangeException do LRaised := True;
      end;
      Check(LRaised,'disabled negative size was accepted');
      Check((LSource.ReadCalls=0) and (LSource.WriteCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'disabled invalid/empty operations performed I/O');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckLengthCachingTogglesWithUnreadBytes;
var LSource: TSizeObservedStream; LBuffered: TBufferedStream; LReads, LSeeks, LSizes: Integer;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8);
    try
      Check(LBuffered.Size=24,'cannot prime the default length cache');
      CheckPattern(LBuffered,1,0);
      LReads := LSource.ReadCalls;
      LSeeks := LSource.SeekCalls;
      LSizes := LSource.SizeCalls;
      LBuffered.EnableStreamLengthCaching := False;
      LBuffered.EnableStreamLengthCaching := False;
      Check(LBuffered.EnableReadBuffering and not LBuffered.EnableStreamLengthCaching,'length toggle changed read buffering');
      Check((LSource.ReadCalls=LReads) and (LSource.SeekCalls=LSeeks) and (LSource.SizeCalls=LSizes),'length toggle performed I/O');
      LSource.Size := 31;
      Check((LBuffered.Size=31) and (LBuffered.Size=31),'disabled length cache did not observe the source');
      Check(LSource.SizeCalls=LSizes+2,'disabled length cache did not forward both queries');
      LBuffered.EnableStreamLengthCaching := True;
      Check(LBuffered.Size=31,'reenabled length cache reused its previous value');
      LSizes := LSource.SizeCalls;
      LBuffered.EnableStreamLengthCaching := True;
      Check((LBuffered.Size=31) and (LSource.SizeCalls=LSizes),'no-op length enable discarded the cached value');
      CheckPattern(LBuffered,7,1);
      Check(LSource.ReadCalls=LReads,'length toggles discarded unread buffered bytes');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckConvenienceModesAreAtomic;
var LSource: TSizeObservedStream; LBuffered: TBufferedStream; LRaised: Boolean;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      LBuffered.EnableBuffering;
      Check(LBuffered.EnableReadBuffering and LBuffered.EnableStreamLengthCaching,'EnableBuffering did not enable both flags');
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'EnableBuffering probed input');
      Check(LBuffered.Size=24,'cannot prime convenience length cache');
      CheckPattern(LBuffered,1,0);
      LSource.ResetCounts;
      LRaised := False;
      try
        LBuffered.DisableBuffering;
      except
        on E: EStreamError do LRaised := True;
      end;
      Check(LRaised and LBuffered.EnableReadBuffering and LBuffered.EnableStreamLengthCaching,'rejected convenience disable changed a flag');
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'rejected convenience disable performed I/O');
      Check((LBuffered.Size=24) and (LSource.SizeCalls=0),'rejected convenience disable invalidated the length cache');
      LBuffered.FlushUnreadData;
      LBuffered.DisableBuffering;
      Check(not LBuffered.EnableReadBuffering and not LBuffered.EnableStreamLengthCaching,'DisableBuffering did not disable both flags');
      Check(LBuffered.BufferSize=8,'convenience disable lost configured buffer capacity');
      LSource.ResetCounts;
      Check((LBuffered.Size=24) and (LBuffered.Size=24) and (LSource.SizeCalls=2),'disabled convenience mode cached length');
      LSource.ResetCounts;
      LBuffered.EnableBuffering;
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'convenience reenable probed input');
      Check((LBuffered.Size=24) and (LBuffered.Size=24) and (LSource.SizeCalls=1),'convenience reenable did not cache new length');
      CheckPattern(LBuffered,1,1);
      Check(LSource.LargestRead=8,'convenience reenable did not restore configured read-ahead');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckIndependentFlagCombination(AReadBuffering, ALengthCaching: Boolean);
var LSource: TSizeObservedStream; LBuffered: TBufferedStream;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,ALengthCaching,AReadBuffering);
    try
      Check((LSource.ReadCalls=0) and (LSource.SeekCalls=0) and (LSource.SizeCalls=0),'flag combination probed input during construction');
      Check((LBuffered.EnableReadBuffering=AReadBuffering) and (LBuffered.EnableStreamLengthCaching=ALengthCaching),'constructor coupled independent flags');
      CheckPattern(LBuffered,1,0);
      if AReadBuffering then
        Check(LSource.LargestRead=8,'read buffering did not read ahead')
      else Check(LSource.LargestRead=1,'length caching enabled read-ahead');
      Check((LBuffered.Size=24) and (LBuffered.Size=24),'flag combination changed Size');
      if ALengthCaching then
        Check(LSource.SizeCalls=1,'enabled length caching queried input twice')
      else Check(LSource.SizeCalls=2,'disabled length caching retained a length');
      Check(LBuffered.Position=1,'size queries changed the logical read cursor');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckLengthCachingTogglesWithoutReadBuffer;
var LSource: TSizeObservedStream; LBuffered: TBufferedStream; LCalls: Integer;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,False,False);
    try
      Check((LBuffered.Size=24) and (LBuffered.Size=24) and (LSource.SizeCalls=2),'uncached direct queries did not forward');
      LBuffered.EnableStreamLengthCaching := True;
      Check(not LBuffered.EnableReadBuffering,'enabling length caching allocated read buffering');
      Check((LBuffered.Size=24) and (LBuffered.Size=24) and (LSource.SizeCalls=3),'length-only enable did not cache a fresh query');
      LBuffered.EnableStreamLengthCaching := True;
      Check((LBuffered.Size=24) and (LSource.SizeCalls=3),'no-op enable discarded length-only cache');
      LBuffered.EnableStreamLengthCaching := False;
      LSource.Size := 31;
      Check((LBuffered.Size=31) and (LSource.SizeCalls=4),'length-only disable reused an old value');
      LBuffered.EnableStreamLengthCaching := True;
      Check((LBuffered.Size=31) and (LSource.SizeCalls=5),'length-only reenable reused an old value');
      LCalls := LSource.SizeCalls;
      LBuffered.EnableReadBuffering := True;
      Check(LSource.SizeCalls=LCalls,'read mode transition queried length eagerly');
      Check(LBuffered.EnableStreamLengthCaching and (LBuffered.Size=31),'read mode transition disabled length caching');
      LCalls := LSource.SizeCalls;
      Check((LBuffered.Size=31) and (LSource.SizeCalls=LCalls),'read mode transition did not retain the new cached value');
      LBuffered.EnableReadBuffering := False;
      Check(LBuffered.EnableStreamLengthCaching,'read disable changed the independent length flag');
      Check((LBuffered.Size=31) and (LSource.SizeCalls=LCalls+1),'read disable did not refresh invalidated metadata lazily');
      LCalls := LSource.SizeCalls;
      Check((LBuffered.Size=31) and (LSource.SizeCalls=LCalls),'read disable bypassed the new length cache');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure CheckDirectCachedLengthSurvivesRejectedMutations;
var LSource: TSizeObservedStream; LBuffered: TBufferedStream; B: Byte; LRaised: Boolean;
begin
  LSource := TSizeObservedStream.Create;
  try
    Seed(LSource,Pattern(24));
    LSource.ResetCounts;
    LBuffered := TBufferedStream.Create(LSource,8,False,True,False);
    try
      Check(LBuffered.Size=24,'cannot prime direct-mode length cache');
      B := 99;
      Check(LBuffered.Write(B,0)=0,'zero direct write changed its result');
      LRaised := False;
      try
        LBuffered.Write(B,-1);
      except
        on E: EArgumentOutOfRangeException do LRaised := True;
      end;
      Check(LRaised,'negative direct write was accepted');
      LRaised := False;
      try
        LBuffered.Size := -1;
      except
        on E: EArgumentOutOfRangeException do LRaised := True;
      end;
      Check(LRaised,'negative direct resize was accepted');
      Check((LSource.WriteCalls=0) and (LSource.ResizeCalls=0),'rejected direct mutations reached the input');
      Check((LBuffered.Size=24) and (LSource.SizeCalls=1),'rejected direct mutations invalidated known length');
    finally
      LBuffered.Free;
    end;
  finally
    LSource.Free;
  end;
end;

function BufferedStreamCheckName(AIndex: Integer): String;
const Names: array[1..BufferedStreamCheckCount] of String = (
  'NilSourceRejected','NonpositiveBufferRejectedWithoutOwnership','BorrowedSourceRemainsAlive','OwnedSourceDisposedOnce',
  'NonzeroInitialPosition','LogicalAndPhysicalCursors','SingleByteReadsCollapseSourceCalls','AllBufferBoundariesAndEOF',
  'EmptyStreamEOF','ZeroCountsPerformNoIO','NegativeCountsRejectedBeforeIO','ReadBufferCrossesBoundaries',
  'ShortReadsDoNotLoseData','AllSeekOrigins','SeekWithinRetainedWindow','SeekOutsideWindowAndBack',
  'SeekBeyondEOFDoesNotResize','WriteAfterReadUsesLogicalCursor','ReadAfterWriteRejectsStaleBuffer','ShortWriteReturnsActualCount',
  'SourceReadFailurePreserved','SourceWriteFailurePreserved','TruncatePreservesCursorSemantics','ExpandThenWritePreservesData',
  'ResizeFailurePreserved','SizeFailurePreserved','ReadOnlySourceStillReadable','Sparse64BitOffsetsAndBoundedReads',
  'DeterministicDifferentialOperations','OwnedSourceDisposedAfterReadError','WriteThroughDoesNotRead',
  'ForwardOnlyReadsPreserveCapabilities','BorrowedDestroyDoesNotSeek','ExplicitSynchronizationAlignsCursor',
  'UnsupportedSourceCapabilitiesPropagate','ForwardReadThenWriteFailsBeforeMutation','ConsumedForwardBufferCanWrite',
  'ForwardDirectWriteNeedsNoSeek','FailedForwardSynchronizationRetainsBuffer','FailedExplicitSynchronizationRetainsBuffer',
  'ForwardResizeFailsBeforeMutation','PartialReadFailureInvalidatesCursor','PartialWriteFailureInvalidatesCursor',
  'PartialSeekFailureInvalidatesCursor','NegativePositionIsNotAdjustedOrCached','NegativeSeekAndSizeRetainUnreadData',
  'NegativeSynchronizationPreventsMutation','NegativeReadResultInvalidatesCursor','NegativeWriteResultInvalidatesCursor',
  'PositiveShortReadMakesOneSourceAttempt','UncachedSizeQueriesRemainDynamic','CachedSizeQueriesSourceOnce',
  'CachedZeroLengthQueriesSourceOnce','CachedConstructionDoesNotProbe','CachedSizeExceptionIsRetried',
  'CachedNegativeSizeIsRetried','CachedSizeAfterOverwriteAndAppend','CachedSizeAfterShortWrite',
  'CachedSizeAfterNegativePartialWrite','CachedSizeAfterThrowingPartialWrite','CachedSizeAfterTruncateAndExpand',
  'CachedSizeAfterFailedAndIgnoredResize','CachedSizeAfterFailedSynchronization','CachedSparseLengthAndZeroWrite',
  'DefaultBufferingEnabled','DisabledConstructorDirectReads','DisabledShortReadsAndEOF','DisabledForwardOnlyReads',
  'DisableUnreadRejectedWithoutIO','FlushUnreadDataAllowsDisabling','EmptyAndRepeatedTogglesDoNoIO',
  'ReenablePreservesDirectCursor','DisabledSparseSeekOrigins','DisabledWriteAndResize','DisabledReadExceptionIdentity',
  'DisabledNegativePartialRead','FailedFlushPreservesUnread','ForwardOnlyFlushPreservesUnread','NegativeFlushPreservesUnread',
  'DisabledReadBufferingCachesLength','ReenableInvalidatesSizeAndPosition','ToggledOwnership','AlternatingModesMatchMemoryStream',
  'DisabledArgumentValidation','LengthCachingTogglesWithUnreadBytes','ConvenienceModesAreAtomic',
  'DirectReadsUncachedLength','DirectReadsCachedLength','BufferedReadsUncachedLength','BufferedReadsCachedLength',
  'DirectCachedLengthAfterOverwriteAndAppend','DirectCachedLengthAfterShortWrite','DirectCachedLengthAfterNegativePartialWrite',
  'DirectCachedLengthAfterThrowingPartialWrite','DirectCachedLengthAfterTruncateAndExpand','DirectCachedLengthAfterFailedAndIgnoredResize',
  'DirectCachedNegativeSizeIsRetried','DirectCachedSizeExceptionIsRetried','DirectCachedSparseLengthAndZeroWrite',
  'LengthCachingTogglesWithoutReadBuffer','DirectCachedLengthSurvivesRejectedMutations');
begin
  Check((AIndex>=Low(Names)) and (AIndex<=High(Names)),'invalid check index');
  Result := Names[AIndex];
end;

procedure RunBufferedStreamCheck(AIndex: Integer);
begin
  case AIndex of
    1: CheckArguments(False);
    2: CheckArguments(True);
    3: CheckOwnership(False,False);
    4: CheckOwnership(True,False);
    5: CheckReadScenario(0);
    6: CheckReadScenario(1);
    7: CheckReadScenario(2);
    8: CheckReadScenario(4);
    9: CheckReadScenario(3);
    10: CheckReadScenario(5);
    11: CheckReadScenario(6);
    12: CheckReadScenario(7);
    13: CheckReadScenario(8);
    14: CheckSeekScenario(0);
    15: CheckSeekScenario(1);
    16: CheckSeekScenario(2);
    17: CheckSeekScenario(3);
    18: CheckMutation(0);
    19: CheckMutation(1);
    20: CheckMutation(2);
    21: CheckSourceFailure(0);
    22: CheckSourceFailure(1);
    23: CheckMutation(3);
    24: CheckMutation(4);
    25: CheckSourceFailure(2);
    26: CheckSourceFailure(3);
    27: CheckSourceFailure(4);
    28: CheckSparse;
    29: CheckMixed;
    30: CheckOwnership(True,True);
    31: CheckWriteOnly;
    32: CheckForwardOnly(0);
    33: CheckBorrowedDestruction;
    34: CheckExplicitSynchronization(False);
    35: CheckForwardOnly(1);
    36: CheckForwardOnly(2);
    37: CheckForwardOnly(3);
    38: CheckForwardOnly(4);
    39: CheckForwardOnly(5);
    40: CheckExplicitSynchronization(True);
    41: CheckForwardOnly(6);
    42: CheckPartialFailure(0);
    43: CheckPartialFailure(1);
    44: CheckPartialFailure(2);
    45: CheckNegativeSeekResult(0);
    46: CheckNegativeSeekResult(1);
    47: CheckNegativeSeekResult(2);
    48: CheckNegativeIoResult(False);
    49: CheckNegativeIoResult(True);
    50: CheckSingleSourceRead;
    51: CheckUncachedSizeIsDynamic;
    52: CheckCachedSizeQueriesOnce(False);
    53: CheckCachedSizeQueriesOnce(True);
    54: CheckCachedConstructionDoesNotProbe;
    55: CheckCachedSizeExceptionIsRetried;
    56: CheckCachedNegativeSizeIsRetried;
    57: CheckCachedSizeAfterOverwriteAndAppend;
    58: CheckCachedSizeAfterWriteFailure(0);
    59: CheckCachedSizeAfterWriteFailure(1);
    60: CheckCachedSizeAfterWriteFailure(2);
    61: CheckCachedSizeAfterResize;
    62: CheckCachedSizeAfterFailedAndIgnoredResize;
    63: CheckCachedSizeAfterFailedSynchronization;
    64: CheckCachedSparseLengthAndZeroWrite;
    65: CheckDefaultBufferingEnabled;
    66: CheckDisabledConstructorDirectReads;
    67: CheckDisabledShortReadsAndEOF;
    68: CheckDisabledForwardOnlyReads;
    69: CheckDisableUnreadRejectedWithoutIO;
    70: CheckFlushUnreadDataAllowsDisabling;
    71: CheckEmptyAndRepeatedTogglesDoNoIO;
    72: CheckReenablePreservesDirectCursor;
    73: CheckDisabledSparseSeekOrigins;
    74: CheckDisabledWriteAndResize;
    75: CheckDisabledReadExceptionIdentity;
    76: CheckDisabledNegativePartialRead;
    77: CheckFailedFlushPreservesUnread;
    78: CheckForwardOnlyFlushPreservesUnread(False);
    79: CheckForwardOnlyFlushPreservesUnread(True);
    80: CheckDisabledReadBufferingCachesLength;
    81: CheckReenableInvalidatesSizeAndPosition;
    82: CheckToggledOwnership;
    83: CheckAlternatingModesMatchMemoryStream;
    84: CheckDisabledArgumentValidation;
    85: CheckLengthCachingTogglesWithUnreadBytes;
    86: CheckConvenienceModesAreAtomic;
    87: CheckIndependentFlagCombination(False,False);
    88: CheckIndependentFlagCombination(False,True);
    89: CheckIndependentFlagCombination(True,False);
    90: CheckIndependentFlagCombination(True,True);
    91: CheckCachedSizeAfterOverwriteAndAppend(False);
    92: CheckCachedSizeAfterWriteFailure(0,False);
    93: CheckCachedSizeAfterWriteFailure(1,False);
    94: CheckCachedSizeAfterWriteFailure(2,False);
    95: CheckCachedSizeAfterResize(False);
    96: CheckCachedSizeAfterFailedAndIgnoredResize(False);
    97: CheckCachedNegativeSizeIsRetried(False);
    98: CheckCachedSizeExceptionIsRetried(False);
    99: CheckCachedSparseLengthAndZeroWrite(False);
    100: CheckLengthCachingTogglesWithoutReadBuffer;
    101: CheckDirectCachedLengthSurvivesRejectedMutations;
  else
    raise EArgumentOutOfRangeException.Create('Invalid buffered stream check index');
  end;
end;

end.
