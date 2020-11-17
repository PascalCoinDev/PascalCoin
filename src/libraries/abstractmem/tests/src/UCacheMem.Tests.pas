unit UCacheMem.Tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
 
 uses
   SysUtils,
   {$IFDEF FPC}
   fpcunit, testutils, testregistry,
   {$ELSE}
   TestFramework,
   {$ENDIF}
   UCacheMem;
 type
   // Test methods for class TCalc
   TestTCacheMem = class(TTestCase)
   strict private
     FCurrentMem : TBytes;
     function OnNeedDataProc(var ABuffer; AStartPos : Integer; ASize : Integer) : Integer;
     function OnSaveDataProc(const ABuffer; AStartPos : Integer; ASize : Integer) : Integer;
     procedure CheckBytes(const ABytes : TBytes; ALoadedStartPos, ASize : Integer);
     procedure InitCurrentMem(ASize : Integer);
   public
     procedure SetUp; override;
     procedure TearDown; override;
   published
     procedure TestCacheMem;
   end;

 implementation

procedure TestTCacheMem.CheckBytes(const ABytes: TBytes; ALoadedStartPos, ASize: Integer);
var i : Integer;
begin
  if ASize<=0 then ASize := Length(ABytes)
  else if ASize > Length(ABytes) then ASize := Length(ABytes);

  for i := 0 to ASize-1 do begin
    if (ABytes[i] <> ((ALoadedStartPos+i+1) MOD 89)) then begin
      raise ETestFailure.Create(Format('Value at pos %d (item %d) should be %d instead of %d',[ALoadedStartPos+i,i,((ALoadedStartPos+i) MOD 89),ABytes[i]]));
    end;

  end;

end;

procedure TestTCacheMem.InitCurrentMem(ASize: Integer);
var i : Integer;
begin
  SetLength(FCurrentMem,ASize);
  for i :=0 to High(FCurrentMem) do begin
    FCurrentMem[i] := ((i+1) MOD 89);
  end;
end;

function TestTCacheMem.OnNeedDataProc(var ABuffer; AStartPos, ASize: Integer): Integer;
begin
  if (High(FCurrentMem) >= AStartPos + ASize) then begin
    Result := ASize;
    Move(FCurrentMem[AStartPos],ABuffer,ASize);
  end else begin
    Result := High(FCurrentMem) - AStartPos;
    if Result>0 then begin
      Move(FCurrentMem[AStartPos],ABuffer,Result);
    end;
  end;
end;

function TestTCacheMem.OnSaveDataProc(const ABuffer; AStartPos, ASize: Integer): Integer;
begin
  if (High(FCurrentMem) >= AStartPos + ASize) then begin
    Result := ASize;
    Move(ABuffer,FCurrentMem[AStartPos],ASize);
  end else begin
    Result := High(FCurrentMem) - AStartPos;
    if Result>0 then begin
      Move(ABuffer,FCurrentMem[AStartPos],Result);
    end;
  end;
end;

procedure TestTCacheMem.SetUp;
begin
  SetLength(FCurrentMem,0);
end;

procedure TestTCacheMem.TearDown;
begin
  SetLength(FCurrentMem,0);
end;

procedure TestTCacheMem.TestCacheMem;
Var LCMem : TCacheMem;
  LBuff : TBytes;
  i : Integer;
begin
  LCMem := TCacheMem.Create(OnNeedDataProc,OnSaveDataProc);
  Try
    InitCurrentMem(11);
    SetLength(LBuff,Length(FCurrentMem));

    LCMem.DefaultCacheDataBlocksSize :=10;
    // Check replacing initial position of buffer on Load
    LCMem.Clear;
    LCMem.LoadData(LBuff[0],3,3);
    CheckBytes(LBuff,3,3);
    LCMem.LoadData(LBuff[0],1,9);
    CheckBytes(LBuff,1,9);
    LCMem.ConsistencyCheck;

    // Check replacing initial position of buffer on Save
    LCMem.Clear;
    LCMem.SaveToCache(LBuff[0],3,3,True);
    LCMem.SaveToCache(LBuff[0],7,0,True);
    LCMem.ConsistencyCheck;

    LCMem.Clear;
    InitCurrentMem(100000);
    SetLength(LBuff,Length(FCurrentMem));

    CheckTrue( LCMem.LoadData(LBuff[0],0,100) );
    // Incremental round
    i := 1;
    while (i+i < High(FCurrentMem)) do begin
      CheckTrue( LCMem.LoadData(LBuff[0],i,i) );
      inc(i);
    end;
    CheckFalse( LCMem.LoadData( LBuff[0],i,i) );

    LCMem.ConsistencyCheck;
  Finally
    LCMem.Free;
  End;
end;

initialization
  RegisterTest(TestTCacheMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
