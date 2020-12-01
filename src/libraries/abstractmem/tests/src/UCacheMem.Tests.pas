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
      raise {$IFDEF FPC}Exception{$ELSE}ETestFailure{$ENDIF}.Create(Format('Value at pos %d (item %d) should be %d instead of %d',[ALoadedStartPos+i,i,((ALoadedStartPos+i) MOD 89),ABytes[i]]));
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
  if (Length(FCurrentMem) >= AStartPos + ASize) then begin
    Result := ASize;
    Move(FCurrentMem[AStartPos],ABuffer,ASize);
  end else begin
    Result := Length(FCurrentMem) - AStartPos;
    if Result>0 then begin
      Move(FCurrentMem[AStartPos],ABuffer,Result);
    end;
  end;
end;

function TestTCacheMem.OnSaveDataProc(const ABuffer; AStartPos, ASize: Integer): Integer;
begin
  if (Length(FCurrentMem) >= AStartPos + ASize) then begin
    Result := ASize;
    Move(ABuffer,FCurrentMem[AStartPos],ASize);
  end else begin
    Result := Length(FCurrentMem) - AStartPos;
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
    InitCurrentMem(22);
    SetLength(LBuff,Length(FCurrentMem));

    LCMem.DefaultCacheDataBlocksSize :=5;
    LCMem.GridCache := True;
    // Check replacing initial position of buffer on Load
    LCMem.Clear;

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],3,3) );
    CheckBytes(LBuff,3,3);

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],1,9) );
    CheckBytes(LBuff,1,9);

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],9,2) );
    CheckBytes(LBuff,9,2);

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],8,3) );
    CheckBytes(LBuff,8,3);

    // Check false and load final data
    FillChar(LBuff[0],Length(LBuff),0);
    CheckFalse( LCMem.LoadData(LBuff[0],Length(FCurrentMem)-3,4) );
    CheckBytes(LBuff,Length(FCurrentMem)-3,3);
    LCMem.ConsistencyCheck;

    // Load all to LBuff
    CheckTrue( LCMem.LoadData(LBuff[0],0,Length(LBuff)) );
    // Check replacing initial position of buffer on Save
    LCMem.Clear;
    LCMem.SaveToCache(LBuff[0],3,3,True);
    LCMem.SaveToCache(LBuff[0],7,0,True);

    // Check saving chunks
    LCMem.Clear;
    LCMem.DefaultCacheDataBlocksSize := 5;
    LCMem.GridCache := False;
    LCMem.SaveToCache(LBuff[2],5,2,True);
    LCMem.SaveToCache(LBuff[1],15,1,True);
    CheckTrue( LCMem.CacheDataBlocks=3, Format('3 Cache blocks: %d',[LCMem.CacheDataBlocks]));
    LCMem.Clear;
    LCMem.GridCache := True;
    LCMem.SaveToCache(LBuff[2],5,2,True);
    LCMem.SaveToCache(LBuff[1],15,1,True);
    CheckTrue( LCMem.CacheDataBlocks=4, Format('4 Cache blocks: %d',[LCMem.CacheDataBlocks]));
    LCMem.Clear;

    // Clear FCurrentMem
    LCMem.Clear;
    FillChar(FCurrentMem[0],Length(FCurrentMem),0);
    // Save from LBuff
    LCMem.SaveToCache(LBuff,0,True);
    LCMem.FlushCache;
    LCMem.ConsistencyCheck;

    LCMem.Clear;
    InitCurrentMem(100);
    SetLength(LBuff,Length(FCurrentMem));

    // Save 3 blocks
    LCMem.LoadData(LBuff[0],2,2*LCMem.DefaultCacheDataBlocksSize);
    LCMem.Clear;
    LCMem.SaveToCache(LBuff[0], 2*LCMem.DefaultCacheDataBlocksSize , 2,True);
    CheckTrue( LCMem.CacheDataBlocks=3, '3 Cache blocks');

    CheckTrue( LCMem.LoadData(LBuff[0],1,98) );
    // Incremental round
    i := 1;
    while (i+i < High(FCurrentMem)) do begin
      CheckTrue( LCMem.LoadData(LBuff[0],i-1,i) );
      CheckBytes(LBuff,i-1,i);
      inc(i);
    end;
    CheckFalse( LCMem.LoadData( LBuff[0],i+1,i) );

    LCMem.ConsistencyCheck;
  Finally
    LCMem.Free;
  End;
end;

initialization
  RegisterTest(TestTCacheMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
